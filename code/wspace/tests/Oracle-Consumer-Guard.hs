{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>), show)
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Use Plutus assoc map (txInfoData is a Plutus AssocMap)
import qualified PlutusTx.AssocMap as AssocMap

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

-- Oracle datum stored on-chain as a datum UTxO (reference input)
data OracleDatum = OracleDatum
    { odPair       :: BuiltinByteString   -- pair id (e.g., "ADA/USD")
    , odPriceNum   :: Integer             -- numerator
    , odPriceDen   :: Integer             -- denominator
    , odValidUntil :: POSIXTime           -- valid-until timestamp
    , odSigner     :: PubKeyHash          -- publisher's pubkey hash
    }
PlutusTx.unstableMakeIsData ''OracleDatum

-- Redeemer from consumer carries pair id to look up
newtype OracleRedeemer = OracleRedeemer BuiltinByteString
PlutusTx.unstableMakeIsData ''OracleRedeemer

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE decimalsValid #-}
decimalsValid :: Integer -> Integer -> Bool
decimalsValid _ den = den > 0 && den <= 1000000000 -- max 9 decimals

{-# INLINABLE datumMapToBuiltinDatas #-}
-- Extract builtin-data values from txInfoData map (AssocMap)
datumMapToBuiltinDatas :: TxInfo -> [BuiltinData]
datumMapToBuiltinDatas info =
    let m = txInfoData info                     -- AssocMap.Map DatumHash Datum
    in [ d | (_, Datum d) <- AssocMap.toList m ]     -- extract BuiltinData

{-# INLINABLE findOracleDatum #-}
-- Find the first OracleDatum in txInfoData that matches pair id
findOracleDatum :: BuiltinByteString -> TxInfo -> Maybe OracleDatum
findOracleDatum pid info =
    let bds = datumMapToBuiltinDatas info
        matches = filter (\bd -> let od = unsafeFromBuiltinData @OracleDatum bd
                                 in odPair od == pid) bds
    in case matches of
         (bd:_) -> Just (unsafeFromBuiltinData @OracleDatum bd)
         []     -> Nothing

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

-- Oracle consumer guard validator
-- Datum type (first arg) is arbitrary here (reusable snippet). We still
-- compile it as a normal validator so it can be written out as a script.
{-# INLINABLE mkValidator #-}
mkValidator :: OracleDatum -> OracleRedeemer -> ScriptContext -> Bool
mkValidator _ (OracleRedeemer pid) ctx =
    let info = scriptContextTxInfo ctx
        txRange = txInfoValidRange info

        mRef = findOracleDatum pid info

        -- presence check
        refPresent = traceIfFalse "oracle datum not found" (isJust mRef)

        -- freshness check (now <= validUntil)
        fresh = case mRef of
                  Just od -> traceIfFalse "oracle expired" (Interval.contains (Interval.to (odValidUntil od)) txRange)
                  Nothing -> False

        -- decimals check
        decOK = case mRef of
                  Just od -> traceIfFalse "bad decimals" (decimalsValid (odPriceNum od) (odPriceDen od))
                  Nothing -> False

        -- signer check: ensure the oracle signer signed this tx
        signerOK = case mRef of
                     Just od -> traceIfFalse "missing signer" (txSignedBy info (odSigner od))
                     Nothing -> False

    in refPresent && fresh && decOK && signerOK

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @OracleDatum d
        red = unsafeFromBuiltinData @OracleRedeemer r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

-- Compute plutus validator hash (using BuiltinByteString)
plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        strictBS = LBS.toStrict bytes                      -- ByteString
        builtin  = Builtins.toBuiltin strictBS             -- BuiltinByteString
    in PlutusV2.ValidatorHash builtin

-- On-chain script address
plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

-- Off-chain Bech32 (Cardano API) address for CLI use (explicit Babbage era)
toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised

        scriptHash :: C.ScriptHash
        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)

        -- explicit era annotation: Babbage
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

------------------------------------------------------------------------
-- File writing
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "oracle-consumer-guard.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Oracle Consumer Guard Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Oracle Consumer Guard validator generated successfully."
