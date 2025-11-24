{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T
import Data.Maybe hiding (isJust)  -- hide Haskell's isJust to avoid ambiguity

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless, isJust)
import qualified PlutusTx.Prelude as PTx
import qualified PlutusTx.Builtins as Builtins

-- Plutus intervals
import Plutus.V1.Ledger.Interval (contains, to)

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data OracleDatum = OracleDatum
    { odPair      :: BuiltinByteString
    , odPriceNum  :: Integer
    , odPriceDen  :: Integer
    , odValidUntil:: POSIXTime
    , odSigner    :: PubKeyHash
    }
PlutusTx.unstableMakeIsData ''OracleDatum

data OracleAction = UpdatePrice
PlutusTx.unstableMakeIsData ''OracleAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE isSignedBy #-}
isSignedBy :: PubKeyHash -> TxInfo -> Bool
isSignedBy pkh info = txSignedBy info pkh

{-# INLINABLE beforeTTL #-}
beforeTTL :: POSIXTime -> TxInfo -> Bool
beforeTTL ttl info = contains (to ttl) (txInfoValidRange info)

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: OracleDatum -> OracleAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
        UpdatePrice ->
            traceIfFalse "not signed by publisher" signedByPublisher &&
            traceIfFalse "UTxO expired"             ttlNotPassed
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByPublisher :: Bool
    signedByPublisher = isSignedBy (odSigner dat) info

    ttlNotPassed :: Bool
    ttlNotPassed = beforeTTL (odValidUntil dat) info

    -- Example: if you have optional new datum check:
    -- mNewDatum :: Maybe Datum
    -- mNewDatum = ...
    -- traceIfFalse "oracle: new datum missing" (PTx.isJust mNewDatum) &&

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @OracleDatum d
        red = unsafeFromBuiltinData @OracleAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
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

    writeValidator "oracleValidator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Oracle Publisher Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "--------------------------------------"
    putStrLn "Oracle Publisher validator generated successfully."
