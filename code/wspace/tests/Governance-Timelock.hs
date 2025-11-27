{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, FilePath)
import qualified Prelude as P
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf)

import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), (<$>))
import PlutusTx.Builtins qualified as Builtins

import Codec.Serialise qualified as Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as CS

---------------------------------------------------------
-- Datum & Redeemer
---------------------------------------------------------

data TimelockDatum = TimelockDatum
    { tdTemplateHash :: BuiltinByteString
    , tdEta          :: POSIXTime
    , tdProposer     :: PubKeyHash
    , tdCurrency     :: CurrencySymbol
    , tdToken        :: TokenName
    }
PlutusTx.unstableMakeIsData ''TimelockDatum

data TimelockAction = Queue | Execute | Cancel
PlutusTx.unstableMakeIsData ''TimelockAction

---------------------------------------------------------
-- Helpers
---------------------------------------------------------

{-# INLINABLE inputHasNFT #-}
inputHasNFT :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
inputHasNFT ctx cs tn =
    case findOwnInput ctx of
        Nothing -> False
        Just i  ->
            let v = txOutValue (txInInfoResolved i)
            in valueOf v cs tn >= 1

{-# INLINABLE now #-}
now :: TxInfo -> POSIXTime
now info =
    case ivTo (txInfoValidRange info) of
        UpperBound (Finite t) _ -> t
        _ -> traceError "invalid time range"

---------------------------------------------------------
-- Validator Logic
---------------------------------------------------------

{-# INLINABLE mkTimelock #-}
mkTimelock :: TimelockDatum -> TimelockAction -> ScriptContext -> Bool
mkTimelock dat action ctx =
    let info    = scriptContextTxInfo ctx
        current = now info
    in case action of

        Queue ->
            traceIfFalse "no sig" (txSignedBy info (tdProposer dat)) &&
            traceIfFalse "missing nft" (inputHasNFT ctx (tdCurrency dat) (tdToken dat))

        Execute ->
            traceIfFalse "too early" (current >= tdEta dat) &&
            traceIfFalse "missing nft" (inputHasNFT ctx (tdCurrency dat) (tdToken dat))

        Cancel ->
            traceIfFalse "no sig" (txSignedBy info (tdProposer dat)) &&
            traceIfFalse "too late" (current < tdEta dat)

---------------------------------------------------------
-- Untyped wrapper
---------------------------------------------------------

{-# INLINABLE mkUntyped #-}
mkUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntyped d r c =
    let dat = unsafeFromBuiltinData @TimelockDatum d
        red = unsafeFromBuiltinData @TimelockAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkTimelock dat red ctx then () else error ()

validator :: Validator
validator =
    mkValidatorScript
        $$(PlutusTx.compile [|| mkUntyped ||])

---------------------------------------------------------
-- Hash & Address
---------------------------------------------------------

validatorHash :: ValidatorHash
validatorHash =
    let bytes = Serialise.serialise validator
        short = SBS.toShort (LBS.toStrict bytes)
    in ValidatorHash (Builtins.toBuiltin (SBS.fromShort short))

scriptAddress :: Address
scriptAddress = Address (ScriptCredential validatorHash) Nothing

bech32Address :: C.NetworkId -> Validator -> P.String
bech32Address network val =
    let serialised = SBS.toShort (LBS.toStrict (Serialise.serialise val))
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (C.PlutusScript CS.PlutusScriptV2 plutusScript)
        addr :: CS.AddressInEra CS.BabbageEra
        addr = C.makeShelleyAddressInEra
                 network
                 (C.PaymentCredentialByScript scriptHash)
                 C.NoStakeAddress
    in P.show (C.serialiseAddress addr)

---------------------------------------------------------
-- Write script to file
---------------------------------------------------------

writeScript :: FilePath -> Validator -> IO ()
writeScript path val = do
    LBS.writeFile path (Serialise.serialise val)
    P.putStrLn ("Wrote validator to: " P.<> path)

---------------------------------------------------------
-- Main
---------------------------------------------------------

main :: IO ()
main = do
    let path = "timelock.plutus"
        network = C.Testnet (C.NetworkMagic 1)

    writeScript path validator

    P.putStrLn "\n----- Timelock Validator -----"
    P.putStrLn ("Validator Hash: " P.<> P.show validatorHash)
    P.putStrLn ("On-chain Script Address: " P.<> P.show scriptAddress)
    P.putStrLn ("Bech32 Address: " P.<> bech32Address network validator)
    P.putStrLn "--------------------------------"
    P.putStrLn "Timelock script generated successfully."
