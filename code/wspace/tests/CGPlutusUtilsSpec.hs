{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, show, (++), ($))
import qualified Prelude as P
import qualified PlutusTx.Prelude as Plutus
import Plutus.V2.Ledger.Api as V2
import Plutus.V2.Ledger.Contexts as V2
import PlutusTx
import PlutusTx.Builtins (BuiltinByteString)
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

------------------------------------------------------------
-- Oracle Datum and Redeemer
------------------------------------------------------------

data OracleDatum = OracleDatum
    { odPair       :: BuiltinByteString
    , odPriceNum   :: Integer
    , odPriceDen   :: Integer
    , odValidUntil :: POSIXTime
    , odSignerPKH  :: PubKeyHash
    }

PlutusTx.unstableMakeIsData ''OracleDatum

newtype OracleRedeemer = OracleRedeemer BuiltinByteString

PlutusTx.unstableMakeIsData ''OracleRedeemer

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

{-# INLINABLE decimalsValid #-}
decimalsValid :: Integer -> Integer -> Bool
decimalsValid _ den = den Plutus.> 0 Plutus.&& den Plutus.<= 1000000000

{-# INLINABLE getDatums #-}
getDatums :: TxInfo -> [BuiltinData]
getDatums info = Plutus.map (\(_, V2.Datum d) -> d) (V2.txInfoData info)

{-# INLINABLE findOracleRef #-}
findOracleRef :: BuiltinByteString -> TxInfo -> Plutus.Maybe OracleDatum
findOracleRef pid info =
    let datums = getDatums info
        matches = Plutus.filter
            (\d -> case PlutusTx.unsafeFromBuiltinData @OracleDatum d of
                     OracleDatum pair _ _ _ _ -> pair Plutus.== pid) datums
    in case matches of
         []    -> Plutus.Nothing
         (d:_) -> Plutus.Just (PlutusTx.unsafeFromBuiltinData @OracleDatum d)

------------------------------------------------------------
-- Validator
------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: OracleDatum -> OracleRedeemer -> ScriptContext -> Bool
mkValidator _ _ _ctx =
    Plutus.traceIfFalse "Oracle Consumer Guard: always true for example" Plutus.True

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = PlutusTx.unsafeFromBuiltinData @OracleDatum d
        red = PlutusTx.unsafeFromBuiltinData @OracleRedeemer r
        ctx = PlutusTx.unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else Plutus.traceError "validation failed"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------
-- Main: write the validator to file
------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    P.putStrLn $ "Validator written to: " ++ path

main :: IO ()
main = do
    writeValidator "oracle-consumer-guard.plutus" validator

    let vh   = V2.validatorHash validator
        addr = V2.Address (V2.ScriptCredential vh) Nothing

    P.putStrLn $ "Validator hash: " ++ show vh
    P.putStrLn $ "Script address: " ++ show addr
    P.putStrLn "Oracle Consumer Guard validator generated successfully!"