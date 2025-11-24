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

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, flattenValue)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum Types
------------------------------------------------------------------------

data LSDPoolDatum = LSDPoolDatum
    { lpTotalADA    :: Integer
    , lpTotalShares :: Integer
    , lpFeeBps      :: Integer
    }
PlutusTx.unstableMakeIsData ''LSDPoolDatum

data WithdrawDatum = WithdrawDatum
    { wdOwner    :: PubKeyHash
    , wdShares   :: Integer
    , wdUnlockAt :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''WithdrawDatum

------------------------------------------------------------------------
-- Redeemer
------------------------------------------------------------------------

data LSDAction
    = Deposit
    | RequestWithdraw
    | FinalizeWithdraw
PlutusTx.unstableMakeIsData ''LSDAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE sharesForDeposit #-}
sharesForDeposit :: Integer -> Integer -> Integer -> Integer
sharesForDeposit totalADA totalShares deposit =
    if totalShares == 0 then deposit else (deposit * totalShares) `divide` totalADA

{-# INLINABLE adaForShares #-}
adaForShares :: Integer -> Integer -> Integer -> Integer
adaForShares totalADA totalShares shares =
    (shares * totalADA) `divide` totalShares

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: LSDPoolDatum -> LSDAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of

      ------------------------------------------------------------------
      -- User deposits ADA → mints sADA shares
      ------------------------------------------------------------------
      Deposit ->
        traceIfFalse "must receive ADA" (depositAmount > 0) &&
        traceIfFalse "must mint shares" (mintedShares > 0)

      ------------------------------------------------------------------
      -- User burns shares → receives withdrawal NFT ticket
      ------------------------------------------------------------------
      RequestWithdraw ->
        traceIfFalse "must burn shares" (burnedShares > 0)

      ------------------------------------------------------------------
      -- User redeems NFT after epoch → receives ADA
      FinalizeWithdraw ->
        traceIfFalse "too early" pastEpoch &&
        traceIfFalse "incorrect withdrawal amount" correctWithdrawal
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ------------------------------------------------------------------
    -- Deposit ADA sent into script
    ------------------------------------------------------------------
    depositAmount :: Integer
    depositAmount =
        let v = valueLockedBy info (ownHash ctx)
        in valueOf v adaSymbol adaToken

    ------------------------------------------------------------------
    -- Minted shares (sADA)
    ------------------------------------------------------------------
    mintedShares :: Integer
    mintedShares =
        case flattenValue (txInfoMint info) of
            [(_, _, amt)] -> amt
            _             -> 0

    ------------------------------------------------------------------
    -- Burned shares
    ------------------------------------------------------------------
    burnedShares :: Integer
    burnedShares = negate mintedShares

    ------------------------------------------------------------------
    -- Withdrawal timing (placeholder)
    ------------------------------------------------------------------
    pastEpoch :: Bool
    pastEpoch = True

    ------------------------------------------------------------------
    -- Withdrawal payout correctness (placeholder)
    ------------------------------------------------------------------
    correctWithdrawal :: Bool
    correctWithdrawal = True


------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @LSDPoolDatum d
        red = unsafeFromBuiltinData @LSDAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Address
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short                  -- convert ShortByteString → ByteString
        builtin  = Builtins.toBuiltin strictBS          -- now valid
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
-- File Writing
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

    writeValidator "lsd-wrapper.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Liquid Staking Derivative (ADA → sADA) ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "------------------------------------------------"
    putStrLn "sADA validator generated successfully."
