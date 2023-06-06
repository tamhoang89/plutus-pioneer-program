{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator, 
                                       TxInfo (txInfoValidRange),
                                       ScriptContext (scriptContextTxInfo),
                                       POSIXTimeRange, 
                                       mkValidatorScript, to)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           Plutus.V1.Ledger.Interval (contains, before)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), traceIfFalse, otherwise)
import           Utilities            (wrapValidator, writeValidatorToFile)
import           Prelude              (IO)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx
    | isSignedBy b1 = traceIfFalse "Signed by beneficiary1, but not before or at the deadline !" isBeforeOrAtDeadline
    | isSignedBy b2 = traceIfFalse "Signed by beneficiary2, but not after the deadline !!" isAfterDeadline
    | otherwise     = traceIfFalse "Didn't sign by beneficiary1 or beneficiary2 !!!" False

  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    dlTime = deadline _dat :: POSIXTime
    txTimeRange = txInfoValidRange info :: POSIXTimeRange
    b1 = beneficiary1 _dat :: PubKeyHash
    b2 = beneficiary2 _dat :: PubKeyHash

    isSignedBy :: PubKeyHash -> Bool
    isSignedBy = txSignedBy info

    isBeforeOrAtDeadline :: Bool
    isBeforeOrAtDeadline = to dlTime `contains` txTimeRange

    isAfterDeadline :: Bool
    isAfterDeadline = dlTime `before` txTimeRange

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/hw1.plutus" validator