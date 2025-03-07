{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Homework2 where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (unstableMakeIsData, compile)
import           PlutusTx.Prelude     (Bool, BuiltinData, (/=), ($), traceIfFalse)
--import           Prelude              (undefined)
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    }

PlutusTx.unstableMakeIsData ''MyRedeemer

{-# INLINABLE mkValidator #-}
-- Create a validator that unlocks the funds if MyRedemeer's flags are different
mkValidator :: () -> MyRedeemer -> PlutusV2.ScriptContext -> Bool
mkValidator _ (MyRedeemer f1 f2) _= traceIfFalse "Two flags must be difference" $ f1 /= f2

wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrapValidator mkValidator
{-# INLINABLE wrappedVal #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedVal ||])