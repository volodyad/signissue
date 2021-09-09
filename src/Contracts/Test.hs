{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE NamedFieldPuns        #-}


module Contracts.Test
    ( ContractParam(..)
    , initTest
    , runTest
    , address
    , testTokenName
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton, MintingPolicyHash)
import qualified Ledger.Scripts            as LedgerScripts
import           Ledger.Constraints        as Constraints
import           Ledger.Oracle
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import qualified Ledger.Contexts           as Validation
import           Plutus.Contracts.Currency as Currency
import           Plutus.Contract.Types     (Promise (..))
import           Plutus.ChainIndex.Tx      (fromOnChainTx)
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude                   as Haskell
import           Schema                    (ToSchema)

data ContractParam = ContractParam
    { cpSigner :: PubKey
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''ContractParam

-- data MessageData = MessageData
--     { mdMessageId:: Integer }
--     deriving (Show, Generic, FromJSON, ToJSON, Haskell.Eq)

-- PlutusTx.unstableMakeIsData ''MessageData
-- PlutusTx.makeLift ''MessageData

data ContractDatum = ContractDatum
        { cdMessageId:: Integer }
        deriving (Show, Generic, FromJSON, ToJSON, Haskell.Eq)
-- type ContractDatum = SignedMessage MessageData
PlutusTx.unstableMakeIsData ''ContractDatum
PlutusTx.makeLift ''ContractDatum
type ContractRedeemer = SignedMessage ContractDatum
-- data ContractRedeemer = ContractRedeemer
--     { crMessage :: Maybe (SignedMessage Integer) }
--     deriving Show
-- PlutusTx.unstableMakeIsData ''ContractRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: ContractParam -> ContractDatum -> ContractRedeemer -> ScriptContext -> Bool
mkValidator contractParam datum r ctx = 
    traceIfFalse "value signed" (isValueSigned $ r)
    -- case datum of
    --     Signed signedMessage -> traceIfFalse "value signed" (isValueSigned signedMessage)
    --     Request message -> traceIfFalse "request not signed" False
    --traceIfFalse "value signed" (isValueSigned datum)
  where 
    verifyValueSigned :: SignedMessage ContractDatum -> PubKey -> Maybe ContractDatum
    verifyValueSigned sm pk = case verifySignedMessageOnChain ctx pk sm of
            Left err -> case err of
                SignatureMismatch sig pk hash -> traceError "SignatureMismatch"
                DatumMissing hash ->  traceError "DatumMissing"
                DecodingError -> traceError "DecodingError"
                DatumNotEqualToExpected -> traceError "DatumNotEqualToExpected"
            Right res -> Just res

    isValueSigned message = isJust $ verifyValueSigned message (cpSigner contractParam)

data Test
instance Scripts.ValidatorTypes Test where
    type instance DatumType Test = ContractDatum
    type instance RedeemerType Test = ContractRedeemer

typedValidator :: ContractParam -> Scripts.TypedValidator Test
typedValidator contractParam = Scripts.mkTypedValidator @Test
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractParam)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ContractDatum @ContractRedeemer

validator :: ContractParam -> Validator
validator = Scripts.validatorScript . typedValidator

validatorHash :: ContractParam -> Ledger.ValidatorHash
validatorHash testData = LedgerScripts.validatorHash . validator $ testData

address :: ContractParam -> Ledger.Address
address = scriptAddress . validator

testTokenName :: TokenName
testTokenName = "testTokenName"

initTest :: forall w s. ContractParam -> PrivateKey -> AssetClass -> Contract w s Text ()
initTest contractParam privateKey uniqueTokenAsset = do 

    let inst = typedValidator contractParam
        mrScript = validator contractParam
        addr = address contractParam
        messageId = 123
        uniqueTokenVal = assetClassValue uniqueTokenAsset 1
    --let contractDatum = Signed $ signMessage MessageData{ mdMessageId = messageId } privateKey
    let contractDatum = ContractDatum{ cdMessageId = messageId }
    let lookups = Constraints.typedValidatorLookups inst 
                <> Constraints.otherScript mrScript

        tx      = Constraints.mustPayToTheScript contractDatum uniqueTokenVal

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

runTest :: forall w s. ContractParam -> PrivateKey -> AssetClass -> Contract w s Text ()
runTest contractParam privateKey uniqueTokenAsset = do 
    logInfo @String "runTest"
    let inst = typedValidator contractParam
        mrScript = validator contractParam
        addr = address contractParam
        messageId = 123
        uniqueTokenVal = assetClassValue uniqueTokenAsset 1
        redeemer = Redeemer $ PlutusTx.toBuiltinData $ signMessage ContractDatum{ cdMessageId = messageId } privateKey

    pkh <- pubKeyHash <$> Contract.ownPubKey
    utxos <- utxoAt addr
    let xs = [ (oref, o)
            | (oref, o) <- Map.toList utxos
            , assetClassValueOf (txOutValue $ txOutTxOut o) uniqueTokenAsset == 1
            ]
    case xs of 
        [(oref, o)] -> do
            let lookups = Constraints.typedValidatorLookups inst 
                        <> Constraints.otherScript mrScript
                        <> Constraints.unspentOutputs (Map.singleton oref o)

                tx      = Constraints.mustSpendScriptOutput oref redeemer
                        <> Constraints.mustPayToPubKey pkh uniqueTokenVal
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
        _ -> throwError "token not found"
   
