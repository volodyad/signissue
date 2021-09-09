{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Spec.Test
    ( tests
    ) where

import           Control.Lens
import           Control.Monad                      (void, when)
import qualified Control.Monad.Freer                as Freer
import qualified Control.Monad.Freer.Error          as Freer
import           Control.Monad.Freer.Extras as Extras
import qualified Data.Semigroup                         as Semigroup
import           Control.Monad.Freer.Extras.Log     (LogLevel (..))
import           Data.Default                       (Default (def))
import           Data.Monoid                        (Last (..))
import           Data.Text                          (Text, pack)
import           Ledger                             hiding (singleton, MintingPolicyHash)
import qualified Ledger.Ada                         as Ada
import           Ledger.Oracle
import           Plutus.Contract                    hiding (currentSlot)
import           Plutus.Contract.Test               hiding (not)
import qualified Plutus.Contracts.Currency          as Currency
import qualified Wallet.Emulator.Folds              as Folds
import qualified Wallet.Emulator.Stream             as Stream

import           Ledger.TimeSlot                    (SlotConfig)
import qualified Ledger.TimeSlot                    as TimeSlot
import qualified Ledger.Value                       as Value
import           Ledger.Value                       (CurrencySymbol, AssetClass(..))
import           Plutus.Contract.Test.ContractModel
import           Contracts.Test             
import qualified Plutus.Trace.Emulator              as Trace
import           PlutusTx.Monoid                    (inv)

import           Test.Tasty

auctionEmulatorCfg :: Trace.EmulatorConfig
auctionEmulatorCfg =
    let initialDistribution = defaultDist
    in (def & Trace.initialChainState .~ Left initialDistribution) & Trace.slotConfig .~ slotCfg

options :: CheckOptions
options = set emulatorConfig auctionEmulatorCfg defaultCheckOptions

slotCfg :: SlotConfig
slotCfg = def

contractParam ::  ContractParam
contractParam = ContractParam
    { cpSigner = walletPubKey w1 }

initTestContract ::  PrivateKey -> AssetClass -> Contract () EmptySchema Text ()
initTestContract privateKey assetClass = initTest contractParam privateKey assetClass

runTestContract ::  PrivateKey -> AssetClass -> Contract () EmptySchema Text ()
runTestContract privateKey assetClass = runTest contractParam privateKey assetClass

w1:: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

testTrace :: Trace.EmulatorTrace ()
testTrace = do
    curHdl <- Trace.activateContractWallet w1 createUniqueToken
    void $ Trace.waitNSlots 5
    currency <- extractAssetClass curHdl
    let tokenAssetClass = AssetClass (Currency.currencySymbol currency, testTokenName)
    initHdl <- Trace.activateContractWallet w1 $ initTestContract (walletPrivKey w1) tokenAssetClass
    void $ Trace.waitNSlots 5
    testHdl <- Trace.activateContractWallet w1 $ runTestContract (walletPrivKey w1) tokenAssetClass
    void $ Trace.waitNSlots 5

createUniqueToken :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
createUniqueToken = do
    ownPK <- pubKeyHash <$> ownPubKey
    cur   <- Currency.mintContract ownPK [(testTokenName, 1)]
    tell $ Just $ Semigroup.Last cur

extractAssetClass :: 
    Trace.ContractHandle (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError 
    -> Trace.EmulatorTrace Currency.OneShotCurrency
extractAssetClass handle = do
    t <- Trace.observableState handle
    case t of
        Just (Semigroup.Last currency) -> pure currency
        _                    -> Trace.throwError (Trace.GenericError "currency not found")

tests :: TestTree
tests =
    testGroup "test"
        [ checkPredicateOptions options "test"
            (assertNoFailedTransactions
            )
            testTrace
        ]