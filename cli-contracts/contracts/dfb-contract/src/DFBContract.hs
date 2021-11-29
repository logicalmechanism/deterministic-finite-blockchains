{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module DFBContract
  ( dfbContractScript
  , dfbContractScriptShortBs
  ) where

import           Codec.Serialise

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS

import           Prelude                   hiding (($))

import           Ledger                    hiding (singleton)
import qualified Ledger.Typed.Scripts      as Scripts

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import qualified PlutusTx
import           PlutusTx.Prelude          as P hiding (Semigroup (..), unless)

import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Contexts as Contexts
import qualified Plutus.V1.Ledger.Scripts  as Plutus

{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 1

  cardano-cli 1.31.0 - linux-x86_64 - ghc-8.10
  git rev 2cbe363874d0261bc62f52185cf23ed492cf4859

-}

-------------------------------------------------------------------------------
-- | Create the token sale parameters data object.
-------------------------------------------------------------------------------
data TokenSaleParams = TokenSaleParams {}
PlutusTx.makeLift ''TokenSaleParams


-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------

data CustomDatumType = CustomDatumType 
  { cdtPlayerPKH  :: !PubKeyHash
  -- ^ The Seller's public key hash.
  }
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType


-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------

data CustomRedeemerType = CustomRedeemerType 
  { crtAction    :: !Integer
  -- ^ The action determines which type of validation to use in the contract.
  }
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType


-------------------------------------------------------------------------------
-- | Define The Token Sale Parameters Here
-------------------------------------------------------------------------------

validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator ts)
  where ts = TokenSaleParams {}


-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: TokenSaleParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator ts datum redeemer context = True
      
-------------------------------------------------------------------------------
-- | This determines the data type for Datum and Redeemer.
-------------------------------------------------------------------------------

data Typed
instance Scripts.ValidatorTypes Typed where
  type instance DatumType    Typed = CustomDatumType
  type instance RedeemerType Typed = CustomRedeemerType


-------------------------------------------------------------------------------
-- | Now we need to compile the Typed Validator.
-------------------------------------------------------------------------------

typedValidator :: TokenSaleParams -> Scripts.TypedValidator Typed
typedValidator ts = Scripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
  $$(PlutusTx.compile  [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer


-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------

script :: Plutus.Script
script = Plutus.unValidatorScript validator

dfbContractScriptShortBs :: SBS.ShortByteString
dfbContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

dfbContractScript :: PlutusScript PlutusScriptV1
dfbContractScript = PlutusScriptSerialised dfbContractScriptShortBs