{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}

module DFBContract
  ( dfbContractScript
  , dfbContractScriptShortBs
  , Schema
  , CustomDatumType
  , contract
  ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise           ( serialise )
import           Control.Monad ( void )
-- import           Control.Monad.Freer.Error ( throwError )

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
-- import qualified Data.Maybe

import           Prelude                   (String)

import           Ledger                    hiding ( singleton )
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Ledger.CardanoWallet      as CW
import           Playground.Contract
import           Wallet.Emulator.Wallet    as W


import qualified PlutusTx
import           PlutusTx.Prelude
-- import           PlutusTx.Builtins         as Internal
import qualified PlutusTx.Builtins.Internal as Internal
import           Plutus.Contract
import qualified Plutus.Trace.Emulator     as Trace
import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Value    as Value

import Plutus.Contracts.Currency qualified as Currency
import Data.Semigroup qualified as Semigroup
-- import Data.Void (Void)
import Plutus.Trace.Emulator qualified as Emulator

{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0

-}

-------------------------------------------------------------------------------
-- | Create the dfb contract parameters data object.
-------------------------------------------------------------------------------
data DFBContractParams = DFBContractParams {}
PlutusTx.makeLift ''DFBContractParams


-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType 
  { cdtPlayerPKH :: !PubKeyHash
  -- ^ The players pkh.
  , cdtValue     :: !Value
  -- ^ The current state of the lovelace distribution.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType


-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------

data CustomRedeemerType = Run | Remove
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ('Run,       0)
                                                , ('Remove,    1)
                                                ]
--PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType

-------------------------------------------------------------------------------
-- | Create the Chain State data object.
-------------------------------------------------------------------------------

data ChainState = ChainState
  { csPlayers :: !Integer
  -- ^ The number of players playing in the chain.
  , csTokens  :: !Integer
  -- ^ The number of tokens used in the chain.
  , csWallets :: ![[Integer]]
  -- ^ The wallet list.
  , csRate    :: !Integer
  -- ^ The mining rate.
  , csPool    :: !Integer
  -- ^ The mining pool.
  , csNumber  :: !Integer
  -- ^ The block number for the chain.
  , csFlag    :: !Bool
  -- ^ The trading flag.
  }
PlutusTx.unstableMakeIsData ''ChainState
PlutusTx.makeLift ''ChainState

-------------------------------------------------------------------------------
-- | Helper Functions
-------------------------------------------------------------------------------

-- | Convert a base 10 integer into base q as a list.
baseQ :: Integer -> Integer -> [Integer] -> [Integer]
baseQ 0 _base list = list
baseQ number base list = baseQ (Internal.divideInteger number base) base ([Internal.modInteger number base] ++ list)


-- | Reduce a number with 3n+1 conjecture.
reduction :: Integer -> Integer -> Integer
reduction 0 _counter = 0
reduction 1 _counter = _counter
reduction number counter
  | Internal.modInteger number 2 == 0 = reduction (Internal.divideInteger number 2) (counter + 1)
  | otherwise = reduction (3 * number + 1) (counter + 1)

-- | Take in a bytestring and convert it to a number
hexStringToInteger :: BuiltinByteString -> Integer -> Integer -> Integer
hexStringToInteger _hex_string 0 _value = _value
hexStringToInteger hex_string counter value = hexStringToInteger hex_string (counter - 1) (value * Internal.indexByteString hex_string counter)

-- | Take two lists and create a list of tuple pairs.
pairs :: [Integer] -> [Integer] -> [(Integer, Integer)] -> [(Integer, Integer)]
pairs [] _ p = p
pairs _ [] p = p
pairs (x:xs) (y:ys) p = pairs xs ys (p ++ [(x,y)])

-- | Take in a ByteString and return a SHA3_256 hash.
getHashFromString :: BuiltinByteString -> BuiltinByteString
getHashFromString string = Internal.sha3_256 string


-- | The mining function for the chain.
mining :: BuiltinByteString -> ChainState -> ChainState
mining blockHash cs = do
  { let number        = hexStringToInteger blockHash (Internal.lengthOfByteString blockHash) 1
  ; let miningList    = baseQ number (csPlayers cs) []
  ; mine miningList cs
  }

mine :: [Integer] -> ChainState -> ChainState
mine miningList cs = cs

-- | The trading function for the chain.
trading :: BuiltinByteString -> ChainState -> ChainState
trading blockHash cs   = do
  { let tradeAHash     = getHashFromString blockHash
  ; let tradeBHash     = getHashFromString tradeAHash
  ; let numberA        = hexStringToInteger tradeAHash (Internal.lengthOfByteString tradeAHash) 1
  ; let numberB        = hexStringToInteger tradeBHash (Internal.lengthOfByteString tradeBHash) 1
  ; let amount         = reduction numberA 0
  ; let traderAList    = baseQ numberA (csPlayers cs) []
  ; let tokenAList     = baseQ numberA (csTokens cs) []
  ; let traderBList    = baseQ numberB (csPlayers cs) []
  ; let tokenBList     = baseQ numberB (csTokens cs) []
  ; let tradingPairs   = pairs traderAList traderBList []
  ; let tokenPairs     = pairs tokenAList tokenBList []
  ; trade tradingPairs tokenPairs cs
  }

trade :: [(Integer, Integer)] -> [(Integer, Integer)] -> ChainState -> ChainState
trade tradingPairs tokenPairs cs = cs

-- | The checking function for seeing if the chain ends.
checking :: ChainState -> Bool
checking cs = do
  { let a   = (csPool cs) > 0
  ; let b   = (csFlag cs) == True
  ; all (==(True :: Bool)) [a,b]
  }

-- | Advance the chain into the new block.
advance :: ChainState -> ChainState
advance cs = cs


-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: DFBContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator dfb datum redeemer context =
  case redeemer of
      Run    -> traceIfFalse "Token Run Failed" $ runDFB
      Remove -> traceIfFalse "Token Removal Failed" $ remove
    where
      -------------------------------------------------------------------------
      -- | Different Types of Validators Here
      -------------------------------------------------------------------------
      
      -- | Put all the buy functions together here
      runDFB :: Bool
      runDFB = do
        { let a = True
        ; all (==(True :: Bool)) [a]
        }
      
      -- | Put all the remove functions together here
      remove :: Bool
      remove = do 
        { let a = checkTokenRemoval
        ; let b = checkTxSigner playerPKH
        ; all (==(True :: Bool)) [a]
        }
      
      -------------------------------------------------------------------------
      
      info :: TxInfo
      info = scriptContextTxInfo context

      currentTxOutputs :: [TxOut]
      currentTxOutputs = txInfoOutputs info

      -------------------------------------------------------------------------
      
      playerPKH :: PubKeyHash
      playerPKH = cdtPlayerPKH datum

      -------------------------------------------------------------------------
      
      playerAddr :: Address
      playerAddr = pubKeyHashAddress playerPKH

      -------------------------------------------------------------------------
      tokenValue :: Value
      tokenValue = cdtValue datum
      
      -------------------------------------------------------------------------
      
        -- | Check if token is being sent back to the seller wallet.
      checkTokenRemoval :: Bool
      checkTokenRemoval = checkTxOutForValueAtAddress currentTxOutputs playerAddr tokenValue

      -------------------------------------------------------------------------
      checkTxSigner :: PubKeyHash -> Bool
      checkTxSigner signee = txSignedBy info signee 
      -- Search each TxOut for the correct address and value.
      checkTxOutForValueAtAddress :: [TxOut] -> Address -> Value -> Bool
      checkTxOutForValueAtAddress [] _addr _val = False
      checkTxOutForValueAtAddress (x:xs) addr val
        | (traceIfFalse "Incorrect Address" $ (txOutAddress x) == addr) && (traceIfFalse "Incorrect Value" $ (txOutValue x) == val) = True
        | otherwise = checkTxOutForValueAtAddress xs addr val
      

      
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

typedValidator :: DFBContractParams -> Scripts.TypedValidator Typed
typedValidator dfb = Scripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode dfb)
  $$(PlutusTx.compile  [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer

-------------------------------------------------------------------------------
-- | Define The Validator Here
-------------------------------------------------------------------------------
validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator $ DFBContractParams {})

-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Plutus.Script
script = Plutus.unValidatorScript validator

dfbContractScriptShortBs :: SBS.ShortByteString
dfbContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

dfbContractScript :: PlutusScript PlutusScriptV1
dfbContractScript = PlutusScriptSerialised dfbContractScriptShortBs




-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------
-- valHash :: Ledger.ValidatorHash
-- valHash = Scripts.validatorHash (typedValidator dfb)
--   where dfb = DFBContractParams {}
-- data JoinParams = JoinParams
--     { amount :: !Value
--     } deriving (Generic, ToJSON, FromJSON, ToSchema)
-- | The schema of the contract, with two endpoints.
type Schema =
    Endpoint "" ()
    -- Endpoint "join" JoinParams
    -- .\/ Endpoint "run" CustomDatumType
    -- .\/ Endpoint "remove" JoinParams

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract
-- contract = selectList [join, run, remove] >> contract
-- scrAddress :: Ledger.Address
-- scrAddress = scriptAddress validator
-- -- | The buy endpoint.
-- run :: AsContractError e => Promise () Schema e ()
-- run =  endpoint @"run" @CustomDatumType $ \(CustomDatumType {..}) -> do
--     buyer          <- pubKeyHash <$> Plutus.Contract.ownPubKey
--     unspentOutputs <- utxosAt (Ledger.scriptAddress $ Scripts.validatorScript (typedValidator dfb))
--     let tx = collectFromScript unspentOutputs Run PlutusTx.Prelude.<> Constraints.mustPayToPubKey buyer cdtValue
--     void $ submitTxConstraintsSpending (typedValidator dfb) unspentOutputs tx
--         where dfb = DFBContractParams {}

-- -- | The remove endpoint.
-- remove :: AsContractError e => Promise () Schema e ()
-- remove =  endpoint @"remove" @JoinParams $ \(JoinParams {..}) -> do
--     player         <- pubKeyHash <$> Plutus.Contract.ownPubKey
--     unspentOutputs <- utxosAt scrAddress
--     logInfo @String $ "Unspent Outputs"
--     logInfo @(Map TxOutRef ChainIndexTxOut) $ unspentOutputs
--     let gameDatum = CustomDatumType {cdtPlayerPKH = player, cdtValue = amount}
--     logInfo @String $ "Datum Hash"
--     logInfo @DatumHash $ Ledger.datumHash (Datum (PlutusTx.toBuiltinData gameDatum))
--     -- let flt _ ciTxOut = either id Ledger.datumHash (Tx._ciTxOutDatum ciTxOut) == Ledger.datumHash (Datum (PlutusTx.toBuiltinData gameDatum)) && Tx._ciTxOutValue ciTxOut == amount
--     let flt _ ciTxOut = Tx._ciTxOutValue ciTxOut == amount
--     let tx = collectFromScriptFilter flt unspentOutputs Remove PlutusTx.Prelude.<> mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Remove) | oref <- (fst <$> Map.toList unspentOutputs)] PlutusTx.Prelude.<> Constraints.mustPayToPubKey player amount PlutusTx.Prelude.<> Constraints.mustBeSignedBy player
--     -- let tx = collectFromScriptFilter flt unspentOutputs Remove PlutusTx.Prelude.<> Constraints.mustPayToPubKey player amount PlutusTx.Prelude.<> Constraints.mustBeSignedBy player
--     logInfo @String $ "TX"
--     logInfo @(Constraints.TxConstraints CustomRedeemerType CustomDatumType) $ tx
--     logInfo @Bool $ Constraints.isSatisfiable tx
--     --void $ submitTxConstraintsSpending (typedValidator dfb) unspentOutputs tx
--     void $ submitTxConstraints (typedValidator dfb) tx
--         where
--           dfb = DFBContractParams {}


-- -- | The join game endpoint.
-- join :: AsContractError e => Promise () Schema e ()
-- join =  endpoint @"join" @JoinParams $ \(JoinParams {..}) -> do
--     player <- pubKeyHash <$> Plutus.Contract.ownPubKey
--     if Value.lt amount (Ada.lovelaceValueOf 10) -- Make this like 2 ada in production
--     then logInfo @String $ "Wrong Amount of ADA"
--     else do
--         let gameDatum = CustomDatumType {cdtPlayerPKH = player, cdtValue = amount}
--         logInfo @String $ "DATUM HASH"
--         logInfo @DatumHash $ Ledger.datumHash (Datum (PlutusTx.toBuiltinData gameDatum))
--         let tx = Constraints.mustPayToTheScript gameDatum amount PlutusTx.Prelude.<> Constraints.mustBeSignedBy player PlutusTx.Prelude.<> Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData gameDatum  )
--         void $ submitTxConstraints (typedValidator dfb) tx
--         logInfo @String $ "Player has joined the game."
--             where dfb = DFBContractParams {}

-- endpoints :: AsContractError e => Contract () Schema e ()
-- endpoints = contract
