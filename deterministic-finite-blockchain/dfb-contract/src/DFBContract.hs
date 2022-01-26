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
  , listLength
  , getHashFromString
  ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise           ( serialise )

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import qualified Data.Maybe

import           Ledger                    hiding ( singleton )
import qualified Ledger.Typed.Scripts      as Scripts
import           Playground.Contract
import qualified Prelude as P (init)
import qualified PlutusTx
import qualified PlutusTx.Builtins.Internal as Internal
import           PlutusTx.Prelude
import           Plutus.Contract
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.V1.Ledger.Ada      as Ada


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
  { cdtOwnerPKH    :: !PubKeyHash
  , cdtPlayers     :: !Integer
  , cdtMiningPool  :: !Integer
  , cdtMiningRate  :: !Integer
  , cdtHalvingRate :: !Integer
  , cdtBlockNumber :: !Integer
  , cdtWalletState :: ![Value]
  , cdtPlayersPKH  :: ![PubKeyHash]
  , cdtStartPhrase :: !BuiltinByteString
  , cdtValidate    :: !Integer
  , cdtChainHashes :: ![BuiltinByteString]
  , cdtChainBlocks :: ![Integer]
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType

instance PlutusTx.Prelude.Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  old == new = ( cdtOwnerPKH    old == cdtOwnerPKH    new) &&
               ( cdtPlayers     old == cdtPlayers     new) &&
               ( cdtMiningPool  old == cdtMiningPool  new) &&
               ( cdtMiningRate  old == cdtMiningRate  new) &&
               ( cdtHalvingRate old == cdtHalvingRate new)
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------

data CustomRedeemerType = CustomRedeemerType
  { crtAction    :: !Integer
  , crtPlayerPKH :: !PubKeyHash
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType

-------------------------------------------------------------------------------
-- | Helper Functions
-------------------------------------------------------------------------------
-- | Take in a ByteString and return a SHA2_256 hash.
{-# INLINABLE getHashFromString #-}
getHashFromString :: BuiltinByteString -> BuiltinByteString
getHashFromString = Internal.blake2b_256


{-# INLINABLE listLength #-}
listLength :: [a] -> Integer
listLength arr = countHowManyElements arr 0
  where
    countHowManyElements [] counter = counter
    countHowManyElements (_:xs) counter = countHowManyElements xs (counter + 1)

-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: DFBContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ datum redeemer context
  | checkActionFlag = True
  | otherwise       = traceIfFalse "Validation Error" False
    where
      -- Either use an integer or use different constructors. What is best?
      checkActionFlag :: Bool
      checkActionFlag
        | actionFlag == 0 = joinDFB
        | actionFlag == 1 = stopDFB
        | actionFlag == 2 = leaveDFB
        | otherwise       = traceIfFalse "Incorrect Action Flag" False -- This can be used as a bypass
          where
            actionFlag :: Integer
            actionFlag = crtAction redeemer
      -------------------------------------------------------------------------
      -- | Different Types of Validators Here
      -------------------------------------------------------------------------

      -- | A player can join the game.
      joinDFB :: Bool
      joinDFB = do
        { let cdt = embeddedDatum scriptTxOutputs
        ; let a = traceIfFalse "UTxO Must Go To Script"  $ checkContTxOutForValue scriptTxOutputs (totalValue $ cdtWalletState cdt)
        ; let b = traceIfFalse "Incorrect Datum Values"  $ cdt == datum
        ; let c = traceIfFalse "Spending Multiple UTxOs" checkForSingleScriptInput
        ; let d = traceIfFalse "Incorret Player Data"    $ listLength (cdtPlayersPKH cdt) == listLength (cdtWalletState cdt)
        ; let e = traceIfFalse "Too Many Players"        $ listLength (cdtPlayersPKH cdt) <= cdtPlayers datum
        ; let f = traceIfFalse "Incorrect Start Phrase"  $ cdtStartPhrase cdt == cdtStartPhrase datum
        ; let g = traceIfFalse "A player is missing"     $ reverse (tail $ reverse $ cdtPlayersPKH cdt) == cdtPlayersPKH datum
        ; let h = traceIfFalse "A wallet is missing"     $ reverse (tail $ reverse $ cdtWalletState cdt) == cdtWalletState datum
        ; all (==(True :: Bool)) [a,b,c,d,e,f,g,h]
        }

      -- | The game owner can stop the game.
      stopDFB :: Bool
      stopDFB = do
        { let a = traceIfFalse "Wrong signer" $ checkTxSigner ownerPKH
        ; let b = traceIfFalse "Wrong paymnet" $ checkAllPayments (cdtPlayersPKH datum) (cdtWalletState datum)
        ; all (==(True :: Bool)) [a,b]
        }

      -- | A player leaves the game.
      leaveDFB :: Bool
      leaveDFB = do
        { let cdt = embeddedDatum scriptTxOutputs
        ; let a = traceIfFalse "Incorrect Signer"            $ checkTxSigner $ crtPlayerPKH redeemer
        ; let b = traceIfFalse "Incorrect Datum Values"      $ cdt == datum
        ; let c = traceIfFalse "BuiltinString"               $ checkContTxOutForValue scriptTxOutputs (totalValue $ cdtWalletState cdt)
        ; let d = traceIfFalse "Incorret Player Data"        $ listLength (cdtPlayersPKH cdt) == listLength (cdtWalletState cdt)
        ; let e = traceIfFalse "Incorrect Start Phrase"      $ cdtStartPhrase cdt == cdtStartPhrase datum
        ; let f = traceIfFalse "Value is not being returned" $ checkTxOutForValueAtPKH currentTxOutputs (crtPlayerPKH redeemer) (findValue $ crtPlayerPKH redeemer)
        ; all (==(True :: Bool)) [a,b,c,d,e,f]
        }

      -- runDFB :: Bool
      -- runDFB = True
      -------------------------------------------------------------------------

      info :: TxInfo
      info = scriptContextTxInfo context

      -- All the outputs going back to the script.
      scriptTxOutputs  :: [TxOut]
      scriptTxOutputs  = getContinuingOutputs context

      currentTxOutputs :: [TxOut]
      currentTxOutputs = txInfoOutputs info

      -------------------------------------------------------------------------

      ownerPKH :: PubKeyHash
      ownerPKH = cdtOwnerPKH datum

      -------------------------------------------------------------------------
      -- values
      emptyValue :: Value
      emptyValue = Ada.lovelaceValueOf 0

      totalValue :: [Value] -> Value
      totalValue wallets = combineValues wallets emptyValue
        where
          combineValues []     value' = value'
          combineValues (x:xs) value' = combineValues xs (value' <> x)

      findValue :: PubKeyHash -> Value
      findValue pkh = pickOutValue (cdtPlayersPKH datum) (cdtWalletState datum) pkh
        where
          pickOutValue :: [PubKeyHash] -> [Value] -> PubKeyHash -> Value
          pickOutValue _ [] _ = emptyValue
          pickOutValue [] _ _ = emptyValue
          pickOutValue (x:xs) (y:ys) pkh'
            | x == pkh' = y
            | otherwise = pickOutValue xs ys pkh'

      -------------------------------------------------------------------------
      checkTxSigner :: PubKeyHash -> Bool
      checkTxSigner signee = txSignedBy info signee

      -- Check for embedded datum in the txout
      embeddedDatum :: [TxOut] -> CustomDatumType
      embeddedDatum [] = datum
      embeddedDatum (x:xs) = case txOutDatumHash x of
        Nothing -> embeddedDatum xs
        Just dh -> case findDatum dh info of
          Nothing         -> datum
          Just (Datum d)  -> Data.Maybe.fromMaybe datum (PlutusTx.fromBuiltinData d)

      -- | Search each TxOut for the value.
      checkContTxOutForValue :: [TxOut] -> Value -> Bool
      checkContTxOutForValue [] _val = False
      checkContTxOutForValue (x:xs) val
        | checkVal  = True
        | otherwise = checkContTxOutForValue xs val
        where
          checkVal :: Bool
          checkVal = Value.geq (txOutValue x) val

      -- Search each TxOut for the correct address and value.
      checkTxOutForValueAtPKH :: [TxOut] -> PubKeyHash -> Value -> Bool
      checkTxOutForValueAtPKH [] _pkh _val = False
      checkTxOutForValueAtPKH (x:xs) pkh val
        | checkAddr && checkVal = True
        | otherwise             = checkTxOutForValueAtPKH xs pkh val
        where
          checkAddr :: Bool
          checkAddr = txOutAddress x == pubKeyHashAddress pkh

          checkVal :: Bool
          checkVal = txOutValue x == val

      -- Force a single script utxo input.
      checkForSingleScriptInput :: Bool
      checkForSingleScriptInput = loopInputs (txInfoInputs info) 0
        where
          loopInputs :: [TxInInfo] -> Integer -> Bool
          loopInputs []     counter = counter == 1
          loopInputs (x:xs) counter = case txOutDatumHash $ txInInfoResolved x of
              Nothing -> do
                if counter > 1
                  then loopInputs [] counter
                  else loopInputs xs counter
              Just _  -> do
                if counter > 1
                  then loopInputs [] counter
                  else loopInputs xs (counter + 1)


      -- Loop the pkh and amount lists, checking each case.
      checkAllPayments :: [PubKeyHash] -> [Value] -> Bool
      checkAllPayments []     []     = True
      checkAllPayments []     _      = True
      checkAllPayments _      []     = True
      checkAllPayments (x:xs) (y:ys)
        | checkTxOutForValueAtPKH currentTxOutputs x y = checkAllPayments xs ys
        | otherwise = traceIfFalse "A Member Of The Group Is Not Being Paid." False



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

type Schema =
    Endpoint "" ()

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract
