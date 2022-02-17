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
  , checkHashInputs
  , getHashFromString
  , reduction
  ) where
import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise           ( serialise )

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import qualified Data.Maybe

import           Ledger                    hiding ( singleton )
import qualified Ledger.Typed.Scripts      as Scripts
import           Playground.Contract
import qualified PlutusTx
-- import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Prelude
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
  { cdtCreatorPKH    :: !PubKeyHash
  , cdtPlayers       :: !Integer
  , cdtMiningPool    :: !Integer
  , cdtMiningRate    :: !Integer
  , cdtHalvingRate   :: !Integer
  , cdtPlayersPKH    :: ![PubKeyHash]
  , cdtStartPhrase   :: !BuiltinByteString
  , cdtValidateStage :: !Integer
  , cdtChainValues   :: ![BuiltinByteString]
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType

instance PlutusTx.Prelude.Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  old == new = ( cdtPlayers     old == cdtPlayers     new) &&
               ( cdtMiningPool  old == cdtMiningPool  new) &&
               ( cdtMiningRate  old == cdtMiningRate  new) &&
               ( cdtHalvingRate old == cdtHalvingRate new) &&
               ( cdtCreatorPKH  old == cdtCreatorPKH  new)
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------

data CustomRedeemerType = CustomRedeemerType
  { crtAction    :: !Integer
  , crtPlayerPKH :: !PubKeyHash
  , crtAHashes   :: ![BuiltinByteString]
  , crtBHashes   :: ![BuiltinByteString]
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
    -- deriving stock (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType

-- Integer Equality is messed up here
checkHashInputs :: [BuiltinByteString] -> [BuiltinByteString] -> [BuiltinByteString] -> Bool
checkHashInputs [] [] [] = True  -- all are equal
checkHashInputs [] _  _  = False
checkHashInputs _  [] _  = False
checkHashInputs _  _  [] = False
checkHashInputs [] [] _  = False
checkHashInputs _  [] [] = False
checkHashInputs []  _  [] = False
checkHashInputs (x:xs) (y:ys) (z:zs)
  -- | getHashFromString (x <> y) == z = checkHashInputs xs ys zs
  | reduction 5 == 5 = checkHashInputs xs ys zs
  | otherwise = traceIfFalse "Incorrect Hash Value" False

-- | Reduce a number with 3n+1 conjecture.
reduction :: Integer -> Integer
reduction number = reduction' number 0
  where
    reduction' :: Integer -> Integer -> Integer
    reduction' 0 _counter = 0
    reduction' 1 _counter = _counter
    reduction' number' counter
      | modulo number' 2 == 0 = reduction' (divide number' 2) (counter + 1)
      | otherwise = reduction' (3 * number' + 1) (counter + 1)

-------------------------------------------------------------------------------
-- | Helper Functions
-------------------------------------------------------------------------------

-- | Take in a ByteString and return a SHA3_256 hash.
getHashFromString :: BuiltinByteString -> BuiltinByteString
getHashFromString bbString = takeByteString 16 $ sha3_256 bbString

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
        | actionFlag == 1 = leaveDFB
        | actionFlag == 2 = removeDFB
        | actionFlag == 3 = phaseOneValidation
        | actionFlag == 4 = phaseTwoValidation
        | actionFlag == 5 = phaseThreeValidation
        | otherwise       = traceIfFalse "Incorrect Action Flag" True -- This can be used as a bypass
          where
            actionFlag :: Integer
            actionFlag = crtAction redeemer


      -------------------------------------------------------------------------
      -- | Different Types of Validators Here
      -------------------------------------------------------------------------

      -- | A player can join the game.
      joinDFB :: Bool
      joinDFB = do
        { let a = traceIfFalse "UTxO Must Go To Script"     $ checkContTxOutForValue scriptTxOutputs adjustValueUp
        ; let b = traceIfFalse "Incorrect Datum Values"     $ newDatum == datum
        ; let c = traceIfFalse "Spending Multiple UTxOs"    checkForSingleScriptInput
        ; let d = traceIfFalse "Too Many Players"           $ listLength (cdtPlayersPKH newDatum) <= cdtPlayers datum
        ; let e = traceIfFalse "Incorrect Start Phrase"     $ cdtStartPhrase newDatum == cdtStartPhrase datum
        ; let f = traceIfFalse "A player is missing"        $ reverse (tail $ reverse $ cdtPlayersPKH  newDatum) == cdtPlayersPKH datum
        ; let g = traceIfFalse "DFB is being validated"     $ cdtValidateStage datum == (0 :: Integer)
        ; let h = traceIfFalse "Incorrect Validation Stage" $ cdtValidateStage newDatum == cdtValidateStage datum
        ; let i = traceIfFalse "Incorrect Signer"           $ checkTxSigner $ head $ reverse $ cdtPlayersPKH newDatum
        ;         traceIfFalse "Join Endpoint Failure"      $ all (==(True :: Bool)) [a,b,c,d,e,f,g,h,i]
        }

      -- | A player leaves the game.
      leaveDFB :: Bool
      leaveDFB = do
        { let a = traceIfFalse "Incorrect Signer"            $ checkTxSigner playerPKH
        ; let b = traceIfFalse "Incorrect Datum Values"      $ newDatum == datum
        ; let c = traceIfFalse "UTxO Must go to script"      $ checkContTxOutForValue scriptTxOutputs adjustValueDown
        ; let d = traceIfFalse "Incorret Player Data"        $ listLength (cdtPlayersPKH newDatum) == listLength (cdtPlayersPKH datum) - 1
        ; let e = traceIfFalse "Incorrect Start Phrase"      $ cdtStartPhrase newDatum == cdtStartPhrase datum
        ; let f = traceIfFalse "Value is not being returned" $ checkTxOutForValueAtPKH currentTxOutputs playerPKH (Ada.lovelaceValueOf minimumAda)
        ; let g = traceIfFalse "DFB is being validated"      $ cdtValidateStage datum == (0 :: Integer)
        ; let h = traceIfFalse "Must be a player"            checkIfPlayer
        ; let i = traceIfFalse "Incorrect Validation Stage"  $ cdtValidateStage newDatum == cdtValidateStage datum
        ; let j = traceIfFalse "Spending Multiple UTxOs"     checkForSingleScriptInput
        ;         traceIfFalse "Leave Endpoint Failure"      $ all (==(True :: Bool)) [a,b,c,d,e,f,g,h,i,j]
        }

      removeDFB :: Bool
      removeDFB = do
        { let a = traceIfFalse "Incorrect Signer"            $ checkTxSigner creatorPKH
        ; let b = traceIfFalse "Value is not being returned" $ checkTxOutForValueAtPKH currentTxOutputs creatorPKH validatedValue
        ; let c = traceIfFalse "Spending Multiple UTxOs"     checkForSingleScriptInput
        ; let d = traceIfFalse "Incorret Player Data"        $ listLength (cdtPlayersPKH datum) == 0
        ;         traceIfFalse "Remove Endpoint Failure"     $ all (==(True :: Bool)) [a,b,c,d]
        }

      phaseOneValidation :: Bool
      phaseOneValidation = do
        { let a = traceIfFalse "DFB is being validated"    $ cdtValidateStage datum == (0 :: Integer)
        ; let b = traceIfFalse "Not Advancing Stages"      $ cdtValidateStage datum + 1 == cdtValidateStage newDatum
        ; let c = traceIfFalse "Incorrect Datum Values"    $ newDatum == datum
        ; let d = traceIfFalse "Incorret Chain Data"       $ listLength (cdtChainValues newDatum) == 4
        ; let e = traceIfFalse "Incorret Player Data"      $ listLength (cdtPlayersPKH newDatum) == listLength (cdtPlayersPKH datum)
        ; let f = traceIfFalse "Incorrect Start Phrase"    $ cdtStartPhrase newDatum == cdtStartPhrase datum
        ; let g = traceIfFalse "Spending Multiple UTxOs"   checkForSingleScriptInput
        ; let h = traceIfFalse "UTxO Must go to script"    $ checkContTxOutForValue scriptTxOutputs validatedValue
        ;         traceIfFalse "Phase 1 Endpoint Failure"  $ all (==(True :: Bool)) [a,b,c,d,e,f,g,h]
        }
      
      phaseTwoValidation :: Bool
      phaseTwoValidation = do
        { let a = traceIfFalse "DFB is being validated"    $ cdtValidateStage datum == (1 :: Integer)
        ; let b = traceIfFalse "Not Advancing Stages"      $ cdtValidateStage datum - 1 == cdtValidateStage newDatum
        ; let c = traceIfFalse "Incorrect Datum Values"    $ newDatum == datum
        ; let d = traceIfFalse "Incorret Player Data"      $ listLength (cdtPlayersPKH newDatum) == listLength (cdtPlayersPKH datum)
        ; let e = traceIfFalse "Incorrect Start Phrase"    $ cdtStartPhrase newDatum == head (reverse $ cdtChainValues datum)
        ; let f = traceIfFalse "Spending Multiple UTxOs"   checkForSingleScriptInput
        ; let g = traceIfFalse "UTxO Must go to script"    $ checkContTxOutForValue scriptTxOutputs validatedValue
        ; let h = traceIfFalse "Hashing Has Failed"        $ checkHashInputs (crtAHashes redeemer) (crtBHashes redeemer) (cdtChainValues datum)
        ;         traceIfFalse "Phase 2 Endpoint Failure"  $ all (==(True :: Bool)) [a,b,c,d,e,f,g,h]
        }
      
      phaseThreeValidation :: Bool
      phaseThreeValidation = do
        { let a = traceIfFalse "DFB is being validated"    $ cdtValidateStage datum == (1 :: Integer)
        ; let b = traceIfFalse "Not Advancing Stages"      $ cdtValidateStage datum == cdtValidateStage newDatum
        ; let c = traceIfFalse "Incorrect Datum Values"    $ newDatum == datum
        ; let d = traceIfFalse "Incorret Chain Data"       $ listLength (cdtChainValues datum) == 4
        ; let e = traceIfFalse "Incorret Player Data"      $ listLength (cdtPlayersPKH newDatum) == listLength (cdtPlayersPKH datum)
        ; let f = traceIfFalse "Incorrect Start Phrase"    $ cdtStartPhrase newDatum == cdtStartPhrase datum
        ; let g = traceIfFalse "Spending Multiple UTxOs"   checkForSingleScriptInput
        ; let h = traceIfFalse "UTxO Must go to script"    $ checkContTxOutForValue scriptTxOutputs validatedValue
        ;         traceIfFalse "Phase 3 Endpoint Failure"  $ all (==(True :: Bool)) [a,b,c,d,e,f,g,h]
        }

      -------------------------------------------------------------------------

      info :: TxInfo
      info = scriptContextTxInfo context

      -- All the outputs going back to the script.
      scriptTxOutputs  :: [TxOut]
      scriptTxOutputs  = getContinuingOutputs context

      currentTxOutputs :: [TxOut]
      currentTxOutputs = txInfoOutputs info

      -------------------------------------------------------------------------

      newDatum :: CustomDatumType
      newDatum = embeddedDatum scriptTxOutputs

      -------------------------------------------------------------------------

      creatorPKH :: PubKeyHash
      creatorPKH = cdtCreatorPKH datum


      playerPKH :: PubKeyHash
      playerPKH = crtPlayerPKH redeemer

      -------------------------------------------------------------------------
      -- values
      minimumAda :: Integer
      minimumAda = 5000000

      validatedValue :: Value
      validatedValue = case findOwnInput context of
          Nothing    -> traceError "No Input to Validate"
          Just input -> txOutValue $ txInInfoResolved input

      adjustValueDown :: Value
      adjustValueDown = Ada.lovelaceValueOf (Value.valueOf validatedValue Ada.adaSymbol Ada.adaToken - minimumAda)

      adjustValueUp :: Value
      adjustValueUp = Ada.lovelaceValueOf (Value.valueOf validatedValue Ada.adaSymbol Ada.adaToken + minimumAda)
      -------------------------------------------------------------------------

      checkIfPlayer :: Bool
      checkIfPlayer = searchPlayers (cdtPlayersPKH datum) playerPKH
        where
          searchPlayers :: [PubKeyHash] -> PubKeyHash -> Bool
          searchPlayers [] _ = False
          searchPlayers (x:xs) pkh'
            | x == pkh' = True
            | otherwise = searchPlayers xs pkh'

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

      -- -- Integer Equality is messed up here
      -- checkHashInputs :: [BuiltinByteString] -> [BuiltinByteString] -> [BuiltinByteString] -> Bool
      -- checkHashInputs [] [] [] = True  -- all are equal
      -- checkHashInputs [] _  _  = False
      -- checkHashInputs _  [] _  = False
      -- checkHashInputs _  _  [] = False
      -- checkHashInputs (x:xs) (y:ys) (z:zs)
      --   | getHashFromString (x <> y) == z = checkHashInputs xs ys zs
      --   | otherwise = traceIfFalse "Incorrect Hash Value" False


      -- -------------------------------------------------------------------------------
      -- -- | Helper Functions
      -- -------------------------------------------------------------------------------

      -- -- | Take in a ByteString and return a SHA3_256 hash.
      -- getHashFromString :: BuiltinByteString -> BuiltinByteString
      -- getHashFromString bbString = takeByteString 16 $ sha3_256 bbString

      listLength :: [a] -> Integer
      listLength arr = countHowManyElements arr 0
        where
          countHowManyElements [] counter = counter
          countHowManyElements (_:xs) counter = countHowManyElements xs (counter + 1)



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
