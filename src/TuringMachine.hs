module TuringMachine where

import Data.Set as Set

import Tape
import Errors

data TuringMachine symbolType stateType =
  TuringMachine { states             :: Set stateType
                , symbols            :: Set symbolType
                , blankSymbol        :: symbolType
                , inputSymbols       :: Set symbolType
                , initialState       :: stateType
                , finalStates        :: Set stateType
                , transitionFunction :: symbolType
                                     -> stateType
                                     -> Tape symbolType
                                     -> Maybe ( stateType
                                              , Tape symbolType)
                }

instance (Show symbolType, Show stateType) => Show (TuringMachine symbolType stateType) where
  show (TuringMachine states
                      symbols
                      blankSymbol
                      inputSymbols
                      initialState
                      finalStates
                      transitionFunction) =
    concat [ "{"
          , show states
          , show symbols
          , show blankSymbol
          , show inputSymbols
          , show initialState
          , show finalStates
          , "}"
          ]

assertValid :: ( Ord symbolType, Ord stateType
               , Show symbolType, Show stateType )
            => TuringMachine symbolType stateType
            -> TuringMachineErrorM Bool
assertValid (TuringMachine states
                           symbols
                           blankSymbol
                           inputSymbols
                           initialState
                           finalStates
                           transitionFunction) =
  assert (blankSymbol `member` symbols) (DefinitionError $ NotAMember ("blankSymbol", show blankSymbol) ("symbols", show symbols)) $
  assert (inputSymbols `isSubsetOf` symbols) (DefinitionError $ NotASubset ("inputSymbols", show inputSymbols) ("symbols", show symbols)) $
  assert (initialState `member` states) (DefinitionError $ NotAMember ("initialState", show initialState) ("states", show states)) $
  assert (finalStates `isSubsetOf` states) (DefinitionError $ NotASubset ("finalStates", show finalStates) ("states", show states)) $
  Right True
    where
  assert True  _     value = value
  assert False error _     = Left error

isValid :: ( Ord symbolType, Ord stateType
           , Show symbolType, Show stateType )
        => TuringMachine symbolType stateType
        -> Bool
isValid tm = case assertValid tm of
               Left _ -> False
               Right _ -> True

run :: (Ord stateType, Ord symbolType, Show symbolType, Show stateType)
    => TuringMachine symbolType stateType
    -> Tape symbolType
    -> TuringMachineErrorM [(stateType, Tape symbolType)]
run (TuringMachine states
                   symbols
                   blankSymbol
                   inputSymbols
                   initialState
                   finalStates
                   transitionFunction) tape =
  if (Set.fromList . Tape.toList $ tape) `isSubsetOf` inputSymbols
    then run' $ (initialState, tape)
    else Left $ RuntimeError $ IllegalTapeInput (show tape) (show inputSymbols)
    where
  run' (currentState, currentTape) =
    case transitionFunction blankSymbol currentState currentTape of
      Just tuple  -> case run' tuple of
                        Left error -> Left error
                        Right rest -> Right $ (currentState, currentTape) : rest
      Nothing     -> if currentState `member` finalStates
                      then Right []
                      else Left $ RuntimeError $ IllegalHaltState (show currentState) (show finalStates)

runWithoutShow :: (Ord stateType, Ord symbolType)
    => TuringMachine symbolType stateType
    -> Tape symbolType
    -> TuringMachineErrorM [(stateType, Tape symbolType)]
runWithoutShow (TuringMachine states
                   symbols
                   blankSymbol
                   inputSymbols
                   initialState
                   finalStates
                   transitionFunction) tape =
  if (Set.fromList . Tape.toList $ tape) `isSubsetOf` inputSymbols
    then run' $ (initialState, tape)
    else Left $ RuntimeError $ IllegalTapeInput "" ""
    where
  run' (currentState, currentTape) =
    case transitionFunction blankSymbol currentState currentTape of
      Just tuple  -> case run' tuple of
                        Left error -> Left error
                        Right rest -> Right $ (currentState, currentTape) : rest
      Nothing     -> if currentState `member` finalStates
                      then Right []
                      else Left $ RuntimeError $ IllegalHaltState "" ""