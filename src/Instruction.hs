module Instruction where

import Direction
import Tape

data Instruction symbolType stateType =
  Instruction
    { state          :: stateType
    , symbol         :: symbolType
    , newSymbol      :: Maybe symbolType
    , shiftDirection :: Direction
    , nextState      :: stateType
    }

fromFiveTuple :: ( stateType
                 , symbolType
                 , Maybe symbolType
                 , Direction
                 , stateType )
              -> Instruction symbolType stateType
fromFiveTuple (state, symbol, newSymbol, moveDirection, nextState) =
  Instruction
    state
    symbol
    newSymbol
    shiftDirection
    nextState
      where
    shiftDirection =
      case moveDirection of
        L -> R
        R -> L
        N -> N

fromFourTuple :: ( stateType
                 , symbolType
                 , Either (Maybe symbolType) Direction
                 , stateType )
              -> Instruction symbolType stateType
fromFourTuple (state, symbol, operation, nextState) =
  Instruction
    state
    symbol
    newSymbol
    shiftDirection
    nextState
      where
    (newSymbol, moveDirection) =
      case operation of
        Left (Just value) -> (Just value, N)
        Right direction   -> (Nothing   , direction)
        _                 -> (Nothing   , N)
    shiftDirection =
      case moveDirection of
        L -> R
        R -> L
        N -> N