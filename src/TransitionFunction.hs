module TransitionFunction where

import Tape
import Instruction

defineTransitionFunction :: (Eq symbolType, Eq stateType)
                         => [Instruction symbolType stateType]
                         -> (  symbolType
                            -> stateType
                            -> Tape symbolType
                            -> Maybe (stateType, Tape symbolType)
                            )
defineTransitionFunction [] = (\_ _ _ -> Nothing)
defineTransitionFunction (instruction:instructions) =
  f'
  where
    f = defineTransitionFunction instructions
    f' blankSymbol currentState tape
      |  currentState == state  instruction
      && cell tape    == symbol instruction =
        Just ( nextState instruction
             , shift (shiftDirection instruction) blankSymbol
               . write (newSymbol instruction) $ tape)
      | otherwise = f blankSymbol currentState tape