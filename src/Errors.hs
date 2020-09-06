module Errors where

import Data.Set as Set (Set)
import Control.Monad.Except (ExceptT)

nameWithValue :: (String, String) -> String
nameWithValue (name, value) = "'" ++ name ++ "'" ++ " (" ++ value ++ ")"


data DefinitionError = NotAMember (String, String) (String, String)
                     | NotASubset (String, String) (String, String)

instance Show DefinitionError where
  show (NotAMember item set) =
    "NotAMember: " ++ nameWithValue item ++ " is not a member of the set " ++ nameWithValue set
  show (NotASubset set superset) =
    "NotASubset: " ++ nameWithValue set ++ " is not a subset of the set " ++ nameWithValue superset


data RuntimeError = IllegalTapeInput String String
                  | IllegalHaltState String String

instance Show RuntimeError where
  show (IllegalTapeInput tape inputSymbols) =
    "IllegalTapeInput: The input tape (" ++ tape ++ ") contains symbols not in the set " ++ nameWithValue ("inputSymbols", inputSymbols)
  show (IllegalHaltState state finalStates) =
    "IllegalHaltState: The TuringMachine halted in state `" ++ state ++ "`, which is not a member of the set " ++ nameWithValue ("finalStates", finalStates)


data TuringMachineError = DefinitionError DefinitionError
                        | RuntimeError RuntimeError

instance Show TuringMachineError where
  show (DefinitionError error) = "DefinitionError: " ++ show error
  show (RuntimeError error)    = "RuntimeError: " ++ show error


type TuringMachineErrorM = Either TuringMachineError