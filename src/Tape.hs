module Tape where

import Data.List (intersperse)
import Data.Maybe (fromMaybe)

import Direction

data Tape symbolType = Tape
    { left  :: [symbolType]
    , cell  :: symbolType
    , right :: [symbolType]
    } deriving Eq

fromList :: [symbolType] -> Int -> Tape symbolType
fromList list cellIndex =
  Tape (reverse left) cell right
    where
    (left, cell:right) = splitAt cellIndex list

toList :: Tape symbolType -> [symbolType]
toList (Tape left cell right) = (reverse left) ++ (cell:right)

shift :: Direction -> symbolType -> Tape symbolType -> Tape symbolType
shift L _ Tape {left = ls, cell = cell, right = r:rs} =
  Tape {left = cell:ls, cell = r, right = rs}

shift R _ Tape {left = l:ls, cell = cell, right = rs} =
  Tape {left = ls, cell = l, right = cell:rs}

-- Allow generation of blank symbols when shifting left.
shift L blankSymbol Tape {left = ls, cell = cell, right = []} =
  shift L blankSymbol (Tape ls cell [blankSymbol])

-- Allow generation of blank symbols when shifting right.
shift R blankSymbol Tape {left = [], cell = cell, right = rs} =
  shift R blankSymbol (Tape [blankSymbol] cell rs)

-- Allow shifting in neither direction
shift N _ tape = tape

write :: Maybe symbolType -> Tape symbolType -> Tape symbolType
write (Just value) tape = tape {cell = value}
write Nothing tape      = tape

read :: Tape symbolType -> symbolType
read tape = cell tape

instance Show symbolType => Show (Tape symbolType) where
  show tape =
    "Tape { " ++ left' ++ " | " ++ cell' ++ " | " ++ right' ++ " }"
      where
    showItemsWithSpaces a = concat . intersperse " " $ show <$> a
    left'  = showItemsWithSpaces $ reverse . left $ tape
    right' = showItemsWithSpaces $ right tape
    cell'  = show . cell $ tape