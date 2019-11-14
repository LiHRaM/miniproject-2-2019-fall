module Sorted
    ( Sorted (Sorted)
    , fromUnsorted
    )
    where

import Data.List

data Sorted a = Sorted a

fromUnsorted :: Ord a => [a] -> Sorted [a]
fromUnsorted lst = Sorted $ sort lst