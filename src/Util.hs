module Util
    ( chunksBy
    )
    where

chunksBy :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunksBy fun [] = []
chunksBy fun lst = chunk : chunksBy fun remainder
    where (chunk, remainder) = fun lst