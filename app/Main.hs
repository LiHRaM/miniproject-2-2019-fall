module Main where

import System.IO (getLine, readFile, writeFile)
import System.Environment (getArgs)
import Huffman (encodeString, decodeString)
import Binary (strToBytes, bytesToStr, writeFile)
import Data.List (sort)

main :: IO ()
main = do
    args <- getArgs
    run args

run :: [String] -> IO ()
run args
    | isStdin = interact (snd . encodeString)
    | isEncodeFile = do
        input <- readFile $ head args
        let (key, encoded) = encodeString input
        System.IO.writeFile "key.puff" $ key
        Binary.writeFile "encoded.huff" $ strToBytes encoded
    | isDecodeFile = do
        let [key, encoded] = args
        key <- readFile key
        encoded <- readFile encoded
        System.IO.writeFile "decoded.txt" $ decodeString key encoded
    | otherwise = error "Invalid call"
    where
        len = length args
        isStdin = len == 0
        isEncodeFile = len == 1
        isDecodeFile = len == 2
