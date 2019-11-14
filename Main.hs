module Main where

import System.IO (getLine, readFile, writeFile)
import System.Environment (getArgs)
import Huffman (encodeString, decodeString)
import Binary (strToBytes, bytesToStr, writeFile)

main :: IO ()
main = do
    args <- getArgs
    case (length args) of
        0 -> interact (snd . encodeString)
        1 -> do
            input <- readFile $ head args
            let (key, encoded) = encodeString input
            print $ strToBytes encoded
            Binary.writeFile "enc/key.puff" $ strToBytes $ "1011"
            Binary.writeFile "enc/encoded.huff" $ strToBytes encoded
        2 -> do
            let [key, encoded] = args
            key <- readFile key
            encoded <- readFile encoded
            System.IO.writeFile "decoded.txt" "Hello!"
        otherwise -> error "Invalid call"
