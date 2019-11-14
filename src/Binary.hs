module Binary
    ( strToBytes
    , bytesToStr
    , BS.writeFile
    , BS.readFile
    ) where

import Data.Char (digitToInt, chr, intToDigit, ord)
import Numeric (showIntAtBase)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Util (chunksBy)

strToBytes :: String -> BS.ByteString
strToBytes = pack . map strToChar . chunksOf8 . strPad
        where
            strToChar = chr . strToNum
            chunksOf8 = chunksBy $ splitAt 8

strToNum :: [Char] -> Int
strToNum str = x1*1 + x2*2 + x4*4 + x8*8 + x16*16 + x32*32 + x64*64 + x128*128
        where
            [x128, x64, x32, x16, x8, x4, x2, x1] = map digitToInt $ take 8 str

strPad :: String -> String
strPad str = pad padLength str
    where
        padLength = if len == 0 then 0 else 8 - len
        len = length str `mod` 8
        pad l str
            | l == 0 = str 
            | otherwise = pad (l - 1) ('0' : str)

bytesToStr :: BS.ByteString -> String
bytesToStr = concatMap (strPad . intToBin . ord) . unpack
    where
        intToBin = flip (showIntAtBase 2 intToDigit) ""
