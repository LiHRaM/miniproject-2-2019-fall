module Huffman
    ( encodeString
    , decodeString
    ) where
    
import Data.List
import Data.Ord
import Data.Map (Map, fromList, (!))
import Sorted
import Util (chunksBy)

type Prefix = String
type CharPrefix = (Char, Prefix)
type CharacterCount = (Char, Int)

count_characters :: Sorted String -> [CharacterCount]
count_characters (Sorted []) = []
count_characters (Sorted lst) = (current, count) : (count_characters rest)
    where
        current = head lst
        count = length $ takeWhile (\y -> y == current) lst
        rest = Sorted $ drop count lst

sortByCount :: [CharacterCount] -> [CharacterCount]
sortByCount lst = sortOn snd lst

data HuffTree = Leaf Int Char | Node Int HuffTree HuffTree
    deriving Show

toLeaf :: CharacterCount -> HuffTree
toLeaf (char, count) = Leaf count char

getCount :: HuffTree -> Int
getCount (Leaf count _)     = count
getCount (Node count _ _)   = count

foldFunc :: HuffTree -> HuffTree -> HuffTree
foldFunc l1 l2 = Node count l1 l2
    where
        count = (getCount l1) + (getCount l2)

fold_leaved :: [HuffTree] -> HuffTree
fold_leaved lst
    | isDone    = folded
    | otherwise = fold_leaved new_lst
    where
        isDone = (length to_drop) == 0
        ([l1, l2], to_drop) = splitAt 2 lst
        folded = foldFunc l2 l1
        new_lst = sortOn getCount (folded : to_drop)

getPrefix :: HuffTree -> [CharPrefix]
getPrefix tree = getPrefixInner tree ""
    where
        getPrefixInner (Node _ left right) prefix = (getPrefixInner left (prefix ++ "1")) ++ (getPrefixInner right (prefix ++ "0"))
        getPrefixInner (Leaf _ char) prefix = [(char, prefix)]

encodeString :: String -> (String, String)
encodeString str = (prefixToString prefix, encoded)
    where
        encoded = concatMap (\x -> prefix_map ! x) str
        prefix_map = fromList prefix
        prefix = getPrefix . fold_leaved . (map toLeaf) . count_characters $ fromUnsorted str

prefixToString :: [CharPrefix] -> String
prefixToString l = concatMap showPrefix l
    where
        showPrefix :: CharPrefix -> String
        showPrefix (c, p) = c : p ++ ['\n']

byPrefix :: String -> (String, String)
byPrefix [] = error "byPrefix received empty list"
byPrefix str = (char:prefix, drop (1 + length prefix) rest)
    where
        ([char], rest) = splitAt 1 str
        prefix = takeWhile (\x -> x /= '\n') rest

stringToSinglePrefix :: String -> CharPrefix
stringToSinglePrefix [] = error "Derp..."
stringToSinglePrefix (x:xs) = (x, xs)

decodeStringInner :: [CharPrefix] -> String -> String
decodeStringInner p [] = error "Derpppp"
decodeStringInner p str = char : decodeStringInner p (drop prefix_len str)
    where
        prefix_len = length encoding
        (char, encoding) = head [x | x <- p, (snd x) `isPrefixOf` str]

decodeString :: String -> String -> String
decodeString p [] = error "Empty list bro!"
decodeString p str = decodeStringInner prefixTree str
    where
        prefix_raw = chunksBy byPrefix p
        prefixTree = map stringToSinglePrefix prefix_raw
