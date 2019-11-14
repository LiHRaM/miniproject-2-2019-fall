module Huffman
( encodeString
, decodeString
) where
    
import Data.List
import Data.Ord
import Data.Map (Map, fromList, (!))

type Prefix = String
type CharacterCount = (Char, Int)
type CharPrefix = (Char, Prefix)

count_characters :: String -> [CharacterCount]
count_characters [] = []
count_characters lst = (current, count) : (count_characters rest)
    where
        current = head lst
        count = length $ takeWhile (\y -> y == current) (lst)
        rest = drop count lst

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

encodeString :: String -> ([CharPrefix], String)
encodeString str = (prefix, encoded)
        where
            encoded = concat $ map (\x -> prefix_map ! x) str
            prefix_map = fromList prefix
            prefix = getPrefix . fold_leaved . (map toLeaf) $ count_characters str

decodeString :: [CharPrefix] -> String -> String
decodeString prefix [] = []
decodeString prefix str = char : decodeString prefix (drop prefix_len str)
    where
        prefix_len = length encoding
        (char, encoding) = head [x | x <- prefix, (snd x) `isPrefixOf` str]

