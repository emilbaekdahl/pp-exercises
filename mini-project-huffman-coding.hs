-- | Emil Bækdahl <ebakda16@student.aau.dk>

import           Data.List
import           Data.Ord
import           Prelude
import           System.Environment
import           Text.Show


-- | Node data type used to describe a Huffman tree.
data Node = Branch Node Node Int | Leaf Char Int deriving (Show, Read)


-- | Bit data type used to describe the bits and bit strings that we wish to
-- encode Strings as.
data Bit = Zero | One
type BitString = [Bit]


-- | When displaying a Bit and BitString on screen, we wish to see a String of
-- 0s and 1s and not a list of Zero's and One's. Thus, we define a custom
-- implementation of the Show type class functions for Bit.
instance Show Bit where
  show Zero = "0"
  show One  = "1"
  showList bitString = ((concatMap show bitString) ++)


-- | Converts a String of 0s and 1s to a BitString by a simple mapping of
-- '0's to Zero's and '1's to One's.
stringToBitString :: String -> BitString
stringToBitString = map (\char -> case char of
                                    '0' -> Zero
                                    '1' -> One
                                    _   -> error $ "Cannot convert character to Bit: " ++ [char])


-- | Returns the character of a Leaf.
getChar :: Node -> Char
getChar (Leaf char _) = char


-- | Returns the weight of a Branch or Leaf.
getWeight :: Node -> Int
getWeight (Leaf _ weight)     = weight
getWeight (Branch _ _ weight) = weight


-- | Finds the frequency of characters occuring in a String by mapping each
-- character to a tuple containing the character in question and its frequency.
charFrequencies :: String -> [(Char, Int)]
charFrequencies str = map (\char -> (char, frequency char)) $ nub str
  where frequency :: Char -> Int
        frequency char = length [c | c <- str, c == char]


-- | Inserts a node in a list of Nodes such that ordering by weight is
-- preserved. This, of course, assumes that the list of Nodes is already
-- ordered by weight.
insertNode :: Node -> [Node] -> [Node]
insertNode node [] = [node]
insertNode node nodes
  | getWeight node <= getWeight head = node : nodes
  | otherwise = head : insertNode node tail
  where (head:tail) = nodes


-- | Builds a Huffman tree from a list of Nodes. First, the list of Nodes is
-- sorted by weight. Then a helper that recursivly builds Branches is called.
-- When we are left with a single Node in the list we return it since that is
-- the root of the tree.
buildTree :: [Node] -> Node
buildTree = buildTree' . (sortBy $ comparing getWeight)
  where buildTree' :: [Node] -> Node
        buildTree' [root] = root
        buildTree' (head:hhead:tail) = buildTree' $ insertNode node tail
          where node = Branch head hhead $ getWeight head + getWeight hhead


-- | Wrapper around buildTree that builds a Huffman tree from a String. Here,
-- we computes the character frequencies in the String and map those to Nodes
-- before we call buildTree.
buildTreeFromString :: String -> Node
buildTreeFromString = buildTree . (map $ uncurry Leaf) . charFrequencies


-- | Encodes a single character as a BitString according to a Huffman tree. When
-- we meet a Leaf and its character matches the one we are looking for, we
-- return the current bit string; otherwise Nothing. When we meet a Branch, we
-- check both its children for the character and add Zero or One to the bit
-- string depending on whether we take the left or right branch. If none of the
-- branches contain the character in question, we return Nothing.
--
-- Remark: This function uses the Maybe Monad for handling missing characters
-- in the tree. We could also just throw an error and thus get around the use
-- of Maybe.
encodeChar :: Node -> Char -> BitString
encodeChar tree char = case encodeChar' tree [] of
                         Just bitString -> bitString
                         Nothing        -> []
  where encodeChar' :: Node -> BitString -> Maybe BitString
        encodeChar' (Leaf c _) bitString
          | c == char = Just bitString
          | otherwise = Nothing
        encodeChar' (Branch t1 t2 _) bitString
          | Just bitString' <- encodeChar' t1 (bitString ++ [Zero]) = Just bitString'
          | Just bitString' <- encodeChar' t2 (bitString ++ [One])  = Just bitString'
          | otherwise = Nothing


-- | Encodes a String as a BitString according to a Huffman tree. This maps
-- every character in the String to a BitString using encodeChar.
encode :: Node -> String -> BitString
encode tree = foldl (\bitString char -> bitString ++ encodeChar tree char) []


-- | Decodes a BitString to a String according to a Huffman tree. This function
-- iterates through each Bit in the BitString. Depending on whether we see a
-- Zero or a One in the bit string we proceed to the left or right branch
-- respectively. When we reach a Leaf, we return its character and start from
-- the root node again with the remainder of the bit string. The base case is
-- the empty BitString which maps to the empty String.
decode :: Node -> BitString -> String
decode tree bitString = decode' tree bitString
  where decode' :: Node -> BitString -> String
        decode' (Branch _ _ _) [] = ""
        decode' (Leaf char _) bitString = char : decode' tree bitString
        decode' (Branch tree1 tree2 _) (head:tail) = decode' subtree tail
          where subtree = case head of
                            Zero -> tree1
                            One  -> tree2


-- | The main function of the program exposes a simple command-line interface
-- that allows the user to 1) produce the Huffman tree of an input String,
-- 2) encode an input String as a BitString, and 2) decode a BitString to a
-- String given file containing a Huffman tree.
main :: IO  ()
main = do
  args <- getArgs
  case args of
    ["tree", content] -> print tree
      where tree = buildTreeFromString content
    ["encode", content] -> print $ encode tree content
      where tree = buildTreeFromString content
    ["decode", treeFile, content] -> do
      fileContent <- readFile treeFile
      print $ decode (read fileContent :: Node) bitString
        where bitString = stringToBitString content
    _ -> putStr $ unlines ["Usage:",
                           "\ttree \"Some string ...\" (Prints the Huffman tree for an input)",
                           "\tencode \"Some string ...\" (Encodes a string as a bit string)",
                           "\tdecode treeFile.txt \"Some string ...\" (Decodes a string given a Huffman tree)"]
