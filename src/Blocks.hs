module Blocks
  ( BlockType(Text, Code, HiddenCode, MultiCode, Header, Error, Empty)
  , Block(Block)
  , concatBlocks
  , blockContents
  , isCode
  ) where

{-
| A datatype which annotates the type of block:
- Text: a block of documentation text.
- Code: a block of single-line code, starting with '> ' in the document.
- HiddenCode: a block of single-line coding starting with '>* ' to be hidden.
- MultiCode: a multiline code block starting with >BEGIN and ending with >END.
- Header: the FREEBIRD header.
- Error: An error type to indicate a parser failure.
- Empty: A non-error empty block, to capture parser edge cases.
-}
data BlockType
  = Text
  | Code
  | HiddenCode
  | MultiCode
  | Header
  | Error
  | Empty
  deriving (Show, Eq)

{-
| Block annotates a string with a BlockType, documented above.
It is used to format a FREEBIRD file into woven or tangled format.
-}
data Block =
  Block BlockType String
  deriving (Show)

-- predicate for whether a block is of 'code' type
isCode :: Block -> Bool
isCode (Block t _)
  | t == Code || t == HiddenCode || t == MultiCode = True
  | otherwise = False

-- function which checks two blocks are both the same block type
sameType :: Block -> Block -> Bool
sameType (Block t1 _) (Block t2 _)
  | t1 == t2 = True
  | otherwise = False

-- function which concatenates two blocks; used to combine code lines into code blocks.
concatBlock :: Block -> Block -> Block
concatBlock (Block type1 val1) (Block _ val2) =
  Block type1 (val1 ++ "\n" ++ val2)

-- map of concatBlock which concatenates adjacent Code blocks in a document.
concatBlocks :: [Block] -> [Block]
concatBlocks [] = []
concatBlocks [x] = [x]
concatBlocks [x, y]
  | isCode x && isCode y && sameType x y = [concatBlock x y]
  | otherwise = [x, y]
concatBlocks (x:y:zs)
  | isCode x && isCode y && sameType x y = concatBlocks (concatBlock x y : zs)
  | otherwise = x : concatBlocks (y : zs)

-- accessor function to return the contents of a block.
blockContents :: Block -> String
blockContents (Block _ str) = str
