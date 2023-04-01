{-# LANGUAGE FlexibleContexts #-}

module Parse
  ( parseToBlocks
  ) where

import Blocks
import Text.Parsec

{-
| General freebird parser for all block types.
Parameters:
    string: the string to be parsed.
Returns:
    [Block]: the string as a list of blocks, annotated with a blocktype.
-}
parseToBlocks :: String -> [Block]
parseToBlocks = handleError . parse parseFreebird ""
  where
    handleError :: Either ParseError [Block] -> [Block]
    handleError (Right msg) = msg
    handleError (Left _) = [Block Blocks.Error "Parser error!"]

-- parser which parses a whole freebird document into blocks
parseFreebird :: Parsec String st [Block]
parseFreebird = many (header <|> codeBlock <|> textBlock)

-- parser for single lines of code, starting with a '> '
markedCodeLine :: Parsec String st Block
markedCodeLine = do
  _ <- string "> "
  block <- manyTill anyChar (string "\n")
  return $ Block Code block

-- parser for hidden single lines of code, starting with a '>* '
hiddenCodeLine :: Parsec String st Block
hiddenCodeLine = do
  _ <- string ">* "
  block <- manyTill anyChar (string "\n")
  return $ Block HiddenCode block

-- parser for multiline code blocks, starting with >BEGIN and ending with >END
multiCodeLine :: Parsec String st Block
multiCodeLine = do
  _ <- string ">BEGIN\n"
  block <- untilP (string ">END\n")
  _ <- string ">END\n"
  return $ Block MultiCode block

-- parses any type of code block; combines the previous three parsers
codeBlock :: Parsec String st Block
codeBlock = try markedCodeLine <|> try hiddenCodeLine <|> try multiCodeLine


-- parses text; anything which isn't a codeblock or header.
paragraph :: Parsec String st Block
paragraph = do
  block <- untilP $ codeBlock <|> quoteLine <|> (eof >> return (Block Blocks.Empty ""))
  if block /= ""
    then return $ Block Text block
    else return $ Block Blocks.Empty ""

-- parses text starting with the >\ token
quoteLine :: Parsec String st Block
quoteLine = do
  _ <- string ">\\ "
  block <- manyTill anyChar (string "\n")
  return $ Block Text ("> " ++ block ++ "\n")

textBlock :: Parsec String st Block
textBlock = try quoteLine <|> try paragraph


-- parser for FREEBIRD header, beginning with FREEBIRD--> and ending with <--
header :: Parsec String st Block
header = do
  _ <- string "FREEBIRD-->\n"
  block <- manyTill anyChar (string "<--")
  return $ Block Header block


-- helper function which parses until reaching a different type of parser
untilP :: Stream s m Char => ParsecT s u m a -> ParsecT s u m String
untilP p = do
  s <- many $ noneOf "\n"
  _ <- newline
  s' <- try (lookAhead p >> return "") <|> untilP p
  return $ s ++ "\n" ++ s'
