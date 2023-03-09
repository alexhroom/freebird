module Main
  ( main
  ) where

import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Options
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , getDirectoryContents
  )
import System.Environment (getArgs)
import System.FilePath ((-<.>), (</>), normalise, takeDirectory)
import System.IO

import Blocks
import Header
  ( HeaderTOML(tomlHeaderBeginSrc, tomlHeaderEndSrc, tomlHeaderTangle,
           tomlHeaderWeave)
  , decodeHeader
  )
import Parse (parseToBlocks)

data MainOptions =
  MainOptions
    { optWeaveDir :: String
    , optTangleDir :: String
    }
  deriving (Show)

{-# ANN module "HLint: ignore Use <$>" #-}

instance Options MainOptions where
  defineOptions =
    pure MainOptions <*>
    defineOption
      optionType_string
      (\o ->
         o
           { optionLongFlags = ["weavedir"]
           , optionShortFlags = ['w']
           , optionDefault = ""
           , optionDescription =
               "The directory to weave to. Defaults to working directory."
           }) <*>
    defineOption
      optionType_string
      (\o ->
         o
           { optionLongFlags = ["tangledir"]
           , optionShortFlags = ['t']
           , optionDefault = ""
           , optionDescription =
               "The directory to tangle to. Defaults to working directory."
           })

main :: IO [()]
main = do
  argv <- getArgs
  let parsed = parseOptions argv :: ParsedOptions MainOptions
  let opts =
        fromMaybe
          MainOptions {optWeaveDir = "", optTangleDir = ""}
          (parsedOptions parsed)
  paths <- getRecursiveContents $ head $ parsedArguments parsed
  mapM (processFile (optWeaveDir opts) (optTangleDir opts)) paths

processFile :: String -> String -> FilePath -> IO ()
processFile weavedir tangledir file = do
  inputfile <- openFile file ReadMode
  contents <- hGetContents inputfile
  let blocks = concatBlocks . parseToBlocks $ contents
  print file
  if not (any isHeader blocks)
    then return ()
    else do
      let header = decodeHeader . blockContents . getHeader $ blocks
                -- get relevant strings from header
      let tanglefile = tangledir </> file -<.> unpack (tomlHeaderTangle header)
      let weavefile = weavedir </> file -<.> unpack (tomlHeaderWeave header)
      let bsrc = unpack $ tomlHeaderBeginSrc header
      let esrc = unpack $ tomlHeaderEndSrc header
      _ <- createDirectoryIfMissing True (takeDirectory tanglefile)
      _ <- createDirectoryIfMissing True (takeDirectory weavefile)
      tangle blocks tanglefile
      weave blocks weavefile bsrc esrc

tangle :: [Block] -> String -> IO ()
tangle = flip writeFile . blocksToString . tangleFilter

weave :: [Block] -> String -> String -> String -> IO ()
weave blocks filename bsrc esrc =
  writeFile filename $
  concatMap (blockContents . padCode bsrc esrc) $ weaveFilter blocks

getHeader :: [Block] -> Block
getHeader = head . filter isHeader

isHeader :: Block -> Bool
isHeader (Block Header _) = True
isHeader _ = False

tangleFilter :: [Block] -> [Block]
tangleFilter = filter isCode

weaveFilter :: [Block] -> [Block]
weaveFilter = filter notHiddenOrHeader
  where
    notHiddenOrHeader :: Block -> Bool
    notHiddenOrHeader (Block t _)
      | t == HiddenCode || t == Header = False
      | otherwise = True

padCode :: String -> String -> Block -> Block
padCode begin end block@(Block t txt)
  | t == Code || t == MultiCode = Block t (concat [begin, "\n", txt, "\n", end])
  | otherwise = block

blocksToString :: [Block] -> String
blocksToString = concatMap ((++ "\n\n") . blockContents)

-- from https://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <-
    forM properNames $ \name -> do
      let path = topdir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then getRecursiveContents path
        else return [normalise path]
  return (concat paths)
