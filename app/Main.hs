module Main (main) where

import System.IO
import System.FilePath.Posix(takeBaseName)
import Data.Text (unpack)
import System.Environment(getArgs)

import Parse(parseToBlocks)
import Blocks
import Header(decodeHeader, HeaderTOML(tomlHeaderBeginSrc, tomlHeaderEndSrc, tomlHeaderWeave, tomlHeaderTangle))

main :: IO ()
main = do
         file <- getArgs
         processFile $ head file

processFile :: String -> IO ()
processFile file = do
                inputfile <- openFile file ReadMode
                contents <- hGetContents inputfile
                let blocks = concatBlocks $ parseToBlocks contents
                let header = decodeHeader $ blockContents $ getHeader blocks

                -- get relevant strings from header
                let tanglefile = changeFileEnding "example.fb" (unpack $ tomlHeaderTangle header)
                let weavefile = changeFileEnding "example.fb" (unpack $ tomlHeaderWeave header)
                let bsrc = unpack $ tomlHeaderBeginSrc header
                let esrc = unpack $ tomlHeaderEndSrc header

                tangle blocks tanglefile
                weave blocks weavefile bsrc esrc

tangle :: [Block] -> String -> IO()
tangle = flip writeFile . blocksToString . tangleFilter

weave :: [Block] -> String -> String -> String -> IO()
weave blocks filename bsrc esrc = writeFile filename 
                                  $ concatMap blockContents
                                  $ map (padCode bsrc esrc) 
                                  $ weaveFilter blocks

getHeader :: [Block] -> Block
getHeader = head . filter isHeader
        where isHeader :: Block -> Bool
              isHeader (Block Header _) = True
              isHeader _ = False

tangleFilter :: [Block] -> [Block]
tangleFilter = filter isCode

weaveFilter :: [Block] -> [Block]
weaveFilter = filter notHiddenOrHeader
        where notHiddenOrHeader :: Block -> Bool
              notHiddenOrHeader (Block t _)
                                | t == HiddenCode || t == Header = False
                                | otherwise = True

padCode :: String -> String -> Block -> Block
padCode begin end block@(Block t txt) 
        | t == Code || t == MultiCode = Block t (concat [begin, "\n", txt, "\n", end])
        | otherwise = block

changeFileEnding :: String -> String -> String
changeFileEnding = (++) . takeBaseName

blocksToString :: [Block] -> String
blocksToString = concatMap ((++ "\n\n") . blockContents)
