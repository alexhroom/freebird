{-# LANGUAGE OverloadedStrings #-}
module Header 
       (
         decodeHeader
       , HeaderTOML(tomlHeaderBeginSrc, tomlHeaderEndSrc, tomlHeaderWeave, tomlHeaderTangle)
       ) where 

import Toml (TomlCodec, (.=))
import qualified Toml
import Data.Text (Text, pack)

data HeaderTOML = HeaderTOML 
    { tomlHeaderWeave :: !Text
    , tomlHeaderTangle :: !Text
    , tomlHeaderBeginSrc :: !Text
    , tomlHeaderEndSrc :: !Text
    } | HeaderError deriving Show

headerCodec :: TomlCodec HeaderTOML
headerCodec = HeaderTOML
       <$> Toml.text "weave"     .= tomlHeaderWeave
       <*> Toml.text "tangle"    .= tomlHeaderTangle
       <*> Toml.text "begin_src" .= tomlHeaderBeginSrc
       <*> Toml.text "end_src"   .= tomlHeaderEndSrc

decodeHeader :: String -> HeaderTOML
decodeHeader = handleError . Toml.decode headerCodec . pack
             where 
               handleError :: Either [Toml.TomlDecodeError] HeaderTOML -> HeaderTOML
               handleError (Right msg) = msg
               handleError (Left _) = HeaderError
