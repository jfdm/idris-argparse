-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A simple parser for command options.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module ArgParse.Parser

import Control.Monad.Identity

import Lightyear
import Lightyear.Strings

import ArgParse.Model
import ArgParse.Utils

%access private

-- ----------------------------------------------------------------- [ Parsers ]

long : Parser String
long = do
    string "--"
    k <- map (pack) $ some (satisfy (isAlphaNum))
    pure k

short : Parser String
short = do
    k <- string "-"*> satisfy isAlphaNum
    pure $ cast k

flagLong : Parser Arg
flagLong = map Flag long

flagShort : Parser Arg
flagShort = map Flag short

kvLong : Parser Arg
kvLong = do
    key <- long
    string "="
    value <- literallyBetween '"'
    pure $ KeyValue key value

kvShort : Parser Arg
kvShort = do
    k <- short
    char ' '
    v <- literallyBetween '"'
    pure $ KeyValue k v

options : Parser Arg
options = kvShort <|> kvLong <|> flagShort <|> flagLong <?> "Options"

public
args : Parser $ List Arg
args = do
    os <- many (options <* space)
    fs <- many $ (url <* space)
    let os' = if isNil fs
      then os
      else (os ++ [Files fs])
    pure $ os'

-- --------------------------------------------------------------------- [ EOF ]
