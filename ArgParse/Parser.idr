-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A simple parser for command options.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module ArgParse.Parser

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import ArgParse.Model
import ArgParse.Utils

%access private

-- ----------------------------------------------------------------- [ Parsers ]

long : Parser String
long = do
    string "--"
    k <- map pack $ some alphaNum
    pure k

short : Parser String
short = do
    k <- string "-" *> alphaNum
    pure $ cast k

flagLong : Parser Arg
flagLong = map Flag long

flagShort : Parser Arg
flagShort = map Flag short

kvLong : Parser Arg
kvLong = do
    key <- long
    string "="
    value <- url
    pure $ KeyValue key value

kvShort : Parser Arg
kvShort = do
    k <- short
    space
    v <- url
    pure $ KeyValue k v

options : Parser Arg
options = kvShort <|> kvLong <|> flagShort <|> flagLong <?> "Options"

export
args : Parser $ List Arg
args = do
    os <- many (options <* spaces)
    fs <- many $ (url <* spaces)
    let os' = if isNil fs
      then os
      else (os ++ [Files fs])
    pure $ os'

-- --------------------------------------------------------------------- [ EOF ]
