-- --------------------------------------------------------------- [ Utils.idr ]
-- Description : Parsing Utils.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module ArgParse.Utils

import Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

%access public
-- ----------------------------------------------------------------- [ Parsers ]
||| Any Char
anyChar : Parser Char
anyChar = satisfy (const True)

private
pathChar : Parser Char
pathChar = urlChar <|> satisfy isAlphaNum <?> "Path Char"
  where
    urlChar : Parser Char
    urlChar = do
      c <- satisfy (const True)
      case c of
        '\\' => pure '\\'
        '/'  => pure '/'
        '.'  => pure '.'
        ':'  => pure ':'
        '#'  => pure '#'
        '='  => pure '='
        '?'  => pure '?'
        '-'  => pure '-'
        _    => satisfy (const False)

||| Parse URIs
url : Parser String
url = map pack (some pathChar) <?> "URL"


literallyBetween : Char -> Parser String
literallyBetween c = map pack $ between (char c) (char c) (some (satisfy (/= c)))

-- --------------------------------------------------------------------- [ EOF ]
