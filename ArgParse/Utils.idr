-- --------------------------------------------------------------- [ Utils.idr ]
-- Description : Parsing Utils.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module ArgParse.Utils

import Control.Monad.Identity

import Lightyear
import Lightyear.Char
import Lightyear.Strings

%access export
-- ----------------------------------------------------------------- [ Parsers ]

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

-- --------------------------------------------------------------------- [ EOF ]
