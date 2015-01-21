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

manyTill : Monad m => ParserT m String a
                   -> ParserT m String b
                   -> ParserT m String (List a)
manyTill p end = scan
  where
    scan = do { end; return List.Nil } <|>
           do { x <- p; xs <- scan; return (x::xs)}


||| EOL
eol : Parser ()
eol = char '\n'

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
