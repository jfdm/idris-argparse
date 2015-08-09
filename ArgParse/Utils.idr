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
    scan : Monad m => ParserT m String (List a)
    scan = do { end; return List.Nil } <|>
           do { x <- p; xs <- scan; return (x::xs)}

||| EOL
eol : Parser ()
eol = char '\n' *> return () <?> "eol"

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


literallyBetweenLR : Char -> Char -> Parser String
literallyBetweenLR l r =
    map pack $ between (lexeme $ char l) (lexeme $ char r) (some (satisfy (/= r)))

literallyBetween : Char -> Parser String
literallyBetween c = literallyBetweenLR c c

-- --------------------------------------------------------------------- [ EOF ]
