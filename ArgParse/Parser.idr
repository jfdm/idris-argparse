-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A simple parser for command options.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module ArgParse.Parser

import ArgParse.Lexer
import ArgParse.Parser.API

import ArgParse.Model

%access private

-- ----------------------------------------------------------------- [ Parsers ]

long : Rule String
long = do
    longFlag
    k <- str
    pure k

short : Rule String
short = do
    shortFlag
    k <- chr
    pure $ k

flagLong : Rule Arg
flagLong = do
  l <- long
  pure $ Flag l

flagShort : Rule Arg
flagShort = do
   s <- short
   pure $ Flag s

kvLong : Rule Arg
kvLong = do
    key <- long
    equals
    value <- (str <|> arg <|> quoted)
    pure $ KeyValue key value

kvShort : Rule Arg
kvShort = do
    k <- short
    v <- (str <|> arg <|> quoted)
    pure $ KeyValue k v

options : Rule Arg
options = kvShort <|> kvLong <|> flagShort <|> flagLong

args : EmptyRule $ List Arg
args = do
    os <- many options
    fs <- many arg
    let os' = if isNil fs
      then os
      else (os ++ [Files fs])
    pure $ os'

public export
data ParseError = ParseFail String (Maybe (Int, Int)) (List Token)
                | LexFail (Int, Int, String)

export
Show ParseError where
  show (ParseFail err loc toks)
      = "Parse error: " ++ err ++ " at " ++ show loc ++ "\n" ++ show toks
  show (LexFail (c, l, str))
      = "Lex error at " ++ show (c, l) ++ " input: " ++ str

export
parseArgs : String -> Either ParseError (List Arg)
parseArgs str =
  case Lexer.lex str of
    Left err => Left (LexFail err)

    Right toks =>
      case parse toks args of

            Left (Error err []) =>
              Left $ ParseFail err Nothing []

            Left (Error err (t :: ts)) =>
              Left $ ParseFail err (Just (line t, col t))
                                   (map tok (t :: ts))
            Right (val, _) => Right val

-- --------------------------------------------------------------------- [ EOF ]
