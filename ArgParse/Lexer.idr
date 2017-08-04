module ArgParse.Lexer

import public Text.Lexer


%default total
%access export

public export
data Token = Str String
           | Ch  String
           | SFlag   String
           | LFlag   String
           | Equals String
           | Quoted String
           | NumI Integer
           | NumD Double
           | WS String
           | Arg String
           | Unknown String

Show Token where
  show (Str    x) = unwords ["Str",   show x]
  show (Ch     x) = unwords ["Ch",   show x]
  show (LFlag    x) = unwords ["LFlag",   show x]
  show (SFlag    x) = unwords ["SFlag",   show x]
  show (Equals   x) = unwords ["Equals", show x]
  show (Quoted  x) = unwords ["Quoted", show x]
  show (NumI    x) = unwords ["Integer", show x]
  show (NumD    x) = unwords ["Double", show x]
  show (WS      x) = unwords ["WS",     show x]
  show (Arg    x) = unwords ["Arg",     show x]
  show (Unknown x) = unwords ["BAD TOKEN", show x]

doubleLit : Lexer
doubleLit = (is '-' <|> empty)
        <+> digits
        <+> (is '.' <+> (digits <|> empty) <|> empty)

ch : Lexer
ch = pred (isAlphaNum)

str : Lexer
str = some (pred isAlphaNum)

shortFlag : Lexer
shortFlag = is '-'

longFlag : Lexer
longFlag = is '-' <+> is '-'

equals : Lexer
equals = is '='

arg : Lexer
arg = any <+> manyTill any space

rawTokens : TokenMap Token
rawTokens =
  [ (space, WS)
  , (intLit, \x => NumI (cast x))
  , (doubleLit, \x => NumD (cast x))
  , (stringLit, Quoted)
  , (longFlag, LFlag)
  , (shortFlag, SFlag)
  , (equals, Equals)
  , (str, Str)
  , (ch, Lexer.Ch)
  , (arg, Arg)
  , (symbol, Unknown)
  ]

export
lex : String -> Either (Int, Int, String) (List (TokenData Token))
lex str
    = case Lexer.lex rawTokens str of
           (tok, (_, _, "")) => Right (filter notComment tok)
           (_, fail) => Left fail
    where
      notComment : TokenData Token -> Bool
      notComment t = case tok t of
                          WS _ => False
                          _ => True


-- --------------------------------------------------------------------- [ EOF ]
