module ArgParse.Parser.API

import Data.String.Views

import public Text.Parser
import ArgParse.Lexer


%default total
%access export

public export
Rule : Type -> Type
Rule ty = Grammar (TokenData Token) True ty

public export
EmptyRule : Type -> Type
EmptyRule ty = Grammar (TokenData Token) False ty


-- Some basic parsers used by all the intermediate forms

export
shortFlag : Rule ()
shortFlag
    = terminal (\x => case tok x of
                           SFlag _ => Just ()
                           _     => Nothing)

export
longFlag : Rule ()
longFlag
    = terminal (\x => case tok x of
                           LFlag _ => Just ()
                           _       => Nothing)

export
str : Rule String
str = terminal
    (\x => case tok x of
             Str s => Just s
             _     => Nothing)

export
chr : Rule String
chr = terminal
    (\x => case tok x of
             Ch s => Just s
             _     => Nothing)


export
arg : Rule String
arg = terminal
  (\x => case tok x of
           Arg s => Just s
           _     => Nothing)

export
equals : Rule ()
equals = terminal
  (\x => case tok x of
           Equals _ => Just ()
           _        => Nothing)


export
int : Rule Integer
int = terminal
  (\x => case tok x of
           NumI s => Just s
           _      => Nothing)

export
double : Rule Double
double = terminal
  (\x => case tok x of
           NumD s => Just s
           _      => Nothing)


export
quoted : Rule String
quoted = terminal
    (\x => case tok x of
             Quoted s => Just $ rmQuotes (unpack s)
             _        => Nothing)
  where
    rmQuotes : List Char -> String
    rmQuotes xs = pack $ filter (=='"') xs

-- --------------------------------------------------------------------- [ EOF ]
