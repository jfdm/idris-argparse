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
shortFlag : Rule String
shortFlag
    = terminal (\x => case tok x of
                           SFlag f => Just (substr 1 (length f) f)
                           _     => Nothing)

export
longFlag : Rule String
longFlag
    = terminal (\x => case tok x of
                           LFlag f => Just (substr 2 (length f) f)
                           _       => Nothing)

export
arg : Rule String
arg = terminal
  (\x => case tok x of
           Arg s => Just (trim s)
           _     => Nothing)

export
equals : Rule ()
equals = terminal
  (\x => case tok x of
           Equals _ => Just ()
           _        => Nothing)

export
quoted : Rule String
quoted = terminal
    (\x => case tok x of
             Quoted s => Just $ rmQuotes s
             _        => Nothing)
  where
    rmQuotes : String -> String
    rmQuotes xs = pack $ filter (not . (==) '"') (unpack xs)

-- --------------------------------------------------------------------- [ EOF ]
