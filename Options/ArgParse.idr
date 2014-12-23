-- ------------------------------------------------------------ [ ArgParse.idr ]
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Options.ArgParse

import Control.Monad.Identity

import public Effects
import public Effect.Exception

import public Lightyear.Core
import public Lightyear.Combinators
import public Lightyear.Strings

import public Options.ArgParse.Model

import Options.ArgParse.Parser

%access public

-- -------------------------------------------------------------------- [ Body ]

getOpts : (Arg -> Maybe a) -> List Arg -> {[EXCEPTION String]} Eff $ List a
getOpts _    Nil       = pure $ Nil
getOpts conv (x :: xs) = case conv x of
    Nothing => raise "Invalid Option"
    Just o  => do
      os <- getOpts conv xs
      pure (o :: os)

parseArgs : (Arg -> Maybe a) -> String -> {[EXCEPTION String]} Eff $ List a
parseArgs func txt = do
    case parse args txt of
      Left err  => raise $ err
      Right res => do
        r <- getOpts func res
        pure r

-- --------------------------------------------------------------------- [ EOF ]
