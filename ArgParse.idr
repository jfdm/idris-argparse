-- ------------------------------------------------------------ [ ArgParse.idr ]
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module ArgParse

import Control.Monad.Identity

import public Effects
import public Effect.Exception
import public Effect.State

import public Lightyear.Core
import public Lightyear.Combinators
import public Lightyear.Strings

import public ArgParse.Model

import ArgParse.Parser

%access public

-- -------------------------------------------------------------------- [ Body ]

private
getOpts : (Arg -> Maybe a)
       -> List Arg
       -> Eff (List a) [EXCEPTION String]
getOpts _    Nil       = pure $ Nil
getOpts conv (x :: xs) = case conv x of
    Nothing => raise $ "Invalid Option " ++ show x
    Just o  => do
      os <- getOpts conv xs
      pure (o :: os)

||| Parse arguments and produce a list of options.
|||
||| @conv A user supplied conversion function used to convert..
||| @args The *unmodified* result of calling `System.getArgs` or `Effects.System.geArgs`.
parseArgs : (conv : Arg -> Maybe a)
          -> (args : List String)
          -> Eff (List a) [EXCEPTION String]
parseArgs func (a::as) = do
    case parse args (unwords as) of
      Left err  => raise err
      Right res => do
        r <- getOpts func res
        pure r
-- ----------------------------------------------------------------- [ Records ]

private
convOpts : (Arg -> a -> Maybe a)
        -> a
        -> List Arg
        -> Eff a [EXCEPTION String]
convOpts  _   o Nil       = pure o
convOpts conv o (x :: xs) = case conv x o of
    Nothing => raise $ "Invalid Option " ++ show x
    Just o' =>  do
      os <- convOpts conv o' xs
      pure os

||| Parse arguments using a record.
|||
||| @orig The starting value of the record representing the options.
||| @conv A user supplied conversion function used to update the record.
||| @args The *unmodified* result of calling `System.getArgs` or `Effects.System.geArgs`.
parseArgsRec : (orig : a)
             -> (conv : Arg -> a -> Maybe a)
             -> (args : List String)
             -> Eff a [EXCEPTION String]
parseArgsRec o func (a::as) = do
    case parse args (unwords as) of
      Left err  => raise err
      Right res => do
        r <- convOpts func o res
        pure r

-- --------------------------------------------------------------------- [ EOF ]
