module Main

import System

import TestRunner
import Options.ArgParse

||| Programs Options
data Opt : Type where
  From    : String -> Opt
  Verbose : Opt
  Args    : List String -> Opt
  Help    : Opt
  Version : Opt

-- Show instance
instance Show Opt where
  show (From x)  = "[From " ++ x ++ "]"
  show (Verbose) = "[Verbose]"
  show (Args xs) = show $ "[Args " ++ show xs ++ "]"
  show (Help)    = "[Help]"
  show (Version) = "[Version]"

-- Eq Instance
instance Eq Opt where
  (==) (From x)  (From y)  = x == y
  (==) (Verbose) (Verbose) = True
  (==) (Args xs) (Args ys) = xs == ys
  (==) (Help)    (Help)    = True
  (==) (Version) (Version) = True
  (==) _         _         = False

||| Convert Arguments into Options
convOpts : Arg -> Maybe Opt
convOpts (KeyValue k v) = case k of
   "from"    => Just $ From v
   otherwise => Nothing
convOpts (Files xs) = Just $ Args xs
convOpts (Flag x) = case x of
    "help"    => Just Help
    "verbose" => Just Verbose
    otherwise => Nothing

test1 : Test
test1 = do
  as <- parseArgs (convOpts) "--verbose --help"
  if as == ([Verbose,Help])
    then pure ()
    else raise "Expected passing test failed."

test2 : Test
test2 = do
  as <- parseArgs (convOpts) "--from=\"value\""
  if as == ([From "value"])
    then pure ()
    else raise "Expected passing test failed."

-- -------------------------------------------------------------------- [ Main ]
main : IO ()
main = do
    run $ tests [test1,test2]
    exit 0

-- --------------------------------------------------------------------- [ EOF ]
