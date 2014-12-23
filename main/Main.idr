-- ---------------------------------------------------------------- [ Main.idr ]
-- Description : Sample Innocation of ArgParse
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Main

import System
import Options.ArgParse

||| Programs Options
data Opt : Type where
  Verbose : Opt
  Args    : List String -> Opt
  Help    : Opt
  Version : Opt

-- Show instance
instance Show Opt where
  show (Verbose) = "Verbose"
  show (Args xs) = show $ "[Args " ++ show xs ++ "]"
  show (Help)    = "Help"
  show (Version) = "Version"

||| Convert Arguments into Options
convOpts : Arg -> Maybe Opt
convOpts (KeyValue k v) = Nothing
convOpts (Files xs) = Just $ Args xs
convOpts (Flag x) = case x of
    "verbose" => Just Verbose
    otherwise => Nothing


main : IO ()
main = do
  args <- getArgs
  let as = fromMaybe [] (tail' args)
  opts <- run $ parseArgs (convOpts) (unwords $ as)
  print opts
-- --------------------------------------------------------------------- [ EOF ]
