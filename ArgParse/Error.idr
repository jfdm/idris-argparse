-- --------------------------------------------------------------- [ Error.idr ]
-- Module    : Error.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module ArgParse.Error

import public ArgParse.Model

import ArgParse.Parser

%access public export

data ArgParseError : Type where
  InvalidOption : Arg -> ArgParseError
  ParseError : ParseError -> ArgParseError

implementation (Show Arg) => Show ArgParseError where
  show (InvalidOption o) = "Invalid Option " ++ show o
  show (ParseError err)  = "Parsing Error\n" ++ show err

-- --------------------------------------------------------------------- [ EOF ]
