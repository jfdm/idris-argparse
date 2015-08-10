-- --------------------------------------------------------------- [ Error.idr ]
-- Module    : Error.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module ArgParse.Error

import public ArgParse.Model

data ArgError : Type where
  InvalidOption : Arg -> ArgError
  ParseError : String -> ArgError

instance (Show Arg) => Show ArgError where
  show (InvalidOption o) = "Invalid Option " ++ show o
  show (ParseError err)  = "Parsing Error\n" ++ err


-- --------------------------------------------------------------------- [ EOF ]
