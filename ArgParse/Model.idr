-- --------------------------------------------------------------- [ Model.idr ]
-- Description : The Model
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module ArgParse.Model

data Arg : Type where
  Flag : String -> Arg
  KeyValue : String -> String -> Arg
  Files : List String -> Arg

instance Show Arg where
  show (Flag f) = "[Flag " ++ show f ++ "]"
  show (KeyValue k v) = "[KeyValue " ++ show k ++ " : " ++ show v ++ "]"
  show (Files fs) = "[Files " ++ show fs ++ "]"

-- --------------------------------------------------------------------- [ EOF ]
