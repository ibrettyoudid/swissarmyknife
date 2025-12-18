module NewTuple where

import Data.Typeable

data a :- b = a :- b deriving (Eq, Show, Typeable)

infixr 1 :-

