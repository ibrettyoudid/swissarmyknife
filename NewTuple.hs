module NewTuple where

data a :- b = a :- b deriving (Eq, Show)

infixr 1 :-

