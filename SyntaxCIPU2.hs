-- Copyright 2025 Brett Curtis
module SyntaxCIPU2 where

data Iso alpha beta
   = Iso String (alpha -> Maybe beta) (beta -> Maybe alpha)

instance Show (Iso a b) where
   show (Iso n f g) = n
 
