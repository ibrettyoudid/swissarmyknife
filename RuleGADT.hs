module RuleGADT where

import SyntaxCIPU
import MHashDynamic
import Data.Word

newtype Ig a = Ig a

data Rule i n v f r where
   Many     ::  Rule i n v f a  -> Rule i n v f [a]
   Seq      :: [Rule i n v f a] -> Rule i n v f [a]
   Alt      :: [Rule i n v f a] -> Rule i n v f a
   And      :: [Rule i n v f a] -> Rule i n v f a
   Not      ::  Rule i n v f a  -> Rule i n v f a
   Then     ::  Rule i n v f a  -> Rule i n v f b -> Rule i n v f (a, b)
   ManyThen ::  Rule i n v f a  -> Rule i n v f b -> Rule i n v f [a]
   Apply    ::   Iso a b        -> Rule i n v f a -> Rule i n v f b 
   Count    ::  Lens a Int      -> Rule i n v f a -> Rule i n v f b -> Rule i n v f (a, [b])
   Pure     ::       a          -> Rule i n v f a
   Try      ::  Rule i n v f a  -> Rule i n v f a
   AnyToken ::  Rule t n v f t
   Token    :: (i ~ a) => i     -> Rule i n v f a
   Range    :: (i ~ a) => i     ->      i         -> Rule i n v f a
   Let      ::       f          -> Rule i n v f a -> Rule i n v f a
   Get      ::       n                            -> Rule i n v f v
   Set      ::       n          -> Rule i n v f v -> Rule i n v f a
   Name     ::  String          -> Rule i n v f a -> Rule i n v f a
