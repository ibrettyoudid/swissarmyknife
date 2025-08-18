module Rule where

import SyntaxCIPU
import MHashDynamic
import Data.Word

newtype Ig a = Ig a

data Rule res where
   Many     ::  Rule a     -> Rule [a]
   Seq      :: [Rule a]    -> Rule [a]
   Alt      :: [Rule a]    -> Rule  a
   And      :: [Rule a]    -> Rule  a
   Not      ::  Rule a     -> Rule  a
   Then     ::  Rule a     -> Rule  b   -> Rule (a, b)
   ManyThen ::  Rule a     -> Rule  b   -> Rule [a]
   Apply    ::   Iso a b   -> Rule  a   -> Rule b
   Count    ::  Lens a Int -> Rule  a   -> Rule b -> Rule (a, [b])
   Pure     ::       a     -> Rule  a
   Try      ::  Rule a     -> Rule  a
   AnyToken ::  Rule Char
   Token    ::       Char  -> Rule Char
   Range    ::       Char  ->      Char -> Rule Char
   Get      ::  String     -> Rule Dynamic
   Set      :: Typeable a =>
                String     -> Rule  a   -> Rule a
   Name     ::  String     -> Rule  a   -> Rule a
