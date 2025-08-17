module Rule where

import SyntaxCIPU

newtype Ig a = Ig a

data Rule res where
   Many     ::  Rule a     -> Rule [a]
   Seq      :: [Rule a]    -> Rule [a]
   Or       :: [Rule a]    -> Rule  a
   And      :: [Rule a]    -> Rule  a
   Not      ::  Rule a     -> Rule  a
   Then     ::  Rule a     -> Rule  b -> Rule (a, b)
   Apply    ::   Iso a b   -> Rule  a -> Rule b
   Count    ::  Lens a Int -> Rule  a -> Rule b -> Rule (a, [b])
   Pure     ::       a     -> Rule  a
   Token    ::  Rule a  
   ManyThen ::  Rule a     -> Rule  b -> Rule [a]
   Name     :: String      -> Rule  a -> Rule a
   Get      :: String      -> Rule  a
   Set      :: String      -> Rule  a -> Rule a
   Ignore   ::  Rule a     -> Rule (Ig a)