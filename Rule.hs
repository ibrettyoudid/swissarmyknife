module Rule where

import SyntaxCIPU

data Rule res where
   Many     ::  Rule a   -> Rule [a]
   Seq      :: [Rule a]  -> Rule [a]
   Or       :: [Rule a]  -> Rule  a
   And      :: [Rule a]  -> Rule  a
   Not      ::  Rule a   -> Rule  a
   Then     ::  Rule a   -> Rule  b -> Rule (a, b)
   Apply    ::   Iso a b -> Rule  a -> Rule b
   Pure     ::       a   -> Rule  a
   Token    ::  Rule a
   ManyThen ::  Rule a   -> Rule  b -> Rule [a]

