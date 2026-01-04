module Zero where

import Data.Text as T

class Zero a where
   zero :: a

{-
instance Num a => Zero a where
      zero = 0
-}
instance Zero Int where
   zero = 0

instance Zero Integer where
   zero = 0

instance Zero [a] where
   zero = []

instance (Zero a, Zero b) => Zero (a, b) where
   zero = (zero, zero)

instance Zero T.Text where
   zero = T.empty

