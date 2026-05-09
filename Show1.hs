{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Show1 where

import Data.List

class Show1 a where
   show1 :: a -> String

instance {-# OVERLAPPING #-} Show1 String where
   show1 = id

#ifdef SHOW1DEFAULT
instance (Show a) => Show1 a where
   show1 = show
#else
instance Show1 Int where
   show1 = show

instance Show1 Bool where
   show1 = show
   
instance Show1 Char where
   show1 = show
   
instance Show1 Double where
   show1 = show
   
instance Show1 Rational where
   show1 = show
   
instance Show1 a => Show1 [a] where
   show1 xs = "[" ++ intercalate ", " (map show1 xs) ++ "]"

instance (Show1 a, Show1 b) => Show1 (a, b) where
   show1 (a, b) = "(" ++ show1 a ++ ", " ++ show1 b ++ ")"
#endif

class Read1 a where
   read1 :: String -> a

instance {-# OVERLAPPING #-} Read1 String where
   read1 = id

instance (Read a) => Read1 a where
   read1 = read

