{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Show1 where

class Show1 a where
   show1 :: a -> String

instance {-# OVERLAPPING #-} Show1 String where
   show1 = id

instance (Show a) => Show1 a where
   show1 = show

class Read1 a where
   read1 :: String -> a

instance {-# OVERLAPPING #-} Read1 String where
   read1 = id

instance (Read a) => Read1 a where
   read1 = read

