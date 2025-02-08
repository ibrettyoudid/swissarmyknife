{-# LANGUAGE UndecidableInstances #-}

module Show1 where

class Show1 a where
   show1 :: a -> String

instance Show1 String where
   show1 = id

instance Show a => Show1 a where
   show1 = show
