{-# LANGUAGE FunctionalDependencies #-}

module Parser6Types where

class Frame name value frame | name frame -> value where
   myget1 :: name -> frame -> value
   myset1 :: name -> value -> frame -> frame

