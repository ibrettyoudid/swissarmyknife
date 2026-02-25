{-# LANGUAGE FunctionalDependencies #-}

module Parser6Types where

import Data.Typeable
import {-# SOURCE #-} MHashDynamic3

class Frame name value frame where
   myget1 :: name -> frame -> value
   myset1 :: name -> value -> frame -> frame

class FrameD name frame where 
   mygetD :: name -> frame -> Dynamic
   mysetD :: name -> Dynamic -> frame -> frame

class FrameS name frame where 
   mygetS :: name -> frame -> string
   mysetS :: name -> string -> frame -> frame

