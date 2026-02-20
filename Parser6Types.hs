{-# LANGUAGE FunctionalDependencies #-}

module Parser6Types where

import Data.Typeable
import {-# SOURCE #-} MHashDynamic3

class Frame name value frame | name frame -> value where
   myget1 :: name -> frame -> value
   myset1 :: name -> value -> frame -> frame

class FrameD name frame where 
   mygetD :: name -> frame -> Dynamic
   mysetD :: name -> Dynamic -> frame -> frame

