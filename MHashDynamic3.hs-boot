module MHashDynamic3 where

import Data.Dynamic qualified as D
import Data.Typeable
import Show1
import GHC.Stack

newtype Dynamic = Dynamic D.Dynamic

fromDynamic :: (Typeable a) => Dynamic -> Maybe a
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
typeOf1 :: (Typeable a) => a -> TypeRep
toDyn :: (Typeable a) => a -> Dynamic
fromDyn :: (Typeable a) => Dynamic -> a -> a

fromDyn1 :: (Typeable a) => Dynamic -> a
fromDyn2 :: (Typeable a, HasCallStack) => String -> Dynamic -> a

instance Eq Dynamic
instance Ord Dynamic
instance Show Dynamic
instance Show1 Dynamic