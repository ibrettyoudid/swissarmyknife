module MHashDynamic4 where

import Data.Dynamic qualified as D
import Data.Typeable
import Show1
import GHC.Stack

newtype Dynamic c t = Dynamic D.Dynamic

fromDynamic :: (Typeable a) => Dynamic c t -> Maybe a
dynApply :: Dynamic c t -> Dynamic c t -> Maybe Dynamic c t
typeOf1 :: (Typeable a) => a -> TypeRep
toDyn :: (Typeable a) => a -> Dynamic c t
fromDyn :: (Typeable a) => Dynamic c t -> a -> a

fromDyn1 :: (Typeable a) => Dynamic c t -> a
fromDyn2 :: (Typeable a, HasCallStack) => String -> Dynamic c t -> a

instance Eq Dynamic
instance Ord Dynamic
instance Show Dynamic
instance Show1 Dynamic