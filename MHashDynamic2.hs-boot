module MHashDynamic2 where

import Data.Dynamic qualified as D
import Data.Typeable

newtype Dynamic = Dynamic D.Dynamic

fromDynamic :: (Typeable a) => Dynamic -> Maybe a
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
typeOf1 :: (Typeable a) => a -> TypeRep
toDyn :: (Typeable a) => a -> Dynamic
fromDyn :: (Typeable a) => Dynamic -> a -> a

fromDyn1 :: (Typeable a) => Dynamic -> a

instance Eq Dynamic
instance Ord Dynamic
instance Show Dynamic