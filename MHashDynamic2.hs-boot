module MHashDynamic2 where

import Data.Dynamic qualified as D
import Data.Typeable

newtype Dynamic = Dynamic D.Dynamic

data MType = MType {mtname :: String, conv :: TypeLink, constr :: Constr}

instance Show MType where

instance Eq MType where

instance Ord MType where

data TypeLink = TypeLink {tlparents :: [MType], tllinear :: [MType]}

data Constr = Constr {cname :: String, tl :: TypeLink, members :: [Member]}

data Member = Member {mtype :: MType, mname :: String}

fromDynamic :: (Typeable a) => Dynamic -> Maybe a
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
typeOf1 :: (Typeable a) => a -> TypeRep
toDyn :: (Typeable a) => a -> Dynamic
fromDyn :: (Typeable a) => Dynamic -> a -> a

fromDyn1 :: (Typeable a) => Dynamic -> a

instance Eq Dynamic
instance Ord Dynamic
instance Show Dynamic