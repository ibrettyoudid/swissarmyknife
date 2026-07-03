module Dyn where

import qualified Data.Map as M

import qualified Data.Dynamic as D
import Data.Dynamic (Typeable, Dynamic)
import Data.Typeable
import Type.Reflection hiding (TypeRep, typeOf, typeRepTyCon)

import GHC.Stack

class Dyn dyn where
   toDyn   :: Typeable a => a -> dyn
   toDyn   = dtoDyn . D.toDyn
   
   fromDynamic :: Typeable a => dyn -> Maybe a
   fromDynamic = D.fromDynamic . dfromDyn

   dtoDyn   :: Dynamic -> dyn
   dfromDyn :: dyn -> Dynamic

   showDyn :: dyn -> String
-- instance Dyn Dynamic where
--    dtoDyn = id
--    dfromDyn = id

data NamedValue dyn = NamedValue { nname :: String, nvalue :: dyn }

data MType = MType { mtname :: String, conv :: TypeLink, constr :: Constr }

data TypeLink = TypeLink { tlparents :: [MType], tllinear :: [MType] } 

data Constr = Constr { cname :: String, tl :: TypeLink, members :: [Member] }

data Member = Member { mtype :: MType, mname :: String }

data MMEntry dyn = Method dyn | Types [MType]

data Multimethod dyn = Multimethod { mmname :: String, funcs :: M.Map [SomeTypeRep] dyn } deriving (Typeable)

--data MultimethodA dyn = MultimethodA {namea :: String, funcsa :: SubArrayD dyn String dyn} deriving (Typeable)

dynTypeRep :: Dyn dyn => dyn -> SomeTypeRep

fromDyn1 :: (Dyn dyn, Typeable a) => dyn -> a

fromDyn2 :: (Dyn dyn, Typeable a, HasCallStack) => String -> dyn -> a

dmean :: (Dyn dyn, Num dyn) => [dyn] -> dyn

applyMultimethod :: forall dyn. (Dyn dyn, Typeable dyn) => Multimethod dyn -> [dyn] -> Either String dyn
