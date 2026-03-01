{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Parser2 where

import Favs
import MyPretty2
import qualified NewTuple
import Iso2 (Iso)

import Prelude hiding (lookup)
import Control.Monad
import Control.Arrow
import Data.Kind
import Data.List hiding (lookup)
import Data.Map qualified as M
import Data.Set qualified as S

class DArrow d where
   --darr   :: (b -> c) -> (c -> b) -> d b c
   darr    :: (Arrow a, Arrow b) => a m n -> b n m -> d a b m n
   (****)  :: (Arrow a, Arrow b) => d a b e o -> d a b m n -> d a b (e, m) (o, n)

data DArr a b m n = DArr (a m n) (b n m)

instance DArrow DArr where
   darr = DArr
   DArr aeo boe **** DArr amn bnm = DArr (aeo *** amn) (boe *** bnm)

DArr amn bnm >><< DArr ano bon = DArr (amn >>> ano) (bnm <<< bon)
DArr amn bnm <<>> DArr ano bon = DArr (amn <<< ano) (bnm >>> bon)

data Record f v = Record {fieldsr :: f, values :: v} deriving (Show)

instance NewTuple.NamedTuple name names values value => Frame name value (Record names values) where
   lookup name (Record names values) = NewTuple.lookup name names values

class Frame name value frame where
   lookup :: name -> frame -> value
   update :: name -> value -> frame -> frame

data Rule s t f i o where
   Call    :: (Frame m u f, Frame n v f) => m -> Rule s t g u v -> n -> Rule s t f u v
   Lambda  :: (Frame j u g, Frame k v g) => j -> Rule s t g u v -> k -> Rule s t g u v
   IsoRule :: Iso a b -> Rule s t f a b

data PResult t f r = Done t f r
{-
parse1 (Call m (Lambda j body k) n) f t =
   case parse1 body (update j (lookup m f) undefined) t of
      Done t1 g r1 -> Done t1 (update n (lookup k g) f) r1
-}