{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Parser2 where

import Favs
import MyPretty2

import Control.Monad
import Control.Arrow
import Data.Kind
import Data.List
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
