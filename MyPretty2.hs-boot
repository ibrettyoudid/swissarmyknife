-- Copyright 2025 Brett Curtis
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Move catMaybes" -}
{- HLINT ignore "Fuse foldr/map" -}
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use all" -}
{- HLINT ignore "Move filter" -}
{- HLINT ignore "Avoid lambda using `infix`" -}
{- HLINT ignore "Use section" -}
{- HLINT ignore "Avoid lambda" -}

module MyPretty2 where

import Favs
import Show1
import ShowTuple

import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace
import GHC.IO (unsafePerformIO)
import GHC.Stack



newtype GridH a = GridH [[a]]
newtype GridV a = GridV [[a]]

class GridHV a where
   h :: a b -> GridH b
   v :: a b -> GridV b

instance GridHV GridH where

instance GridHV GridV where

instance Show1 a => Show (GridV a) where

instance Show1 a => Show (GridH a) where

{-
instance (Integral a, Integral b, Fractional c) => Divisible a b c where
      n // d = fromIntegral n / fromIntegral d
-}

colWidths1A :: (Integral a) => a -> [[a]] -> ((a, a, [a], [a]) -> (a, a, [a], [a]), (a, a, [a], [a]), (a, a, [a], [a]) -> [a])
factors :: [M.Map Int Int]
showGrid :: [[String]] -> String
putGrid :: [[String]] -> IO ()