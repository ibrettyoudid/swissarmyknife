
module PosItems where

import Favs
import Parser3Types
import qualified SetList as SL

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as I

import GHC.Stack
--data states  a = states  { set :: S.Set a, list :: [a]}

data PosItems tok state = PosItems { imap :: M.Map (Rule tok) (SL.SetList state), setlist :: SL.SetList state } deriving Show
{-
want to be able to look up states by end token number and subitem for complete
states also need to be checked for uniqueness
by start token number
-}

{-
insert st = let
   n = to st
   k = item st
   m1 = case I.lookup n m of
      Nothing -> empty
      Just  j -> j
   m1new = insert k st m1

   in insert n m1new m
-}
(!) :: (HasCallStack, Ord tok, Show tok, Show state) => PosItems tok state -> Rule tok -> SL.SetList state
i ! r = case PosItems.lookup r i of
   Nothing -> error $ "no entry for "++show r++" in "++show i
   Just  j -> j

lookup r (PosItems i s) = M.lookup r i

fromList l = PosItems (mapFromList SL.insert SL.empty $ mapfxx (rule . item) l) (SL.fromList l)

fromSL sl = PosItems (mapFromList SL.insert SL.empty $ mapfxx (rule . item) $ SL.toList sl) sl

singleton e = PosItems M.empty (SL.singleton e)

empty = PosItems M.empty SL.empty

instance Foldable (PosItems tok) where
   foldl f z xs = foldl f z $ setlist xs
   foldr f z xs = foldr f z $ setlist xs
