
module States where

import Favs
import Parser3Types
import qualified SetList as SL

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as I

--data states  a = states  { set :: S.Set a, list :: [a]}

data States tok state = States { imap :: M.Map (Item tok) (SL.SetList state), setlist :: SL.SetList state }
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

fromList l = States (mapFromList SL.insert SL.empty $ mapfxx item l) (SL.fromList l)

fromSL sl = States (mapFromList SL.insert SL.empty $ mapfxx item $ SL.toList sl) sl

singleton e = States (M.singleton (item e) (SL.singleton e)) (SL.singleton e)

empty = States M.empty SL.empty

instance Foldable (States tok) where
   foldl f z xs = foldl f z $ setlist xs
   foldr f z xs = foldr f z $ setlist xs

