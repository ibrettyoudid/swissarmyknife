
module States where

import Parser3Types
import qualified SetList as SL

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as I

--data states  a = states  { set :: S.Set a, list :: [a]}
newtype StateSeq state tok = StateSeq (I.IntMap (States state tok)) 

data States state tok = States { imap :: M.Map (Item tok) (SL.SetList state), setlist :: SL.SetList state }
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
insert st m = let
   n = to st
   i = item st
   m1 = case I.lookup n m of
            Nothing -> States M.empty SL.empty
            Just  j -> j
   m2 = case M.lookup i (imap m1) of
            Nothing -> SL.empty
            Just  j -> j
   m2new = SL.insert st m2
   m1new = States (M.insert i m2new $ imap m1) (SL.insert st $ setlist m1)

   in I.insert n m1new m

lookup n (StateSeq m) = I.lookup n m

lookupNI n i (StateSeq m) = do
   s <- I.lookup n m
   M.lookup i (imap s)

toList (StateSeq ss) = L.concatMap (SL.toList . setlist . snd) $ I.toList ss

map f m = L.map f $ toList m

zipWith f m1 m2 = L.zipWith f (toList m1) (toList m2)