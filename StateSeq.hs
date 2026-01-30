module StateSeq where

import qualified States

import Favs
import Parser3Types
import qualified SetList as SL

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as I

--data states  a = states  { set :: S.Set a, list :: [a]}
newtype StateSeq state tok = StateSeq (I.IntMap (States.States state tok)) 

insert st m = let
   n = to st
   i = item st
   m1 = case I.lookup n m of
            Nothing -> States.States M.empty SL.empty
            Just  j -> j
   m2 = case M.lookup i (imap m1) of
            Nothing -> SL.empty
            Just  j -> j
   m2new = SL.insert st m2
   m1new = States.States (M.insert i m2new $ imap m1) (SL.insert st $ setlist m1)

   in I.insert n m1new m

lookup n (StateSeq m) = I.lookup n m

lookupNI n i (StateSeq m) = do
   s <- I.lookup n m
   M.lookup i (imap s)

range from to (StateSeq i) = StateSeq $ I.takeWhileAntitone (<= to) $ I.dropWhileAntitone (< from) i

fromList l = StateSeq $ I.fromList l

toList (StateSeq ss) = L.concatMap (SL.toList . setlist . snd) $ I.toList ss

map f m = L.map f $ toList m

zipWith f m1 m2 = L.zipWith f (toList m1) (toList m2)

