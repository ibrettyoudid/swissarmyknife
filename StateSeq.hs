module StateSeq where

import qualified States

import Favs
import Parser3Types
import qualified SetList as SL

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Control.Monad.State as St

--data states  a = states  { set :: S.Set a, list :: [a]}
newtype StateSeq tok state = StateSeq (I.IntMap (States.States tok state)) 

insert st m = let
   n = to st
   i = item st
   m1 = case I.lookup n m of
            Nothing -> States.States M.empty SL.empty
            Just  j -> j
   m2 = case M.lookup i (States.imap m1) of
            Nothing -> SL.empty
            Just  j -> j
   m2new = SL.insert st m2
   m1new = States.States (M.insert i m2new $ States.imap m1) (SL.insert st $ States.setlist m1)

   in I.insert n m1new m

insertM st = do
   m <- St.get
   let
      n = to st
      i = item st
      m1 = case I.lookup n m of
               Nothing -> States.States M.empty SL.empty
               Just  j -> j
      m2 = case M.lookup i (States.imap m1) of
               Nothing -> SL.empty
               Just  j -> j
      m2new = SL.insert st m2
      m1new = States.States (M.insert i m2new $ States.imap m1) (SL.insert st $ States.setlist m1)

   St.put $ I.insert n m1new m
   return $ not $ null m2

lookup n (StateSeq m) = I.lookup n m

lookupNI n i (StateSeq m) = do
   s <- I.lookup n m
   M.lookup i (States.imap s)

range from to (StateSeq i) = StateSeq $ I.takeWhileAntitone (<= to) $ I.dropWhileAntitone (< from) i

fromElems l = StateSeq $ I.fromList $ zip [0..] l

fromAssocs l = StateSeq $ I.fromList l

toList (StateSeq ss) = L.concatMap (SL.toList . States.setlist . snd) $ I.toList ss

map f m = L.map f $ toList m

zipWith f m1 m2 = L.zipWith f (toList m1) (toList m2)
{-
instance Foldable (StateSeq tok) where
   foldl f z (StateSeq xs) = foldl f z $ L.map snd $ I.toList xs

-}

size (StateSeq ss) = I.size ss