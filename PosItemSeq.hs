module PosItemSeq where

import qualified PosItems as PI

import Favs
import Parser3Types
import qualified SetList as SL

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as I
import Control.Monad
import Control.Monad.State

import Prelude hiding (lookup)

--data states  a = states  { set :: S.Set a, list :: [a]}
newtype PosItemSeq tok positem = PosItemSeq (I.IntMap (PI.PosItems tok positem)) 

insert st m = let
   n = to st
   r = rule $ item st
   m1 = case I.lookup n m of
            Nothing -> PI.PosItems M.empty SL.empty
            Just  j -> j
   m2 = case M.lookup r (PI.imap m1) of
            Nothing -> SL.empty
            Just  j -> j
   m2new = SL.insert st m2
   m1new = PI.PosItems (M.insert r m2new $ PI.imap m1) (SL.insert st $ PI.setlist m1)

   in I.insert n m1new m

insertP main sub = do
   PosItemSeq m <- get
   let
      n = to main
      r = rule $ item sub
      m1 = case I.lookup n m of
               Nothing -> PI.PosItems M.empty SL.empty
               Just  j -> j
      m1r = PI.imap m1
      m1s = PI.setlist m1
      m2r = case M.lookup r m1r of
               Nothing -> SL.empty
               Just  j -> j
      needsAddingr = SL.member main m2r
      m1rnew = if needsAddingr
         then M.insert r (SL.insert main m2r) m1r
         else m1r
      needsAddings = SL.member sub m1s
      m1snew = if needsAddings
         then SL.insert sub m1s
         else m1s
   if needsAddingr || needsAddings
      then do
         liftIO $ putStrLn $ show "Predict "++show main++" --> "++show sub++if needsAddingr then " new connection" else ""
         put $ PosItemSeq $ I.insert n (PI.PosItems m1rnew m1snew) m
         return [sub]
      else
         return []

insertC :: (PosItemTo positem, MonadState (PosItemSeq tok (positem tok)) m, MonadIO m, Show (positem tok), Ord (positem tok)) => positem tok -> positem tok -> m [positem tok]
insertC main sub = do
   PosItemSeq m <- get
   let
      n = to main
      m1 = case I.lookup n m of
               Nothing -> PI.PosItems M.empty SL.empty
               Just  j -> j
      m1r = PI.imap m1
      m1s = PI.setlist m1
      needsAddings = SL.member sub m1s
      m1snew = if needsAddings
         then SL.insert sub m1s
         else m1s
   if needsAddings
      then do
         liftIO $ putStrLn $ show "Complete "++show main++" <-- "++show sub
         put $ PosItemSeq $ I.insert n (PI.PosItems m1r m1snew) m
         return [main]
      else
         return []

m ! n = fromJust $ lookup n m

singleton pi = PosItemSeq $ I.singleton (to pi) $ PI.singleton pi

lookup n (PosItemSeq m) = I.lookup n m

lookupNI n i (PosItemSeq m) = do
   s <- I.lookup n m
   M.lookup i (PI.imap s)

domain (PosItemSeq i) = (fst $ fromJust $ I.lookupMin i, fst $ fromJust $ I.lookupMax i)

range (from, to) (PosItemSeq i) = PosItemSeq $ I.takeWhileAntitone (<= to) $ I.dropWhileAntitone (< from) i

fromElems l = PosItemSeq $ I.fromList $ zip [0..] l

fromAssocs l = PosItemSeq $ I.fromList l

toList (PosItemSeq ss) = L.concatMap (SL.toList . PI.setlist . snd) $ I.toList ss

map f m = L.map f $ toList m

zipWith f m1 m2 = L.zipWith f (toList m1) (toList m2)
{-
instance Foldable (PosItemSeq tok) where
   foldl f z (PosItemSeq xs) = foldl f z $ L.map snd $ I.toList xs

-}

size (PosItemSeq ss) = I.size ss