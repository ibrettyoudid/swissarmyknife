module Multimap where

import qualified SetList as SL

import qualified Data.Map as M
import qualified Data.Set as S

insert k k1 v m = case M.lookup k m of
   Just m1 -> M.insert k (M.insert    k1 v m1) m
   Nothing -> M.insert k (M.singleton k1 v   ) m

delete k k1 m = case M.lookup k m of
   Just  j -> let
      m1 = M.delete k1 j
      in if M.null m1 then M.delete k m else M.insert k m1 m
   Nothing -> m

lookupMin k m = case M.lookup k m of
   Just m1 -> M.lookupMin m1
   Nothing -> Nothing
