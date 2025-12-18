module Multimap where

import qualified SetList as SL

import qualified Data.Map as M
import qualified Data.Set as S

insert k v m = case M.lookup k m of
   Just  j -> M.insert k (SL.insert  v j) m
   Nothing -> M.insert k (SL.singleton v) m

delete k v m = case M.lookup k m of
   Just  j -> let
      sl = SL.delete v j
      in if SL.null sl then M.delete k m else M.insert k sl m
   Nothing -> m