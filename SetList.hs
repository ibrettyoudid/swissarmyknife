module SetList where

import qualified Data.Set as S
import qualified Data.List as L

data SetList a = SetList { set :: S.Set a, list :: [a]}

empty = SetList S.empty []

insert v setlist  | member v setlist = setlist
                  | otherwise        = SetList (S.insert v $ set setlist) (v:list setlist)

fromList l = L.foldr insert empty l

fromSet s = SetList s $ S.toList s

member v setlist = S.member v $ set setlist

map f sl = let
   l = L.map f $ list sl
   in SetList (S.fromList l) l

union a b = let
   s = S.union (set a) (set b)
   l = list a ++ list b
   in SetList s l

a \\ b = let
   s = set a S.\\ set b
   l = L.filter (flip S.member s) (list a)
   in SetList s l

null a = S.null $ set a