module SetList where

import qualified Data.Set as S
import qualified Data.List as L
import Prelude hiding (map, null)

data SetList a = SetList { set :: S.Set a, list :: [a]}

empty = SetList S.empty []

insert v setlist  | member v setlist = setlist
                  | otherwise        = SetList (S.insert v $ set setlist) (v:list setlist)

insertD setlist (v, fv) | member fv setlist = ((False, v, fv), setlist)
                        | otherwise         = ((True , v, fv), insert fv setlist)

fromList l = L.foldr insert empty l

fromSet s = SetList s $ S.toList s

toList = list

toSet = set

member v setlist = S.member v $ set setlist

map f sl = fromList $ L.map f $ list sl

mapM f sl = fromList <$> (Prelude.mapM f $ list sl)

union a b = let
   s = S.union (set a) (set b)
   l = list a ++ L.filter (\x -> not $ S.member x $ set a) (list b)
   in SetList s l

unions as = foldr union empty as

concat as = foldr union empty as

delete x sl = SetList (S.delete x $ set sl) (L.filter (/= x) $ list sl)

a \\ b = let
   s = set a S.\\ set b
   l = L.filter (flip S.member s) (list a)
   in SetList s l

null a = S.null $ set a

singleton a = SetList (S.singleton a) (L.singleton a)

closure f active = closure1 f empty active

closure1 f collect active = do
   let 
      new = fromList $ L.concatMap f $ list active
      newnew = new \\ collect
   if null newnew
      then collect
      else closure1 f (union collect newnew) newnew

filter pred (SetList s l) = SetList (S.filter pred s) (L.filter pred l)

instance Show a => Show (SetList a) where
   show a = "fromList "++show (list a)

instance Eq a => Eq (SetList a) where
   SetList a c == SetList b d = a == b

instance Ord a => Ord (SetList a) where
   compare (SetList a c) (SetList b d) = compare a b

instance Foldable SetList where
   foldl f z xs = foldl f z $ list xs
   foldr f z xs = foldr f z $ list xs

{- cant do functor as our map requires an Ord instance, and it would need to be ANY type, because Functor is not parameterised by the 'element' type of fmap
instance Functor SetList where
   fmap f xs = SetList.map f xs

instance Traversable SetList where
   mapM f sl = fromList <$> (mapM f $ list sl)
-}
