-- Copyright 2025 Brett Curtis
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LexicalNegation #-}

module Tree where


import qualified Data.List as L
import Favs
--data Tree k v = Node k Int (Tree k v) (Tree k v) | Leaf k v
data Tree v = Node { key::Int, left::Tree v, right::Tree v } | Leaf { key::Int, val::v } | Empty deriving (Eq, Ord)

instance Show v => Show (Tree v) where
   show = showTree

showTree t = show $ toList t

showTree1 (Leaf k v) = ["Leaf "++show k++" "++show v]
showTree1 (Node k l r) = let
   s1 = "Node "++show k
   pad = L.map (replicate (length s1 + 1) ' '++)
   in pad (showTree1 l) ++ [s1] ++ pad (showTree1 r)
showTree1 Empty = ["Empty"]

lookup k1 (Node k l r) | k1 <  k   = Tree.lookup k1 l
                       | otherwise = Tree.lookup (k1-k) r
lookup k1 (Leaf k v) | k1 == k   = Just v
                     | otherwise = Nothing 
lookup k1 Empty = Nothing

lookupMin (Node _ l _) = lookupMin l
lookupMin (Leaf k v) = Just (k, v)
lookupMin Empty = Nothing

lookupMax (Node k _ r) = do (k1, v) <- lookupMax r; return (k+k1, v)
lookupMax (Leaf k v) = Just (k, v)
lookupMax Empty = Nothing

(!) = flip Tree.lookup

map f (Node k l r) = Node k (Tree.map f l) (Tree.map f r)
map f (Leaf k v) = Leaf k (f v)
map f Empty = Empty

instance Functor Tree where
   fmap = Tree.map

untree tl = fromList $ combine (:) [] $ concatMap toList tl

--treezip (Node k l r) = Node k (treezip f l) (treezip f r)
--treezip (Leaf 

slice from to (Node k l r) = let
   l1 = if from <= k then slice from to l         else Empty
   r1 = if to   >= k then slice (from-k) (to-k) r else Empty

   in case (l1, r1) of
      (Empty, Empty) -> Empty
      (l1   , Empty) -> l1
      (Empty, r1   ) -> r1
      (l1   , r1   ) -> Node (k-from) l1 r1

slice from to leaf@(Leaf k v)
   | from <= k && to >= k = Leaf (k-from) v
   | otherwise            = Empty

union n t = foldr (uncurry insert) t $ toList n

unionWithShift s n t = foldr (uncurry insert) t $ toList $ shift s n

insert k v (Node k1 l r)
   | k < k1    = Node k1 (insert k v l) r
   | otherwise = Node k1 l (insert (k-k1) v r)

insert k v l@(Leaf k1 v1) = let
   k2 = (k1 + k) `div` 2

   in case compare k k1 of
         LT -> Node k2 (Leaf k v) (Leaf (k1-k2) v1)
         GT -> Node k2 l          (Leaf ( k-k2) v )
         EQ -> Leaf k v

alter f k (Node k1 l r)
   | k < k1    = case alter f k l of
                  Empty -> shift k1 r
                  res   -> Node k1 res r
   | otherwise = case alter f (k-k1) r of
                     Empty -> l
                     res   -> Node k1 l res

alter f k leaf@(Leaf k1 v1) = let
   k2 = (k1 + k) `div` 2

   in case compare k k1 of
         LT -> case f Nothing of
            Just v  -> Node k2 (Leaf k v) (Leaf (k1-k2) v1)
            Nothing -> leaf
         GT -> case f Nothing of
            Just v  -> Node k2 leaf (Leaf (k-k2) v)
            Nothing -> leaf
         EQ -> case f (Just leaf) of
            Just v  -> Leaf k v
            Nothing -> Empty

shift n (Node k l r) = Node (k+n) (shift n l) r
shift n (Leaf k v) = Leaf (k+n) v

append l = Node (size l) l

size (Node k _ r) = k + size r
size (Leaf k _) = k + 1

span (Node k l r) = (fst $ Tree.span l, (k+) $ snd $ Tree.span r)
span (Leaf k _) = (k, k)

toList = L.map leafToPair . toList1 0

toList1 add (Node k l r) = toList1 add l ++ toList1 (add+k) r
toList1 add Empty = []
toList1 add (Leaf k v) = [Leaf (add+k) v]

toElems t = L.map snd (toList t)

leafToPair (Leaf k v) = (k, v)
pairToLeaf (k, v) = Leaf k v

fromList t = balance1 0 $ L.map pairToLeaf t

fromElems :: Show v => [v] -> Tree v
fromElems = fromList . zip [0..]

singleton :: Show v => v -> Tree v
singleton = fromElems . L.singleton

balance tree = balance1 0 $ toList1 0 tree


balance1 sub [Leaf k v] = Leaf (k-sub) v
balance1 sub leaves = let
   k1 = L.map key leaves
   m  = round (fromIntegral (sum k1) / fromIntegral (length leaves)+0.4 :: Double) :: Int
   (l, r) = L.partition (\lf -> key lf < m) leaves

   lr = balance1 sub l
   rr = balance1 m r
   in if null l || null r then error ("empty list leaves="++show leaves) else Node (m-sub) lr rr

balance2 leaves = let
   k = L.map fst leaves
   m = sum k `div` length leaves
   (l, r) = L.partition (\lf -> fst lf <= m) leaves

   lr = balance2 l
   rr = balance2 r
   in Node (size lr) lr rr

t = fromList $ zip [0..] "hello there"

{-
>>> toList $ slice 4 8 t
[(0,'o'),(1,' '),(2,'t'),(3,'h'),(4,'e')]

>>> t
                     Leaf 0 'h'
              Node 1
                     Leaf 0 'e'
       Node 2
                     Leaf 0 'l'
              Node 1
                            Leaf 0 'l'
                     Node 1
                            Leaf 0 'o'
Node 5
                     Leaf 0 ' '
              Node 1
                            Leaf 0 't'
                     Node 1
                            Leaf 0 'h'
       Node 3
                     Leaf 0 'e'
              Node 1
                            Leaf 0 'r'
                     Node 1
                            Leaf 0 'e'
-}
