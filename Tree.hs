-- Copyright 2025 Brett Curtis
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Tree where

import Favs hiding (split)

import qualified Data.List as L
import qualified Data.Bits as B
import qualified Data.IntMap as I

import Control.DeepSeq

import GHC.Generics

-- By making all the keys relative to where they are in the tree, we can shift parts of the tree around very fast

--data Tree k v = Node k Int (Tree k v) (Tree k v) | Leaf k v
data Tree v = Node { key::Int, left::Tree v, right::Tree v } | Leaf { key::Int, val::v } | Empty deriving (Generic)

instance NFData v => NFData (Tree v)

instance Show v => Show (Tree v) where
   show = unlines . showTree1

instance Eq (Tree v) where
   Leaf a _ == Leaf b _ = a == b

instance Ord (Tree v) where
   compare (Leaf a _) (Leaf b _) = compare a b

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

insert k1 v1 n@(Node k l r)
   | k1 < k    = if k1 < ka && k > ka
                     then Node ka (Leaf k1 v1) (shift (-ka) n)
                     else Node k (insert k1 v1 l) r
   | k1 >= k2  = Node k1a n (Leaf (k1-k1a) v1)
   | otherwise = Node k l (insert (k1-k) v1 r)
      where
         k2  = k * 2
         ka  = power2 k
         k1a = power2 k1
{-
insert k v n@(Node k1 l r)
   | k < k1    = if k1 > k4 && k < k4
                     then Node k4 (Leaf k v) (shift (-k4) n)
                     else Node k1 (insert k v l) r
   | k >= k2   = Node k3 n (Leaf (k-k3) v)
   | otherwise = Node k1 l (insert (k-k1) v r)
      where
         k2 = k1 * 2
         k3 = power2 k
         k4 = power2 k1
-}
insert k1 v1 l@(Leaf k v) = let
   k2 = nextKey2 k k1

   in if k >= k2 && k1 >= k2 
         then
            Node k2 Empty (insert (k1-k2) v1 (Leaf (k-k2) v))
         else
            case compare k1 k of
               LT -> Node k2 (Leaf k1 v1) (Leaf ( k-k2) v )
               GT -> Node k2 l            (Leaf (k1-k2) v1)
               EQ -> Leaf k1 v1

insert k1 v1 Empty = Leaf k1 v1


log2 k = ceiling $ logBase 2 $ fromIntegral k

power2 k = B.shift 1 $ msbit k

nextKey2 k k1 = let
   k2 = k `B.xor` k1
   b  = msbit k2
   in B.shift (B.shift (max k k1) -b) b

lsbit k = fromJust $ L.findIndex (\x -> x B..&. 1 == 1) $ iterate (`B.shift` -1) k

msbit k = snd $ fromJust $ I.lookupLE k msbitmap

msbitmap = I.fromList $ mapfxx (B.shift 1) [0..63]

map f (Node k l r) = Node k (Tree.map f l) (Tree.map f r)
map f (Leaf k v) = Leaf k (f v)
map f Empty = Empty

foldr f z t = L.foldr f z $ toList t
foldl f z t = L.foldl f z $ toList t

instance Functor Tree where
   fmap = Tree.map

untree t = fromList $ combine (:) [] $ concatMap toList t

unzip t = (Tree.map fst t, Tree.map snd t)

snoc e t = insert (snd $ Tree.span t) e t

--treezip (Node k l r) = Node k (treezip f l) (treezip f r)
--treezip (Leaf 

slice from to (Node k l r) = let
   l1 = if from <  k then slice  from     to    l else Empty
   r1 = if to   >= k then slice (from-k) (to-k) r else Empty

   in case (l1, r1) of
      (Empty, Empty) -> Empty
      (l1   , Empty) -> l1
      (Empty, r1   ) -> shift k r1
      (l1   , r1   ) -> Node k l1 r1

slice from to leaf@(Leaf k v)
   | from <= k && to >= k = Leaf k v
   | otherwise            = Empty

converge a@(Node ka la ra) b@(Node kb lb rb)
   | ka <  kb = converge a lb ++ converge ra b
   | ka == kb = [(a, b)]
   | ka >  kb = converge a rb ++ converge la b

converge a@(Leaf ka va) b@(Leaf kb vb)
   | ka == kb  = [(a, b)]
   | otherwise = []

rotate at (Node k l r) 
   | at < k = let 
{-
         k              at
        / \            / \
       m   r   =>     ll  k
      / \                / \
     ll rl              rl  r
-}
      in case rotate at l of
            Node m ll rl -> Node at ll (Node (k-at) (shift (at-m) rl) r)
   | at == k = Node k l r
   | k < at = case rotate (at-k) r of
{-
         k              at
        / \            / \
       l   m    =>    k   rr
          / \        / \
        lr   rr     l   lr
-}
                  Node m lr rr -> Node at (Node k l lr) (shift (at-k-m) rr)

union1 (Node kn ln rn) (Node k l r)
   | kn < k = Node k (Node kn (union ln (slice 0 (kn-1) l)) (union (slice 0 (k-1) rn) (shift -kn $ slice kn 1000000000 l))) (union (shift (k-kn) $ slice k 1000000000 rn) r)

imin = -9223372036854775808
imax = 9223372036854775807

union2 a b = let
   (amin, amax) = Tree.span a
   (bmin, bmax) = Tree.span b
   in 0



--union3 fn bn (Node kn ln rn) f b (Node k l r)

union n t = L.foldr (uncurry insert) t $ toList n

unionWithShift s n t = L.foldr (uncurry insert) t $ toList $ shift s n
{-
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
-}
shift n (Node k l r) = Node (k+n) (shift n l) r
shift n (Leaf k v) = Leaf (k+n) v

append k Empty Empty = Empty
append k l     Empty = l    
append k Empty r     = shift k r    
append k l     r     = Node k l r

size (Node k _ r) = k + size r
size (Leaf k _) = k + 1

count (Node _ l r) = count l + count r
count (Leaf _ _) = 1
count Empty = 0

span (Node k l r) = (fst $ Tree.span l, (k+) $ snd $ Tree.span r)
span (Leaf k _) = (k, k)

toList = L.map leafToPair . toList1 0

toList1 add (Node k l r) = toList1 add l ++ toList1 (add+k) r
toList1 add Empty = []
toList1 add (Leaf k v) = [Leaf (add+k) v]

toElems t = L.map snd (toList t)

leafToPair (Leaf k v) = (k, v)
pairToLeaf (k, v) = Leaf k v

--fromList t = balance3 0 $ L.sort $ L.map pairToLeaf t
fromList6 :: [(Int, b)] -> Tree b
fromList6 = fromList7

fromList7 l = L.foldl (flip $ uncurry insert) Empty l

fromAscList is = fromList1 0 (length is) Empty is

fromList is = fromList1 0 (length is) Empty (L.sortOn fst is)

fromList1 n m l 
   | n == 0 = \case 
         [] -> Empty
         ((k, v):is) -> fromList1 1 m (Leaf k v) is
   | n < m  = \case
         [] -> l
         i  -> let
               (ir, is) = splitAt n i
               r        = fromList1 0 n Empty ir
               k        = power2 $ key r
               in fromList1 (n*2) m (Node k l (shift -k r)) is
   | otherwise = const l



fromElems :: (Show v) => [v] -> Tree v
fromElems = fromAscList . zip [0..]

singleton :: (Show v) => v -> Tree v
singleton = fromElems . L.singleton

empty = Empty

showTree t = show $ toList t

showTree1 t = showTree2 0 t

showTree2 k1 (Leaf k v) = ["Leaf "++show (k+k1)++" "++show v]
showTree2 k1 (Node k l r) = let
   s1 = "Node "++show (k+k1)
   pad = L.map (replicate (length s1 + 1) ' '++)
   in pad (showTree2 k1 l) ++ [s1] ++ pad (showTree2 (k+k1) r)
showTree2 k1 Empty = ["Empty"]
{-
balance tree = balance3 0 $ toList1 0 tree

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

balance3 sub [Leaf k v] = Leaf (k - sub) v
balance3 sub leaves = let
   len = length leaves
   (l, r) = splitAt (div len 2) leaves
   m = key $ head r
   lr = balance3 sub l
   rr = balance3 m r

   in Node (m - sub) lr rr
-}
t = fromList $ zip [1..] "hello there"

{-
>>> slice 3 8 t
/home/brett/swissarmyknife/Tree.hs:247:15-57: Non-exhaustive patterns in lambda

>>> fromList [(15,15), (4,4)]
        Leaf 4 4
Node 15
        Leaf 15 15



                     Leaf 1 'h'
               Node 2
                           Leaf 2 'e'
                     Node 3
                           Leaf 3 'l'
      Node 4
                           Leaf 4 'l'
Node 5
               Leaf 5 'o'
      Node 6
                     Leaf 6 ' '
               Node 7
                     Leaf 7 't'
   Node 8
                     Leaf 8 'h'
               Node 9
                     Leaf 9 'e'
      Node 10
                        Leaf 10 'r'
               Node 11
                        Leaf 11 'e'
-}
