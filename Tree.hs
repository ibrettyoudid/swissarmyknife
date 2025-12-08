-- Copyright 2025 Brett Curtis
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Redundant multi-way if" -}
{- HLINT ignore "Eta reduce" -}

module Tree where

import Favs hiding (split, left, right)

import Prelude hiding (span, lookup, null, lookup)

import qualified Data.List as L
import qualified Data.Bits as B
import qualified Data.IntMap as I

import Control.DeepSeq

import GHC.Generics

import Debug.Trace

-- By making all the keys relative to where they are in the tree, we can shift parts of the tree around very fast

--data Tree k v = Node k Int (Tree k v) (Tree k v) | Leaf k v
data Tree v = Node { key::Int, left::Tree v, right::Tree v } | Leaf { key::Int, val::v } | Empty deriving (Generic)

instance NFData v => NFData (Tree v)

instance Show v => Show (Tree v) where
   show = unlines . showTree1

instance Eq v => Eq (Tree v) where
   a == b = toList a == toList b

instance Ord v => Ord (Tree v) where
   compare a b = compare (toList a) (toList b)

lookup k1 (Node k l r)  | k1 <  k   = lookup k1 l
                        | otherwise = lookup (k1-k) r
lookup k1 (Leaf k v) | k1 == k   = Just v
                     | otherwise = Nothing 
lookup k1 Empty = Nothing

lookupMin (Node k l r) = 
   case lookupMin l of
      Just  j -> Just j
      Nothing -> 
         case lookupMin r of
            Just (k1, v) -> Just (k+k1, v)
            Nothing      -> Nothing
lookupMin (Leaf k v) = Just (k, v)
lookupMin Empty = Nothing

lookupMax (Node k l r) =
   case lookupMax r of
      Just (k1, v) -> Just (k+k1, v)
      Nothing      -> 
         case lookupMax l of
            Just (k1, v) -> Just (k1, v)
            Nothing      -> Nothing
lookupMax (Leaf k v) = Just (k, v)
lookupMax Empty = Nothing

minKey (Node k l r) = minKey l
minKey (Leaf k v) = k
minKey Empty = intmin

maxKey (Node k l r) = k + maxKey r
maxKey (Leaf k v) = k
maxKey Empty = intmax

(!) = flip lookup

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

insertTree Empty n     = n
insertTree i     Empty = i
insertTree i     n     = 
   case lookupMin i of
      Nothing        -> n
      Just (imin, _) -> 
         case lookupMax i of
            Nothing        -> n
            Just (imax, _) -> 
               case lookupMin n of
                  Nothing        -> i
                  Just (nmin, _) ->
                     case lookupMax n of
                        Nothing        -> i
                        Just (nmax, _) -> 
                           insertTree1 (i, imin, imax) (n, nmin, nmax)

      
insertTree1 i@(it, imin, imax) n@(nt@(Node k l r), nmin, nmax) = let
   k2  = k * 2
   res = 
      if -- | imax * 2 < nmin -> insertTree it (Node k3 Empty (shift -k3 nt))
         | imax < nmin -> if
            | k - nmin > imax - imin -> Node k (insertTree it l) r
            | otherwise -> let k3 = nextKey2 imax nmin
                           in Node k3 it (shift -k3 nt)
         | nmax < imin -> if
            | imax < k2 -> Node k l (insertTree (shift -k it) r)
            | otherwise -> let k3 = nextKey2 nmax imin
                           in Node k3 nt (shift -k3 it)
         | imin >= k -> if
            | imax < k  -> error "imin greater than imax"
            | otherwise -> if
               | imax > k2 -> insertTree it (Node k2 nt Empty)
               | otherwise -> Node k l (insertTree (shift -k it) r)
         | otherwise -> if
            | imax < k -> Node k (insertTree it l) r
            | otherwise -> let
               ik = rotate k it
               il = left  ik
               ir = right ik
               in Node k (insertTree il l) (insertTree ir r)
   in res
   --in trace (mcat [["insertTree1"], showTree1 it, ["+"], showTree1 nt, ["="], showTree1 res]) res

insertTree1 i@(it, _, _) n@(nt@(Leaf k v), nmin, nmax) =
   case lookup k it of
      Just  j -> it -- its in i as well, don't overwrite it with the old one
      Nothing -> insert k v it

insertTree1 i@(it, _, _) n@(Empty, nmin, nmax) = it

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

--filterKey = 

instance Functor Tree where
   fmap = Tree.map

untree t = fromList $ combine (:) [] $ concatMap toList t

unzip t = (Tree.map fst t, Tree.map snd t)

snoc e t = insert (fst $ fromJust $ lookupMax t) e t

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
   | at < k = case rotate at l of
{-
each node gets shifted by 
the right sides it was on - the right sides its now on
watch out for ancestors being shifted
at == m < k

         k              at
        / \            / \
       m   r   =>     ll  k
      / \                / \
     ll rl              rl  r

     must have k2 >= at && k2 >= m && k2 <= k

-}
                  Node m ll rl -> Node at ll (Node (k-at) rl r)
{-
at == m < k

         k              at
        / \            / \
       m   r   =>  Empty  k
                         / \
                        m   r

                        k2 >= at && k2 >= m && k2 <= k
-}
                  Leaf m v     -> Node at Empty (Node (k-at) (Leaf (m-at) v) r)

   | at == k = Node k l r
   | k < at = case rotate (at-k) r of
{-
at == m > k

         k              at
        / \            / \
       l   m    =>    k   rr
          / \        / \
        lr   rr     l   lr

        k2 >= k && k2 < at
-}
                  Node m lr rr -> Node at (Node k l lr) rr
{-
at == m > k

         k              at
        / \            / \
       l   m    =>    k   Empty
                     / \
                    l   m 

           k2 >= m && k2 < at
-}
                  Leaf m v     -> Node at (Node k l (Leaf m v)) Empty

rotate at lf@(Leaf k v)
   | at <= k = Node at Empty (Leaf (k-at) v)
   -- | at == k = lf
   | at >  k = Node at lf Empty
{-
>>> rotate 16 $ fromList [(15,15), (4,4)]
               Leaf 4 4
        Node 8
               Leaf 15 15
Node 16
        Empty

-}
union n t = fromAscList $ merge (toList n) (toList t)

merge a               [             ] = a
merge [             ] b               = b
merge a@((ka, va):la) b@((kb, vb):lb) 
   | ka <  kb = (ka, va):merge la  b
   | ka == kb = (ka, va):merge la lb
   | ka >  kb = (kb, vb):merge  a lb

union1 n t = L.foldr (uncurry insert) t $ toList n

union2 a b = let
   (amin, amax) = span a
   (bmin, bmax) = span b
   in 0

union2a (amin, amax, a) (bmin, bmax, b) = let
   omin = min amin bmin
   imin = max amin bmin
   imax = min amax bmax
   omax = max amax bmax
   ak = key a
   bk = key b
   l = if amin < bmin
         then rotate bmin a
         else rotate amin b
   r = if amax > bmax
         then rotate bmax a
         else rotate amax b
   rl = right l
   lr = left  r
   i  = union rl lr
   in insertTree i $ insertTree (left l) (right r)

unionWithShift s n t = L.foldr (uncurry insert) t $ toList $ shift s n

union3 (Node kn ln rn) (Node k l r)
   | kn < k = Node k (Node kn (union3 ln (slice 0 (kn-1) l)) (union3 (slice 0 (k-1) rn) (shift -kn $ slice kn 1000000000 l))) (union (shift (k-kn) $ slice k 1000000000 rn) r)

intmin = -9223372036854775808
intmax = 9223372036854775807 :: Int


--union3 fn bn (Node kn ln rn) f b (Node k l r)

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
shift n (Leaf k v  ) = Leaf (k+n) v
shift n  Empty       = Empty

append k l r = insertTree (shift k r) l
{-
append k Empty Empty = Empty
append k l     Empty = l    
append k Empty r     = shift k r    
append k l     r     = Node k l r
-}
size (Node k _ r) = k + size r
size (Leaf k _) = k + 1

count (Node _ l r) = count l + count r
count (Leaf _ _) = 1
count Empty = 0

span n = (fst $ fromJust $ lookupMin n, fst $ fromJust $ lookupMax n)

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

fromAscList is = fromList1 (length is) is

fromList is = fromList1 (length is) (L.sortOn fst is)

fromList1 :: Int -> [(Int, v)] -> Tree v
fromList1 m
   | m == 0    = const Empty
   | m == 1    = \[(k, v)] -> Leaf k v
   | otherwise = \i  -> let
               n        = m `div` 2
               (il, ir) = splitAt n i
               l        = fromList1      n  il
               r        = fromList1 (m - n) ir
               lm       = maxKey l
               rm       = minKey r
               k        = nextKey2 lm rm
               in Node k l (shift -k r)

--fromTreeList ts = fromTreeList1 (length ts) (sortts

fromAscTreeList ts = fromTreeList1 (length ts) ts

fromTreeList1 :: Int -> [Tree v] -> Tree v
fromTreeList1 m
   | m == 0    = const Empty
   | m == 1    = \[t] -> t
   | otherwise = \i  -> let
               n        = m `div` 2
               (il, ir) = splitAt n i
               l        = fromTreeList1      n  il
               r        = fromTreeList1 (m - n) ir
               lm       = maxKey l
               rm       = minKey r
               k        = nextKey2 lm rm
               in Node k l (shift -k r)



fromElems :: (Show v) => [v] -> Tree v
fromElems = fromAscList . zip [0..]

singleton :: (Show v) => v -> Tree v
singleton = fromElems . L.singleton

empty = Empty

null Empty = True
null (Node _ l r) = null l && null r
null (Leaf _ _) = False

showTree t = show $ toList t

showTree1 t = showTree2 0 t

showTree2 k1 (Leaf k v) = ["Leaf "++show (k+k1)++" "++show v]
showTree2 k1 (Node k l r) = let
   s1 = "Node "++show (k+k1)
   pad = L.map (replicate (length s1 + 1) ' '++)
   in pad (showTree2 k1 l) ++ [s1] ++ pad (showTree2 (k+k1) r)
showTree2 k1 Empty = ["Empty"]

mapp a b = let
   n = max (length a) (length b)
   in zipWith3 (\a b c -> a ++ b ++ c) 
         (padRWith ' ' (padCWith1 "" n a))
                       (padRWith1 "  " n [])
         (padRWith ' ' (padCWith1 "" n b))

mcat xs = unlines $ replicate 80 '-' : L.foldr mapp [] xs

rebalance t = let
   tmin = minKey t
   tmax = maxKey t
   tdiff    = tmax - tmin
   levels   = msbit (tmax - tmin)
   size     = 2^(levels `div` 2)
   treeList = toTreeList size t
   res      = fromAscTreeList treeList
   
   in trace ("tmin="++show tmin++" tmax="++show tmax++" tmax-tmin="++show tdiff++" levels="++show levels++" res="++show res) res

rebalance1 t = let
   tmin = minKey t
   tmax = maxKey t
   tdiff    = tmax - tmin
   levels   = msbit (tmax - tmin)
   find     = div (tmin + tmax) 2
   newroot  = findNearest find $ top (div levels 2) t
   res      = rotate newroot t
   
   in trace ("tmin="++show tmin++" tmax="++show tmax++" tmax-tmin="++show tdiff++" levels="++show levels++" find="++show find++" nearest="++show newroot) res

{-
findNearest x (Node k l r) 
   | k <  x = findNearest (x-k) r
   | k == x = let
      l1 = findNearest (x-k) r
      r1 = findNearest x l
      in if abs (l1 - x) < abs (r1 - x) then l1 else r1
   | k >  x = findNearest x l
-}

nearestTo x a b = if abs (x - a) < abs (x - b) then a else b

findNearest x (Node k l r) = let
      l1 = findNearest x l
      r1 = findNearest (x-k) r + k
      res = nearestTo x k $ nearestTo x l1 r1 
      in trace ("findNearest x="++show x++" k="++show k++" l1="++show l1++" r1="++show r1++" result="++show res) res

findNearest x (Leaf k v) = k

findNearest x Empty = intmin

top 0      n            = Empty
top levels (Node k l r) = Node k (top (levels-1) l) (top (levels-1) r)
top levels (Leaf k v)   = Leaf k v

toTreeList s n = let
   nmin = minKey n
   nmax = maxKey n
   
   in toTreeList1 nmin nmax s n

toTreeList1 nmin nmax s n
   | nmax - nmin < s = [n]
   | otherwise   = let
      l = left  n
      r = right n
      lmax = maxKey l
      rmin = minKey r

      in toTreeList1 nmin lmax s l ++ toTreeList1 rmin nmax s r

t x = fromList $ zip x x

{-
>>> insertTree (t [0..9]) (t [20..29])
Maybe.fromJust: Nothing




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
