{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LexicalNegation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# LANGUAGE MultiWayIf #-}

module FuzzyMatch where

import Favs
import BString

import MHashDynamic3

import qualified Multimap as MM
import qualified SetList as SL

import Prelude hiding (null, init, tail, head, elem, length, (++), (!!), toLower, split, last, take, drop, notElem, concat, takeWhile, dropWhile, putStrLn, putStr)
import Data.Graph.AStar
import Data.List (sort, transpose)
import GHC.Generics
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Hashable as HS
import qualified Data.HashSet as HS

import qualified Data.KdTree.Static as KD
import Debug.Trace

--heuristic [x, y] = abs ((length a - x) - (length b - y))
fuzzyMatch6 a b = let
   la = length a
   lb = length b
   index a = multimap $ zip a [0..]
   ai = index a
   bi = index b
   i = M.map (\(as, bs) -> (sort as, sort bs)) $ M.intersectionWith (,) ai bi
   u = M.map (\(as, bs) -> (sort as, sort bs)) $ unionWith3 (,) (,[]) ([],) ai bi
   --m = M.map (\(as, bs) -> (S.fromList as, S.fromList bs)) $ M.intersectionWith (,) ai bi
   s = replicate (max la lb) ' '
   in putStr $ unlines $ map (\a -> let
      (as, bs) = u M.! a
      in foldr (\i -> replaceIndex i a) s bs) a

data Point k v = Point v [k] deriving (Eq, Show, Generic, Ord)

instance (Eq k, Eq v, HS.Hashable v, HS.Hashable k) => HS.Hashable (Point k v)

pointfn (Point v ks) = ks

-- the node is at x,y
-- the next node diagonally is x1,y1
-- sw is all nodes swx, swy such that x < swx < x1, and y such that y1 <= swy
-- ne is all nodes nex, ney such that y < ney < y1, and x such that x1 <= nex
-- sw is all nodes swx, swy such that x < swx < x1, and y such that y < swy < y1

data Tree k v  = Tree { x :: k, y :: k, se :: Tree k v, sw :: Tree k v, ne :: Tree k v }
               | Leaf { x :: k, y :: k, value :: v }
               | Empty

fuzzyMatch7 a b = let
   la = length a
   lb = length b
   dla = fromIntegral la
   dlb = fromIntegral lb
   index a = multimap $ zip a [(1::Double) ..]
   int a b = M.map (\(xs, ys) -> (sort xs, sort ys)) $ M.intersectionWith (,) (index a) (index b)
   i = int a b
   --u = M.map (\(xs, ys) -> (sort xs, sort ys)) $ unionWith3 (,) (,[]) ([],) ai bi
   fa = M.fromList $ concatMap (\(v, (xs, ys)) -> map (, (v, ys)) xs) $ M.toList i
   fb = M.fromList $ concatMap (\(v, (xs, ys)) -> map (, (v, xs)) ys) $ M.toList i
   start = Point '^' [0, 0]
   goal = Point '$' [dla + 1, dlb + 1]

   ps = ([start, goal]++) $ concatMap (\(c, (xs, ys)) -> map (Point c) $ crossList [xs, ys]) $ M.toList i
   actDist (Point v k0) (Point w k1) = let z = map abs $ zipWith (-) k0 k1 in if all (==1) z then 0 else sum z
   hDist (Point v k) = let d = zipWith (-) (pointfn goal) k in maximum d - minimum d
   t = KD.build pointfn ps
   --m = M.map (\(as, bs) -> (S.fromList as, S.fromList bs)) $ M.intersectionWith (,) ai bi
   nearest c = filter (\d -> and $ zipWith (<) (pointfn c) (pointfn d)) $ KD.kNearest t 8 c
   hsnearest = HS.fromList . nearest

   s = replicate (max la lb) ' '

   Just j = aStar hsnearest actDist hDist (== goal) start
   --in mapM print ps
   --in mapM print j
   ds = zipWith actDist j (tail j)
   --in (sum ds, ds)
   blah start = mapxfx (actDist start) $ nearest start
   in mapM (\x -> putStrLn $ show x ++ " = " ++ show (blah x)) j
   --in t
   {-
   in putStr $ unlines $ map (\a -> let
      (xs, ys) = u M.! a
      in foldr (\i -> replaceIndex (round i) '*') s ys) a
-}
{-

fuzzyMatch8 a b = let
   la = length a
   lb = length b
   dla = fromIntegral la
   dlb = fromIntegral lb
   index a = multimap $ zip ('^':a++"$") [0::(Int) ..]
   ai = index a
   bi = index b
   int a b = M.map (\(xs, ys) -> (S.fromList xs, S.fromList ys)) $ M.intersectionWith (,) (index a) (index b)
   i = int a b
   fx = M.fromList $ concatMap (\(v, (xs, ys)) -> map (, ys) $ S.toList xs) $ M.toList i
   fy = M.fromList $ concatMap (\(v, (xs, ys)) -> map (, xs) $ S.toList ys) $ M.toList i

   --loop x y se = let
{-
we're only interested in nodes to the right and below any node (SE = south east)
the tree has its root at the top left
its children are a set of nodes such that none is on the SE of any other

this is still quite unbalanced but hopefully in such a way the nodes you most want are easiest to get

node A doesn't care about the bs and cs and ds, those are stored in B and C (ds are in both)

              x  
         d/c  |  d/c
              |     
         y----A---------
              |     Cccccc
         d/c  |     cccccc
              |Bbbbbdddddd
              |bbbbbdddddd

we sweep a SW-NE line from the SE corner to the NW
at some point, as we sweep, remembering B & C, say we find E and F simultaneously

                 /
                E    
               /    
              /     Cccccc
             F      cccccc
            /       cccccc
           /Bbbbbbbbdddddd
          / bbbbbbbbdddddd

C is dominated by E but not F
B is dominated by neither

create E and F
continue to sweep, now remembering B, E & F
-}
      -- find node with greatest x less than current x, may be lots in that column
      (x1, xys) = M.lookupLT x fx
      --find node with greatest y less than current y
      (y1, yxs) = M.lookupLT y fy
      
      (xyb, xya) = S.split (S.lookupLT y xys) xys
      ne = foldr (\ne y -> Tree x1 y se sw ne) Empty xya
      (yxb, yxa) = S.split (S.lookupLT x yxs) yxs
      sw = foldr (\sw x -> Tree x y1 se sw ne) Empty yxa
      r = Tree x1 y1 se sw ne
      in r
   start = Point '^' [0, 0]
   goal = Point '$' [dla + 1, dlb + 1]

   ps = ([start, goal]++) $ concatMap (\(c, (xs, ys)) -> map (Point c) $ crossList [xs, ys]) $ M.toList i
   actDist (Point v k0) (Point w k1) = let z = map abs $ zipWith (-) k0 k1 in if all (==1) z then 0 else sum z
   hDist (Point v k) = let d = zipWith (-) (pointfn goal) k in maximum d - minimum d
   t = KD.build pointfn ps
   --m = M.map (\(as, bs) -> (S.fromList as, S.fromList bs)) $ M.intersectionWith (,) ai bi
   nearest c = filter (\d -> and $ zipWith (<) (pointfn c) (pointfn d)) $ KD.kNearest t 8 c
   hsnearest = HS.fromList . nearest
   
   s = replicate (max la lb) ' '

   Just j = aStar hsnearest actDist hDist (== goal) start
   --in mapM print ps
   --in mapM print j
   ds = zipWith actDist j (tail j)
   --in (sum ds, ds)
   blah start = mapxfx (actDist start) $ nearest start
   in mapM (\x -> putStrLn $ show x ++ " = " ++ show (blah x)) j
   --in t
-}
{- 
 on the first day of christmas my true love sent to me
o*                *
n *
   *   *     *   *  *         *  *    *    *    *  *
t   *       *
h    *                *
e     *
   *   *     *   *  *         *  *    *    *    *  *
f       *
i        *
r         *            *
s          *
t   *       *
   *   *     *   *  *         *  *    *    *    *  *
d             *
a              *
y               *
   *   *     *   *  *         *  *    *    *    *  *
o*                *
f                  *   
   *   *     *   *  *         *  *    *    *    *  *
c                    * 
h    *                *
r         *            *
i
s
t
m
a
s
-}
data Node   = AtLeast { dos1 :: Int, ds1 :: Int, pos1 :: [Int] } 
            | Exactly { dsn1 :: Int, ds1 :: Int, step1 :: Int, pos1 :: [Int], path1 :: [(Int, [Int])] } | AtLeastH Int | ExactlyH Int
            | Goal    { dsn1 :: Int, ds1 :: Int, step1 :: Int, pos1 :: [Int], path1 :: [(Int, [Int])] } deriving Show

data NodeList = AtLeastL { dos :: Int, ds :: Int, step :: Int, pos :: [[Int]], path :: [(Int, [Int])] } | ExactlyL { dsn :: Int, pos :: [[Int]] } deriving (Show)

instance Eq Node where
   a == b = compare a b == EQ

instance Ord Node where
   compare = compare `on` gocompare1

instance Eq NodeList where
   a == b = compare a b == EQ

instance Ord NodeList where
   compare = compare `on` gocompare

gocompare1 x@(Exactly {}) = (dsn1 x, 0)
gocompare1 x@(AtLeast {}) = (dos1 x, 1)

gocompare x@(ExactlyL {}) = (dsn x, 0)
gocompare x@(AtLeastL {}) = (dos x, 1)

expand heuristic (AtLeastL dos ds step xs prev) = map (\x -> Exactly (ds + heuristic x) ds step x ((step, x):prev)) xs

head1 x@(ExactlyL {}) = ExactlyH $ dsn x
head1 x@(AtLeastL {}) = AtLeastH $ dos x

merge :: (Show t, Ord t) => [(t, [Node])] -> [(t, Node)]
merge xs = merge2 $ merge1 xs

merge1 :: (Ord b1) => [(b1, [Node])] -> M.Map (Int, b1) [Node]
merge1 xs = M.fromList $ map (\(name, list) -> ((dsn1 $ head list, name), list)) $ filter (not . null . snd) xs

merge2 m = 
   case M.toList $ M.take 2 m of
      [] -> []
      [((_, name), list)] -> map (name,) list
      [((_, name), list), ((next, _), _)] -> let
         (done, todo) = span ((<= next) . dsn1) list
         in map (name,) done ++ merge2 (M.unionWith (++) (M.drop 1 m) (merge1 [(name, todo)]))

firstMatchesOnly xs = firstMatchesOnly2 $ merge1 xs

firstMatchesOnly2 m = 
   case M.toList $ M.take 2 m of
      [] -> []
      [((_, name), list)] -> let
         f = filter isGoal list
         in map (name,) f
      [((_, (x, y)), list), ((next, _), _)] -> let
         (done, todo) = span ((<= next) . dsn1) list
         f = filter isGoal done
         in if not $ null f
            then map ((x, y),) f ++ firstMatchesOnly2 (M.filterWithKey (\(_, (x1, y1)) list1 -> x /= x1 && y /= y1) (M.drop 1 m))
            else firstMatchesOnly2 (M.unionWith (++) (M.drop 1 m) (merge1 [((x, y), todo)]))

mergeB a = concatMap snd $ M.toList $ multimap $ mapfxx dos a

mergeC a = foldr1 (M.unionWith (++)) $ map (M.map sing . M.fromAscList . mapfxx dos) a

sing a = [a]

test1 = fuzzyMatch9 "the quick brown fox" "the quiet blue fog"

test2 = fuzzyMatch9 "on the first day of christmas" "i'll tell you what i want, what i really really want"

test3 = merge [("a", test1), ("b", test2)]

test4 = firstMatch test3

join xs ys = firstMatchesOnly $ concat $ crossWith (\x y -> ((x, y), fuzzyMatch9 x y)) xs ys

showNearest nearest heuristic = mapM_ (\(d, c, x, y) ->
   case nearest [x, y] of
      [] -> return ()
      n  -> do
         print (c, x, y, heuristic [x, y])
         putStrLn $ stringHyper $ fromAssocsDA (\x y -> y) "" $ map (\(step, to) -> (map toDyn to, show step)) n
      ) . sort . map (\(c, x, y) -> (x + y, c, x, y))

isGoal (Goal {}) = True
isGoal _         = False

continue maxDist (Left xs) = let
   loop (goal@(Goal {}):xs) = Right goal
   loop (x : xs)
      | dsn1 x <= maxDist = loop xs
      | otherwise = Left xs
   in loop xs

firstMatch [] = error "reached end of list with no match"
firstMatch ((name, goal@(Goal {})):rest) = (name, goal)
firstMatch (_:rest) = firstMatch rest

fuzzyMatch maxDist a b = continue maxDist $ Left $ fuzzyMatch9 a b

fuzzyMatch9 a b = let
   la = length a
   lb = length b
   index a = multimap $ zip (cons '^' $ a++"$") [0..]
   int = M.intersectionWith (,) (index a) (index b)
   i = M.map (\(xs, ys) -> (S.fromList xs, S.fromList ys)) int
   --u = M.map (\(xs, ys) -> (sort xs, sort ys)) $ unionWith3 (,) (,[]) ([],) ai bi
   z = concatMap (\(c, (xs, ys)) -> concat $ crossWith (c,,) xs ys) $ M.toList int
   fx = M.fromList $ concatMap (\(v, (xs, ys)) -> map (,(v, ys)) $ S.toList xs) $ M.toList i
   fy = M.fromList $ concatMap (\(v, (xs, ys)) -> map (,(v, xs)) $ S.toList ys) $ M.toList i

   start = [0, 0]
   goal = [la + 1, lb + 1]

   actDist k0 k1 = subtract 1 $ maximum $ map abs $ zipWith (-) k0 k1
   heuristic k   = let d = zipWith (-) goal k in maximum d - minimum d
   --m = M.map (\(as, bs) -> (S.fromList as, S.fromList bs)) $ M.intersectionWith (,) ai bi

   nearest (Exactly _ done _ pos prev) = let
      [x0, y0] = pos
      oldh = heuristic pos
      neighbour step tonodes = AtLeastL (done + oldh + step) (done + step) step tonodes prev
      xys = map (\(x, (c, ys)) -> neighbour (x - x0 - 1) $
                              map (\y -> [x, y]) $
                              S.toList $
                              S.takeWhileAntitone (<= x - x0 + y0) $
                              S.dropWhileAntitone (<= y0) ys) $
            M.toList $
            M.dropWhileAntitone (<= x0) fx
      yxs = map (\(y, (c, xs)) -> neighbour (y - y0 - 1)
                              $ map (\x -> [x, y]) $
                              S.toList $
                              S.takeWhileAntitone (<  y - y0 + x0) $
                              S.dropWhileAntitone (<= x0) xs) $
            M.toList $
            M.dropWhileAntitone (<= y0) fy

      --in map (\(x, y) -> (actDist c [x, y], [x, y])) $ concat $ zipWith merge xys yxs
      in mergeC [xys, yxs]

   --in showNearest nearest heuristic z
   in astar nearest heuristic (pos1 $= goal) start
{-
   --Just j = aStar hsnearest actDist hDist (== goal) start
   in case astar nearest heuristic (== goal) start 10 of
      Right (d, g, j) -> let
         --in mapM print ps
         --in mapM print j
         ds = zipWith actDist j (tail j)
         --in (sum ds, ds)
         blah start = sort $ mapfxx (\(n, x) -> actDist start x + heuristic x) $ nearest start
         --in blah [15, 15]
         in mapM (\x -> putStrLn $ show x ++ " = " ++ show (blah x)) j
      Left (d, g, j) -> let
         --in mapM print ps
         --in mapM print j
         ds = zipWith actDist j (tail j)
         --in (sum ds, ds)
         blah start = sort $ mapfxx (\(n, x) -> actDist start x + heuristic x) $ nearest start
         --in blah [15, 15]
         in mapM (\x -> putStrLn $ show x ++ " = " ++ show (blah x)) j
  -}

astar nearest heuristic isGoal start =
   let
      --heuristic (x, y) = sqrt $ fromIntegral ((length a - x)^2 + (length b - y)^2)
      add (byDist, byPos) node@(Exactly totalNew doneNew step to from) = let
         byPos1 = M.insert to node byPos
         byDist1 = M.insert totalNew node byDist
         in case M.lookup to byPos of
            Just (Exactly _ k1 _ _ _) ->
               if doneNew < k1
                  then (byDist1, byPos1)
                  else (byDist, byPos1)

            Nothing -> (byDist1, byPos1)

      mergeA2 e p a = let
         mergeA3 ek ev = if isGoal ev  then [Goal (dsn1 ev) (ds1 ev) (step1 ev) (pos1 ev) (path1 ev)]
                                       else ev : mergeA2 (M.drop 1 e) p (M.unionWith (++) a (nearest ev))
         mergeA4 ak av = let
            (newe, newp) = foldl add (e, p) $ concatMap (expand heuristic) av
            in mergeA2 newe newp (M.drop 1 a)

         in case M.lookupMin e of
               Just (ek, ev) ->
                  case M.lookupMin a of
                     Just (ak, av) ->
                        if ak < ek
                           then mergeA4 ak av
                           else mergeA3 ek ev
                     Nothing -> mergeA3 ek ev
               Nothing ->
                  case M.lookupMin a of
                     Just (ak, av) -> mergeA4 ak av
                     Nothing       -> []
   
   in mergeA2 (M.fromList [(0, Exactly 0 0 0 start [])]) M.empty M.empty

astar1 nearest checkDist heuristic isGoal start maxDist =
   let
      --heuristic (x, y) = sqrt $ fromIntegral ((length a - x)^2 + (length b - y)^2)
      add (byDist, byPos) ((step, to), (oldtotal, done, from, path)) = let
         hFrom = heuristic from
         hTo = heuristic to
         doneNew = done + step
         totalNew = doneNew + hTo
         atNew = to
         pathNew = (step, to):path
         tdapNew = (totalNew, doneNew, atNew, pathNew)
         in if hFrom <= hTo + step
            then let
                  byPos1 = MM.insert to doneNew tdapNew byPos
                  byDist1 = MM.insert totalNew to tdapNew byDist
                  in case MM.lookupMin to byPos of
                     Just (k1, v) ->
                        if doneNew < k1
                           then (byDist1, byPos1)
                           else (byDist, byPos1)

                     Nothing -> (byDist1, byPos1)

            else
               error "heuristic is not consistent!"

      loop queues@(byDist, byPos) = let
         Just (total, m1) = M.lookupMin byDist
         Just (at1, j@(total1, done, at, path)) = M.lookupMax m1

         in if | isGoal at -> j
               | total < maxDist -> let
                  near1 = nearest at
                  --let check = map (\((step, to), (total, done, at, path)) -> (checkDist at to, step, to)) near1
                  byDist1 = MM.delete total at1 byDist
                  in loop $ foldl add (byDist1, byPos) near1

               | otherwise -> j

   in loop $ add (M.empty, M.empty) ((0, start), (0, 0, start, []))

--mergeA heuristic a = mergeA2 heuristic M.empty $ multimap $ mapfxx dos a

astarD nearest checkDist heuristic isGoal start maxDist =
   let
      --heuristic (x, y) = sqrt $ fromIntegral ((length a - x)^2 + (length b - y)^2)
      add (byDist, byPos) ((step, to), (oldtotal, done, from, path)) = let
         hFrom = heuristic from
         hTo = heuristic to
         doneNew = done + step
         totalNew = doneNew + hTo
         atNew = to
         pathNew = (step, to):path
         tdapNew = (totalNew, doneNew, atNew, pathNew)
         in if hFrom <= hTo + step
            then let
               byPos1 = MM.insert to doneNew tdapNew byPos
               byDist1 = MM.insert totalNew to tdapNew byDist
               in case MM.lookupMin to byPos of
                     Just (k1, v) ->
                        if doneNew < k1
                           then (byDist1, byPos1)
                           else  (byDist, byPos1)

                     Nothing -> (byDist1, byPos1)

            else
               error "heuristic is not consistent!"

      loop queues@(byDist, byPos) = let
         Just (total, m1) = M.lookupMin byDist
         Just (at1, j@(total1, done, at, path)) = M.lookupMax m1

         in do
            putStrLn ""
            putStrLn "-------------------------------------------------------------"
            --mapM_ (\(pos, sl) -> mapM (\(to, (total1, done, at, path)) -> print (to, total1, done, at, path)) $ M.toList sl) $ M.toList byPos
            putStrLn $ stringHyper $ fromAssocsDA (flip const) "" $ map (\(pos, map1) -> let Just (done, tdap) = M.lookupMin map1 in (map toDyn pos, show done)) $ M.toList byPos
            putStrLn ""
            putStrLn $ "total="++show total++" done="++show done++" at="++show at++" path="++show path
            --mapM_ (\(total, sl) -> do mapM_ (\(total1, done, at, path) -> print (total1, done, at, path)) $ SL.toList sl) $ M.toList byDist
            putStrLn ""
            putStrLn $ "total="++show total++" done="++show done++" at="++show at++" path="++show path
            putStrLn ""
            if | isGoal at -> return []
               | total < maxDist -> do
                  let near1 = map (,j) $ nearest at
                  let check = map (\((step, to), (total, done, at, path)) -> (checkDist at to, step, to)) near1
                  putStrLn $ "AT="++show at
                  putStrLn $ stringHyper $ fromAssocsDA (flip const) "" $ map (\((step, to), _) -> (map toDyn to, show step)) near1
                  let byDist1 = MM.delete total at1 byDist
                  let (byDist2, byPos2) = foldl add (byDist1, byPos) near1
                  loop (byDist2, byPos2)

{-
                  if and $ map (\(a, b, c) -> a == b) check
                     then loop $ foldl add (byDist1, byPos) near1
                     else error "checkDist gave a different answer"
-}
               | otherwise -> return []

   in loop $ add (M.empty, M.empty) ((0, start), (0, 0, start, []))


--bot = 
fuzzyMatch10 a b = let
   la = length a
   lb = length b
   dla = fromIntegral la
   dlb = fromIntegral lb
   index a = multimap $ zip ('^':a++"$") [0::(Int) ..]
   ai = index a
   bi = index b

   -- this returns (d, e). 
   -- d is the distance from the origin along the line going down to the right
   -- e is the distance from that line going up to the right (positive) or down to the left (negative)
   diag x y = (x + y, (x - y, (x, y)))

   -- work out the range of e values to the right of x and down from y
   diage d x y = (2 * x - d, 2 * y - d)
   --diag x y = x + y
   int a b = M.map M.fromList $ multimap $ concatMap (\(c, (xs, ys)) -> concat $ crossWith diag xs ys) $ M.toList $ M.intersectionWith (,) (index a) (index b)
   i = int a b

   start = [0, 0]
   goal = [dla + 1, dlb + 1]

   --ps = ([start, goal]++) $ concatMap (\(c, (xs, ys)) -> crossList [xs, ys]) $ M.toList i
   actDist k0 k1 = let z = map abs $ zipWith (-) k0 k1 in if all (==1) z then 0 else sum z
   hDist k = let d = zipWith (-) goal k in maximum d - minimum d
   --m = M.map (\(as, bs) -> (S.fromList as, S.fromList bs)) $ M.intersectionWith (,) ai bi
   nearest c = let
      [x, y] = c
      (d, (e, _)) = diag x y
      (e1, e2) = diage d x y
      xs = concatMap (map snd . M.toList . M.takeWhileAntitone (< e2) . M.dropWhileAntitone (<= e1) . snd) $ M.toList $ M.dropWhileAntitone (<= d) i
      --ys = concatMap snd $ M.toList $ M.dropWhileAntitone (and . zipWith (<=) c) fx
      in [[0, 0]]

   hsnearest = HS.fromList . nearest

   s = replicate (max la lb) ' '

   Just j = aStar hsnearest actDist hDist (== goal) start
   --in mapM print ps
   --in mapM print j
   ds = zipWith actDist j (tail j)
   --in (sum ds, ds)
   blah start = mapxfx (actDist start) $ nearest start
   in mapM (\x -> putStrLn $ show x ++ " = " ++ show (blah x)) j
{-x = not allowed

i think the first actual distance grid here is better, that's what i've used
centred on the point we're at
01234
11234
22234
33334
44444

01234
12345
23456
34567
45678

the heuristic grid is like this:
centred on the goal diagonal

01234    10123
10123    21012
21012 or 32101
32101    43210
43210    54321

the sum looks like this, depending on how far we are W or E from the diagonal that ends at the goal:
centre   1 west   2 west   3 west
02468    11357    22246    33335791
21357    32246    43335    54444680
43246 or 54335 or 65444 or 76555579
65435    76544    87655    98766668
87654    98765    09876    10987777
                           32109888

the way I have it now, they come out in reverse Ls
the reverse L at the bottom of 3 west
     5          2          7
     5          1          6
     5  +       0          5
     5          1          6
     5          2          7
555555     876543     321098

is made up of

321098 and 876567

we want

566778890123
-}
{-
--astar nextdists heuristic goal start
submap from to = M.takeWhileAntitone (<= to) . M.dropWhileAntitone (< from)

common as bs = length $ takeWhile id $ zipWith (==) as bs

closeTo target m = let
   before = map (\(k, a) -> (common target k, a)) $ reverse $ M.toList $ M.takeWhileAntitone (< target) m
   after  = map (\(k, a) -> (common target k, a)) $ M.toList $ M.dropWhileAntitone (< target) m
   in merge before after

merge as            []            = as
merge []            bs            = bs
merge ((ai, ax):as) ((bi, bx):bs) = if ai < bi then (ai, ax):merge as ((bi, bx):bs) else (bi, bx):merge ((ai, ax):as) bs
{-
insertWith2 combine one key map = M.alter f k map
where
f Nothing = Just $ one
f (Just v2) = Just $ combine v2
-}
-}
--fuzzyMatchMulti :: [[a]]

checkAscending xs = let
   ys = map fst xs
   f = filter (uncurry (>)) $ zip ys (tail ys)
   in if null f
         then xs
         else error $ "not ascending! "++show f
{-
fuzzyMatch11 seqs = let
   lens = map length seqs
   index a = multimap $ zip ('^':a++"$") [(0::Int) ..]
   inter = foldr (\seq rest -> M.intersectionWith (:) (index seq) rest) (M.map sing $ index (last seqs)) $ init seqs
   union = foldr (\seq rest -> unionWith3 (:) sing id (index seq) rest) (M.map sing $ index (last seqs)) $ init seqs
   --i = M.map (\[xs, ys, zs] -> (S.fromList xs, S.fromList ys, S.fromList zs)) int
   --u = M.map (\(xs, ys) -> (sort xs, sort ys)) $ unionWith3 (,) (,[]) ([],) ai bi
   i = M.map (map S.fromList) inter
   u = M.map (map S.fromList) union
   z = concatMap (\(c, ls) -> map (c,) $ concat $ crossList ls) $ M.toList union
   --fx = M.fromList $ concatMap (\(c, (xs, ys)) -> map (, ys) $ S.toList xs) $ M.toList i
   maps = map (\n -> M.fromList $ concatMap (\(c, sets) -> map (, deleteIndex n sets) $ S.toList $ sets !! n) (M.toList u)) [0..length seqs - 1]

   start = replicate (length seqs) 0
   goal = map (+1) lens

   --ps = ([start, goal]++) $ concatMap (\(c, (xs, ys)) -> crossList [xs, ys]) $ M.toList i
   actDist k0 k1 = max 0 $ subtract 1 $ maximum $ map abs $ zipWith (-) k0 k1
   heuristic k   = let d = zipWith (-) goal k in maximum d - minimum d
   --m = M.map (\(as, bs) -> (S.fromList as, S.fromList bs)) $ M.intersectionWith (,) ai bi
{-
   nearest xs =   concat $ zipWith3 (\n x1 map1 -> 
                     concatMap (\(x2, sets) -> let
                        cross1 = map (insertIndex n x2) $ crossList $ zipWith (\x3 set -> --trace ("x3="++show x3++" x4="++show (x2 - x1 + x3)) $
                              S.toList $
                              S.takeWhileAntitone (<= x2 - x1 + x3) $
                              S.dropWhileAntitone (<= x3) set) (deleteIndex n xs) sets
                        withdists = map (x2 - x1 - 1,) cross1
                        correct = and $ map ((== x2 - x1 - 1) . actDist xs) cross1

                        in if not correct
                           then error ("something wrong with node distances, from "++show xs++", these should have distance "++show (x2-x1-1)++": "++show (mapfxx (actDist xs) cross1)) --withdists
                           else withdists
                           
                              ) $ 
                              M.toList $
                              M.dropWhileAntitone (<= x1) map1) [0..length seqs - 1] xs maps

-}
   nearest xs = let
      block = mytrace $ transpose $ zipWith3 (\n x1 map1 ->
                     map (\(x2, sets) -> let
                        cross1 = map (insertIndex n x2) $ crossList $ zipWith (\x3 set -> --trace ("x3="++show x3++" x4="++show (x2 - x1 + x3)) $
                              S.toList $
                              S.takeWhileAntitone (<= x2 - x1 + x3) $
                              S.dropWhileAntitone (<= x3) set) (deleteIndex n xs) sets
                        withdists = map (x2 - x1 - 1,) cross1
                        correct = and $ map ((== x2 - x1 - 1) . actDist xs) cross1

                        in if not correct
                           then error ("something wrong with node distances, from "++show xs++", these should have distance "++show (x2-x1-1)++": "++show (mapfxx (actDist xs) cross1)) --withdists
                           else withdists

                              ) $
                              M.toList $
                              M.dropWhileAntitone (<= x1) map1) [0..length seqs - 1] xs maps

      in checkAscending $ concat $ concat block -- concat is probably more efficient than merge, but does it always work? transpose can give surprising answers if the input is not rectangular

   --in showNearest nearest heuristic z
   in astar nearest actDist heuristic (== goal) start 100

-- fuzzyMatch11 ["jkhkhkjhjkhjkhjkhjkhjkhjkhkjkjhkhjk", "kjhkjhkjhkhjkhjkjhjkhjkhkjhjkhkj"]
-- total=10 done=10 at=[36,33] path=[(2,[36,33]),(0,[33,30]),(1,[32,29]),(1,[30,28]),(0,[28,26]),(0,[27,25]),(0,[26,24]),(0,[25,23]),(0,[24,22]),(0,[23,21]),(0,[22,20]),(0,[21,19]),(1,[20,18]),(1,[18,17]),(0,[16,16]),(0,[15,15]),(0,[14,14]),(0,[13,13]),(0,[12,12]),(0,[11,11]),(1,[10,10]),(0,[8,9]),(0,[7,8]),(0,[6,7]),(1,[5,6]),(0,[4,4]),(2,[3,3]),(0,[0,0])]
   --in print int
{-
   --Just j = aStar hsnearest actDist hDist (== goal) start
   in case astar nearest heuristic (== goal) start 10 of
      Right (d, g, j) -> let
         --in mapM print ps
         --in mapM print j
         ds = zipWith actDist j (tail j)
         --in (sum ds, ds)
         blah start = sort $ mapfxx (\(n, x) -> actDist start x + heuristic x) $ nearest start
         --in blah [15, 15]
         in mapM (\x -> putStrLn $ show x ++ " = " ++ show (blah x)) j
      Left (d, g, j) -> let
         --in mapM print ps
         --in mapM print j
         ds = zipWith actDist j (tail j)
         --in (sum ds, ds)
         blah start = sort $ mapfxx (\(n, x) -> actDist start x + heuristic x) $ nearest start
         --in blah [15, 15]
         in mapM (\x -> putStrLn $ show x ++ " = " ++ show (blah x)) j
  -}
-}
--bot = 


fuzzyMatchMulti xs = let
   nseq = length xs
   nseq1 = nseq - 1
   lens = map length xs
   ch = 1
   indices = zipWith (\x -> mapFromList (:) [] . (`zip` map (x,) [1..]) . init . tails) [0..nseq1] xs
   index = M.filter ((> 1) . length) $ foldr (M.unionWith (++)) M.empty indices
   steps = mapfxx ((nseq -) . sum) $ tail $ crossList $ replicate nseq [0, 1]
   em = replicate nseq []
   --nexts from = HS.fromList $ map (zipWith (+) from . snd) steps

   {-
   next x f = take 4 $ (f:) $ (++replicate 4 f) $ sort $ take 20 $ concat
      $ mapMaybe ((mapMaybe (\(x1, i) -> ifJust (x == x1 && i >= f) i) <$>) . (`M.lookup` index) . take ch) 
      $ init $ tails $ drop f $ xs !! x
   nexts from = HS.fromList $ filter (/= from) $ transpose $ zipWith next [0..nseq1] from
   -}
   nextsIO from = do
      putStrLn $ "nexts from "++show from
      let n = nexts from
      putStrLn $ "result="++show n
      return n
   {-
   nexts from = let
      to = map (+1) from
      vs = zipWith (!!) xs to
      cs = counts vs
   nexts from = let
      z = zip [0..nseq1] from
      el s (mx, mn) = foldr (\(n, f) -> insertWith2 (:) [] (xs !! n !! (f + s), (n, f))) m (zip [0..nseq1] from)
      e (n, f) s m = let
      k = xs !! n !! (f + s)
      vx = fromMaybe [] $ M.lookup k mx
      vx1 = (n, f):v
      mx1 = M.insert k v1 mx
      kn = length vx1
   -}

   nexts from = let
      z = zip [0..nseq1] from
      --el s m = foldr (\(x, f) -> insertWith2 (:) [] (xs !! x !! (f + s), (x, f + s))) m z
      el m s = foldr (\(x, f) m ->
         if f + s < lens !! x
         then insertWith2 (\y -> modifyIndex x (y:)) em (xs !! x !! (f + s), f + s) m
         else m) m z
      els = scanl el M.empty [0..7]
      el1 = map (rsortOn (length . filter (not . null)) . map snd . M.toList) els
      --el1 = map (map (sort . concat) . transpose . filter ((> 1) . length . filter (not . null)) . map snd . M.toList) els
      in el1

   dist from to = let
      --lose a point for each sequence you don't move forward on
      --lose a point for each different value you have on the ones you do move forward on
      fwd = zipWith (-) to from
      fwdscore = sum $ map (abs . (- 1)) fwd
      vals = length $ nubSet $ catMaybes $ zipWith3 (\x f to1 -> ifJust (f /= 0) (x !! (to1 - 1))) xs fwd to

      in if or $ zipWith (>) to lens then error ("dist"++show from++"->"++show to) else fwdscore + (vals - 1)

   distIO from to = return $ dist from to

   heuristic from = let
      z = zipWith (-) lens from
      in maximum z - minimum z

   heuristicIO from = return $ heuristic from

   goalIO at = return $ at == lens

   start = replicate nseq 0

   startIO = return start :: IO [Int]

   --path = aStar nexts dist heuristic (==lens) start

   in nexts start
   --in aStarM nextsIO distIO heuristicIO goalIO startIO
   {-
   in case path of
      Just path1 -> let
      path2 = zipWith (\from to -> (zipWith (\f t -> ifJust (t > f) (t - 1)) from to, dist from to)) (start:path1) path1
      (path3, dists) = unzip path2
      in
      Just (path3, sum dists)
      Nothing    -> Nothing
   -}
{-
fuzzyMatchMulti1 xs = let
   nseq = length xs
   nseq1 = nseq - 1
   lens = map length xs
   ch = 1
   indices = zipWith (\x -> mapFromList (:) [] . (`zip` map (x,) [1..]) . map (take ch) . init . tails) [0..nseq1] xs
   index = M.filter ((> 1) . length) $ foldr (M.unionWith (++)) M.empty indices
   steps = mapfxx ((nseq -) . sum) $ tail $ crossList $ replicate nseq [0, 1]
   --nexts from = HS.fromList $ map (zipWith (+) from . snd) steps


   next x f = take 4 $ (f:) $ (++replicate 4 f) $ sort $ take 20 $ concat
      $ mapMaybe ((mapMaybe (\(x1, i) -> ifJust (x == x1 && i >= f) i) <$>) . (`M.lookup` index) . take ch)
      $ init $ tails $ drop f $ xs !! x
   nexts from = HS.fromList $ filter (/= from) $ transpose $ zipWith next [0..nseq1] from

   nextsIO from = do
      putStrLn $ "nexts from "++show from
      let n = nexts from
      putStrLn $ "result="++show n
      return n
   {-
   nexts from = let
      to = map (+1) from
      vs = zipWith (!!) xs to
      cs = counts vs
   nexts from = let
      z = zip [0..nseq1] from
      el s (mx, mn) = foldr (\(n, f) -> insertWith2 (:) [] (xs !! n !! (f + s), (n, f))) m (zip [0..nseq1] from)
      e (n, f) s m = let
      k = xs !! n !! (f + s)
      vx = fromMaybe [] $ M.lookup k mx
      vx1 = (n, f):v
      mx1 = M.insert k v1 mx
      kn = length vx1

      el s m = foldr (\(x, f) -> insertWith2 (:) [] (xs !! x !! (f + s), (x, f + s))) m z
      els = scanr el M.empty [0..]
      el1 = find ((>= nseq) . length . filter ((> 1) . length) . map snd . M.toList) els
   -}
   dist from to =
      let
      --lose a point for each sequence you don't move forward on
      --lose a point for each different value you have on the ones you do move forward on
      fwd = zipWith (-) to from
      fwdscore = sum $ map (abs . (- 1)) fwd
      vals = length $ nubSet $ catMaybes $ zipWith3 (\x f to1 -> ifJust (f /= 0) (x !! (to1 - 1))) xs fwd to

      in if or $ zipWith (>) to lens then error ("dist"++show from++"->"++show to) else fwdscore + (vals - 1)

   distIO from to = return $ dist from to

   heuristic from = let
      z = zipWith (-) lens from
      in maximum z - minimum z

   heuristicIO from = return $ heuristic from

   goalIO at = return $ at == lens

   start = replicate nseq 0

   startIO = return start :: IO [Int]

   --path = aStar nexts dist heuristic (==lens) start

   in nexts start
   --in aStarM nextsIO distIO heuristicIO goalIO startIO
   {-
   in case path of
      Just path1 -> let
      path2 = zipWith (\from to -> (zipWith (\f t -> ifJust (t > f) (t - 1)) from to, dist from to)) (start:path1) path1
      (path3, dists) = unzip path2
      in
      Just (path3, sum dists)
      Nothing    -> Nothing
   -}

fuzzyMatchMulti2 xs = let
   len = length xs
   len1 = len - 1
   lens = map length xs
   steps = mapfxx ((len -) . sum) $ tail $ crossList $ replicate (length xs) [0, 1]
   nexts from = HS.fromList $ map (zipWith (+) from . snd) steps
   dist from to =
      let
      --lose a point for each sequence you don't move forward on
      --lose a point for each different value you have on the ones you do move forward on
      fwd = zipWith (-) to from
      fwdscore = len - sum fwd
      vals = length $ nubSet $ catMaybes $ zipWith3 (\x f to1 -> ifJust (f /= 0) (x !! (to1 - 1))) xs fwd to

      in fwdscore + (vals - 1)
   heuristic from = let
      z = zipWith (-) lens from
      in maximum z - minimum z
   start = replicate len 0

   path = aStar nexts dist heuristic (==lens) start

   in case path of
      Just path1 -> let
         path2 = zipWith (\from to -> (zipWith (\f t -> ifJust (t > f) (t - 1)) from to, dist from to)) (start:path1) path1
         (path3, dists) = unzip path2
            
         in Just (path3, sum dists)
      Nothing    -> Nothing
{-       
>>> fuzzyMatchMulti2 ["abc", "abc", "def", "df"]
Prelude.!!: index too large
-}
{-
commonSubsequencesMulti1 xs =
let
len = length xs
len1 = len - 1
steps = mapfxx ((len -) . sum) $ tail $ crossList $ replicate (length xs) [0, 1]
scoremap = M.fromList $ map commonSubsequencesA $ crossList $ map (\x -> [0 .. length x]) xs
commonSubsequencesA coords
| elem 0 coords = (coords, (0, []))
| otherwise =
   let
   zs = zipWith (\x y -> x !! (y - 1)) xs coords
   --lose a point for each sequence you don't move forward on
   --lose a point for each different value you have on the ones you do move forward on
   as = map (\(fwdscore, fwd) -> let
      at = zipWith (-) coords fwd
      (oldscore, oldpath) = scoremap M.! at
      vals = length $ nubSet $ catMaybes $ zipWith (\z f -> ifJust (f /= 0) z) zs fwd

      in (oldscore - fwdscore - (vals - 1), oldpath)) steps
   (newscore, oldpath) = maximum as
   in
   (coords, (newscore, coords : oldpath))
in
scoremap M.! map length xs
{-
>>> commonSubsequencesMulti ["abc", "abc", "def", "def"]
(-3,[[3,3,3,3],[2,2,2,2],[1,1,1,1]])
-}

-}
-}