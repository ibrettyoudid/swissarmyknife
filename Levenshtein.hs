{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LexicalNegation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}

module Levenshtein where

import Favs

import Data.List
import Data.Graph.AStar
import qualified Data.Map as M
import qualified Data.HashSet as HS

{-
01234
12345
23456
34567
45678
-}
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
levenshteinMulti xs =
   let
      nseq = length xs
      nseq1 = nseq - 1
      lens = map length xs
      ch = 1
      indices = zipWith (\x -> mapFromList (:) [] . (`zip` map (x,) [1..]) . init . tails) [0..nseq1] xs
      index = M.filter ((> 1) . length) $ foldr (M.unionWith (++)) M.empty indices
      steps = mapfxx ((nseq -) . sum) $ tail $ crossList $ replicate nseq [0, 1]
      em = replicate nseq []
      --nexts from = HS.fromList $ map (zipWith (+) from . snd) steps
      u = filter (not . null) [[]]

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

levenshteinMulti1 xs =
   let
      nseq = length xs
      nseq1 = nseq - 1
      lens = map length xs
      ch = 1
      indices = zipWith (\x -> mapFromList (:) [] . (`zip` map (x,) [1..]) . map (take ch) . init . tails) [0..nseq1] xs
      index = M.filter ((> 1) . length) $ foldr (M.unionWith (++)) M.empty $ indices
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

levenshteinMulti2 xs =
   let
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
            in
               Just (path3, sum dists)
         Nothing    -> Nothing
{-       
>>> commonSubsequencesMulti ["abc", "abc", "def", "def"]
(-3,[[3,3,3,3],[2,2,2,2],[1,1,1,1]])
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
