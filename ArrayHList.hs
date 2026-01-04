{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Array2 where

import MyPretty2
import Favs

import Data.Array.IArray hiding ((!), assocs, index, indices)
import Data.List
import qualified Data.Map as M
import qualified Data.Array.IArray as A
import Data.HList

infixr 0 :-
data a :- b = (:-) { fstT::a, sndT::b } deriving Show

data Nil = Nil deriving Show

data DimInt = DimInt { diLower::Int, diUpper::Int, diMult::Int } deriving (Eq, Ord, Show)
data DimMap i = DimMap { dmLower::Int, dmUpper::Int, dmMult::Int, dmMap1::M.Map Int i, dmMap2::M.Map i Int } deriving (Eq, Ord, Show)

data SubArray dimList e = SubArray { dims::dimList, offset::Int, payload::Array Int e }

getElem (SubArray Nil off pay) = pay A.! off

getSub i (SubArray (HCons d dl) off pay) = SubArray dl (off+(i-diLower d)*diMult d) pay

getSubMap i (SubArray (HCons d dl) off pay) = SubArray dl (off+((fromJust $ M.lookup i $ dmMap2 d)-dmLower d)*dmMult d) pay

fromElems es = SubArray (HCons (DimInt 0 (l-1) 1) HNil) 0 $ A.listArray (0, l-1) es where l = length es

--fromSubs ss 
--elemOffset :: (TupleOf i Int, TupleOf d DimInt) => i -> d -> Int
elemOffset i dims = hFoldr (+) (0::Int) $ hZipWith FMultDim $ zipT i dims

fromAssocs as = let
   is   = map fst as
   --in is
   tran = unzipT is
   --in tran
   mins = mapT FMin tran
   --in mins
   maxs = mapT FMax tran
   lens = mapT FRange tran
   --in lens
   (len :- muls) = scanRT FMult (1::Int) lens
   dims = mapT FDim $ zip3T mins maxs muls
   in SubArray dims 0 $ A.array (0, len - 1) $ map (\(i, e) -> (elemOffset i dims, e)) as

assocsFromList1 es = zipWith (\x e -> (HCons x HNil, e)) [0..] es
assocsFromList2 ess = concat $ zipWith (\x es -> zipWith (\y e -> (HCons x $ HCons y HNil, e)) [0..] es) [0..] ess
assocsFromList3 esss = concat $ zipWith (\x ess -> concat $ zipWith (\y es -> zipWith (\z e -> (HCons x $ HCons y $ HCons z HNil, e)) [0..] es) [0..] ess) [0..] esss

fromList1a = fromAssocs . assocsFromList1
fromList2a = fromAssocs . assocsFromList2
fromList3a = fromAssocs . assocsFromList3

fromList2 zero ess = let
   d0 = length ess
   d1 = maximum $ map length ess

   in SubArray (HCons (DimInt 0 (d0-1) d1) $ HCons (DimInt 0 (d1-1) 1) HNil) 0 $ A.listArray (0, d0 * d1 - 1) $ concatMap (padRWith1 zero d1) ess

fromList3 :: e -> [[[e]]] -> SubArray (HList [DimInt, DimInt, DimInt]) e
fromList3 zero esss = let
   d0 = length esss
   d1 = maximum $ map length esss
   d2 = maximum $ map (maximum . map length) esss

   in SubArray (HCons (DimInt 0 (d0-1) (d1*d2)) $ HCons (DimInt 0 (d1-1) d2) $ HCons (DimInt 0 (d2-1) 1) HNil) 0 $ A.listArray (0, d0 * d1 * d2 - 1) $ concatMap (padRWith1 zero (d1 * d2) . concatMap (padRWith1 zero d2)) esss

class AssocsFromList d e a | d e -> a where
   assocsFromList :: d -> e -> a

--instance AssocsFromList Z e [(Nil, e)] where
--   assocsFromList Z e = [(Nil, e)]

--instance (AssocsFromList d es [(i, e)], HList i) => AssocsFromList (S d) [es] [(HList (Int : i), e)] where

--zassocsFromList (S d) es = concat $ zipWith (\i e -> map (\(i2, e2) -> (HCons i i2, e2)) $ assocsFromList d e) [0..] es

fromList d = fromAssocs . assocsFromList d

--cartesian [1,2,3] [[4],[5],[6]] = [[1,4],[2,4],[3,4],[1,5],[2,5],[3,5],[1,6],[2,6],[3,6]]
--cartesian :: [Int] -> [[Int]] -> [[Int]]
--cartesian xs ys = concatMap (\x -> map (x:) ys) xs

dimRange (DimInt dl du _) = [dl..du]
dimRanges = map dimRange

indicesA a = indices $ dims a
indices = foldr cartesian [[]]

listFromTuple t = foldRT FCons [] t

select indices from = map (from !!) indices

inversePerm indices = map snd $ sort $ zip indices [0..]

forceLookup m k = fromJust $ M.lookup k m

getElemDyn is a = let
   (dl, du::[Int], dm) = unzip3 $ listFromTuple $ mapT FGetDim $ dims a
   in payload a A.! (offset a + sum (zipWith3 (\i l m -> (i - l) * m) is dl dm))

toGrid3 yn xn da a = let
   xd = select xn da
   yd = select yn da
   xs = indices $ map fst xd
   ys = indices $ map fst yd
   xl = map snd xd
   yl = map snd yd
   d  = xn ++ yn
   ip = inversePerm d

   in (replicate (length xn) "", map (zipWith forceLookup yl) ys, map (zipWith forceLookup xl) xs, map (\x -> map (\y -> getElemDyn (select ip (x++y)) a) ys) xs)
--   in (length xn, xs, xl, ys, yl, map (\x -> map (\y -> getElemDyn (select ip (x++y)) a) ys) xs)

--toGrid :: (FoldRT FCons [(Int, Int, Int)] as1, FoldRT FCons [([Int], M.Map Int String)] as2, MapT FGetDim dimList as1,  MapT FShowDim dimList as2) => [Int] -> [Int] -> SubArray dimList b2 -> [[b2]]
toGrid2 yn xn a = let
   da = listFromTuple $ mapT FShowDim $ dims a :: [([Int], M.Map Int String)]
   xd = select xn da
   yd = select yn da
   xs = indices $ map fst xd
   ys = indices $ map fst yd
   xl = map snd xd
   yl = map snd yd
   d  = xn ++ yn
   ip = inversePerm d

   in (replicate (length xn) "", map (zipWith forceLookup yl) ys, map (zipWith forceLookup xl) xs, map (\x -> map (\y -> getElemDyn (select ip (x++y)) a) ys) xs)

toGrid1 yn xn da a = let
   (tl, tr, bl, br) = toGrid3 yn xn da a

   in map (tl ++) (Data.List.transpose tr) ++ zipWith (++) bl (map2 show1 br)


toGrid yn xn a = let
   (_, _, _, g) = toGrid2 yn xn a

   in g

--showHyper yn xn a = showGrid width $ map2 show1 $ toGrid yn xn a
showHyper1 yn xn da a = showGrid $ toGrid1 yn xn da a
   --da = listFromTuple $ mapT FShowDim $ dims a :: [([Int], M.Map Int String)]

--showHyperA1 :: (Show1 e, MapT FShowDim dimList a, MapT FGetDim dimList b, FoldRT FCons [([Int], M.Map Int [Char])] a, FoldRT FCons [(Int, Int, Int)] b) => SubArray dimList e -> String
showHyper a = let
   da = listFromTuple $ mapT FShowDim $ dims a :: [([Int], M.Map Int String)]
   n = length da
   hn = div n 2
   in showHyper1 [0..hn-1] [hn..n-1] da a

toList2 a@(SubArray (HCons d0 (HCons d1 e)) o p) = map (\x -> map (\y -> getElemDyn [x, y] a) [diLower d0..diUpper d0]) [diLower d1..diUpper d1]

{-
showHyper yn xn a = let
   da = listFromTuple $ mapT FShowDim $ dims a
   xd = select xn da
   yd = select yn da
   xs = indices $ map fst xd
   ys = indices $ map fst yd
   xl = map snd xd
   yl = map snd yd
   d  = xn ++ yn
   ip = inversePerm d

   in showGrid width 
      $  map (replicate (length xn) "" ++) (Data.List.transpose $ map (zipWith forceLookup yl) ys)
      ++ map (\x -> zipWith forceLookup xl x ++ map (\y -> show1 $ getElemDyn (select ip (x++y)) a) ys) xs

showHyper1 :: (Show1 e, MapT FShowDim dimList a, MapT FGetDim dimList b, FoldRT FCons [([Int], M.Map Int [Char])] a, FoldRT FCons [(Int, Int, Int)] b) => SubArray dimList e -> String
showHyper1 a = let
   l = listFromTuple $ mapT FShowDim $ dims a :: [([Int], M.Map Int String)]
   n = length l
   hn = div n 2
   in showHyper [0..hn-1] [hn..n-1] a
-}
--mapDims df a = SubArray (applyT df $ dims a) (offset a) (payload a)

class DimList dimList
instance DimList Nil
instance DimList dl => DimList (DimInt :- dl)
instance DimList dl => DimList (DimMap i :- dl)

--instance (Show1 e, MapT FShowDim dl a, MapT FGetDim dl b, MapT FShowDim dl c, FoldRT FCons [([Int], M.Map Int [Char])] a, FoldRT FCons [(Int, Int, Int)] b, FoldRT FCons [([Int], M.Map Int String)] c) => Show (SubArray dl e) where
--instance (Show1 e, MapT FShowDim dl a, MapT FGetDim dl b, FoldRT FCons [([Int], M.Map Int [Char])] a, FoldRT FCons [(Int, Int, Int)] b) => Show (SubArray dl e) where
--   show = showHyper

class Show1 a where
   show1 :: a -> String

instance Show1 String where
   show1 = id

instance {-# OVERLAPPABLE #-} Show a => Show1 a where
   show1 = show

getDimInt d = ( diLower d, diUpper d, diMult d )
getDimMap d = ( dmLower d, dmUpper d, dmMult d )

showDimInt d = ( [diLower d..diUpper d], M.fromList $ mapxfx show1 [diLower d..diUpper d] )
showDimMap d = ( [dmLower d..dmUpper d], M.map show1 $ dmMap1 d )
--showLabel (DimMap _ _ _ dm1 _) i = fromJust $ M.lookup i dm1

showDimMapS d = ( map snd $ M.toList $ dmMap2 d, M.map show1 $ dmMap1 d )

--test d = fromAssocs $ mapxfx id $ indices $ replicate d $ DimInt 0 2 1
