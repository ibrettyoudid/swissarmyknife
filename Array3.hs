-- Copyright 2025 Brett Curtis
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Array3 where

import MyPretty2
import Favs
import Numeric
import NewTuple
import qualified BString as B
--import qualified NumberParsers as NP
import Show1

import Data.List
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import Data.IORef
import qualified Data.Array.IArray as A


import GHC.Stack
import Parser5 (d1)
import Graphics.UI.Gtk (afterOutput)
--import Control.Exception

--import Syntax3 hiding (foldl, foldr, print, right)
--import Syntax3 qualified as S3

class Dimension d where
   dimLower :: d -> Int
   dimUpper :: d -> Int
   dimMult  :: d -> Int
   dimRange :: d -> [Int]

instance Dimension DimInt where
   dimLower = diLower
   dimUpper = diUpper
   dimMult  = diMult
   dimRange d = [diLower d..diUpper d]

instance Dimension (DimMap typ) where
   dimLower = dmLower
   dimUpper = dmUpper
   dimMult  = dmMult
   dimRange d = [dmLower d..dmUpper d]

class DimMapping i d | d -> i where
   mapIndex :: i -> d -> Int
   mapRange :: Int -> d -> i

instance DimMapping Int DimInt where
   mapIndex i d = i
   mapRange i d = i

instance Ord typ => DimMapping typ (DimMap typ) where
   mapIndex i d = fromMaybe (error "not found in index") $ M.lookup i $ dimMap2 d
   mapRange i d = fromMaybe (error "not found in range") $ M.lookup i $ dimMap1 d

elemOffset1 i d = (mapIndex i d - dimLower d) * dimMult d

{-
class DimName d n where
   dimName :: d -> n

instance DimName (DimInt n) n where
   dimName = diName

instance DimName (DimMap n typ) n where
   dimName = dmName

class DimNames d n where
   dimNames :: d -> n

instance DimName d n => DimNames (d :- ds) (n :- ns) where
   dimNames (d :- ds) = dimName d :- dimNames ds

instance DimNames () () where
   dimNames () = ()
-}
class CreateDim i d where
   create :: [i] -> Int -> (d, Int)

instance CreateDim Int DimInt where
   create is m = let
      mi = minimum is
      ma = maximum is
      le = ma - mi + 1

      in (DimInt mi ma m, m * le)

instance Ord typ => CreateDim typ (DimMap typ) where
   create is m = let
      is1 = nubSet is
      le = length is1
      dm1 = M.fromList $ zip [0..le-1] is1
      dm2 = M.fromList $ zip is1 [0..le-1]

      in (DimMap 0 (le-1) m dm1 dm2, m * le)

class CreateDims is ds where
   creates :: [is] -> (ds, Int)

instance CreateDims () () where
   creates [] = ((), 1)

instance (Ord i, CreateDim i d, CreateDims is ds) => CreateDims (i :- is) (d :- ds) where
   creates iis = let 
      (ds, m) = creates (map sndT iis) :: (ds, Int)
      (d, le) = create (map fstT iis) m :: (d, Int)

      in (d :- ds, le)

data DimInt     = DimInt { diLower :: Int, diUpper :: Int, diMult :: Int } deriving (Show)
data DimMap typ = DimMap { dmLower :: Int, dmUpper :: Int, dmMult :: Int, dimMap1 :: M.Map Int typ, dimMap2 :: M.Map typ Int } deriving (Show)
--            | DimCat { dimDim::Dimension, dimDiv::[Int], dimMult1::[Int] }


data SubArray dimList e dimNames = SubArray { dimNames :: dimNames, dims :: dimList, offset :: Int, payload :: A.Array Int e}

-- main = print $ Main.transpose 0 1 $ fromList2z 0 [[1,2,3],[4,5,6],[7,8,9]]

refold2 f z [] = []
refold2 f z (x : xs) = let (a, b) = f z x in a : refold f b xs

-- foldl f   z (x:xs) = f x $ foldl f z xs

refold1 f z xs = snd $ foldr (\i (z, o) -> let (a, b) = f z i in (b, a : o)) (z, []) xs

class DimMappings is ds | ds -> is where
   elemOffset :: is -> ds -> Int

instance (Dimension d, DimMapping i d, DimMappings is ds) => DimMappings (i :- is) (d :- ds) where
   elemOffset (i :- is) (d :- ds) = elemOffset1 i d + elemOffset is ds

instance DimMappings () () where
   elemOffset () () = 0

-- elemOffset1 i (DimCat d di mu) = sum $ zipWith (*) mu $ refold divMod (elemOffset1 i d) di
--elemOffset is dims = foldrT ((+) :: Int -> Int -> Int) (0::Int) $ zipWithT ElemOffset1 is dims :: Int

getElem is a = payload a A.! (offset a + elemOffset is (dims a))

getSub i a = let
   (d :- ds) = dims a
   (n :- ns) = dimNames a

   in SubArray ns ds (offset a + elemOffset1 i d) (payload a)
{-}
getSubs is a = SubArray (dropT (lengthT is) $ dims a) (offset a + elemOffset is (dims a)) (payload a)
-}
getSubDimI :: forall dimNamesIn dimNameOut dimNamesOut1 dimNamesOut2 dimNamesOut dimListIn dimListOut1 dimListOut2 dimListOut d p e.
   (Append dimNamesOut1 dimNamesOut2 dimNamesOut,
    Append dimListOut1 dimListOut2 dimListOut,
    Dimension d,
    SplitAt p dimNamesIn dimNamesOut1 (dimNameOut :- dimNamesOut2),
    SplitAt p dimListIn dimListOut1 (d :- dimListOut2))
      => p -> Int -> SubArray dimListIn e dimNamesIn -> SubArray dimListOut e dimNamesOut

getSubDimI dn i a = SubArray (appendT nbefore nafter) (appendT dbefore dafter) (offset a + (i - dimLower d) * dimMult d) (payload a)
   where
      (dbefore, d :- dafter) = splitAtT dn $ dims a
      (nbefore, n :- nafter) = splitAtT dn $ dimNames a

getSubDim1 :: forall e d i name listIn namesIn namesOut listOut. 
   (Dimension d, DimMapping i d,
   Index name listIn d,
   DeleteIndex name namesIn namesOut listIn listOut)
      => name -> i -> SubArray listIn e namesIn -> SubArray listOut e namesOut
getSubDim1 dn i a = SubArray nafter dafter (offset a + elemOffset1 i d) (payload a)
   where
      d = NewTuple.indexT dn (dims a) :: d
      (nafter, dafter) = NewTuple.deleteIndex dn (dimNames a) (dims a) :: (namesOut, listOut)

class SubDim dn i ain aout | dn i ain -> aout where
   getSubDim :: dn -> i -> ain -> aout

-- undecidable instance
instance (Dimension d, DimMapping i d,
    NamedTuple name namesIn listIn d,
    Delete name namesIn namesOut listIn listOut)
      => SubDim name i (SubArray listIn e namesIn) (SubArray listOut e namesOut) where
   getSubDim dn i a = SubArray nafter dafter (offset a + elemOffset1 i d) (payload a)
      where
         d = NewTuple.lookup dn (dimNames a) (dims a)
         (nafter, dafter) = NewTuple.delete dn (dimNames a) (dims a)

class SubDims ds ain aout where
   getSubDims :: ds -> ain -> aout

instance SubDims () a a where
   getSubDims () = id

instance (SubDim dn i ain amid, SubDims ds amid aout) => SubDims ((dn, i) :- ds) ain aout where
   getSubDims ((dn, i) :- ds) ain = getSubDims ds $ getSubDim dn i ain

class SubDims2 dns is ain aout where
   getSubDims2 :: dns -> is -> ain -> aout

instance SubDims2 () () a a where
   getSubDims2 () () = id

instance (SubDim dn i ain amid, SubDims2 dns is amid aout) => SubDims2 (dn :- dns) (i :- is) ain aout where
   getSubDims2 (dn :- dns) (i :- is) ain = getSubDims2 dns is $ getSubDim dn i ain


--getSubDims ds a = foldr (uncurry getSubDim) a ds

{-
getSubDimDyn dn i a = SubArray (dbefore ++ dafter) (offset a + ((dimMap2 d M.! i) - dimLower d) * dimMult d) (payload a)
   where
      (dbefore, d : dafter) = splitAt dn $ dims a

getSubDimsDyn ds a = foldr (uncurry getSubDimDyn) a ds
-}
copy a = fromAssocs $ toAssocs a

zipWitha f a b = fromAssocs $ zipWith (\i j -> ([i], f (getSub i a) (getSub j b))) (dimRange $ headT $ dims a) (dimRange $ headT $ dims b)

mapEE f a = a { payload = A.amap f $ payload a }

mapEA :: forall e is ds ns e1 is1 ds1 ns1 rs1 is2 ds2 ns2 rs2. 
   (Append is1 is2 is, Append ds1 ds2 ds, Append ns1 ns2 ns, 
   DimMappings is ds, CreateDims is ds, 
   DimRanges ds1 rs1, CrossList rs1 is1, DimMappings is1 ds1, 
   DimRanges ds2 rs2, CrossList rs2 is2, DimMappings is2 ds2) => (e1 -> SubArray ds2 e ns2) -> SubArray ds1 e1 ns1 -> SubArray ds e ns
mapEA f a = let
   assocs1 = toAssocs a :: [(is1, e1)]
   (i, e) = head assocs1
   example = f e

   in fromAssocs (appendT (dimNames a) (dimNames example)) $ concatMap (\(i1, e1) -> map (\(i2, e2) -> (appendT i1 i2, e2)) $ toAssocs $ f e1) assocs1

mapAE f nsel a = let
   (vsel, nleft, vleft) = selectT nsel (dimNames a) (dims a)
   assocs1 = map (\i -> (i, f $ getSubDims2 nsel i a)) $ indices vsel

   in fromAssocs nsel assocs1

mapAA f lNames a = let
   --(lNames, lDims) = difference rNames (dimNames a) (dims a)
   --lrNames = appendT lNames rNames
   lDims = select lNames (dims a)
   indices1 = indices lDims
   example = f $ getSubDims2 lDims (head indices1) a
   assocs1 = concatMap (\i1 -> map (\(i2, e2) -> (appendT i1 i2, e2)) $ toAssocs $ f $ getSubDims2 lDims i1 a) indices1

   in fromAssocs (appendT lNames (dimNames example)) assocs1

--inversePerm :: forall a b c d. (Number a () b, Sort b c, MapSnd c d) => a -> d
--inversePerm a = mapSndT (sortT (numberT a () :: b) :: c) :: d

fromAssocs :: forall is dimList e dimNames. (DimMappings is dimList, CreateDims is dimList) => dimNames -> [(is, e)] -> SubArray dimList e dimNames
fromAssocs dnames as = let
   (dims :: dimList, len) = creates (map fst as)

   in SubArray dnames dims 0 $ A.array (0, len - 1) $ map (\(i, e) -> (elemOffset i dims, e)) as

toAssocs :: (DimMappings is a1, CrossList a2 is, DimRanges a1 a2) => SubArray a1 e dimNames -> [(is, e)]
toAssocs a = map (\i -> (i, getElem i a)) $ indicesA a

class DimRanges a b | a -> b where
   dimRanges :: a -> b

instance DimRanges () () where
   dimRanges () = ()

instance (Dimension a, DimRanges as bs) => DimRanges (a :- as) ([Int] :- bs) where
   dimRanges (a :- as) = dimRange a :- dimRanges as

class CrossList a b | a -> b, b -> a where
   crossListT :: a -> [b]

instance CrossList () () where
   crossListT () = [()]

instance CrossList as bs => CrossList ([a] :- as) (a :- bs) where
   crossListT (a :- as) = concatMap (\xs -> map (:- xs) a) $ crossListT as

indicesA a = indices $ dims a
indices ds = crossListT $ dimRanges ds

{-}
class Select a b c | a b -> c where
   select :: a -> b -> c

instance Select () b () where
   select () b = ()

instance (Index i from found, Select indices from done) => Select (i :- indices) from (found :- done) where
   select (i :- indices) from = indexT i from :- select indices from
-}
{-

foldE f a = f $ map snd $ toAssocs a

appendFold (n, f) a = let
   l = toAssocsD a
   s = f $ map snd l
   in
   fromAssocsD $ l ++ [(n, s)]

--asum = appendFold ([toDyn "Total"], sum)

--admean = appendFold ([toDyn "Mean"], dmean)

--mafs f ts a = foldr (mapAA f . singleton) a ts

mapDim f (DimInt dl du dm) = DimMap dl du dm (M.fromList dm1) (M.fromList $ map tflip dm1) where dm1 = mapxfx f [dl .. du]

zipDims fs (SubArray n d o p) = SubArray n (zipWith mapDim fs d) o p

reverseA a = let
   (DimInt dl du dm : ds) = dims a

   in SubArray n (DimInt dn dl du (-dm) : ds) (offset a + (du - dl + 1) * dm) (payload a)

reverseDim d a = let
   ds = dims a
   (DimInt dl du dm) = ds !! d

   in SubArray n (replaceIndex d (DimInt dn dl du (-dm)) ds) (offset a + (du - dl + 1) * dm) (payload a)

shiftDim d s a = let
   ds = dims a
   (DimInt dl du dm) = ds !! d

   in SubArray n (replaceIndex d (DimInt dn (dl - s) (du - s) dm) ds) (offset a + s * dm) (payload a)

sliceDim d s t a = let
   ds = dims a
   (DimInt dl du dm) = ds !! d

   in SubArray n (replaceIndex d (DimInt dn (dl - s) (t - s + 1) dm) ds) (offset a + s * dm) (payload a)

transposeA d1 d2 a = let
   ds = dims a
   e1 = ds !! d1
   e2 = ds !! d2

   in SubArray n (replaceIndex d2 e1 $ replaceIndex d1 e2 ds) (offset a) (payload a)

-- cartesian [1,2,3] [[4],[5],[6]] = [[1,4],[2,4],[3,4],[1,5],[2,5],[3,5],[1,6],[2,6],[3,6]]
dimRangeD (DimMap dl du _ dm1 _) = map (dm1 M.!) [dl .. du]

class DimRanges a b | a -> b where
   dimRanges :: a -> b

instance DimRanges () () where
   dimRanges () = ()

instance (Dimension a, DimRanges as bs) => DimRanges (a :- as) ([Int] :- bs) where
   dimRanges (a :- as) = dimRange a :- dimRanges as

class CrossList a b | a -> b, b -> a where
   crossListT :: a -> b

instance CrossList () [()] where
   crossListT () = [()]

instance CrossList as bs => CrossList ([a] :- [as]) [a :- bs] where
   crossListT (a :- as) = concatMap (\xs -> map (:- xs) a) $ crossListT as

--dimRanges = map dimRange
dimRangesD = map dimRangeD
--dimNames = map dimName

indicesA a = indices $ dims a
indices :: (DimRanges a b, CrossList b c) => a -> c
indices ds = crossListT $ dimRanges ds
indicesD ds = crossList $ dimRangesD ds

toAssocs :: SubArray dimList e dimName -> [([Int], e)]
toAssocs a = map (\i -> (i, getElem i a)) $ indicesA a

toAssocsD a = map (\i -> (zipWith elemName i d, getElem i a)) $ indicesA a where d = dims a

elemName i (DimInt dl du dm) = toDyn i
elemName i (DimMap dl du dm dm1 dm2) = dm1 M.! i

toDim is = let
   mi = minimum is
   ma = maximum is
   le = ma - mi + 1

   in (mi, ma, le)

fromAssocsA :: forall is dimList e ns dimName. (DimMappings is dimList, CreateDims is dimList) => (e -> e -> e) -> e -> ns -> [(is, e)] -> SubArray dimList e dimName
fromAssocsA f zeroel dnames as = let
   (dims, len) = creates dnames (map fst as)

   in if null as
         then error "no assocs in fromAssocsDA, can't figure out dimension information"
         else SubArray dims 0 $ A.accumArray f zeroel (0, len - 1) $ (map (, zeroel) [0..len-1]++) $ map (\(i, e) -> (elemOffset i dims, e)) as

checkRectilinear as = let
   lens = map length as
   mi = minimum lens
   ma = maximum lens

   in if mi /= ma then error "not rectilinear, varying number of dimensions in assocs" else as

{-
toDimD is = if
   | all isInt is -> toDimII $ map (`fromDyn` int0) is
   | otherwise    -> toDimIS1 is
-}
toDimD is = toDimIS1 is

toDim2 name min max mul []   = DimInt name min max mul
toDim2 name min max mul vals = DimMap name min max mul (M.fromList $ zip [0 ..] vals) (M.fromList $ zip vals [0 ..])

isInt d = dynTypeRep d == intR

int0 = 0 :: Int
intD = toDyn int0
intR = dynTypeRep intD

toDimII is = let
   mi = minimum is
   ma = maximum is
   le = ma - mi + 1

   in (mi, ma, le, [])

toDimIS1 is = let
   se = S.fromList is
   le = S.size se

   in (0, le - 1, le, S.toList se)

toDimIS2 is = let
   mi = minimum is
   mx = maximum is
   se = [mi .. mx]
   le = length se

   in (0, le, le, se)

-- fromList1 es = SubArray [DimInt 0 l 1] 0 (A.array (0, l) (zip [0..] es)) where l = length es - 1
fromList1 es = SubArray [DimInt "" 0 (l - 1) 1] 0 $ A.listArray (0, l - 1) es where l = length es
fromList2z zero ess = let
   d0 = length ess
   d1 = maximum $ map length ess

   in SubArray (DimInt 0 (d0 - 1) d1 :- DimInt 0 (d1 - 1) 1 :- ()) 0 $ A.listArray (0, d0 * d1 - 1) $ concatMap (padRWith1 zero d1) ess

fromList2 = fromList2z undefined

fromList3 zero esss = let
   d0 = length esss
   d1 = maximum $ map length esss
   d2 = maximum $ map (maximum . map length) esss

   in SubArray (DimInt 0 (d0 - 1) (d1 * d2) :- DimInt 0 (d1 - 1) d2 :- DimInt 0 (d2 - 1) 1 :- ()) 0 $ A.listArray (0, d0 * d1 * d2 - 1) $ concatMap (padRWith1 zero (d1 * d2) . concatMap (padRWith1 zero d2)) esss

toList1 a = map (`getElem` a) $ indicesA a

toList2 a = map (\x -> map (\y -> getElem [x, y] a) dy) dx where [dx, dy] = dimRanges $ dims a

toList3 a = map (\x -> map (\y -> map (\z -> getElem [x, y, z] a) dz) dy) dx where [dx, dy, dz] = dimRanges $ dims a

fromArrayList as = fromAssocs $ concat $ zipWith (\inew a -> map (\(i, e) -> (inew : i, e)) $ toAssocs a) [0 ..] as

fromArrayList2 dl du as = let
   miny = map minimum $ Data.List.transpose $ map (map dimLower . dims) as
   maxy = map maximum $ Data.List.transpose $ map (map dimUpper . dims) as
   leny1 = zipWith (\min max -> max - min + 1) miny maxy
   muly = map product $ tails leny1
   dimy = zipWith4 DimInt (repeat "") miny maxy $ tail muly
   leny = head muly

   in SubArray
         (DimInt "" dl du leny : dimy) 0 (A.array
            (0, length as - 1)
            (zipWith (\x (y, e) -> (x * leny + elemOffset y dimy, e)) [0 ..] $ concatMap toAssocs as))

fromAOA a = fromAssocs $ concatMap (\(i1, e1) -> map (\(i2, e2) -> (i1 ++ i2, e2)) $ toAssocs e1) $ toAssocs a

{-
toAssocs :: SubArray e -> [([Int], e)]
toAssocs a = map (\i -> (i, getElem i a)) $ indices a
-}
--select indices from = map (from !!) indices
-- if indices and from are the same length then select indices (select (inversePerm indices) from)) == from
--inversePerm indices = map snd $ sort $ zip indices [0 ..]

splitAt2 major minor ls = (unzip $ map (splitAt minor) $ take major ls, unzip $ map (splitAt minor) $ drop major ls)

readUniqueHeader h [] = h
readUniqueHeader h (x : xs) = if S.member x h then h else readUniqueHeader (S.insert x h) xs

readUniqueHeaders [] _ = []
readUniqueHeaders (h : hs) mult = let
   h1 = S.toList $ readUniqueHeader S.empty $ map (\(x : xs) -> x) $ groupN mult h

   in h1 : readUniqueHeaders hs (mult * length h1)

groupN1 n (SubArray ns (DimInt dl du dm : ds) o p)
   | mod l n == 0 = SubArray ns (DimInt 0 (div l n - 1) (dm * n) : DimInt 0 (n - 1) dm : ds) o p
   | otherwise = error ("not divisible into groups of " ++ show n)
      where
         l = du - dl + 1

groupMulti ns a = foldr groupN1 a ns

readHyper9 xls yls a = groupMulti yls $ groupMulti xls a

readUniqueHyper xhn yhn a = let
   (xh1, a1) = splitAt xhn a
   (yh1, a2) = unzip $ map (splitAt yhn) a1
   xh = readUniqueHeaders (reverse $ map (drop yhn) xh1) 1
   yh = readUniqueHeaders (reverse $ transpose yh1) 1
   d = readHyper9 (map length xh) (map length yh) $ fromList1 a2

   in d
-}

readHyper2 xh yh a = concat $ zipWith zip (crossWith (++) xh yh) a

readHyper1 xhn yhn ri re a = let
   (_, xh1, yh, a1) = readQuarters xhn yhn a
   xh = transpose xh1
   d = readHyper2 xh yh $ map2 re a1

   in d

readQuarters xhn yhn a = let
   (w, e) = unzip $ map (splitAt yhn) a
   (nw, sw) = splitAt xhn w
   (ne, se) = splitAt xhn e

   in (nw, ne, sw, se)

showQuarters3 :: ([[String]], [[String]], [[String]]) -> [[String]]
showQuarters3 (xh, yh, a) = let
   nw = replicate (length $ head xh) $ replicate (length $ head yh) ""

   in zipWith (++) (nw ++ yh) (transpose xh ++ transpose a)

showQuarters (xn, yn, xh, yh, a) = let
   xl = length xn
   yl = length yn
   nw = zipWith (\a b -> a ++ [b]) (replicate (xl - 1) (replicate yl "") ++ [yn]) xn

   in --transpose (nw ++ yh)
      --[replicate (length xh) "£"]
      --zipWith3 (\a b c -> a ++ b ++ c) (nw ++ yh) [replicate (length xh) "£"] (transpose xh ++ transpose a)
      --zipWith (++) (nw ++ yh) (transpose (map (["£"]++) xh) ++ transpose a)
      zipWith (++) (nw ++ yh) (transpose xh ++ map ("£":) (transpose a))

--transpose $ map concat $ transpose [a, b] == zipWith (++) a b


showHyper2 f xn yn a = let
   xd = select xn $ dims a
   yd = select yn $ dims a
   xs = indices xd
   ys = indices yd
   d  = xn ++ yn
   ip = inversePerm d

   in (
         map (show . dimName) xd,
         map (show . dimName) yd,
         map (zipWith showLabel xd) xs,
         map (zipWith showLabel yd) ys,
         crossWith (\x y -> f $ getElem (select ip $ x ++ y) a) xs ys
      )

showHyper1 xn yn a = showGrid $ showQuarters $ showHyper2 show xn yn a

showHyper a = let
   n = length $ dims a
   hn = div n 2

   in showGrid $ showQuarters $ showHyper2 show [hn .. n - 1] [0 .. hn - 1] a
   --showGrid $ showQuarters $ showHyper2 [hn .. n - 1] [0 .. hn - 1] a

stringHyper a = let
   n = length $ dims a
   hn = div n 2

   in showGrid $ showQuarters $ showHyper2 id [hn .. n - 1] [0 .. hn - 1] a

class ShowLabel d where
   showLabel :: d -> Int -> String

instance ShowLabel DimInt where
   showLabel d i = show i

instance ShowLabel (DimMap typ) where
   showLabel (DimMap _ _ _ dm1 _) i = show $ fromJust $ M.lookup i dm1

test d = fromAssocs (map (\x -> "dim"++ show x) [0..d-1]) $ mapxfx id $ indices $ replicate d $ DimInt 0 2 1

-- printa a = putTableF $ arrayToElemList a

{-
instance SubArray (SubArray3 b) (SubArray2 b)  where
   SubArray3 a ! i = SubArray2 $ getSub i a
-}
unionAA f z a1 a2 = fromAssocsDA f z $ toAssocsD a1 ++ toAssocsD a2
unionsAA f z as = fromAssocsDA f z $ concatMap toAssocsD as
joinAAAdd f z dn dv a1 a2 = fromAssocsDA f z $ map (\(i, e) -> (insertAt dn dv i, e)) (toAssocsD a1) ++ toAssocsD a2

ignore = const id

insertAt n v l = let
   (b, a) = splitAt n l

   in b ++ v : a
