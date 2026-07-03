module ArrayDyn where

import Favs
import {-# SOURCE #-} Dyn
import MyPretty2
import Show1

import Data.List
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.Array.IArray as A

data Dimension name dyn
   = DimInt {dimName :: name, dimLower :: Int, dimUpper :: Int, dimMult :: Int}
   | DimMap {dimName :: name, dimLower :: Int, dimUpper :: Int, dimMult :: Int, dimMap1 :: M.Map Int dyn, dimMap2 :: M.Map dyn Int}
--            | DimCat { dimDim::Dimension, dimDiv::[Int], dimMult1::[Int] }
   deriving (Show)

data SubArrayD e dimName dyn = SubArrayD {dims :: [Dimension dimName dyn], offset :: Int, payload :: A.Array Int e}

-- main = print $ Main.transpose 0 1 $ fromList2z 0 [[1,2,3],[4,5,6],[7,8,9]]

-- elemOffset1 i (DimCat d di mu) = sum $ zipWith (*) mu $ refold divMod (elemOffset1 i d) di
elemOffset1 i d = (i - dimLower d) * dimMult d

elemOffset1Dyn i d = case M.lookup i (dimMap2 d) of
   Just j -> elemOffset1 j d
   Nothing -> error (show i ++ " not found in map " ++ show (dimMap2 d))

elemOffset is dims = sum (zipWith elemOffset1 is dims)

elemOffsetDyn is dims = sum (zipWith elemOffset1Dyn is dims)

getElem is a = payload a A.! (offset a + elemOffset is (dims a))

getElemDyn is a = payload a A.! (offset a + elemOffsetDyn is (dims a))

getSubDyn i a =
   let
      (d : ds) = dims a

   in SubArrayD ds (offset a + elemOffset1Dyn i d) (payload a)

getSub i a = let
   (d : ds) = dims a
   
   in SubArrayD ds (offset a + elemOffset1 i d) (payload a)

getSubs is a = SubArrayD (drop (length is) $ dims a) (offset a + elemOffset is (dims a)) (payload a)

getSubDim dn i a = SubArrayD (dbefore ++ dafter) (offset a + (i - dimLower d) * dimMult d) (payload a)
   where
      (dbefore, d : dafter) = splitAt dn $ dims a

getSubDimDyn dn i a = SubArrayD (dbefore ++ dafter) (offset a + ((dimMap2 d M.! i) - dimLower d) * dimMult d) (payload a)
   where
      (dbefore, d : dafter) = splitAt dn $ dims a

getSubDims ds a = foldr (uncurry getSubDim) a ds

getSubDimsDyn ds a = foldr (uncurry getSubDimDyn) a ds

copy a = fromAssocs $ toAssocs a

zipWitha f a b = fromAssocs $ zipWith (\i j -> ([i], f (getSub i a) (getSub j b))) (dimRange $ head $ dims a) (dimRange $ head $ dims b)

mapEE f a = a { payload = A.amap f $ payload a }

mapEA f a = let
   assocs1 = toAssocs a
   (i, e) = head assocs1
   example = f e
   
   in fromAssocs (dimNames (dims a) ++ dimNames (dims example)) $ concatMap (\(i1, e1) -> map (\(i2, e2) -> (i1 ++ i2, e2)) $ toAssocs $ f e1) assocs1

mapAE f s a = let
   indices1 = indices $ select s $ dims a
   assocs1 = map (\i -> (i, f $ getSubDims (zip s i) a)) indices1
   
   in fromAssocs (dimNames $ dims a) assocs1

mapAA f t a = let
   s = [0 .. length (dims a) - 1] \\ sort t
   ist = inversePerm $ s ++ t
   indices1 = indicesD $ select s $ dims a
   example = f $ getSubDimsDyn (zip s (head indices1)) a
   assocs1 = concatMap (\i1 -> map (\(i2, e2) -> (select ist $ i1 ++ i2, e2)) $ toAssocsD $ f $ getSubDimsDyn (zip s i1) a) indices1
   
   in fromAssocsD (select ist $ dimNames (dims a) ++ dimNames (dims example)) assocs1

foldE f a = f $ map snd $ toAssocs a

appendFold :: (Ord dyn, Dyn dyn, Show dyn) => ([dyn], [e1] -> e1) -> SubArrayD e1 name dyn -> [([dyn], e2)] -> SubArrayD e2 ([dyn], e1) dyn
appendFold (n, f) a = let
   l = toAssocsD a
   s = f $ map snd l
   
   in fromAssocsD $ l ++ [(n, s)]

asum a = appendFold ([toDyn "Total"], sum) a

admean a = appendFold ([toDyn "Mean"], dmean) a

mafs f ts a = foldr (mapAA f . singleton) a ts

mapDim f (DimInt dn dl du dm) = DimMap dn dl du dm (M.fromList dm1) (M.fromList $ map tflip dm1) where dm1 = mapxfx f [dl .. du]

zipDims fs (SubArrayD d o p) = SubArrayD (zipWith mapDim fs d) o p

reverseA a = let
   (DimInt dn dl du dm : ds) = dims a
   
   in SubArrayD (DimInt dn dl du (-dm) : ds) (offset a + (du - dl + 1) * dm) (payload a)

reverseDim d a = let
   ds = dims a
   (DimInt dn dl du dm) = ds !! d
   
   in SubArrayD (replaceIndex d (DimInt dn dl du (-dm)) ds) (offset a + (du - dl + 1) * dm) (payload a)

shiftDim d s a = let
   ds = dims a
   (DimInt dn dl du dm) = ds !! d
   
   in SubArrayD (replaceIndex d (DimInt dn (dl - s) (du - s) dm) ds) (offset a + s * dm) (payload a)

sliceDim d s t a = let
   ds = dims a
   (DimInt dn dl du dm) = ds !! d
   
   in SubArrayD (replaceIndex d (DimInt dn (dl - s) (t - s + 1) dm) ds) (offset a + s * dm) (payload a)

transposeA d1 d2 a = let
   ds = dims a
   e1 = ds !! d1
   e2 = ds !! d2
   
   in SubArrayD (replaceIndex d2 e1 $ replaceIndex d1 e2 ds) (offset a) (payload a)

{-
toAssocs :: SubArrayD e -> [(Int, e)]
toAssocs a = let
   (d:ds) = dims a
   in map (\i -> (i, getElem [i] a)) [dimLower d..dimUpper d]
-}
-- cartesian [1,2,3] [[4],[5],[6]] = [[1,4],[2,4],[3,4],[1,5],[2,5],[3,5],[1,6],[2,6],[3,6]]
dimRange (DimInt dn dl du _) = [dl .. du]
dimRange (DimMap dn dl du _ _ _) = [dl .. du]
dimRangeD (DimMap dn dl du _ dm1 _) = map (dm1 M.!) [dl .. du]
dimRanges = map dimRange
dimRangesD = map dimRangeD
dimNames = map dimName

indicesA a = indices $ dims a
indices ds = crossList $ dimRanges ds
indicesD ds = crossList $ dimRangesD ds

toAssocs :: SubArrayD e dimName dyn -> [([Int], e)]
toAssocs a = map (\i -> (i, getElem i a)) $ indicesA a

toAssocsD a = map (\i -> (zipWith elemName i d, getElem i a)) $ indicesA a where d = dims a

elemName i (DimInt dn dl du dm) = toDyn i
elemName i (DimMap dn dl du dm dm1 dm2) = dm1 M.! i

fromAssocs :: [dimName] -> [([Int], e)] -> SubArrayD e dimName dyn
fromAssocs ns as = let
   (mins, maxs, lens) = unzip3 $ map toDim $ transpose $ checkRectilinear $ map fst as
   (len : muls) = scanr (*) 1 lens
   dims = zipWith4 DimInt ns mins maxs muls
   
   in SubArrayD dims 0 $ A.array (0, len - 1) $ map (\(i, e) -> (elemOffset i dims, e)) as

toDim is = let
   mi = minimum is
   ma = maximum is
   le = ma - mi + 1
   
   in (mi, ma, le)

fromAssocsD :: (Ord dyn, Dyn dyn, Show dyn) => [dimName] -> [([dyn], e)] -> SubArrayD e dimName dyn
fromAssocsD dnames as = let
   (mins, maxs, lens, vals) = unzip4 $ map toDimD $ transpose $ checkRectilinear $ map fst as
   (len : muls) = scanr (*) 1 lens
   dims = zipWith5 toDim2 dnames mins maxs muls vals
   
   in SubArrayD dims 0 $ A.array (0, len - 1) $ map (\(i, e) -> (elemOffsetDyn i dims, e)) as

fromAssocsDA f zeroel dnames as = let
   (mins, maxs, lens, vals) = unzip4 $ map toDimD $ transpose $ checkRectilinear $ map fst as
   (len : muls) = scanr (*) 1 lens
   dims = zipWith5 toDim2 dnames mins maxs muls vals
   
   in if null as
         then error "no assocs in fromAssocsDA, can't figure out dimension information"
         else SubArrayD dims 0 $ A.accumArray f zeroel (0, len - 1) $ (zip [0..len-1] (repeat zeroel) ++) $ map (\(i, e) -> (elemOffsetDyn i dims, e)) as

checkRectilinear as = let
   lens = map length as
   mi = minimum lens
   ma = maximum lens
   
   in if mi /= ma then error "not rectilinear, varying number of dimensions in assocs" else as

{-
toDimD is = if
   | all isInt is -> toDimII $ map (`fromDyn` int0) is
   | otherwise    -> toDimIS1 is

isInt d = dynTypeRep d == intR

int0 = 0 :: Int
intD = toDyn int0
intR = dynTypeRep intD
-}
toDimD is = toDimIS1 is

toDim2 name min max mul []   = DimInt name min max mul
toDim2 name min max mul vals = DimMap name min max mul (M.fromList $ zip [0 ..] vals) (M.fromList $ zip vals [0 ..])

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

-- fromList1 es = SubArrayD [DimInt 0 l 1] 0 (A.array (0, l) (zip [0..] es)) where l = length es - 1
fromList1 es = SubArrayD [DimInt "" 0 (l - 1) 1] 0 $ A.listArray (0, l - 1) es where l = length es
fromList2z zero ess = let
   d0 = length ess
   d1 = maximum $ map length ess
   
   in SubArrayD [DimInt "" 0 (d0 - 1) d1, DimInt "" 0 (d1 - 1) 1] 0 $ A.listArray (0, d0 * d1 - 1) $ concatMap (padRWith1 zero d1) ess

fromList2 = fromList2z undefined

fromList3 zero esss = let
   d0 = length esss
   d1 = maximum $ map length esss
   d2 = maximum $ map (maximum . map length) esss
   
   in SubArrayD [DimInt "" 0 (d0 - 1) (d1 * d2), DimInt "" 0 (d1 - 1) d2, DimInt "" 0 (d2 - 1) 1] 0 $ A.listArray (0, d0 * d1 * d2 - 1) $ concatMap (padRWith1 zero (d1 * d2) . concatMap (padRWith1 zero d2)) esss

toList1 a = map (`getElem` a) $ indicesA a

toList2 a = map (\x -> map (\y -> getElem [x, y] a) dy) dx where [dx, dy] = dimRanges $ dims a

toList3 a = map (\x -> map (\y -> map (\z -> getElem [x, y, z] a) dz) dy) dx where [dx, dy, dz] = dimRanges $ dims a

fromArrayList as = fromAssocs $ concat $ zipWith (\inew a -> map (\(i, e) -> (inew : i, e)) $ toAssocs a) [0 ..] as

fromArrayList2 dl du as = let
   miny = map minimum $ Data.List.transpose $ map (map dimLower . dims) as
   maxy = map maximum $ Data.List.transpose $ map (map dimUpper . dims) as
   leny1 = zipWith (\min max -> max - min + 1) miny maxy
   (leny:muly) = map product $ tails leny1
   dimy = zipWith4 DimInt (repeat "") miny maxy muly
   
   in SubArrayD
         (DimInt "" dl du leny : dimy) 0 (A.array 
            (0, length as - 1)
            (zipWith (\x (y, e) -> (x * leny + elemOffset y dimy, e)) [0 ..] $ concatMap toAssocs as))

fromAOA a = fromAssocs $ concatMap (\(i1, e1) -> map (\(i2, e2) -> (i1 ++ i2, e2)) $ toAssocs e1) $ toAssocs a
{-}
newtype SubArray1 b dimName = SubArray1 (SubArrayD b dimName)
newtype SubArray2 b dimName = SubArray2 (SubArrayD b dimName)
newtype SubArray3 b dimName = SubArray3 (SubArrayD b dimName)

class SubArray a e where
   (!) :: a -> Int -> e
   (?) :: a -> Dynamic -> e

instance SubArray (SubArray1 b dimName) b where
   SubArray1 a ! i = getElem [i] a
   SubArray1 a ? i = getElem [] $ getSubDyn i a

instance SubArray (SubArray2 b dimName) (SubArray1 b dimName) where
   SubArray2 a ! i = SubArray1 $ getSub i a
   SubArray2 a ? i = SubArray1 $ getSubDyn i a
-}
instance (Show dimName, Show e, Show1 dyn) => Show (SubArrayD e dimName dyn) where
   show = showHyper

{-
toAssocs :: SubArrayD e -> [([Int], e)]
toAssocs a = map (\i -> (i, getElem i a)) $ indices a
-}
select indices from = map (from !!) indices

-- if indices and from are the same length then select indices (select (inversePerm indices) from)) == from
inversePerm indices = map snd $ sort $ zip indices [0 ..]

splitAt2 major minor ls = (unzip $ map (splitAt minor) $ take major ls, unzip $ map (splitAt minor) $ drop major ls)

readUniqueHeader h [] = h
readUniqueHeader h (x : xs) = if S.member x h then h else readUniqueHeader (S.insert x h) xs

readUniqueHeaders [] _ = []
readUniqueHeaders (h : hs) mult = let
   h1 = S.toList $ readUniqueHeader S.empty $ map (\(x : xs) -> x) $ groupN mult h
   
   in h1 : readUniqueHeaders hs (mult * length h1)

groupN1 n (SubArrayD (DimInt dn dl du dm : ds) o p)
   | mod l n == 0 = SubArrayD (DimInt dn 0 (div l n - 1) (dm * n) : DimInt (dn ++ "1") 0 (n - 1) dm : ds) o p
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
         crossWith (\x y -> show $ getElem (select ip $ x ++ y) a) xs ys
      )

showHyper1 :: forall a name dyn. (Show name, Show a, Show1 dyn) => [Int] -> [Int] -> SubArrayD a name dyn -> String
showHyper1 xn yn a = showGrid $ showQuarters $ showHyper2 (show :: a -> String) xn yn a

showHyper :: forall a name dyn. (Show name, Show a, Show1 dyn) => SubArrayD a name dyn -> String
showHyper a = let
   n = length $ dims a
   hn = div n 2
   
   in showGrid $ showQuarters $ showHyper2 (show :: a -> String) [hn .. n - 1] [0 .. hn - 1] a
   --showGrid $ showQuarters $ showHyper2 [hn .. n - 1] [0 .. hn - 1] a

stringHyper a = let
   n = length $ dims a
   hn = div n 2
   
   in showGrid $ showQuarters $ showHyper2 id [hn .. n - 1] [0 .. hn - 1] a

showLabel (DimInt{}) i = show1 i
showLabel (DimMap dn _ _ _ dm1 _) i = show1 $ fromJust $ M.lookup i dm1

test d = fromAssocs (map (\x -> "dim"++ show x) [0..d-1]) $ mapxfx id $ indices $ replicate d $ DimInt "" 0 2 1

{-
instance SubArray (SubArray3 b) (SubArray2 b)  where
   SubArray3 a ! i = SubArray2 $ getSub i a
-}
unionAA f z a1 a2 = fromAssocsDA f z $ toAssocsD a1 ++ toAssocsD a2
unionsAA f z as = fromAssocsDA f z $ concatMap toAssocsD as
joinAAAdd f z dn dv a1 a2 = fromAssocsDA f z $ map (\(i, e) -> (insertAt dn dv i, e)) (toAssocsD a1) ++ toAssocsD a2

ignore = const  id

insertAt n v l = let
   (b, a) = splitAt n l
   
   in b ++ v : a
