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
import GHC.TypeLits
import GHC.TypeNats
import GHC.LanguageExtensions (Extension(DisambiguateRecordFields))

infixr 0 :-
data a :- b = (:-) { fstT::a, sndT::b } deriving Show

data Nil = Nil deriving Show

data DimInt = DimInt { diLower::Int, diUpper::Int, diMult::Int } deriving (Eq, Ord, Show)
data DimMap i = DimMap { dmLower::Int, dmUpper::Int, dmMult::Int, dmMap1::M.Map Int i, dmMap2::M.Map i Int } deriving (Eq, Ord, Show)

data SubArray dimList e = SubArray { dims::dimList, offset::Int, payload::Array Int e }

getElem (SubArray Nil off pay) = pay A.! off

getSub i (SubArray (d :- dl) off pay) = SubArray dl (off+(i-diLower d)*diMult d) pay

getSubMap i (SubArray (d :- dl) off pay) = SubArray dl (off+((fromJust $ M.lookup i $ dmMap2 d)-dmLower d)*dmMult d) pay

fromElems es = SubArray (DimInt 0 (l-1) 1 :- Nil) 0 $ A.listArray (0, l-1) es where l = length es

--fromSubs ss 
--elemOffset :: (TupleOf i Int, TupleOf d DimInt) => i -> d -> Int
elemOffset :: (FoldRT FPlus Int mr, MapT FMultDim zr mr, ZipT i d zr) => i -> d -> Int
elemOffset i dims = foldRT FPlus (0::Int) $ mapT FMultDim $ zipT i dims

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

assocsFromList1 es = zipWith (\x e -> (x :- Nil, e)) [0..] es
assocsFromList2 ess = concat $ zipWith (\x es -> zipWith (\y e -> (x :- y :- Nil, e)) [0..] es) [0..] ess
assocsFromList3 esss = concat $ zipWith (\x ess -> concat $ zipWith (\y es -> zipWith (\z e -> (x :- y :- z :- Nil, e)) [0..] es) [0..] ess) [0..] esss

fromList1a = fromAssocs . assocsFromList1
fromList2a = fromAssocs . assocsFromList2
fromList3a = fromAssocs . assocsFromList3

fromList2 zero ess = let
   d0 = length ess
   d1 = maximum $ map length ess

   in SubArray (DimInt 0 (d0-1) d1 :- DimInt 0 (d1-1) 1 :- Nil) 0 $ A.listArray (0, d0 * d1 - 1) $ concatMap (padRWith1 zero d1) ess

fromList3 :: e -> [[[e]]] -> SubArray (DimInt :- (DimInt :- (DimInt :- Nil))) e
fromList3 zero esss = let
   d0 = length esss
   d1 = maximum $ map length esss
   d2 = maximum $ map (maximum . map length) esss

   in SubArray (DimInt 0 (d0-1) (d1*d2) :- DimInt 0 (d1-1) d2 :- DimInt 0 (d2-1) 1 :- Nil) 0 $ A.listArray (0, d0 * d1 * d2 - 1) $ concatMap (padRWith1 zero (d1 * d2) . concatMap (padRWith1 zero d2)) esss

class AssocsFromList d e a | d e -> a where
   assocsFromList :: d -> e -> a

instance AssocsFromList Z e [(Nil, e)] where
   assocsFromList Z e = [(Nil, e)]

instance AssocsFromList d es [(i, e)] => AssocsFromList (S d) [es] [(Int :- i, e)] where
   assocsFromList (S d) es = concat $ zipWith (\i e -> map (\(i2, e2) -> (i :- i2, e2)) $ assocsFromList d e) [0..] es

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

toList2 a@(SubArray (d0 :- d1 :- e) o p) = map (\x -> map (\y -> getElemDyn [x, y] a) [diLower d0..diUpper d0]) [diLower d1..diUpper d1]

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
mapDims df a = SubArray (applyT df $ dims a) (offset a) (payload a)

class DimList dimList
instance DimList Nil
instance DimList dl => DimList (DimInt :- dl)
instance DimList dl => DimList (DimMap i :- dl)

--instance (Show1 e, MapT FShowDim dl a, MapT FGetDim dl b, MapT FShowDim dl c, FoldRT FCons [([Int], M.Map Int [Char])] a, FoldRT FCons [(Int, Int, Int)] b, FoldRT FCons [([Int], M.Map Int String)] c) => Show (SubArray dl e) where
instance (Show1 e, MapT FShowDim dl a, MapT FGetDim dl b, FoldRT FCons [([Int], M.Map Int [Char])] a, FoldRT FCons [(Int, Int, Int)] b) => Show (SubArray dl e) where
   show = showHyper

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

class SubArrayC a i e where
   (!) :: a -> i -> e

instance SubArrayC (SubArray (DimInt :- Nil) e) Int e where
   a ! i = getElem $ getSub i a

instance SubArrayC (SubArray (DimInt :- dl) e) Int (SubArray dl e) where
   a ! i = getSub i a

instance Ord i => SubArrayC (SubArray (DimMap i :- Nil) e) i e where
   a ! i = getElem $ getSubMap i a

instance Ord i => SubArrayC (SubArray (DimMap i :- dl) e) i (SubArray dl e) where
   a ! i = getSubMap i a

class Tuple t

instance Tuple Nil

instance Tuple b => Tuple (a :- b)

class Tuple t => TupleOf t o

instance TupleOf Nil o

instance TupleOf t o => TupleOf (o :- t) o

--class Tuple loft => Unzip loft tofl | loft -> tofl where
class Unzip loft tofl | loft -> tofl where
   unzipT :: [loft] -> tofl

instance Unzip Nil Nil where
   unzipT a = Nil

instance Unzip b c => Unzip (a :- b) ([a] :- c) where
   unzipT loft = map fstT loft :- unzipT (map sndT loft)

class Zip3T a b c d | a b c -> d where
   zip3T :: a -> b -> c -> d

instance Zip3T Nil Nil Nil Nil where
   zip3T Nil Nil Nil = Nil

instance Zip3T as bs cs ds => Zip3T (a :- as) (b :- bs) (c :- cs) ((a, b, c) :- ds) where
   zip3T (a :- as) (b :- bs) (c :- cs) = (a, b, c) :- zip3T as bs cs

class ZipT a b c | a b -> c where
   zipT :: a -> b -> c

instance ZipT Nil Nil Nil where
   zipT Nil Nil = Nil

instance ZipT as bs cs => ZipT (a :- as) (b :- bs) ((a, b) :- cs) where
   zipT (a :- as) (b :- bs) = (a, b) :- zipT as bs

class F f a b | f a -> b where
   apply1 :: f -> a -> b

data FMax     = FMax
data FMin     = FMin
data FRange   = FRange
data FDim     = FDim
data FMultDim = FMultDim
data FPlus    = FPlus
data FPlus1   = FPlus1
data FList    = FList
data FCons    = FCons
data FShowDim = FShowDim
data FShowDimS= FShowDimS
data FGetDim  = FGetDim


instance Ord a => F FMax [a] a where
   apply1 f = maximum

instance Ord a => F FMin [a] a where
   apply1 f = minimum

instance (Num a, Ord a) => F FRange [a] a where
   apply1 f xs = maximum xs - minimum xs + 1

instance F FDim (Int, Int, Int) DimInt where
   apply1 f (a, b, c) = DimInt a b c

instance F FMultDim (Int, DimInt) Int where
   apply1 f (a, DimInt _ _ b) = a * b

instance Num a => F FPlus1 a a where
   apply1 f = (+1)

instance F FList a [a] where
   apply1 f = singleton

instance F FShowDim DimInt ([Int], M.Map Int String) where
   apply1 f = showDimInt

instance Show i => F FShowDim (DimMap i) ([Int], M.Map Int String) where
   apply1 f = showDimMap

instance F FShowDimS DimInt ([Int], M.Map Int String) where
   apply1 f = showDimInt

instance Show i => F FShowDimS (DimMap i) ([Int], M.Map Int String) where
   apply1 f = showDimMapS

instance F FGetDim DimInt (Int, Int, Int) where
   apply1 f = getDimInt

instance F FGetDim (DimMap i) (Int, Int, Int) where
   apply1 f = getDimMap

class MapT f a b | f a -> b where
   mapT :: f -> a -> b

instance MapT f Nil Nil where
   mapT _ Nil = Nil

instance (F f a b, MapT f as bs) => MapT f (a :- as) (b :- bs) where
   mapT f (a :- as) = apply1 f a :- mapT f as

class F2 f a b c | f a -> b c, f b -> a c where
   apply2 :: f -> a -> b -> c

data FMult = FMult

instance Num a => F2 FPlus a a a where
   apply2 f = (+)

instance Num a => F2 FMult a a a where
   apply2 f = (*)

instance F2 FCons a [a] [a] where
   apply2 f = (:)

class FoldRT f b as where
   foldRT :: f -> b -> as -> b

instance FoldRT f z Nil where
   foldRT f z Nil = z

instance (F2 f a b b, FoldRT f b as) => FoldRT f b (a :- as) where
   foldRT f z (a :- as) = apply2 f a b where b = foldRT f z as

class ScanRT f z as bs | f z as -> bs where
   scanRT :: f -> z -> as -> bs

instance ScanRT f z Nil (z :- Nil) where
   scanRT f z Nil = z :- Nil

instance (F2 f a b c, ScanRT f z as (b :- bs)) => ScanRT f z (a :- as) (c :- (b :- bs)) where
   scanRT f z (a :- as) = apply2 f a b :- b :- bs where b :- bs = scanRT f z as

class ApplyT fs xs fxs | fs -> xs fxs, xs fxs -> fs where
   applyT :: fs -> xs -> fxs

instance ApplyT Nil Nil Nil where
   applyT Nil Nil = Nil

instance ApplyT g h i => ApplyT ((a1 -> a2) :- g) (a1 :- h) (a2 :- i) where
   applyT (f :- g) (a1 :- h) = f a1 :- applyT g h

input1 = (1::Int) :- (2::Double) :- (3::Float) :- Nil

input2 = (1::Int) :- (2::Int) :- (3::Int) :- Nil

input3 = [((1::Int) :- (2::Int) :- (3::Int) :- Nil, 123::Int), (4 :- 5 :- 6 :- Nil, 456)]

test1 = mapT FPlus1 input1

test2 = mapT FList input1

k f (a, b) = (f a, f b)

newtype S a = S a
data Z = Z

n0 = Z
n1 = S Z
n2 = S (S Z)
n3 = S (S (S Z))

class SplitAtT i t a b where
   splitAtT :: i -> t -> (a, b)
   --takeT    :: i -> t -> a
   --dropT    :: i -> t -> b


instance SplitAtT Z a Nil a where
   splitAtT Z a = (Nil, a)
   --takeT    Z a = Nil
   --dropT    Z a = a

instance SplitAtT i b b1 c => SplitAtT (S i) (a :- b) (a :- b1) c where
   splitAtT (S i) (a :- b) = (a :- b1, c) where (b1, c) = splitAtT i b
   --takeT    (S i) (a :- b) = a :- b1 where b1 = takeT i b
   --atT      (S i) (a :- b) = atT i b

class GetT i t e where
   getT :: i -> t -> e

instance GetT Z (a :- b) a where
   getT Z (a :- b) = a

instance GetT i b c => GetT (S i) (a :- b) c where
   getT (S i) (a :- b) = getT i b

--takeT i t = a where (a, b) = splitAtT i t

--atT i t = b where (_, b :- c) = splitAtT i t

{-
data N (n :: Natural) = N

n0 = N :: N 0
n1 = N :: N 1
n2 = N :: N 2
n3 = N :: N 3
n4 = N :: N 4
n5 = N :: N 5
n6 = N :: N 6
n7 = N :: N 7
n8 = N :: N 8
n9 = N :: N 9

class NiceTuple t i e where
   gett :: t -> i -> e

instance NiceTuple t (N 0) t where
   gett t i = t

instance NiceTuple (a :- b) (N 0) a where
   gett (a :- b) i = a

instance NiceTuple b (N i) e => NiceTuple (a :- b) (N (i+1)) e where
-}
