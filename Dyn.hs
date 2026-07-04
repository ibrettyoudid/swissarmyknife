{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyn (
   module Dyn,
   Dynamic,
   SomeTypeRep,
   Typeable,
   typeOf,
   typeRepTyCon
) where

import Favs
import ArrayDyn
import NewTuple hiding (delete)
import NumberParsers as NP

import qualified Data.Dynamic as D
import Data.Dynamic (Typeable, Dynamic)
import Data.Typeable
import Type.Reflection hiding (TypeRep, typeOf, typeRepTyCon)

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import Data.IORef

import GHC.Stack

class Dyn dyn where
   toDyn   :: Typeable a => a -> dyn
   toDyn   = dtoDyn . D.toDyn
   
   fromDynamic :: Typeable a => dyn -> Maybe a
   fromDynamic = D.fromDynamic . dfromDyn

   dtoDyn   :: Dynamic -> dyn
   dfromDyn :: dyn -> Dynamic

   showDyn  :: dyn -> String

instance Dyn Dynamic where
   dtoDyn = id
   dfromDyn = id

   showDyn = show

fromDyn d e = fromMaybe e $ fromDynamic d

fromDyn1 :: (Dyn dyn, Typeable a) => dyn -> a
fromDyn1 d = case fromDynamic d of
   Just j -> j
   n@Nothing -> error ("fromDyn1: expected " ++ show (typeOf n) ++ " got " ++ show (dynTypeRep d))

fromDyn2 :: (Dyn dyn, Typeable a, HasCallStack) => String -> dyn -> a
fromDyn2 e d = case fromDynamic d of
   Just j -> j
   Nothing -> error $ e ++ ", is " ++ showDyn d

dynApply :: Dyn dyn => dyn -> dyn -> Maybe dyn
dynApply f x = dtoDyn <$> D.dynApply (dfromDyn f) (dfromDyn x)

dynTypeRep :: Dyn dyn => dyn -> SomeTypeRep
dynTypeRep = D.dynTypeRep . dfromDyn

typeOf1 t = typeOf t

data NamedValue dyn = NamedValue {nname :: String, nvalue :: dyn}

instance Dyn dyn => Eq (NamedValue dyn) where
   a == b = nname a == nname b

data MType = MType {mtname :: String, conv :: TypeLink, constr :: Constr}

instance Eq MType where
   a == b = mtname a == mtname b

instance Ord MType where
   compare = compare `on` mtname

instance Show MType where
   show = mtname

data TypeLink = TypeLink {tlparents :: [MType], tllinear :: [MType]} deriving (Eq, Show)

data Constr = Constr {cname :: String, tl :: TypeLink, members :: [Member]} deriving (Show, Eq)

data Member = Member {mtype :: MType, mname :: String} deriving (Show, Eq)

data MMEntry dyn = Method dyn | Types [MType] deriving Show

data Multimethod dyn = Multimethod {mmname :: String, funcs :: M.Map [SomeTypeRep] dyn} deriving (Typeable)

data MultimethodA dyn = MultimethodA {namea :: String, funcsa :: SubArrayD dyn String dyn} deriving (Typeable)

dmean :: (Dyn dyn, Num dyn) => [dyn] -> dyn
dmean xs = toDyn (fromJust (fromDynamic (sum xs)) / fromIntegral (length xs) :: Double)

am m xs = fromJust $ fromDynamic $ right $ applyMultimethod m xs

am2 m = right . applyMultimethod m

am3 m xs = case applyMultimethod m xs of
   Left l -> toDyn (0 :: Int)
   Right r -> r

applyMulti :: Dyn dyn => MMEntry dyn -> [dyn] -> dyn
applyMulti f xs = case f of
   Method m -> applyMulti1 m xs
   Types t -> error "Ambiguous call of multimethod"

applyMulti1 :: Dyn dyn => dyn -> [dyn] -> dyn
applyMulti1 = foldl (fromJust .: dynApply)

zz = toDyn 'a'

convertArg t x = if
   | t == dynTypeRep x -> x
   -- | t == typeOf zz -> toDyn x
   | True -> right $ applyMultimethod2 convertm t [x]

convertArg1 f x = convertArg (head $ args $ dynTypeRep f) x

convertArgs f xs = zipWith convertArg (init $ args $ dynTypeRep f) xs

applyMultimethod :: forall dyn. (Dyn dyn, Typeable dyn) => Multimethod dyn -> [dyn] -> Either String dyn
applyMultimethod (Multimethod n fs) xs = let
   xts = map dynTypeRep xs
   
   in case M.lookup xts fs of
         Nothing -> Left $ "Multimethod " ++ n ++ " does not have an entry to match " ++ if elem n ["show", "convert"] then show xts else showDyn (toDyn xs :: dyn)
         Just f -> Right $ applyMulti1 f $ convertArgs f xs

applyMultimethod2 :: forall dyn. (Dyn dyn, Typeable dyn) => Multimethod dyn -> SomeTypeRep -> [dyn] -> Either String dyn
applyMultimethod2 (Multimethod n fs) returnType xs = let
   xts = map dynTypeRep xs ++ [returnType]
   
   in case M.lookup xts fs of
         Nothing -> Left $ "Multimethod " ++ n ++ " does not have an entry to match " ++ if elem n ["show", "convert"] then show xts else showDyn (toDyn xs :: dyn)
         Just f -> Right $ applyMulti1 f xs

applyMultimethodIO :: forall dyn. (Dyn dyn, Typeable dyn) => Multimethod dyn -> [dyn] -> IO (Either String dyn)
applyMultimethodIO m@(Multimethod n fs) xs = do
   xds <- mapM expandVal xs
   let rs = mapMaybe (\xs -> do f <- M.lookup (map dynTypeRep xs) fs; return $ applyMulti1 f xs) $ crossList xds
   case rs of
      (r : _) -> return $ Right r
      [] -> return $ Left $ "Multimethod " ++ n ++ " does not have an entry to match " ++ if elem n ["show", "convert"] then show $ map2 showDyn xds else showDyn (toDyn xs :: dyn)

applyMultimethodA (MultimethodA n a) xs = let
   xts = map (toDyn . dynTypeRep) xs
   in
   applyMulti1 (getElemDyn xts a) xs

expandVal :: forall dyn. (Typeable dyn, Dyn dyn) => dyn -> IO [dyn]
expandVal d = do
   case fromDynamic d :: Maybe (IORef dyn) of
      Just r -> do x <- readIORef r; y <- expandVal2 x; return (d : x : y)
      Nothing -> return [d]

expandVal2 :: forall dyn. (Typeable dyn, Dyn dyn) => dyn -> IO [dyn]
expandVal2 d = do
   case fromDynamic d :: Maybe (IORef dyn) of
      Just r -> do x <- readIORef r; y <- expandVal2 x; return (x : y)
      Nothing -> return [d]

collectMulti :: (Dyn dyn, Typeable dyn) => [(String, dyn)] -> [(String, dyn)]
collectMulti env = map (\(n, fs) -> (n, toDyn $ createMultimethod n fs)) $ nubMulti env

myTypeOf :: (Typeable a) => a -> TypeRep
myTypeOf a = Data.Typeable.typeOf a

tiRational = htype "Rational" []
tiInteger = htype "Integer" [tiRational]
tiDouble = htype "Double" [tiRational]
tiInt = htype "Int" [tiInteger, tiDouble]
tiFloat = htype "Float" [tiDouble]
tiString = htype "String" [tiDouble]
tiList = htype "List" []
tiChar = htype "Char" [tiString]
tiOrdering = htype "Ordering" []
tiDay = htype "Day" []
tiDOW = htype "DOW" []
tiPerson = htype "Person" []
tiField = htype "Field" []
tiTC = htype "TC" []
tiBool = htype "Bool" []
tiVoid = htype "Void" []
tiDynamic = htype "Dynamic" []
tiMaybe = htype "Maybe Dynamic" []
tiNamedValue = htype "NamedValue" []

unknown = htype "Unknown" []

typeMap :: M.Map TypeRep MType
typeMap = M.fromList typeList

typeMapR :: M.Map MType TypeRep
typeMapR = M.fromList $ map tflip typeList

z = z

typeList :: [(TypeRep, MType)]
typeList =
   [ (myTypeOf (z :: Int), tiInt)
   , (myTypeOf (z :: Integer), tiInteger)
   , (myTypeOf (z :: Float), tiFloat)
   , (myTypeOf (z :: Double), tiDouble)
   , (myTypeOf (z :: Rational), tiRational)
   , (myTypeOf (z :: String), tiString)
   -- , (myTypeOf (z :: [Dynamic]), tiList)
   , (myTypeOf (z :: Ordering), tiOrdering)
   --, (myTypeOf (z :: TC), tiTC)
   -- , (myTypeOf (z :: Day), tiDay)
   -- , (myTypeOf (z :: Person), tiPerson)
   -- , (myTypeOf (z :: DayOfWeek), tiDOW)
   -- , (myTypeOf (z :: Field), tiField)
   , (myTypeOf (z :: Char), tiChar)
   , (myTypeOf (z :: Bool), tiBool)
   , (myTypeOf (z :: ()), tiVoid)
   -- , (myTypeOf (z :: Dynamic), tiDynamic)
   -- , (myTypeOf (z :: Maybe Dynamic), tiMaybe)
   -- , (myTypeOf (z :: NamedValue), tiNamedValue)
   ]

void = Constr "void" (TypeLink [] []) []

htype n p = t where t = MType n (TypeLink p $ c3 conv t) void

c3 m t = t : mergeSuper (map (c3 m) (tlparents tl) ++ [tlparents tl]) where tl = m t

mergeSuper tss = unfoldr mergeSuper1 $ filter (not . null) tss

mergeSuper1 [] = Nothing
mergeSuper1 tss =
   let
      h = goodhead tss
   in
      Just (h, filter (not . null) $ map (delete h) tss)

goodhead tss =
   let
      tails = S.unions $ map (S.fromList . tail) tss
   in
      mapFJE (error "no good heads") (ifPred (`S.notMember` tails)) (map head tss)

createMultimethod :: Dyn dyn => String -> [dyn] -> Multimethod dyn
createMultimethod n fs = Multimethod n $ M.fromList $ mergeMethods fs

createMultimethod1 n fs = Multimethod n $ M.fromList $ mapfxx (init . args . dynTypeRep) fs

createMultimethod2 n fs = Multimethod n $ M.fromList $ mapfxx (args . dynTypeRep) fs

createMultimethodA n fs = MultimethodA n $ fromAssocsDA ignore (toDyn "") (map (\x -> "arg" ++ show x) [0..]) $ map (\(i, e) -> (map toDyn i, e)) fs

args :: TypeRep -> [TypeRep]
args f
   | isFunc f =
         let
         (a : r : _) = typeRepArgs f
         in
         a : args r
   | otherwise = [f]

isFunc :: TypeRep -> Bool
isFunc f = show (typeRepTyCon f) == "FUN"

argCount :: TypeRep -> Int
argCount f = length $ args f

showTypeLink ht = show (tlparents ht) ++ " " ++ show (tllinear ht)

-- showTypeLinks hts = show $ map htname hts

mergeMethods methods = let
   methods2 = mapfxx (map (\t -> fromMaybe (error $ "type " ++ show t ++ " not found in mergeMethods") $ M.lookup t typeMap) . getFType) methods
   types = transpose $ map fst methods2
   inmap = M.map Method $ M.fromList methods2
   outmap = M.fromList $ mapxfx (mergeMethods1 inmap outmap) $ crossList types
   retype = mapMaybe (\(k, v) -> case v of Method m -> Just (map (typeMapR M.!) k, m); Types _ -> Nothing) $ M.toList outmap
   in
   retype

mergeMethodsD methods = let
   methods2 = mapfxx (map (typeMap M.!) . getFType) methods
   types = transpose $ map fst methods2
   inmap = M.map Method $ M.fromList methods2
   comb = crossList types
   (a, b) = unzip $ mapxfx (mergeMethods1D inmap outmap) comb
   (c, d) = unzip b
   outmap = M.fromList $ zip a c
   retype = M.fromList $ mapMaybe (\(k, v) -> case v of Method m -> Just (map (typeMapR M.!) k, m); Types _ -> Nothing) $ M.toList outmap
   in
   outmap M.! [tiInt, tiInt]

mergeMethods1 inmap outmap types = 
   case M.lookup types inmap of
      Just j -> j
      Nothing -> let
         parentMethods = concatMap (checkParents outmap types) [0 .. length types - 1]
         (parentMethods2, parentMethodsTypes) = unzip $ mapMaybe (\x -> (x,) <$> getType x) parentMethods
         bestTypes1 = bestTypes (map conv types) $ transpose parentMethodsTypes
         in
         case elemIndex bestTypes1 parentMethodsTypes of
            Just i -> parentMethods2 !! i
            Nothing -> Types bestTypes1

mergeMethods1D inmap outmap types = let
   methods2 = concatMap (checkParents outmap types) [0 .. length types - 1]
   types2 = mapMaybe getType methods2
   types3 = bestTypes (map conv types) types2
   in
   ( case M.lookup types inmap of
      Just j -> j
      Nothing -> case elemIndex types3 types2 of
         Just i -> methods2 !! i
         Nothing -> Types types3
   , methods2
   )

checkParents methods types paramn = mapMaybe (\p -> M.lookup (replaceIndex paramn p types) methods) $ tlparents $ conv $ types !! paramn

mergeMethodsB methods = let
   methods2 = mapfxx (map (typeMap M.!) . getFType) methods
   types = transpose $ map fst methods2
   inmap = M.map Method $ M.fromList methods2
   outmap = M.fromList $ mapxfx (mergeMethods1B inmap outmap) $ crossList types
   retype = M.fromList $ mapMaybe (\(k, v) -> case v of Method m -> Just (map (typeMapR M.!) k, m); Types _ -> Nothing) $ M.toList outmap
   in
   retype

mergeMethods1B inmap outmap types = let
   methods2 = concatMap (checkParentsB inmap outmap types) [0 .. length types - 1]
   types2 = mapMaybe getType methods2
   types3 = bestTypes (map conv types) types2
   in
   case M.lookup types inmap of
      Just j -> j
      Nothing -> case elemIndex types3 types2 of
         Just i -> methods2 !! i
         Nothing -> Types types3

checkParentsB inmap methods types paramn = let
   t = conv $ types !! paramn
   ps = tlparents t
   rep p = let (b, x : a) = splitAt paramn types in mergeMethods1B inmap methods (b ++ p : a)

   in map rep ps

bestTypes = zipWith bestType

bestType t = (tllinear t !!) . minimum . mapMaybe (`elemIndex` tllinear t)

minimumOn f = minimumBy (compare `on` f)

getType (Method d) = Just $ map (typeMap M.!) $ getFType d
getType (Types t) = Nothing

getFType = init . args . dynTypeRep

cmm m =
   let
      in 0

fromdynm d = mapMaybe (\f -> dynApply f d)
   [toDyn (fromDynamic :: Dynamic -> Maybe Int),
   toDyn (fromDynamic :: Dynamic -> Maybe Char),
   toDyn (fromDynamic :: Dynamic -> Maybe String),
   toDyn (fromDynamic :: Dynamic -> Maybe [Dynamic]),
   toDyn (fromDynamic :: Dynamic -> Maybe (Dynamic, Dynamic)),
   toDyn (fromDynamic :: Dynamic -> Maybe (Dynamic :- Dynamic)),
   toDyn (fromDynamic :: Dynamic -> Maybe Dynamic)]


--readRational s = try $ evaluate $ read s
readNum2 str = case NP.parse NP.floating "convert String -> Double" str of
   Left  l -> 0
   Right r -> r



instance Read Dynamic where
   readsPrec p s0 = px (readsPrec p :: ReadS Int) $ px (readsPrec p :: ReadS Double) $ px (readsPrec p :: ReadS Bool) $ px (readsPrec p :: ReadS String) $ px (readsPrec p :: ReadS Char) [(toDyn s0, "")]
      where
         px p y = let (r, s) = unzip $ p s0 in if not $ null r then zip (map toDyn r) s else y

--instance Show (MMEntry dyn)

--deriving instance Show HTTPTypes.HVar

cei = fromIntegral :: Int      -> Integer
cie = fromIntegral :: Integer  -> Int
ced = round        :: Double   -> Integer
cde = fromIntegral :: Integer  -> Double
cfd = realToFrac   :: Double   -> Float
cdf = realToFrac   :: Float    -> Double
cdr = realToFrac   :: Rational -> Double
crd = realToFrac   :: Double   -> Rational
cre = fromIntegral :: Integer  -> Rational
cer = round        :: Rational -> Integer 
cdi = fromIntegral :: Int      -> Double  
cid = round        :: Double   -> Int     
cds = readNum2     :: String   -> Double  
cri = realToFrac   :: Int      -> Rational

convertm =
   createMultimethod2
      "convert"
         ([ toDyn cie
         , toDyn cei
         , toDyn cde
         , toDyn ced
         , toDyn cdf
         , toDyn cfd
         , toDyn cdr
         , toDyn crd
         , toDyn (cfd . cde)
         , toDyn (ced . cdf)
         , toDyn cre
         , toDyn cer
         , toDyn cdi
         , toDyn cid
         , toDyn cds
         , toDyn cri
         ])

