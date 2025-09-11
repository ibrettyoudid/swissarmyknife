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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MHashDynamic (
  module MHashDynamic,
  Typeable,
  Data.Typeable.typeOf,
  typeRepArgs,
  typeRepTyCon,
  TypeRep,
)
where

import MyPretty2

import Data.Array qualified as A
import Type.Reflection hiding (TypeRep, typeOf, typeRepTyCon)

import Favs

import Numeric

import HTTPTypes qualified

import Data.Dynamic qualified as D
import Data.IORef
import Data.List
import Data.Time.Calendar
import Data.Typeable
import Type.Reflection hiding (TypeRep, typeOf, typeRepTyCon)

import Data.Map.Lazy qualified as M
import Data.Set qualified as S

import Syntax3 hiding (foldl, foldr, print, right)
import Syntax3 qualified as S3
import SyntaxCIPU

data NamedValue = NamedValue {nname :: String, nvalue :: Dynamic}

instance Eq NamedValue where
  a == b = nname a == nname b

data Expr
  = Value {etype :: MType, value :: Dynamic}
  | VarRef {etype :: MType, name :: String, frameIndex :: Int, memIndex :: Int}
  | VarDef {xtype :: Expr, name :: String, frameIndex :: Int, memIndex :: Int}
  | VarRef1 {etype :: MType, name :: String}
  | Lambda {etype :: MType, econstr :: Constr, subexpr :: Expr}
  | Let {etype :: MType, econstr :: Constr, vals :: [Expr], subexpr :: Expr}
  | Block {etype :: MType, econstr :: Constr, subexprs :: [Expr]}
  | Apply {etype :: MType, subexprs :: [Expr]}
  | If {etype :: MType, clauses :: [(Expr, Expr)]}
  | Case {etype :: MType, case1 :: Expr, clauses :: [(Expr, Expr)]}
  | Else
  | Exprs {etype :: MType, exprs1 :: [Expr]}
  deriving (Typeable, Eq, Show)

data Closure = Closure Constr Expr Env deriving (Typeable)

type Env = [Frame]

data Frame = Frame {fconstr :: Constr, items :: [Dynamic]} deriving (Show, Eq)

pattern Co n m = Constr n (TypeLink [] []) m

data Constr = Constr {cname :: String, tl :: TypeLink, members :: [Member]} deriving (Show, Eq)

data Member = Member {mtype :: MType, mname :: String} deriving (Show, Eq)

data PartApply = PartApply [Dynamic] Constr Expr Env deriving (Typeable)

data Person = Tyrone | Danny | James | David deriving (Eq, Ord, Show, Read)

data Field = Borrowed | Profit | Owed deriving (Eq, Ord, Show, Read)

data TC = Trans | Cum deriving (Eq, Ord, Show, Read)

newtype MyDynamic = Dynamic D.Dynamic

type Dynamic = MyDynamic

data MType = MType {mtname :: String, conv :: TypeLink, constr :: Constr}

data TypeLink = TypeLink {tlparents :: [MType], tllinear :: [MType]} deriving (Eq, Show)

instance Show MType where
  show = mtname

instance Eq MType where
  a == b = mtname a == mtname b

instance Ord MType where
  compare = compare `on` mtname

data MMEntry = Method Dynamic | Types [MType]

data Multimethod = Multimethod {mmname :: String, funcs :: M.Map [SomeTypeRep] Dynamic} deriving (Typeable)

data MultimethodA = MultimethodA {namea :: String, funcsa :: SubArrayD Dynamic} deriving (Typeable)

und :: Dynamic -> D.Dynamic
und (Dynamic d) = d

fromDynamic :: (Typeable a) => Dynamic -> Maybe a
fromDynamic d = D.fromDynamic $ und d
dynApply f x = Dynamic <$> D.dynApply (und f) (und x)
dynTypeRep :: Dynamic -> SomeTypeRep
dynTypeRep = D.dynTypeRep . und
typeOf1 t = typeOf t
toDyn :: (Typeable a) => a -> Dynamic
toDyn a = Dynamic $ D.toDyn a
fromDyn :: (Typeable a) => Dynamic -> a -> a
fromDyn a = D.fromDyn $ und a

fromDyn1 :: (Typeable a) => Dynamic -> a
fromDyn1 d = case fromDynamic d of
  Just j -> j
  n@Nothing -> error ("fromDyn1: expected " ++ show (typeOf n) ++ " got " ++ show (und d))

instance Eq Dynamic where
  a == b = am eqm [a, b]

instance Ord Dynamic where
  compare a b = case applyMultimethod comparem [a, b] of
    Right d -> fromDyn d (error "should be an Ordering")
    Left e -> compare (showDyn a) (showDyn b)

instance Num Dynamic where
  a + b = am3 addm [a, b]
  a - b = am3 subm [a, b]
  a * b = am3 mulm [a, b]
  abs a = am3 absm [a]
  signum a = am3 signumm [a]
  fromInteger a = toDyn (fromInteger a :: Double)

instance Fractional Dynamic where
  a / b = am3 divfracm [a, b]

dmean xs = toDyn (fromJust (fromDynamic (sum xs)) / fromIntegral (length xs) :: Double)

am m xs = fromJust $ fromDynamic $ right $ applyMultimethod m xs

am2 m = right . applyMultimethod m

am3 m xs = case applyMultimethod m xs of
  Left l -> toDyn (0 :: Int)
  Right r -> r

applyMulti :: MMEntry -> [Dynamic] -> Dynamic
applyMulti f xs = case f of
  Method m -> applyMulti1 m xs
  Types t -> error "Ambiguous call of multimethod"

applyMulti1 :: Dynamic -> [Dynamic] -> Dynamic
applyMulti1 = foldl (fromJust .: dynApply)

zz = toDyn 'a'

convertArg t x =
  if
    | t == dynTypeRep x -> x
    | t == typeOf zz -> toDyn x
    | True -> right $ applyMultimethod2 convertm t [x]

convertArg1 f x = convertArg (head $ args $ dynTypeRep f) x

convertArgs f xs = zipWith convertArg (init $ args $ dynTypeRep f) xs

applyMultimethod :: Multimethod -> [Dynamic] -> Either String Dynamic
applyMultimethod (Multimethod n fs) xs =
  let
    xts = map dynTypeRep xs
   in
    case M.lookup xts fs of
      Nothing -> Left $ "Multimethod " ++ n ++ " does not have an entry to match " ++ if elem n ["show", "convert"] then show xts else showDyn (toDyn xs)
      Just f -> Right $ applyMulti1 f $ convertArgs f xs

applyMultimethod2 (Multimethod n fs) rt xs =
  let
    xts = map dynTypeRep xs ++ [rt]
   in
    case M.lookup xts fs of
      Nothing -> Left $ "Multimethod " ++ n ++ " does not have an entry to match " ++ if elem n ["show", "convert"] then show xts else showDyn (toDyn xs)
      Just f -> Right $ applyMulti1 f xs

applyMultimethodIO :: Multimethod -> [Dynamic] -> IO (Either String Dynamic)
applyMultimethodIO m@(Multimethod n fs) xs = do
  xds <- mapM expandVal xs
  let rs = mapMaybe (\xs -> do f <- M.lookup (map dynTypeRep xs) fs; return $ applyMulti1 f xs) $ crossList xds
  case rs of
    (r : _) -> return $ Right r
    [] -> return $ Left $ "Multimethod " ++ n ++ " does not have an entry to match " ++ if elem n ["show", "convert"] then show xds else showDyn (toDyn xs)

applyMultimethodA (MultimethodA n a) xs =
  let
    xts = map (toDyn . dynTypeRep) xs
   in
    applyMulti1 (getElemDyn xts a) xs

expandVal d = do
  case fromDynamic d :: Maybe (IORef Dynamic) of
    Just r -> do x <- readIORef r; y <- expandVal2 x; return (d : x : y)
    Nothing -> return [d]

expandVal2 d = do
  case fromDynamic d :: Maybe (IORef Dynamic) of
    Just r -> do x <- readIORef r; y <- expandVal2 x; return (x : y)
    Nothing -> return [d]

collectMulti :: [(String, Dynamic)] -> [(String, Dynamic)]
collectMulti env = map (\(n, fs) -> (n, toDyn $ createMultimethod n fs)) $ nubMulti env

myTypeOf :: (Typeable a) => a -> TypeRep
myTypeOf a = Data.Typeable.typeOf a

tiRational = htype "Rational" []
tiInteger = htype "Integer" [tiRational]
tiDouble = htype "Double" [tiRational]
tiInt = htype "Int" [tiInteger, tiDouble]
tiFloat = htype "Float" [tiDouble]
tiString = htype "String" [tiRational]
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
  , (myTypeOf (z :: [Dynamic]), tiList)
  , (myTypeOf (z :: Ordering), tiOrdering)
  , (myTypeOf (z :: TC), tiTC)
  , (myTypeOf (z :: Day), tiDay)
  , (myTypeOf (z :: Person), tiPerson)
  , (myTypeOf (z :: DayOfWeek), tiDOW)
  , (myTypeOf (z :: Field), tiField)
  , (myTypeOf (z :: Char), tiChar)
  , (myTypeOf (z :: Bool), tiBool)
  , (myTypeOf (z :: ()), tiVoid)
  , (myTypeOf (z :: Dynamic), tiDynamic)
  , (myTypeOf (z :: Maybe Dynamic), tiMaybe)
  , (myTypeOf (z :: NamedValue), tiNamedValue)
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

createMultimethod :: String -> [Dynamic] -> Multimethod
createMultimethod n fs = Multimethod n $ M.fromList $ mergeMethods fs

createMultimethod1 n fs = Multimethod n $ M.fromList $ mapfxx (init . args . dynTypeRep) fs

createMultimethod2 n fs = Multimethod n $ M.fromList $ mapfxx (args . dynTypeRep) fs

createMultimethodA n fs = MultimethodA n $ fromAssocsDA (\_ x -> x) (toDyn "") $ map (\(i, e) -> (map toDyn i, e)) fs

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

mergeMethods methods =
  let
    methods2 = mapfxx (map (\t -> fromMaybe (error $ "type " ++ show t ++ " not found in mergeMethods") $ M.lookup t typeMap) . getFType) methods
    types = transpose $ map fst methods2
    inmap = M.map Method $ M.fromList methods2
    outmap = M.fromList $ mapxfx (mergeMethods1 inmap outmap) $ crossList types
    retype = mapMaybe (\(k, v) -> case v of Method m -> Just (map (typeMapR M.!) k, m); Types _ -> Nothing) $ M.toList outmap
   in
    retype

mergeMethodsD methods =
  let
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
    Nothing ->
      let
        parentMethods = concatMap (checkParents outmap types) [0 .. length types - 1]
        (parentMethods2, parentMethodsTypes) = unzip $ mapMaybe (\x -> (x,) <$> getType x) parentMethods
        bestTypes1 = bestTypes (map conv types) $ transpose parentMethodsTypes
       in
        case elemIndex bestTypes1 parentMethodsTypes of
          Just i -> parentMethods2 !! i
          Nothing -> Types bestTypes1

mergeMethods1D inmap outmap types =
  let
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

mergeMethodsB methods =
  let
    methods2 = mapfxx (map (typeMap M.!) . getFType) methods
    types = transpose $ map fst methods2
    inmap = M.map Method $ M.fromList methods2
    outmap = M.fromList $ mapxfx (mergeMethods1B inmap outmap) $ crossList types
    retype = M.fromList $ mapMaybe (\(k, v) -> case v of Method m -> Just (map (typeMapR M.!) k, m); Types _ -> Nothing) $ M.toList outmap
   in
    retype

mergeMethods1B inmap outmap types =
  let
    methods2 = concatMap (checkParentsB inmap outmap types) [0 .. length types - 1]
    types2 = mapMaybe getType methods2
    types3 = bestTypes (map conv types) types2
   in
    case M.lookup types inmap of
      Just j -> j
      Nothing -> case elemIndex types3 types2 of
        Just i -> methods2 !! i
        Nothing -> Types types3

checkParentsB inmap methods types paramn =
  let
    t = conv $ types !! paramn
    ps = tlparents t
    rep p = let (b, x : a) = splitAt paramn types in mergeMethods1B inmap methods (b ++ p : a)
   in
    map rep ps

{-
mergeMethodsBD methods = do
  print "methods2="
  let methods2 = mapfxx (map (typeMap M.!) . getFType) methods
  print methods2
  let types = transpose $ map fst methods2
  let inmap = M.map Method $ M.fromList methods2
  print "inmap="
  print inmap
  print "done inmap"
  let comb = crossList types
  let (types2, methods3) = unzip $ mapxfx (mergeMethods1BD inmap 0) comb
  methods4 <- sequence methods3
  let retype = M.fromList $ mapMaybe (\(k, v) -> case v of Method m -> Just (map (typeMapR M.!) k, m); Types _ -> Nothing) $ zip types2 methods4
  return retype

rp r = printD "return" r >> return r

printD d r = putStrLn (d ++ "=" ++ show r)

mergeMethods1BD inmap outmap types = do
  printD "types" types
  case M.lookup types inmap of
    Just j -> rp j
    Nothing -> do
      print "calling checkParents"
      methods1 <- mapM (checkParentsBD inmap outmap types) [0 .. length types - 1]
      let methods2 = concat methods1
      printD "methods2" methods2
      let types2 = mapMaybe getType methods2
      printD "types2" types2
      let types3 = bestTypes types $ transpose types2
      printD "types3" types3
      case elemIndex types3 types2 of
        Just i -> rp $ methods2 !! i
        Nothing -> rp $ Types types3

checkParentsBD inmap outmap types paramn = do
  print types
  print paramn
  let rep p = mergeMethods1BD inmap outmap $ replaceIndex paramn p types

  mapM rep $ tlparents $ conv $ types !! paramn
-}
bestTypes = zipWith bestType

bestType t = (tllinear t !!) . minimum . mapMaybe (`elemIndex` tllinear t)

minimumOn f = minimumBy (compare `on` f)

getType (Method d) = Just $ map (typeMap M.!) $ getFType d
getType (Types t) = Nothing

getFType = init . args . dynTypeRep

cmm m =
  let
   in 0

cei = fromIntegral :: Int -> Integer
cie = fromIntegral :: Integer -> Int
ced = round :: Double -> Integer
cde = fromIntegral :: Integer -> Double
cfd = realToFrac :: Double -> Float
cdf = realToFrac :: Float -> Double
cdr = realToFrac :: Rational -> Double
crd = realToFrac :: Double -> Rational

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
    , toDyn (fromIntegral :: Integer -> Rational)
    , toDyn (round :: Rational -> Integer)
    , toDyn (fromIntegral :: Int -> Double)
    , toDyn (round :: Double -> Int)
    , toDyn (realToFrac :: Double -> Rational)
    , toDyn (realToFrac :: Rational -> Double)
    , toDyn (read :: String -> Rational)
    , toDyn (realToFrac :: Int -> Rational)
    ] ++ showl)

showAny :: (Typeable a) => a -> String
showAny = showDyn . toDyn

showDyn :: Dynamic -> String
showDyn x = case applyMultimethod showm [x] of
  Left e -> show $ und x
  Right r -> fromMaybe "all show functions must return String" $ fromDynamic r

showAnyIO :: (Typeable a) => a -> IO String
showAnyIO = showDynIO . toDyn

showDynIO :: Dynamic -> IO String
showDynIO x = do
  xr <- applyMultimethodIO showm [x]
  case xr of
    Left l -> do return $ show $ und x
    Right r -> do return $ show (und x) ++ ":" ++ fromDyn1 r

{-}
  return $ case applyMultimethod showm [x] of
    Left e -> show $ und x
    Right r -> fromMaybe "all show functions must return String" $ fromDynamic r
-}
showTerm b a l d = case fromDynamic d :: Maybe String of
  Just s -> (0, 0, length s, padr l s)
  Nothing -> case fromDynamic d :: Maybe Int of
    Just i -> let s = show i in (length s, 0, 0, padl b s)
    Nothing -> case fromDynamic d :: Maybe Double of
      Just d ->
        let
          s = showFFloat (Just 3) d ""
          l = length s
          p = fromMaybe l $ elemIndex '.' s
         in
          (p, l - p, 0, replicate (b - p) ' ' ++ s ++ replicate (a - (l - p)) ' ')
      Nothing -> let s = show d in (length s, 0, 0, padr l s)

showColD col =
  let
    (b, a, l, c) = unzip4 $ map (showTerm b2 a1 l2) col
    b1 = maximum b
    a1 = maximum a
    l1 = maximum l
    l2 = max (b1 + a1) l1
    b2 = max b1 (l2 - a1)
   in
    c

putAny :: (Typeable a) => a -> IO ()
putAny x = showAnyIO x >>= putStr
putAnyLn :: (Typeable a) => a -> IO ()
putAnyLn x = showAnyIO x >>= putStrLn

putDyn :: Dynamic -> IO ()
putDyn x = showDynIO x >>= putStr
putDynLn :: Dynamic -> IO ()
putDynLn x = showDynIO x >>= putStrLn

eqm :: Multimethod
eqm =
  createMultimethod
    "=="
    [ toDyn ((==) :: Int -> Int -> Bool)
    , toDyn ((==) :: Integer -> Integer -> Bool)
    , toDyn ((==) :: Rational -> Rational -> Bool)
    , toDyn ((==) :: Float -> Float -> Bool)
    , toDyn ((==) :: Double -> Double -> Bool)
    , toDyn ((==) :: String -> String -> Bool)
    , toDyn ((==) :: Char -> Char -> Bool)
    , toDyn ((==) :: Bool -> Bool -> Bool)
    , toDyn ((==) :: () -> () -> Bool)
    , toDyn ((==) :: Maybe Dynamic -> Maybe Dynamic -> Bool)
    , toDyn ((==) :: [Dynamic] -> [Dynamic] -> Bool)
    , toDyn ((==) :: TC -> TC -> Bool)
    , toDyn ((==) :: Day -> Day -> Bool)
    , toDyn ((==) :: DayOfWeek -> DayOfWeek -> Bool)
    , toDyn ((==) :: Field -> Field -> Bool)
    , toDyn ((==) :: Person -> Person -> Bool)
    , toDyn ((==) :: NamedValue -> NamedValue -> Bool)
    ]

comparem :: Multimethod
comparem = createMultimethod "compare" comparel
comparel =
  [ toDyn (compare :: Int -> Int -> Ordering)
  , toDyn (compare :: Integer -> Integer -> Ordering)
  , toDyn (compare :: Float -> Float -> Ordering)
  , toDyn (compare :: Double -> Double -> Ordering)
  , toDyn (compare :: Rational -> Rational -> Ordering)
  , toDyn (compare :: String -> String -> Ordering)
  , toDyn (compare :: Char -> Char -> Ordering)
  , toDyn (compare :: TC -> TC -> Ordering)
  , toDyn (compare :: Day -> Day -> Ordering)
  , toDyn (compare :: DayOfWeek -> DayOfWeek -> Ordering)
  , toDyn (compare :: Field -> Field -> Ordering)
  , toDyn (compare :: Person -> Person -> Ordering)
  ]

addm = createMultimethod "+" $ numl (+)
subm = createMultimethod "-" $ numl (-)
mulm = createMultimethod "*" $ numl (*)

divfracm =
  createMultimethod
    "/"
    [ toDyn ((/) :: Rational -> Rational -> Rational)
    , toDyn ((/) :: Double -> Double -> Double)
    , toDyn ((/) :: Float -> Float -> Float)
    ]

numl :: (forall n. (Num n) => n -> n -> n) -> [Dynamic]
numl op =
  [ toDyn (op :: Int -> Int -> Int)
  , toDyn (op :: Integer -> Integer -> Integer)
  , toDyn (op :: Float -> Float -> Float)
  , toDyn (op :: Double -> Double -> Double)
  , toDyn (op :: Rational -> Rational -> Rational)
  ]

absm =
  createMultimethod
    "abs"
    [ toDyn (abs :: Int -> Int)
    , toDyn (abs :: Integer -> Integer)
    , toDyn (abs :: Double -> Double)
    , toDyn (abs :: Float -> Float)
    , toDyn (abs :: Rational -> Rational)
    ]

signumm =
  createMultimethod
    "signum"
    [ toDyn (signum :: Int -> Int)
    , toDyn (signum :: Integer -> Integer)
    , toDyn (signum :: Double -> Double)
    , toDyn (signum :: Float -> Float)
    , toDyn (signum :: Rational -> Rational)
    ]

fromIntegerm =
  createMultimethod
    "fromInteger"
    [ toDyn (fromInteger :: Integer -> Int)
    , toDyn (fromInteger :: Integer -> Integer)
    , toDyn (fromInteger :: Integer -> Float)
    , toDyn (fromInteger :: Integer -> Double)
    , toDyn (fromInteger :: Integer -> Rational)
    ]

toIntegerm =
  createMultimethod
    "toInteger"
    [ toDyn (toInteger :: Int -> Integer)
    , toDyn (toInteger :: Integer -> Integer)
    , toDyn (round :: Float -> Integer)
    , toDyn (round :: Double -> Integer)
    , toDyn (round :: Rational -> Integer)
    ]

instance Show Closure where
  show (Closure constr expr env) = "{" ++ show (Lambda u constr expr) ++ "}"

instance Show Dynamic where
  show = showDyn

instance Show Multimethod where
  show mm = show $ fromAssocsDA (\_ x -> x) (toDyn "") $ map (\(i, e) -> (map toDyn i, e)) $ M.toList $ funcs mm

instance Show MultimethodA where
  show (MultimethodA n a) = "name: " ++ n ++ "\n" ++ show a


instance Read Dynamic where
  readsPrec p s0 = px (readsPrec p :: ReadS Int) $ px (readsPrec p :: ReadS Double) $ px (readsPrec p :: ReadS Bool) $ px (readsPrec p :: ReadS String) $ px (readsPrec p :: ReadS Char) [(toDyn s0, "")]
   where
    px p y = let (r, s) = unzip $ p s0 in if not $ null r then zip (map toDyn r) s else y

deriving instance Show MMEntry

--deriving instance Show HTTPTypes.HVar

showm :: Multimethod
showm =
  createMultimethod1
    "show" showl

showl =    
    [ toDyn (show :: Int -> String)
    , toDyn (show :: Integer -> String)
    , toDyn (show :: String -> String)
    , toDyn (show :: Bool -> String)
    , toDyn (singleton :: Char -> String)
    , toDyn ((\d -> showFFloat (Just 8) d "") :: Double -> String)
    , toDyn ((\d -> showFFloat (Just 4) d "") :: Float -> String)
    , toDyn (show :: Rational -> String)
    , toDyn (show :: () -> String)
    , toDyn (show :: Maybe Dynamic -> String)
    , toDyn (show :: [Dynamic] -> String)
    , toDyn (show :: Multimethod -> String)
    , toDyn (show :: MultimethodA -> String)
    , toDyn (show :: M.Map [SomeTypeRep] Dynamic -> String)
    , toDyn (show :: [([SomeTypeRep], Dynamic)] -> String)
    , toDyn (show :: ([SomeTypeRep], Dynamic) -> String)
    , toDyn (show :: [SomeTypeRep] -> String)
    , toDyn (show :: SomeTypeRep -> String)
    , toDyn (show :: Dynamic -> String)
    , toDyn (show :: M.Map HTTPTypes.HVar Dynamic -> String)
    , toDyn (show :: Ordering -> String)
    , toDyn (show :: MMEntry -> String)
    , toDyn (show :: Person -> String)
    , toDyn (show :: Field -> String)
    , toDyn (show :: TC -> String)
    , toDyn (show :: Day -> String)
    , toDyn (show :: Expr -> String)
    , toDyn (show :: Closure -> String)
    , toDyn (fromJust . fp expr)
    ]

u = unknown

num = "num" <=> valueIso >$< number

valueIso = Iso (Just . Value u . toDyn) (\case Value _ n -> fromDynamic n; _ -> Nothing)

var = "var" <=> variso >$< identifier

vardefiso = Iso (\(n, t) -> Just $ VarDef t n 0 0) (\case VarDef t n _ _ -> Just (n, t); _ -> Nothing)

vardef = "vardef" <=> vardefiso >$< text "var" *< identifier >*< ((skipSpace *< text "::" *< skipSpace *< expr) <|> (valueIso >$< Syntax3.pure u))

typeanno = expr >*< skipSpace *< text "::" *< skipSpace *< expr

mem = Iso (Just . Member u) (\(Member _ n) -> Just n) >$< identifier

varfn (VarRef1 _ v) = Just v
varfn (VarRef _ v _ _) = Just v
varfn _ = Nothing

varfnt (VarRef t v _ _) = Just (v, t)
varfnt (VarRef1 t v) = Just (v, t)
varfnt _ = Nothing

app (Apply _ l) = l

variso = Iso (Just . VarRef1 u) varfn

varisot = Iso (\(v, t) -> Just $ VarRef1 t v) varfnt

prediso p = Iso (ifPred p) (ifPred p)

opiso ops =
  Iso
    (\(a, (op, b)) -> Just $ Apply u [VarRef1 u op, a, b])
    ( \case
        Apply _ [varfn -> Just op, a, b] -> ifJust (op `elem` ops) (a, (op, b))
        _ -> Nothing
    )

opc ops = Prelude.foldr (<|>) empty $ map text1 ops

opl ops term = chainl1 term (opc ops) $ opiso ops

opr ops term = chainr1 term (opc ops) $ opiso ops

ops = ["+", "-", "*", "/", "$", ".", "=", "=="]

op = "op" <=> variso >$< text "(" *< opc ops >* text ")"

leftsec =
  Iso
    (\(a, op) -> Just $ Lambda u (Co "lam" [Member u "b"]) (Apply u [VarRef1 u op, a, VarRef1 u "b"]))
    ( \case
        (Lambda _ (Co _ locals) (Apply _ [VarRef1 _ op, a, VarRef1 _ b])) -> ifJust (b `elem` map mname locals) (a, op)
        _ -> Nothing
    )
    >$< text "("
    *< term
    >*< opc ops
    >* text ")"

rightsec =
  Iso
    (\(op, b) -> Just $ Lambda u (Co "lam" [Member u "a"]) (Apply u [VarRef1 u op, VarRef1 u "a", b]))
    ( \case
        (Lambda _ (Co _ locals) (Apply _ [VarRef1 _ op, VarRef1 _ a, b])) -> ifJust (a `elem` map mname locals) (op, b)
        _ -> Nothing
    )
    >$< text "("
    *< opc ops
    >*< term
    >* text ")"

parens = text "(" *< expr >* text ")"

list = text "[" *< list2 >* text "]"

list3 = chainr1 expr (text ",")
list2 = Iso (Just . Apply u . (VarRef1 u "list" :)) (\(Apply _ (f : xs)) -> ifJust (f == VarRef1 u "list") xs) >$< expr `sepBy` text ","

term = "term" <=> num <|> var <|> leftsec <|> rightsec <|> op <|> parens <|> list

varname (VarRef1 _ v) = v
varname (VarRef _ v _ _) = v
varname _ = ""

applic =
  "application"
    <=> Iso
      (\ts -> Just $ if length ts >= 2 then Apply u ts else head ts)
      ( \case
          Apply _ (f : xs) | notElem (varname f) ops -> Just (f : xs)
          x -> Just [x]
      )
    >$< sepBy term sepSpace

-- a :: A @ b :: B @ c :: C

expr10 = "expr10" <=> opl ["@", "::"] applic

expr9 = "expr9" <=> opr ["."] expr10

expr7 = "expr7" <=> opl ["*", "/"] expr9

-- expr7 = "expr7" <=> (opiso ["*"] >$< term >*< text1 "*" >*< expr7) <|> term

expr6 = "expr6" <=> opl ["+", "-"] expr7

expr5 = expr6

expr4 = "expr4" <=> opl ["<", "<=", "==", ">=", ">", "/="] expr5

expr3 = "expr3" <=> opr ["&&", "||"] expr4

expr0 = "expr0" <=> opr ["$", "="] expr3

ifSyn =
  Iso
    (Just . If u)
    (\case If _ blah -> Just blah; _ -> Nothing)
    >$< ci (text "if")
    *< groupOf (expr0 >* text "->" >*< expr0)

conIso = Iso (Just . Co "data") (\case Co _ members -> Just members)

lambdaSyn =
  Iso
    (\(params, exp) -> Just $ Lambda u (Co "" params) exp)
    (\case Lambda _ (Co _ params) exp -> Just (params, exp); _ -> Nothing)
    >$< text "\\" *< mem `sepBy` sepSpace >* text "->" >*< expr0

blockSyn =
  Iso
    (\exp -> Just $ Block u (Co "" []) exp)
    (\case Block _ _ exp -> Just exp; _ -> Nothing)
    >$< groupOf expr0

dataSyn = valueIso >$< conIso >$< text "data" *< sepSpace *< mem `sepBy` sepSpace

expr = ifSyn <|> lambdaSyn <|> expr0

exprs = groupOf expr

data Dimension
  = DimInt {dimLower :: Int, dimUpper :: Int, dimMult :: Int}
  | DimMap {dimLower :: Int, dimUpper :: Int, dimMult :: Int, dimMap1 :: M.Map Int Dynamic, dimMap2 :: M.Map Dynamic Int}
  --            | DimCat { dimDim::Dimension, dimDiv::[Int], dimMult1::[Int] }
  deriving (Show)

data SubArrayD e = SubArrayD {dims :: [Dimension], offset :: Int, payload :: A.Array Int e}

-- main = print $ Main.transpose 0 1 $ fromList2z 0 [[1,2,3],[4,5,6],[7,8,9]]

refold f z [] = []
refold f z (x : xs) = let (a, b) = f z x in a : refold f b xs

-- foldl f   z (x:xs) = f x $ foldl f z xs

refold1 f z xs = snd $ foldr (\i (z, o) -> let (a, b) = f z i in (b, a : o)) (z, []) xs

refold4 f z xs = let (rs, zs) = unzip $ zipWith f (z : zs) xs in rs

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
   in
    SubArrayD ds (offset a + elemOffset1Dyn i d) (payload a)

getSub i a =
  let
    (d : ds) = dims a
   in
    SubArrayD ds (offset a + elemOffset1 i d) (payload a)

getSubs is a = SubArrayD (drop (length is) $ dims a) (offset a + elemOffset is (dims a)) (payload a)

getSubDim dn i a = SubArrayD (dbefore ++ dafter) (offset a + (i - dimLower d) * dimMult d) (payload a)
 where
  (dbefore, d : dafter) = splitAt dn $ dims a

getSubDimDyn dn i a = SubArrayD (dbefore ++ dafter) (offset a + ((dimMap2 d M.! i) - dimLower d) * dimMult d) (payload a)
 where
  (dbefore, d : dafter) = splitAt dn $ dims a

getSubDims ds a = foldr (uncurry getSubDim) a ds

getSubDimDyns ds a = foldr (uncurry getSubDimDyn) a ds

copy a = fromAssocs $ toAssocs a

zipWitha f a b = fromAssocs $ zipWith (\i j -> ([i], f (getSub i a) (getSub j b))) (dimRange $ head $ dims a) (dimRange $ head $ dims b)

mapEE f a = fromAssocs $ map (\(i, e) -> (i, f e)) $ toAssocs a

mapEA f a = fromAssocs $ concatMap (\(i1, e1) -> map (\(i2, e2) -> (i1 ++ i2, e2)) $ toAssocs $ f e1) $ toAssocs a

mapAE f s a =
  let
    indices1 = indices $ select s $ dims a
    assocs1 = map (\i -> (i, f $ getSubDims (zip s i) a)) indices1
   in
    fromAssocs assocs1

mapAA f t a =
  let
    s = [0 .. length (dims a) - 1] \\ sort t
    ist = inversePerm $ s ++ t
    indices1 = indicesD $ select s $ dims a
    assocs1 = concatMap (\i1 -> map (\(i2, e2) -> (select ist $ i1 ++ i2, e2)) $ toAssocsD $ f $ getSubDimDyns (zip s i1) a) indices1
   in
    fromAssocsD assocs1

foldE f a = f $ map snd $ toAssocs a

appendFold (n, f) a =
  let
    l = toAssocsD a
    s = f $ map snd l
   in
    fromAssocsD $ l ++ [(n, s)]

asum = appendFold ([toDyn "Total"], sum)

admean = appendFold ([toDyn "Mean"], dmean)

mafs f ts a = foldr (mapAA f . singleton) a ts

mapDim f (DimInt dl du dm) = DimMap dl du dm (M.fromList dm1) (M.fromList $ map tflip dm1) where dm1 = mapxfx f [dl .. du]

zipDims fs (SubArrayD d o p) = SubArrayD (zipWith mapDim fs d) o p

reverseA a =
  let
    (DimInt dl du dm : ds) = dims a
   in
    SubArrayD (DimInt dl du (-dm) : ds) (offset a + (du - dl + 1) * dm) (payload a)

reverseDim d a =
  let
    ds = dims a
    (DimInt dl du dm) = ds !! d
   in
    SubArrayD (replaceIndex d (DimInt dl du (-dm)) ds) (offset a + (du - dl + 1) * dm) (payload a)

shiftDim d s a =
  let
    ds = dims a
    (DimInt dl du dm) = ds !! d
   in
    SubArrayD (replaceIndex d (DimInt (dl - s) (du - s) dm) ds) (offset a + s * dm) (payload a)

sliceDim d s t a =
  let
    ds = dims a
    (DimInt dl du dm) = ds !! d
   in
    SubArrayD (replaceIndex d (DimInt (dl - s) (t - s + 1) dm) ds) (offset a + s * dm) (payload a)

transposeA d1 d2 a =
  let
    ds = dims a
    e1 = ds !! d1
    e2 = ds !! d2
   in
    SubArrayD (replaceIndex d2 e1 $ replaceIndex d1 e2 ds) (offset a) (payload a)

{-
toAssocs :: SubArrayD e -> [(Int, e)]
toAssocs a = let
   (d:ds) = dims a
   in map (\i -> (i, getElem [i] a)) [dimLower d..dimUpper d]
-}
-- cartesian [1,2,3] [[4],[5],[6]] = [[1,4],[2,4],[3,4],[1,5],[2,5],[3,5],[1,6],[2,6],[3,6]]
dimRange (DimInt dl du _) = [dl .. du]
dimRange (DimMap dl du _ _ _) = [dl .. du]
dimRangeD (DimMap dl du _ dm1 _) = map (dm1 M.!) [dl .. du]
dimRanges = map dimRange
dimRangesD = map dimRangeD

indicesA a = indices $ dims a
indices ds = crossList $ dimRanges ds
indicesD ds = crossList $ dimRangesD ds

toAssocs :: SubArrayD e -> [([Int], e)]
toAssocs a = map (\i -> (i, getElem i a)) $ indicesA a

toAssocsD a = map (\i -> (zipWith elemName i d, getElem i a)) $ indicesA a where d = dims a

elemName i (DimInt dl du dm) = toDyn i
elemName i (DimMap dl du dm dm1 dm2) = dm1 M.! i

fromAssocs :: [([Int], e)] -> SubArrayD e
fromAssocs as =
  let
    (mins, maxs, lens) = unzip3 $ map toDim $ transpose $ checkRectangular $ map fst as
    (len : muls) = scanr (*) 1 lens
    dims = zipWith3 DimInt mins maxs muls
   in
    SubArrayD dims 0 $ A.array (0, len - 1) $ map (\(i, e) -> (elemOffset i dims, e)) as

toDim is =
  let
    mi = minimum is
    ma = maximum is
    le = ma - mi + 1
   in
    (mi, ma, le)

fromAssocsD :: [([Dynamic], e)] -> SubArrayD e
fromAssocsD as =
  let
    (mins, maxs, lens, vals) = unzip4 $ map toDimD $ transpose $ checkRectangular $ map fst as
    (len : muls) = scanr (*) 1 lens
    dims = zipWith4 toDim2 mins maxs muls vals
   in
    SubArrayD dims 0 $ A.array (0, len - 1) $ map (\(i, e) -> (elemOffsetDyn i dims, e)) as

fromAssocsDA f z as =
  let
    (mins, maxs, lens, vals) = unzip4 $ map toDimD $ transpose $ checkRectangular $ map fst as
    (len : muls) = scanr (*) 1 lens
    dims = zipWith4 toDim2 mins maxs muls vals
   in
    SubArrayD dims 0 $ A.accumArray f z (0, len - 1) $ map (\(i, e) -> (elemOffsetDyn i dims, e)) as

checkRectangular as =
  let
    lens = map length as
    mi = minimum lens
    ma = maximum lens
   in
    if mi /= ma then error "varying number of dimensions" else as

{-
toDimD is = if
   | all isInt is -> toDimII $ map (`fromDyn` int0) is
   | otherwise    -> toDimIS1 is
-}
toDimD is = toDimIS1 is

toDim2 min max mul [] = DimInt min max mul
toDim2 min max mul vals = DimMap min max mul (M.fromList $ zip [0 ..] vals) (M.fromList $ zip vals [0 ..])

isInt d = dynTypeRep d == intR

int0 = (0 :: Int)
intD = toDyn int0
intR = dynTypeRep intD

toDimII is =
  let
    mi = minimum is
    ma = maximum is
    le = ma - mi + 1
   in
    (mi, ma, le, [])

toDimIS1 is =
  let
    se = S.fromList is
    le = S.size se
   in
    (0, le - 1, le, S.toList se)

toDimIS2 is =
  let
    mi = minimum is
    mx = maximum is
    se = [mi .. mx]
    le = length se
   in
    (0, le, le, se)

-- fromList1 es = SubArrayD [DimInt 0 l 1] 0 (A.array (0, l) (zip [0..] es)) where l = length es - 1
fromList1 es = SubArrayD [DimInt 0 (l - 1) 1] 0 $ A.listArray (0, l - 1) es where l = length es
fromList2z zero ess =
  let
    d0 = length ess
    d1 = maximum $ map length ess
   in
    SubArrayD [DimInt 0 (d0 - 1) d1, DimInt 0 (d1 - 1) 1] 0 $ A.listArray (0, d0 * d1 - 1) $ concatMap (padRWith1 zero d1) ess

fromList2 = fromList2z undefined

fromList3 zero esss =
  let
    d0 = length esss
    d1 = maximum $ map length esss
    d2 = maximum $ map (maximum . map length) esss
   in
    SubArrayD [DimInt 0 (d0 - 1) (d1 * d2), DimInt 0 (d1 - 1) d2, DimInt 0 (d2 - 1) 1] 0 $ A.listArray (0, d0 * d1 * d2 - 1) $ concatMap (padRWith1 zero (d1 * d2) . concatMap (padRWith1 zero d2)) esss

toList1 a = map (`getElem` a) $ indicesA a

toList2 a = map (\x -> map (\y -> getElem [x, y] a) dy) dx where [dx, dy] = dimRanges $ dims a

toList3 a = map (\x -> map (\y -> map (\z -> getElem [x, y, z] a) dz) dy) dx where [dx, dy, dz] = dimRanges $ dims a

fromArrayList as = fromAssocs $ concat $ zipWith (\inew a -> map (\(i, e) -> (inew : i, e)) $ toAssocs a) [0 ..] as

fromArrayList2 dl du as =
  let
    miny = map minimum $ Data.List.transpose $ map (map dimLower . dims) as
    maxy = map maximum $ Data.List.transpose $ map (map dimUpper . dims) as
    leny1 = zipWith (\min max -> max - min + 1) miny maxy
    muly = map product $ tails leny1
    dimy = zipWith3 DimInt miny maxy $ tail muly
    leny = head muly
   in
    SubArrayD
      (DimInt dl du leny : dimy)
      0
      ( A.array
          (0, length as - 1)
          (zipWith (\x (y, e) -> (x * leny + elemOffset y dimy, e)) [0 ..] $ concatMap toAssocs as)
      )

fromAOA a = fromAssocs $ concatMap (\(i1, e1) -> map (\(i2, e2) -> (i1 ++ i2, e2)) $ toAssocs e1) $ toAssocs a

newtype SubArray1 b = SubArray1 (SubArrayD b)
newtype SubArray2 b = SubArray2 (SubArrayD b)
newtype SubArray3 b = SubArray3 (SubArrayD b)

class SubArray a e where
  (!) :: a -> Int -> e
  (?) :: a -> Dynamic -> e

instance SubArray (SubArray1 b) b where
  SubArray1 a ! i = getElem [i] a
  SubArray1 a ? i = getElem [] $ getSubDyn i a

instance SubArray (SubArray2 b) (SubArray1 b) where
  SubArray2 a ! i = SubArray1 $ getSub i a
  SubArray2 a ? i = SubArray1 $ getSubDyn i a

instance (Show e) => Show (SubArrayD e) where
  show = showHyper

{-
toAssocs :: SubArrayD e -> [([Int], e)]
toAssocs a = map (\i -> (i, getElem i a)) $ indices a
-}
select indices from = map (from !!) indices

inversePerm indices = map snd $ sort $ zip indices [0 ..]

splitAt2 major minor ls = (unzip $ map (splitAt minor) $ take major ls, unzip $ map (splitAt minor) $ drop major ls)

readUniqueHeader h [] = h
readUniqueHeader h (x : xs) = if S.member x h then h else readUniqueHeader (S.insert x h) xs

readUniqueHeaders [] _ = []
readUniqueHeaders (h : hs) mult =
  let
    h1 = S.toList $ readUniqueHeader S.empty $ map (\(x : xs) -> x) $ groupN mult h
   in
    h1 : readUniqueHeaders hs (mult * length h1)

groupN1 n (SubArrayD (DimInt dl du dm : ds) o p)
  | mod l n == 0 = SubArrayD (DimInt 0 (div l n - 1) (dm * n) : DimInt 0 (n - 1) dm : ds) o p
  | otherwise = error ("not divisible into groups of " ++ show n)
 where
  l = du - dl + 1

groupMulti ns a = foldr groupN1 a ns

readHyper9 xls yls a =
  let
   in groupMulti yls $ groupMulti xls a

readUniqueHyper xhn yhn a =
  let
    (xh1, a1) = splitAt xhn a
    (yh1, a2) = unzip $ map (splitAt yhn) a1
    xh = readUniqueHeaders (reverse $ map (drop yhn) xh1) 1
    yh = readUniqueHeaders (reverse $ transpose yh1) 1
    d = readHyper9 (map length xh) (map length yh) $ fromList1 a2
   in
    d

readHyper2 xh yh a = concat $ zipWith zip (crossWith (++) xh yh) a

readHyper1 xhn yhn ri re a =
  let
    (_, xh1, yh, a1) = readQuarters xhn yhn a
    xh = transpose xh1
    d = readHyper2 xh yh $ map2 re a1
   in
    d

readQuarters xhn yhn a =
  let
    (w, e) = unzip $ map (splitAt yhn) a
    (nw, sw) = splitAt xhn w
    (ne, se) = splitAt xhn e
   in
    (nw, ne, sw, se)

showQuarters :: ([[String]], [[String]], [[String]]) -> [[String]]
showQuarters (xh, yh, a) =
  let
    nw = replicate (length $ head xh) $ replicate (length $ head yh) ""
    b = zipWith (++) (nw ++ yh) (transpose xh ++ transpose a)
   in
    b

showHyper2 xn yn a =
  let
    xd = select xn $ dims a
    yd = select yn $ dims a
    xs = indices xd
    ys = indices yd
    d = xn ++ yn
    ip = inversePerm d
   in
    ( map (zipWith showLabel xd) xs
    , map (zipWith showLabel yd) ys
    , crossWith (\x y -> show $ getElem (select ip $ x ++ y) a) xs ys
    )

showHyper1 xn yn a = showGrid $ showQuarters $ showHyper2 xn yn a

showHyper a =
  let
    n = length $ dims a
    hn = div n 2
   in
    showGrid $ showQuarters $ showHyper2 [hn .. n - 1] [0 .. hn - 1] a

showLabel (DimInt{}) i = show i
showLabel (DimMap _ _ _ dm1 _) i = show $ fromJust $ M.lookup i dm1

test d = fromAssocs $ mapxfx id $ indices $ replicate d $ DimInt 0 2 1

-- printa a = putTableF $ arrayToElemList a

{-
instance SubArray (SubArray3 b) (SubArray2 b)  where
   SubArray3 a ! i = SubArray2 $ getSub i a
-}
appendAA f z a1 a2 = fromAssocsDA f z $ toAssocsD a1 ++ toAssocsD a2
concatAA f z as = fromAssocsDA f z $ concatMap toAssocsD as
joinAA f z dn dv a1 a2 = fromAssocsDA f z $ map (\(i, e) -> (insertAt dn dv i, e)) (toAssocsD a1) ++ toAssocsD a2

insertAt n v l =
  let
    (b, a) = splitAt n l
   in
    b ++ v : a
