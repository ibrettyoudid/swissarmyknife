{-# LANGUAGE PatternSynonyms #-}

module HashDyn where

import Favs 
import Dyn hiding (toDyn, fromDyn, fromDynamic)
import qualified Dyn
import Show1
import NewTuple
import BString as B
import qualified HTTPTypes

import {-# SOURCE #-} Parser3
import {-# SOURCE #-} HashParser

import Numeric

import Data.Time.Calendar

import Data.List hiding ((++), length, elemIndex)
import qualified Data.Map as M

import Prelude hiding ((++), length, putStr, putStrLn)

newtype HashDyn = HashDyn Dynamic

hashDyn (HashDyn d) = d

toDyn :: Typeable a => a -> HashDyn
toDyn a = Dyn.toDyn a

fromDyn :: Typeable a => HashDyn -> a -> a
fromDyn d e = Dyn.fromDyn d e

fromDynamic :: Typeable a => HashDyn -> Maybe a
fromDynamic d = Dyn.fromDynamic d

instance Dyn HashDyn where
   dtoDyn = HashDyn
   dfromDyn = hashDyn

   showDyn = showHashDyn

instance Eq HashDyn where
   a == b = am eqm [a, b]

instance Ord HashDyn where
   compare a b = case applyMultimethod comparem [a, b] of
      Right d -> fromDyn d (error "should be an Ordering")
      Left e -> compare (showDyn a) (showDyn b)

instance Num HashDyn where
   a + b = am3 addm [a, b]
   a - b = am3 subm [a, b]
   a * b = am3 mulm [a, b]
   abs a = am3 absm [a]
   signum a = am3 signumm [a]
   fromInteger a = toDyn (fromInteger a :: Double)

instance Fractional HashDyn where
   a / b = am3 divfracm [a, b]
   fromRational = toDyn

instance Show HashDyn where
   show = showDyn

instance {-# OVERLAPPING #-} Show1 HashDyn where
   show1 = showDyn1

data Expr
   = Value   {etype :: MType, value :: HashDyn}
   | VarRef  {etype :: MType, name :: String, frameIndex :: Int, memIndex :: Int}
   | VarDef  {xtype :: Expr , name :: String, frameIndex :: Int, memIndex :: Int}
   | VarRef1 {etype :: MType, name :: String}
   | Lambda  {etype :: MType, econstr :: Constr,                     subexpr :: Expr}
   | Let     {etype :: MType, econstr :: Constr, subexprs :: [Expr], subexpr :: Expr}
   | Block   {etype :: MType, econstr :: Constr, subexprs :: [Expr]}
   | Apply   {etype :: MType,                    subexprs :: [Expr]}
   | If      {etype :: MType,                clauses :: [(Expr :- Expr)]}
   | Case    {etype :: MType, case1 :: Expr, clauses :: [(Expr :- Expr)]}
   | Else
   | Exprs   {etype :: MType, exprs1 :: [Expr]}
   | Keyword String
   deriving (Typeable, Eq, Show)

data Closure = Closure Constr Expr Env deriving (Typeable, Show)

type Env = [Frame]

data Frame = Frame {fconstr :: Constr, items :: [HashDyn]} deriving (Show, Eq)

pattern Co n m = Constr n (TypeLink [] []) m

data PartApply = PartApply [HashDyn] Constr Expr Env deriving (Typeable)

data TC = Trans | Cum deriving (Eq, Ord, Show, Read)

data Person = Tyrone | Danny | James | David deriving (Eq, Ord, Show, Read)

data Field = Borrowed | Profit | Owed deriving (Eq, Ord, Show, Read)

eqm :: Multimethod HashDyn
eqm = createMultimethod "=="
   [ toDyn ((==) :: Int -> Int -> Bool)
   , toDyn ((==) :: Integer -> Integer -> Bool)
   , toDyn ((==) :: Rational -> Rational -> Bool)
   , toDyn ((==) :: Float -> Float -> Bool)
   , toDyn ((==) :: Double -> Double -> Bool)
   , toDyn ((==) :: String -> String -> Bool)
   , toDyn ((==) :: Char -> Char -> Bool)
   , toDyn ((==) :: Bool -> Bool -> Bool)
   , toDyn ((==) :: () -> () -> Bool)
   , toDyn ((==) :: Maybe HashDyn -> Maybe HashDyn -> Bool)
   , toDyn ((==) :: [HashDyn] -> [HashDyn] -> Bool)
   , toDyn ((==) :: Day -> Day -> Bool)
   , toDyn ((==) :: DayOfWeek -> DayOfWeek -> Bool)
   , toDyn ((==) :: TC -> TC -> Bool)
   , toDyn ((==) :: Field -> Field -> Bool)
   , toDyn ((==) :: Person -> Person -> Bool)
   , toDyn ((==) :: NamedValue HashDyn -> NamedValue HashDyn -> Bool)
   ]

comparem :: Multimethod HashDyn
comparem = createMultimethod "compare" comparel

comparel =
   [ toDyn (compare :: Int -> Int -> Ordering)
   , toDyn (compare :: Integer -> Integer -> Ordering)
   , toDyn (compare :: Float -> Float -> Ordering)
   , toDyn (compare :: Double -> Double -> Ordering)
   , toDyn (compare :: Rational -> Rational -> Ordering)
   , toDyn (compare :: String -> String -> Ordering)
   , toDyn (compare :: Char -> Char -> Ordering)
   -- , toDyn (compare :: TC -> TC -> Ordering)
   -- , toDyn (compare :: Day -> Day -> Ordering)
   -- , toDyn (compare :: DayOfWeek -> DayOfWeek -> Ordering)
   -- , toDyn (compare :: Field -> Field -> Ordering)
   -- , toDyn (compare :: Person -> Person -> Ordering)
   ]

addm = createMultimethod "+" $ numl (+)
subm = createMultimethod "-" $ numl (-)
mulm = createMultimethod "*" $ numl (*)

divfracm =
   createMultimethod
      "/"
      [ 
      toDyn ((/) :: Double -> Double -> Double), 
      toDyn ((/) :: Float -> Float -> Float),
      toDyn ((/) :: Rational -> Rational -> Rational)
      ]

numl :: (forall n. (Num n) => n -> n -> n) -> [HashDyn]
numl op =
   [ 
   toDyn (op :: Int -> Int -> Int), 
   toDyn (op :: Integer -> Integer -> Integer), 
   toDyn (op :: Double -> Double -> Double), 
   toDyn (op :: Float -> Float -> Float), 
   toDyn (op :: Rational -> Rational -> Rational)
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

fromIntegerm :: Multimethod HashDyn =
   createMultimethod
      "fromInteger"
      [ toDyn (fromInteger :: Integer -> Int)
      , toDyn (fromInteger :: Integer -> Integer)
      , toDyn (fromInteger :: Integer -> Double)
      , toDyn (fromInteger :: Integer -> Float)
      , toDyn (fromInteger :: Integer -> Rational)
      ]

toIntegerm :: Multimethod HashDyn =
   createMultimethod
      "toInteger"
      [ toDyn (toInteger :: Int -> Integer)
      , toDyn (toInteger :: Integer -> Integer)
      , toDyn (round :: Double -> Integer)
      , toDyn (round :: Float -> Integer)
      , toDyn (round :: Rational -> Integer)
      ]

showm :: Multimethod HashDyn
showm =
   createMultimethod1
      "show" showl

showl =    
   [ toDyn (show :: Int -> String)
   , toDyn (show :: Integer -> String)
   , toDyn (show :: String -> String)
   , toDyn (show :: B.ByteString -> String)
   , toDyn (show :: B.LByteString -> String)
   , toDyn (show :: B.Text -> String)
   , toDyn (show :: B.LText -> String)
   , toDyn (show :: Bool -> String)
   , toDyn (singleton :: Char -> String)
   , toDyn ((\d -> showFFloat (Just 8) d "") :: Double -> String)
   , toDyn ((\d -> showFFloat (Just 4) d "") :: Float -> String)
   , toDyn (show :: Rational -> String)
   , toDyn (show :: () -> String)
   , toDyn (show :: Maybe HashDyn -> String)
   , toDyn (show :: [HashDyn] -> String)
   --, toDyn (show :: Multimethod -> String)
   --, toDyn (show :: MultimethodA -> String)
   , toDyn (show :: M.Map [SomeTypeRep] HashDyn -> String)
   , toDyn (show :: [([SomeTypeRep], HashDyn)] -> String)
   , toDyn (show :: ([SomeTypeRep], HashDyn) -> String)
   , toDyn (show :: [SomeTypeRep] -> String)
   , toDyn (show :: SomeTypeRep -> String)
   , toDyn (show :: M.Map HTTPTypes.HVar HashDyn -> String)
   , toDyn (show :: Ordering -> String)
   , toDyn (show :: MMEntry HashDyn -> String)
   , toDyn (show :: Person -> String)
   , toDyn (show :: Field -> String)
   , toDyn (show :: TC -> String)
   , toDyn showDate
   , toDyn (show :: Expr -> String)
   , toDyn (show :: Closure -> String)
   , toDyn (fromJust . fp expr :: Expr -> String)
   , toDyn (show :: (HashDyn :- HashDyn) -> String)
   , toDyn (show :: (HashDyn, HashDyn) -> String)
   ]

showm1 =
   createMultimethod1
      "show1" showl1

showl1 =    
   [ toDyn (show :: Int -> String)
   , toDyn (show :: Integer -> String)
   , toDyn (id :: String -> String)
   , toDyn (B.convertString :: B.ByteString -> String)
   , toDyn (B.convertString :: B.Text -> String)
   , toDyn (B.convertString :: B.LByteString -> String)
   , toDyn (B.convertString :: B.LText -> String)
   , toDyn (show :: Bool -> String)
   , toDyn (singleton :: Char -> String)
   , toDyn ((\d -> showFFloat (Just 8) d "") :: Double -> String)
   , toDyn ((\d -> showFFloat (Just 4) d "") :: Float -> String)
   , toDyn (show :: Rational -> String)
   , toDyn (show :: () -> String)
   , toDyn (show :: Maybe HashDyn -> String)
   , toDyn (show :: [HashDyn] -> String)
   --, toDyn (show :: Multimethod -> String)
   --, toDyn (show :: MultimethodA -> String)
   , toDyn (show :: M.Map [SomeTypeRep] HashDyn -> String)
   , toDyn (show :: [([SomeTypeRep], HashDyn)] -> String)
   , toDyn (show :: ([SomeTypeRep], HashDyn) -> String)
   , toDyn (show :: [SomeTypeRep] -> String)
   , toDyn (show :: SomeTypeRep -> String)
   , toDyn (show :: M.Map HTTPTypes.HVar HashDyn -> String)
   , toDyn (show :: Ordering -> String)
   , toDyn (show :: MMEntry HashDyn -> String)
   , toDyn (show :: Person -> String)
   , toDyn (show :: Field -> String)
   , toDyn (show :: TC -> String)
   , toDyn showDate
   , toDyn (show :: Expr -> String)
   , toDyn (show :: Closure -> String)
   , toDyn (fromJust . fp expr :: Expr -> String)
   , toDyn (show :: (HashDyn :- HashDyn) -> String)
   , toDyn (show :: (HashDyn, HashDyn) -> String)
   ]

showDate (YearMonthDay y m d) = pad0 2 (show d) ++ "/" ++ pad0 2 (show m) ++ "/" ++ show y

showAny :: (Typeable a) => a -> String
showAny a = showDyn $ (toDyn a :: HashDyn)

showHashDyn :: HashDyn -> String
showHashDyn x = case applyMultimethod showm [x] of
   Left e -> show $ dfromDyn x
   Right r -> fromMaybe "all show functions must return String" $ fromDynamic r

showDyn1 :: HashDyn -> String
showDyn1 x = case applyMultimethod showm1 [x] of
   Left e -> show $ dfromDyn x
   Right r -> fromMaybe "all show functions must return String" $ fromDynamic r

showRec x = case applyMultimethod showm [x] of
   Left e -> show $ dfromDyn x
   Right r -> 
      case fromdynm x of
         (a:_) -> "Dyn "++showRec a
         [] -> show $ dfromDyn x

showAnyIO :: (Typeable a) => a -> IO String
showAnyIO = showDynIO . toDyn

showDynIO :: HashDyn -> IO String
showDynIO x = do
   xr <- applyMultimethodIO showm [x]
   case xr of
      Left l -> do return $ show $ dfromDyn x
      Right r -> do return $ show (dfromDyn x) ++ ":" ++ fromDyn1 r

{-}
return $ case applyMultimethod showm [x] of
   Left e -> show $ dtoDyn x
   Right r -> fromMaybe "all show functions must return String" $ fromDynamic r
-}
showTerm b a l d = 
   case fromDynamic d :: Maybe String of
      Just s -> (0, 0, length s, padr l s)
      Nothing -> 
         case fromDynamic d :: Maybe Int of
            Just i -> let s = show i in (length s, 0, 0, padl b s)
            Nothing -> 
               case fromDynamic d :: Maybe Double of
                  Just d -> let
                     s = showFFloat (Just 3) d ""
                     l = length s
                     p = fromMaybe l $ elemIndex '.' s
                     
                     in (p, l - p, 0, replicate (b - p) ' ' ++ s ++ replicate (a - (l - p)) ' ')
                  Nothing -> let s = show d in (length s, 0, 0, padr l s)


{-
showColD formats columns of Dynamics in a hopefully aesthetically pleasing way,
namely numbers right justified, strings left justified, and doubles with the decimal points lining up

using showTerm above, each entry OUTPUTS a tuple (b, a, l, c)
b = characters before the decimal point
a = characters after and including the decimal point
l = total length
c = a string formatted according to the INPUT arguments b a l d

the input arguments are
b a l = the maxima of the outputs, with some calculation
d = the HashDyn value to be formatted

d THROUGH LAZINESS IS CALCULATED FROM THE MAXIMA OF THE FIRST 3 ENTRIES OF THE *OUTPUT* TUPLE FOR THE WHOLE COLUMN

the fourth entry therefore depends on the first three entries of the input tuple, which depend on the three of the output tuple
-}
showColD col = let
   (b, a, l, c) = unzip4 $ map (showTerm b2 a1 l2) col
   b1 = maximum b
   a1 = maximum a
   l1 = maximum l
   l2 = max (b1 + a1) l1
   b2 = max b1 (l2 - a1)
   
   in c

showColDH fcol col = let
   (b, a, l, c) = unzip4 $ map (showTerm b2 a1 l2) col
   b1 = maximum b
   a1 = maximum a
   l1 = maximum l
   l2 = maximum ([b1 + a1, l1] ++ map length fcol)
   b2 = max b1 (l2 - a1)
   
   in c

putAny :: (Typeable a) => a -> IO ()
putAny x = showAnyIO x >>= putStr
putAnyLn :: (Typeable a) => a -> IO ()
putAnyLn x = showAnyIO x >>= putStrLn

putDyn :: HashDyn -> IO ()
putDyn x = showDynIO x >>= putStr
putDynLn :: HashDyn -> IO ()
putDynLn x = showDynIO x >>= putStrLn

-- instance Show Closure where
--    show (Closure constr expr env) = "{" ++ show (Lambda u constr expr) ++ "}"

--instance Show Multimethod where
--   show mm = show $ fromAssocsDA (\_ x -> x) (toDyn "") (map (\x -> "arg"++show x) [0..]) $ map (\(i, e) -> (map toDyn i, e)) $ M.toList $ funcs mm

--instance Show MultimethodA where
--   show (MultimethodA n a) = "name: " ++ n ++ "\n" ++ show a

