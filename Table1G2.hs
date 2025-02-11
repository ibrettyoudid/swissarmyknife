{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}

module Table1G where

import Favs
import MyPretty2
import NumberParsers
import Https
import qualified Tree as T

import Data.Char
import Data.List
import Data.Dynamic
import Data.Typeable

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.LocalTime

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator


data Person = T | J | D

data Type = M | To

type Amount = Int

ymd = YearMonthDay

data Trans = Trans { day::Day, amount::Amount, person::Person, typ::Type }

--go = putGridF =<< readfods

linux = True

importdir = if linux then "/home/brett/SOL/Office/" else "D:/Office/"

names = ["Tyrone", "Danny", "James"]

trans = [(ymd 2023 6 13, (-20), T, M),
         (ymd 2023 6 13, (-30), T, M),
         (ymd 2023 6 13, (-20), J, M),
         (ymd 2023 6 13, (-25), J, M),
         (ymd 2023 6 13, ( -3), J, To),
         (ymd 2023 6 13, (-20), D, M)]

counts :: (Ord a, Show a) => [a] -> String
counts = show . reverse . sort . mode1
countUnique l = S.size $ S.fromList l

mode1 :: Ord a => [a] -> [(Int, a)]
mode1 = map tflip . combine (+) 0 . map (, 1)
mode :: Ord a => [a] -> a
mode = snd . maximum . mode1
modez z [] = z
modez _ xs = mode xs

groupBy f = combine (:) [] . mapfxx f

applyL fs x = map ($ x) fs

combine2 :: [[Term] -> Term] -> [[[Term] -> Term]] -> [[Term]] -> [[Term]]
combine2 fs ts rows = let
   frows = map (applyL fs) rows
   fcols = transpose frows

   --rows2 = map (applyL fs) $ transpose rows
   --in frows ++ (transpose $ padRWith (String1 "") $ zipWith applyL ts fcols)
   in zipWith (++) fcols $ padRWith (String1 "") $ zipWith applyL ts fcols

combine3 :: ([Term] -> Term) -> [[Term] -> Term] -> [[[Term] -> Term]] -> [[Term]] -> [[Term]]
combine3 g fs ts rows = let
   frows = map (applyL fs) rows
   fcols = transpose frows
   gcol  = map g rows
   gf    = combine (:) [] $ zip gcol fcols
   x g f = [g] : zipWith (++) f (padRWith (String1 "") $ zipWith applyL ts f)
   blah  = concatMap (transpose . padRWith (String1 "") . uncurry x) gf

   --rows2 = map (applyL fs) $ transpose rows
   --in frows ++ (transpose $ padRWith (String1 "") $ zipWith applyL ts fcols)
   in blah

readfods = do
   Right r <- parseFromFile html (importdir++"money.fods")
   return $ transpose1 $ concatMap (trimGrid2b . transpose1 . trimGrid1 . convertGrid2)
          $ drop 3 $ findTypes "table" $ nest $
            map (renameAttribs . renameTag) r

renameTag t = setType t $ case tagType t of
                     "table:table"      -> "table"
                     "table:table-row"  -> "tr"
                     "table:table-cell" -> "td"
                     x                  -> x

renameAttribs t = setAttribs t $ map renameAttrib $ attribs t

renameAttrib (a, v) = (case a of
                           "table:number-columns-repeated" -> "colspan"
                           x                               -> x
                        , v)

trimGrid1 = take 8

trimGrid1a :: [[String]] -> [[String]]
trimGrid1a = takeWhile (any (/= ""))

trimGrid1b :: [[String]] -> [[String]]
trimGrid1b = reverse . dropWhile (all (== "")) . reverse

trimGrid2 = takeWhile (\x -> x !! 2 /= "")

trimGrid2b :: [[String]] -> [[String]]
trimGrid2b = reverse . dropWhile ((!! 2) $= "") . reverse . drop 2

convertGrid1 f = map2 (parse1 csvcell f)

convertGrid2 table = let
   t = textGrid table
   n = tagAttrib "table:name" table
   l = maximum $ map length t
   in replicate l n : t

readcsvs = concat <$> mapM readCSVFile names

readCSVFile fn = do
   let fn1 = importdir ++ fn ++ ".csv"
   f <- readFile fn1
   case parse csv fn f of
      Left l -> do print l; return []
      Right r -> return $ map (String1 fn:) r

csv :: Parsec String u [[Term]]
csv = many csvline

csvline = sepBy csvcell $ char ','

fodscell = try date <|> try number <|> String1 <$> many anyChar

csvcell = try date <|> try number <|> txt

date = do
   dow <-
      string "Mon" <|>
      try (string "Tue") <|>
      string "Wed" <|>
      string "Thu" <|>
      string "Fri" <|>
      try (string "Sat") <|>
      string "Sun"
   char ' '
   dom <- int
   char '/'
   moy <- int
   char '/'
   year <- integer
   return $ Date $ ymd (year+2000) moy dom

number = Int1 <$> int <|> (Double1 <$> floating)

int = fromInteger <$> integer

txt = String1 <$> do char '"'; t <- manyTill anyChar $ char '"'; char '"'; return t

data Table = Table { fields :: M.Map String Int, tgroup :: Group }

data Field = FieldStr String Int

data Record = Record { fieldsr :: M.Map String Int, values :: T.Tree Term }

data Group = Map (M.Map Term Group) | MapInt (T.Tree Group) | Rec (T.Tree Term) deriving Show



instance Show Table where
   show = showTable

--showTable t = showGrid $ transpose $ (("":fieldsU t):) $ map (map show . uncurry (:)) $ M.toList $ records t

--showTable t = showGrid $ transpose $ (("":fieldsU t):) $ map (\(i, r) -> show i : map (\(j, t) -> show t) r) $ M.toList $ (\(Recs r) -> r) $ tgroup t
showTable t = showGrid $ transposez "" $ map (take 10) $ (fieldsU t:) $ map2 show $ ungroup $ tgroup t

ungroup (Map    m) = concatMap (ungroup . snd) $ M.toList m
ungroup (MapInt m) = concatMap (ungroup . snd) $ T.toList m
ungroup (Rec r) = [ungroup2 r]

ungroup2 r = map snd $ T.toList r

fieldsU t = map fst $ sortOn snd $ M.toList $ fields t

inversePerm indices = map snd $ sort $ zip indices [0..]

pTerm text = let
   t = takeWhile isDigit $ filter (`notElem` ", ") text

   in if null t then String1 text else Int1 $ readInt t

--fromGridG (fields:recordl) = fromGridG1 fields recordl

fromGrid fields recordl = Table (M.fromList $ zip fields [0..]) $ MapInt $ tz $ map (Rec . tz) recordl

tz :: Show a => [a] -> T.Tree a
tz = T.fromList . zip [0..]

mz = M.fromList . zip [0..]
--fromGrid1 indexFunc g = Table (head g) $ M.fromList $ map (\row -> (indexFunc row, row)) $ map2 (right . parse csvcell "") g

--convert to list of records
toList (Map  r) = concatMap (toList . snd) $ M.toList r
toList (MapInt r) = map snd $ T.toList r

byz func = mapFromList (:) [] . mapfxx func

--byy1 :: (T.Tree Term -> Term) -> [T.Tree Term] -> Group
byy1 f = Map . M.map (MapInt . tz . map Rec) . byz f

byy2 f g = Map . M.map (byy1 g) . byz f

byyn []     = MapInt . tz
byyn (f:fs) = Map . M.map (byyn fs) . byz f


by f (Table fields tgroup) = Table fields $ byy1 (f . Record fields) $ toList tgroup

--by2 f g (Table fields tgroup) = Table fields $ byy2 (f . Record fields) (g . Record fields) $ toList tgroup

--thenby f (Table flds tgroup) = Table flds $ mapRecs f flds tgroup

--mapRecs f flds (Map m) = Map $ M.map (mapRecs f flds) m
--mapRecs f flds g@(MapInt r) = byy1 (f . Record flds) $ toList g

byl fs (Table flds tgroup) = Table flds $ byyn (map (. Record flds) fs) $ toList tgroup

--byw f tab = byx ()

fieldsUR t = map fst $ sortOn snd $ M.toList $ fieldsr t

putTable = putStr . showTable

--join f l r = Table (M.fromList $ zip (fieldsU l ++ fieldsU r) [0..]) $ M.map (\v -> (v ++) $ values $ lookupt (f $ Record (fields l) v) r) $ records l

--joinMulti f l r = Table (M.fromList $ zip (fieldsU l ++ fieldsU r) [0..]) $ M.map (\v -> (v ++) $ singleton $ Table1 $ map (r !) (f $ Record (fields l) v)) $ records l

join2 f l r = mapTable (\re -> appendRec re $ r ! f re) l -- probably very inefficient

appendRec l r = Record (M.fromList $ zip (fieldsUR l ++ fieldsUR r) [0..]) $ T.append (values l) $ values r
--mapTable fieldName f l = Table (M.fromList $ zip (fieldsU l ++ [fieldName]) [0..]) $ M.map (\v -> (v ++) $ values $ f $ Record (fields l) v) $ records l

mapTable f (Table flds g) = Table (mapFields f flds g) $ mapGroup f flds g

mapGroup f fields (Map  m) = Map  $ M.map (mapGroup f fields) m
mapGroup f fields (MapInt r) = Recs $ T.treemap (mapGroupR f fields) r
mapGroupR f fields r = values $ f $ Record fields r

mapFields f flds (Map  m) = mapFields  f flds $ snd $ fromMaybe (error "Map empty" ) $ M.lookupMin m
mapFields f flds (MapInt r) = mapFieldsR f flds $ snd $ fromMaybe (error "Recs empty") $ T.lookupMin r

mapFieldsR f flds r = fieldsr $ f $ Record flds r
{-}
delField fieldName t = if head (fields t) == fieldName
   then error ("can't delete index " ++ fieldName)
   else let
      Just fieldN = elemIndex fieldName $ fields t
      in Table (deleteIndex fieldN $ fields t) $ M.fromList $ map (\(k, v) -> (k, deleteIndex (fieldN-1) v)) $ M.toList $ records t
-}
deleteIndex i l = let (b, d:a) = splitAt i l in b ++ a

class LookupR a b c where
   (!) :: a -> b -> c

instance LookupR Table Term Table where
   (!) = flip lookupgk

instance LookupR Table Int Record where
   (!) = flip lookupg2

lookupr k r = fromJust $ values r T.! fromJust (M.lookup k $ fieldsr r)

lookupgk k (Table f (Map m)) = Table f $ fromJust $ M.lookup k m

lookupg2 k (Table f (MapInt m)) = Record f $ fromJust $ T.lookup k m

--lookupgf k (Table f (Rec r)) = fromJust $ T.lookup (fromJust (M.lookup k f)) r

--unrec (Rec r) = r

r ? k = lookupr k r

