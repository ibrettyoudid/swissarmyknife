{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}

module Table1 where

import Favs
import HTML
import MyPretty2
import NumberParsers

import Data.Char
import Data.Dynamic
import Data.List
import Data.Typeable

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Time.Calendar
import Data.Time.LocalTime

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String

data Person = T | J | D

data Type = M | To

type Amount = Int

ymd = YearMonthDay

data Trans = Trans {day :: Day, amount :: Amount, person :: Person, typ :: Type}

-- go = putGridF =<< readfods

linux = True

importdir = if linux then "/home/brett/SOL/Office/" else "D:/Office/"

names = ["Tyrone", "Danny", "James"]

trans =
   [ (ymd 2023 6 13, (-20), T, M)
   , (ymd 2023 6 13, (-30), T, M)
   , (ymd 2023 6 13, (-20), J, M)
   , (ymd 2023 6 13, (-25), J, M)
   , (ymd 2023 6 13, (-3), J, To)
   , (ymd 2023 6 13, (-20), D, M)
   ]

countsa :: (Ord a) => [a] -> [(Int, a)]
countsa = counts
countss :: (Show a, Ord a) => [a] -> String
countss = show . countsa
countUnique l = S.size $ S.fromList l

groupBy f = combine (:) [] . mapfxx f

applyL fs x = map ($ x) fs

combine2 :: [[Term] -> Term] -> [[[Term] -> Term]] -> [[Term]] -> [[Term]]
combine2 fs ts rows =
   let
      frows = map (applyL fs) rows
      fcols = transpose frows
      in
      -- rows2 = map (applyL fs) $ transpose rows
      -- in frows ++ (transpose $ padRWith (String1 "") $ zipWith applyL ts fcols)

      zipWith (++) fcols $ padRWith (String1 "") $ zipWith applyL ts fcols

combine3 :: ([Term] -> Term) -> [[Term] -> Term] -> [[[Term] -> Term]] -> [[Term]] -> [[Term]]
combine3 g fs ts rows =
   let
      frows = map (applyL fs) rows
      fcols = transpose frows
      gcol = map g rows
      gf = combine (:) [] $ zip gcol fcols
      x g f = [g] : zipWith (++) f (padRWith (String1 "") $ zipWith applyL ts f)
      blah = concatMap (transpose . padRWith (String1 "") . uncurry x) gf
      in
      -- rows2 = map (applyL fs) $ transpose rows
      -- in frows ++ (transpose $ padRWith (String1 "") $ zipWith applyL ts fcols)

      blah

readfods = do
   Right r <- parseFromFile htmlP (importdir ++ "money.fods")
   return $
      transpose1 $
         concatMap (trimGrid2b . transpose1 . trimGrid1 . convertGrid2) $
            drop 3 $
               findTypes "table" $
                  nest $
                     map (renameAttribs . renameTag) r

renameTag t = setType t $ case tagType t of
   "table:table" -> "table"
   "table:table-row" -> "tr"
   "table:table-cell" -> "td"
   x -> x

renameAttribs t = setAttribs t $ map renameAttrib $ attribs t

renameAttrib (a, v) =
   ( case a of
         "table:number-columns-repeated" -> "colspan"
         x -> x
   , v
   )

trimGrid1 = take 8

trimGrid1a :: [[String]] -> [[String]]
trimGrid1a = takeWhile (any (/= ""))

trimGrid1b :: [[String]] -> [[String]]
trimGrid1b = reverse . dropWhile (all (== "")) . reverse

trimGrid2 = takeWhile (\x -> x !! 2 /= "")

trimGrid2b :: [[String]] -> [[String]]
trimGrid2b = reverse . dropWhile ((!! 2) $= "") . reverse . drop 2

convertGrid1 f = map2 (parse1 csvcell f)

convertGrid2 table =
   let
      t = cTextGrid table
      n = tagAttrib "table:name" table
      l = maximum $ map length t
      in
      replicate l n : t

readcsvs = concat <$> mapM readCSVFile names

readCSVFile fn = do
   let fn1 = importdir ++ fn ++ ".csv"
   f <- readFile fn1
   case parse csv fn f of
      Left l -> do print l; return []
      Right r -> return $ map (String1 fn :) r

csv :: Parsec String u [[Term]]
csv = many csvline

csvline = sepBy csvcell $ char ','

fodscell = try date <|> try number <|> String1 <$> many anyChar

csvcell = try date <|> try number <|> txt

date = do
   dow <-
      string "Mon"
         <|> try (string "Tue")
         <|> string "Wed"
         <|> string "Thu"
         <|> string "Fri"
         <|> try (string "Sat")
         <|> string "Sun"
   char ' '
   dom <- int
   char '/'
   moy <- int
   char '/'
   year <- integer
   return $ Date $ ymd (year + 2000) moy dom

number = Int1 <$> int <|> (Double1 <$> floating)

int = fromInteger <$> integer

txt = String1 <$> do char '"'; t <- manyTill anyChar $ char '"'; char '"'; return t

data Table = Table {fields :: M.Map String Int, records :: M.Map Term [Term]} deriving (Eq, Ord, Read) -- the primary index is the key in "records"

-- data TableM = TableM { fieldsm :: M.Map String Int, recordsm :: M.Map Term (M.Map Term [Term]) }
data TableM = TableM {fieldsm :: M.Map String Int, recordsm :: M.Map Term [[Term]]}

data TableG = TableG {fieldsg :: M.Map String Int, recordsg :: Group}

data Record = Record {fieldsr :: M.Map String Int, dat :: [Term]}

data RecordM = RecordM {fieldsrm :: M.Map String Int, datm :: [[Term]]}

data Group = Map (M.Map Term Group) | Recs (M.Map Int Group) | Rec (M.Map Int Term) | Levels [Term] [[Term]]

instance Show Table where
   show = showTable

showTable t = showGrid $ transpose $ (("" : fieldsU t) :) $ map (map show . uncurry (:)) $ M.toList $ records t

fieldsU t = map fst $ sortOn snd $ M.toList $ fields t

lookupt k t = Record (fields t) $ fromJust $ M.lookup k $ records t

-- lookuptm k t = Table (fieldsm t) $ M.fromList $ mapfxx indexf $ fromJust $ M.lookup k $ recordsm t
lookuptm k t = RecordM (fieldsm t) $ fromJust $ M.lookup k $ recordsm t

lookupr k r = dat r !! fromJust (M.lookup k $ fieldsr r)

lookuprm k r = Record (fieldsrm r) $ datm r !! k

lookupgk k (TableG f (Map m)) = TableG f $ fromJust $ M.lookup k m

lookupgn k (TableG f (Recs r)) = TableG f $ fromJust (M.lookup k r)

lookupgf k (TableG f (Rec r)) = fromJust $ M.lookup (fromJust (M.lookup k f)) r

class LookupR a b c where
   (!) :: a -> b -> c

instance LookupR Table Term Record where
   t ! k = lookupt k t

instance LookupR TableM Term RecordM where
   t ! k = lookuptm k t

instance LookupR RecordM Int Record where
   r ! n = lookuprm n r

instance LookupR TableG Term TableG where
   (!) = flip lookupgk

instance LookupR TableG Int TableG where
   (!) = flip lookupgn

instance LookupR TableG String Term where
   (!) = flip lookupgf

r ? k = lookupr k r

pTerm text =
   let
      t = takeWhile isDigit $ filter (`notElem` ", ") text
      in
      if null t then String1 text else Int1 $ readInt t

fromGrid :: String -> [[String]] -> Table
fromGrid indexName g =
   let
      (fields : recordl) = g
      Just indexN = findIndex (indexName `isInfixOf`) fields
      in
      Table (M.fromList $ zip fields [0 ..]) $ M.fromList $ map (\row -> (row !! indexN, row)) $ map2 pTerm recordl

-- in Table (indexName:deleteIndex indexN fields) $ M.fromList $ map (\row -> (row !! indexN, deleteIndex indexN row)) $ map2 (right . parse csvcell "") g

fromGridN indexN g =
   let
      (fields : recordl) = g
      in
      Table (M.fromList $ zip fields [0 ..]) $ M.fromList $ map (\row -> (row !! indexN, row)) $ map2 pTerm recordl

fromGridT indexN fields g =
   let
      in -- (fields:recordl) = g

         Table (M.fromList $ zip fields [0 ..]) $ M.fromList $ map (\row -> (row !! indexN, row)) g

fromGridTM indexN indexM fields g =
   let
      in -- (fields:recordl) = g

         TableM (M.fromList $ zip fields [0 ..]) $ M.fromList $ map (\row -> (row !! indexN, map singleton $ (\(List l) -> l) $ row !! indexM)) g

-- fromGridG (fields:recordl) = fromGridG1 fields recordl

fromGridG fields recordl = TableG (M.fromList $ zip fields [0 ..]) $ Map $ M.fromList [(String1 "all", Recs $ M.fromList $ zip [0 ..] $ map (Rec . M.fromList . zip [0 ..]) recordl)]

-- fromGrid1 indexFunc g = Table (head g) $ M.fromList $ map (\row -> (indexFunc row, row)) $ map2 (right . parse csvcell "") g

fieldsUR t = map fst $ sortOn snd $ M.toList $ fieldsr t

putTable = putStr . showTable

by fieldName t = byF fieldName (!! (fromJust $ M.lookup fieldName $ fields t)) t

byF fieldName func t =
   let
      jfieldN = M.lookup fieldName $ fields t
      Just fieldN = jfieldN
      in
      -- records1 = map M.toList $ records t

      Table (fields t) $ M.fromList $ map (\(k, v) -> (func v, v)) $ M.toList $ records t

join f l r = Table (M.fromList $ zip (fieldsU l ++ fieldsU r) [0 ..]) $ M.map (\v -> (v ++) $ dat $ lookupt (f $ Record (fields l) v) r) $ records l

-- joinMulti f l r = Table (M.fromList $ zip (fieldsU l ++ fieldsU r) [0..]) $ M.map (\v -> (v ++) $ singleton $ Table1 $ map (r !) (f $ Record (fields l) v)) $ records l

join2 f l r = mapTable (\re -> appendRec re $ lookupt (f re) r) l -- probably very inefficient

appendRec l r = Record (M.fromList $ zip (fieldsUR l ++ fieldsUR r) [0 ..]) (dat l ++ dat r)

-- mapTable fieldName f l = Table (M.fromList $ zip (fieldsU l ++ [fieldName]) [0..]) $ M.map (\v -> (v ++) $ dat $ f $ Record (fields l) v) $ records l

fromRecMap map1 =
   let
      first = snd $ head $ M.toList map1
      in
      Table (fieldsr first) $ M.map dat map1

fromRecList index recs =
   let
      first = head recs
      in
      Table (fieldsr first) $ M.fromList $ mapfxx index $ map dat recs

mapTable f l =
   let
      mapped = M.map (f . Record (fields l)) $ records l
      first = snd $ head $ M.toList mapped
      in
      Table (fieldsr first) $ M.map dat mapped

{-}
delField fieldName t = if head (fields t) == fieldName
      then error ("can't delete index " ++ fieldName)
      else let
         Just fieldN = elemIndex fieldName $ fields t
         in Table (deleteIndex fieldN $ fields t) $ M.fromList $ map (\(k, v) -> (k, deleteIndex (fieldN-1) v)) $ M.toList $ records t
-}
deleteIndex i l = let (b, d : a) = splitAt i l in b ++ a
