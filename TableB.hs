-- Copyright 2025 Brett Curtis
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Eta reduce" #-}

module TableB where

import Favs hiding (levenshtein, trim, squash)
import MHashDynamic2 hiding (toList2, (?))
import MyPretty2
import NumberParsers
import ShowTuple
import Show1
import Tree qualified as T
import BString
import HTMLB
import FuzzyMatch

import Data.Char
import Prelude hiding (null, init, tail, head, elem, length, (++), (!!), toLower, split, last, take, drop, notElem, concat, takeWhile, dropWhile, putStrLn, putStr, readFile, writeFile)
import qualified Prelude
import Data.List (singleton, transpose, sort, elemIndex, findIndex, partition)

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Time.Calendar
import Data.Time.LocalTime

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String

import Control.Monad hiding (join)

import Control.DeepSeq

import System.IO.Unsafe

import GHC.Generics

import Debug.Trace

data Type = M | To

type Amount = Int

ymd = YearMonthDay

data Trans = Trans {day :: Day, amount :: Amount, person :: Person, typ :: Type}

-- go = putGridF =<< readfods

linux = True

importdir = if linux then "/home/brett/SOL/Office/" else "D:/Office/"

names = ["Tyrone", "Danny", "James"]

trans =
   [ (ymd 2023 6 13, -20, Tyrone, M)
   , (ymd 2023 6 13, -30, Tyrone, M)
   , (ymd 2023 6 13, -20, James, M)
   , (ymd 2023 6 13, -25, James, M)
   , (ymd 2023 6 13, -3, James, To)
   , (ymd 2023 6 13, -20, Danny, M)
   ]

-- counts :: (Ord a, Show a) => [a] -> String
-- counts = show . rsort . mode1
countUnique l = S.size $ S.fromList l

groupBy f = combine (:) [] . mapfxx f

applyL fs x = map ($ x) fs

combine2 :: [[Term] -> Term] -> [[[Term] -> Term]] -> [[Term]] -> [[Term]]
combine2 fs ts rows = let
   frows = map (applyL fs) rows

   fcols = transpose frows
   in
   -- rows2 = map (applyL fs) $ transpose rows
   -- in frows ++ (transpose $ padRWith (String1 "") $ zipWith applyL ts fcols)

   zipWith (++) fcols $ padRWith (String1 "") $ zipWith applyL ts fcols

combine3 :: ([Term] -> Term) -> [[Term] -> Term] -> [[[Term] -> Term]] -> [[Term]] -> [[Term]]
combine3 g fs ts rows = let
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
{-
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
-}
{-
trimGrid1 x = take 8 x

trimGrid1a :: [[String]] -> [[String]]
trimGrid1a = takeWhile (any (/= ""))

trimGrid1b :: [[String]] -> [[String]]
trimGrid1b = reverse . dropWhile (all (== "")) . reverse

trimGrid2 = takeWhile (\x -> x !! 2 /= "")

trimGrid2b :: [[String]] -> [[String]]
trimGrid2b = reverse . dropWhile ((!! 2) $= "") . reverse . drop 2
-}
convertGrid1 f = map2 (parse1 csvcell f)
{-
convertGrid2 table =
let
   t = cTextGrid table
   n = tagAttrib "table:name" table
   l = maximum $ map length t
   in
   replicate l n : t
-}
readcsvs = concat <$> mapM readCSVFile names

readCSVFile fn = do
   let fn1 = importdir ++ fn ++ ".csv"
   f <- readFile fn1
   case parse csv fn f of
      Left l -> do print l; return []
      Right r -> return $ map (String1 fn :) r

csv = many csvline

csvline = sepBy csvcell $ char ','


dynCell  = parse1 (try (toDyn <$> dateExcel) <|> try numberDyn <|> toDyn . bof <$> many anyChar) "dynCell"

fodscell = try (Date <$> dateExcel) <|> try numberC <|> (String1 <$> many anyChar)

csvcell  = try (Date <$> dateExcel) <|> try number <|> txt

wikicell = try (Date <$> dateExcel) <|> try numberC <|> txt

number = Int1 <$> int <|> (Double1 <$> floating)

int = fromInteger <$> integer

numberC = Int1 <$> intC <|> (Double1 <$> floating)

numberDyn = try (toDyn <$> forceFloating) <|> try (toDyn <$> intC)

intC = fromInteger <$> integerC

atleast n x r = try $ do
   z <- try $ foldr ((\a b -> do w <- a; v <- optionMaybe b; return $ case v of { Just j -> w:j; Nothing -> [w]}) . (\b -> char b <|> char (toUpper b))) (return []) x
   guard (length z >= n)
   return r

dateExcel = try (do
   optional (do
      try $ choice $ zipWith (atleast 2) ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"] [1..7]
      char ' ')
   dom1 <- dom
   choice [char '/', char '-', try (do string " of "; return 'o'), char ' ']
   moy1 <- moy
   oneOf "/- "
   year <- integer
   return $ ymd (if  | year <  40 -> year + 2000
                     | year < 100 -> year + 1900
                     | otherwise  -> year) moy1 dom1)
   <|> try (do
      optional (do
         try $ choice $ zipWith (atleast 2) ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"] [1..7]
         char ' ')
      moy1 <- moy
      char '/' <|> char '-' <|> try (do string " the "; return 't') <|> char ' '
      dom1 <- dom
      oneOf "/- "
      year <- integer
      return $ ymd (if  | year <  40 -> year + 2000
                        | year < 100 -> year + 1900
                        | otherwise  -> year) moy1 dom1)



dom = do
   dom1 <- int
   guard (dom1 <= 31)
   dmark <- try (string "st" <|> string "nd" <|> string "th" <|> string "rd") <|> string ""
   return dom1


moy = do
   moy1 <- (try $ choice $ zipWith (atleast 3) ["january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"] [1..12]) <|> int
   guard (moy1 <= 12)
   return moy1


txt = String1 <$> do char '"'; t <- manyTill anyChar $ char '"'; char '"'; return t

data Table f i r = Table {fields :: M.Map f Int, tgroup :: Group i r} deriving (Generic, NFData)

data Group i r = INode (M.Map i (Group i r)) | Recs (T.Tree (Group i r)) | Rec (T.Tree r) deriving (Eq, Ord, Show, Generic, NFData)

data Field = FieldStr { fname::String, fnum :: Int }

data Record f r = Record {fieldsr :: M.Map f Int, values :: T.Tree r} deriving (Show, Generic, NFData)

class UniqueList a where
   uniquify :: (Enum b, Num b) => [(a, b)] -> M.Map a b
   showField :: a -> [String]

instance UniqueList String where
   uniquify = foldl (flip insertWith4) M.empty
   showField x = [x]

instance UniqueList ByteString where
   uniquify = foldl (flip insertWith4) M.empty
   showField x = [show x]

instance (Ord a, Ord b, Ord c, Ord d, Show a, Show b, Show c, Show d) => UniqueList (a, b, c, d) where
   uniquify = M.fromList
   showField = showT

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Show a, Show b, Show c, Show d, Show e) => UniqueList (a, b, c, d, e) where
   uniquify = M.fromList
   showField = showT

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => UniqueList (a, b, c, d, e, f, g, h, i, j) where
   uniquify = M.fromList
   showField = showT

-- Tables contain INodes or Recses
-- INodes contain Recses or Recs
-- Recses contain Recs or INodes

empty = Table M.empty $ Rec T.Empty

unmap (INode m) = m
unrecs (Recs r) = r
unrec (Rec r) = r

instance {-# OVERLAPPING #-} Show1 ByteString where
   show1 = convertString

instance {-# OVERLAPPING #-} (Show a) => Show (Table String a Dynamic) where
   show = showGrid . showTable2

instance (Show i, Show r) => Show (Table String i r) where
   show = showTable1

instance (Show a, Show b, Show c, Show d) => Show (Table (a, b, c, d) Dynamic Dynamic) where
   show t = showGrid $ {-zipWith (++) (map showT $ fieldsUT t) $-} map showColD $ transposez (toDyn "") $ ungroup $ tgroup t

instance (Show a, Show b, Show c, Show d, Show e) => Show (Table (a, b, c, d, e) Dynamic Dynamic) where
   show t = showGrid $ zipWith (++) (map showT $ fieldsUT t) $ map showColD $ transposez (toDyn "") $ ungroup $ tgroup t

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => Show (Table (a, b, c, d, e, f, g, h, i, j) z Dynamic) where
   show t = showGrid $ zipWith showColDH (map showT $ fieldsUT t) $ transposez (toDyn "") $ ungroup $ tgroup t
   --show t = showGridD colWidths5 showGrid1 width $ zipWith (++) (map (map toDyn . showT) $ fieldsUT t) $ transposez (toDyn "") $ ungroup $ tgroup t

showTable  t = showTableC 0 15 t
showTableC cs cn t = showGrid $ drop cs $ take cn $ showTable2 t
showTable1 t = showGrid $ zipWith (++) (map showField $ fieldsUT t) $ transposez "" $ map2 show $ ungroup $ tgroup t
showTable2 t = zipWith (++) (map showField $ fieldsUT t) $ map showColD $ transposez (toDyn "") $ ungroup $ tgroup t

toCsv t = unlines $ map (intercalate ",") $ (transpose (map showField $ fieldsUT t)++) $ map2 show $ ungroup $ tgroup t

showTableMeta (Table fields (INode records)) = show (M.size fields) ++ "x" ++ show (M.size records) ++ " " ++ show (fieldsU fields)
showTableMeta (Table fields (Recs  records)) = "NO INDEX " ++ show (M.size fields) ++ "x" ++ show (T.size records) ++ " " ++ show (fieldsU fields)
showTableMeta (Table fields (Rec   record )) = "NO INDEX " ++ show (M.size fields) ++ "x" ++ show 1 ++ " " ++ show (fieldsU fields)
showTableMeta1 (Table fields (INode records)) = show (M.size fields) ++ "x" ++ show (M.size records)
showTableMeta1 (Table fields (Recs  records)) = "NO INDEX " ++ show (M.size fields) ++ "x" ++ show (T.size records)
showTableMeta1 (Table fields (Rec   record )) = "NO INDEX " ++ show (M.size fields) ++ "x" ++ show 1
showTableMeta2 t = GridH $ map showT $ fieldsUT t

setFields newnames (Table fields group) = let
   (names, numbers) = unzip $ sortOn snd $ M.toList fields
   names1 = newnames ++ drop (length newnames) names

   in Table (M.fromList $ zip names1 numbers) group

setFieldsD newnames = setFields (map toDyn newnames)

filterFields pred t = let
   fieldsList = M.toList $ fields t
   fieldsChosen = filter (pred . fst) fieldsList
   fieldsNameToNumMap = M.fromList fieldsChosen
   fieldsNumSet = S.fromList $ map snd fieldsChosen
   func record = Record fieldsNameToNumMap $ T.fromList $ filter (\(k, v) -> S.member k fieldsNumSet) $ T.toList $ values record
      
   in mapTable func t

size t = length $ ungroup $ tgroup t

ungroup (INode m) = concatMap (ungroup . snd) $ M.toList m
ungroup (Recs r) = map (ungroup2 . snd) $ T.toList r
ungroup (Rec r) = [ungroup2 (Rec r)]

ungroup2 (Rec r) = map snd $ T.toList r

applygroup f (Table fs g) = Table fs $ f g

fieldsU x = map fst $ sortOn snd $ M.toList x
fieldsUT = fieldsU . fields
fieldsUR = fieldsU . fieldsr

inversePerm indices = map snd $ sort $ zip indices [0 ..]

fromGrid g = fromGridH $ transpose g
fromGridD = fromGridHD . transpose
fromGrid1 f = fromGridH1 f . transpose

fromGridH  (fs : rs) = fromGridH1 fs rs
fromGridHD []        = TableB.empty
fromGridHD (fs : rs) = fromGridH1 (map clean fs) $ map2 (dynCell . clean) rs

fromGridHD4 u n (fs : rs) = fromGridH1 (zipWith (\x y -> (u, n, x, clean y)) [1..] fs) $ map2 (dynCell . clean) rs

fromGridHD5 c u n (fs : rs) = fromGridH1 (zipWith (\x y -> (c, u, n, x, clean y)) [1..] fs) $ map2 (dynCell . clean) rs

fromGridHD10 c u n [] = TableB.empty
fromGridHD10 c u n (fs : rs) = fromGridH1 (zipWith (\x y -> (c !! 0, c !! 1, c !! 2, c !! 3, c !! 4, c !! 5, u, n, x, clean y)) [1..] fs) $ map2 (dynCell . clean) rs

fromGridH1 :: (UniqueList f, Ord f, Show r) => [f] -> [[r]] -> Table f Dynamic r
fromGridH1 fields recordl = Table (uniquify $ zip fields [0..]) $ Recs $ T.fromElems $ map (Rec . T.fromElems) recordl

tz :: (Show a) => [a] -> T.Tree a
tz = T.fromList . zip [0..]

mz = M.fromList . zip [0..]

-- fromGrid1 indexFunc g = Table (head g) $ M.fromList $ map (\row -> (indexFunc row, row)) $ map2 (right . parse csvcell "") g

-- convert to list of records
toList (INode r) = concatMap (toList . snd) $ M.toList r
toList (Recs  r) = concatMap (toList . snd) $ T.toList r
toList r@(Rec _) = [r]

toList2 = map unrec . toList

scrub = smap (\c -> if (c >= 65 && c <= 90) || (c >= 97 && c <= 122) then c else 32)

--cleanDyn x = read $ clean $ show x
autoJoin joinType maxDist bzero master tab =
   case findIndexField maxDist bzero master tab of
      Just indexField -> joinFuzzy joinType maxDist bzero master $ byscrub (? indexField) tab
      Nothing -> error "could not find a matching index field"

autoIndex maxDist bzero master tab = 
   case findIndexField maxDist bzero master tab of
      Just indexField -> byscrub (? indexField) tab
      Nothing -> error "could not find a matching index field"

findIndexField maxDist bzero master tab =
   snd <$> (
      find ((> fromIntegral (size tab) / 4) . fromIntegral . fst)
         $ map (\f -> (\r -> (size r, f))
         $ join jInner bzero master
         $ byscrub (? f) tab) 
         $ map fst
         $ M.toList
         $ fields tab)

byz func = mapFromList (:) [] . mapfxx func

-- byy1 :: (Show j, Ord j, Show r) => (T.Tree r -> j) -> [T.Tree r] -> Group j r
byy1 f = INode . M.map (Recs . tz . map Rec) . byz f

byy2 f g = INode . M.map (byy1 g) . byz f

byyn [] = Recs . tz . map Rec
byyn (f : fs) = INode . M.map (byyn fs) . byz f

byscrub f = by (trim . squash . scrub . convertString . show . f)

by f (Table fields tgroup) = Table fields $ byy1 (f . Record fields) $ toList2 tgroup

by2 f g (Table fields tgroup) = Table fields $ byy2 (f . Record fields) (g . Record fields) $ toList2 tgroup

thenby f (Table flds tgroup) = Table flds $ mapRecs f flds tgroup

mapRecs f flds (INode m) = INode $ M.map (mapRecs f flds) m

-- mapRecs f flds g@(Recs r) = byy1 (f . Record flds) $ toList g

byl fs (Table flds tgroup) = Table flds $ byyn (map (. Record flds) fs) $ toList2 tgroup

-- byw f tab = byx ()

putTable x = putStr $ showTable x

-- join f l r = Table (M.fromList $ zip (fieldsUT l ++ fieldsUT r) [0..]) $ M.map (\v -> (v ++) $ values $ lookupt (f $ Record (fields l) v) r) $ records l

-- joinMulti f l r = Table (M.fromList $ zip (fieldsUT l ++ fieldsUT r) [0..]) $ M.map (\v -> (v ++) $ singleton $ Table1 $ map (r !) (f $ Record (fields l) v)) $ records l
jInner = (True , False, False)
jLeft  = (True , True , False)
jRight = (True , False, True )
jOuter = (True , True , True )

join include empty l r = let
   (flr, j1) = joinClear empty l r

   in Table flr $ INode $ joinInclude include j1

foldlj f xs = foldl f (head xs, []) (tail xs)

miss x = (x, [])

joinCollectMisses include empty (l, ml) (r, mr) = let
   (flr, j1@(i, l1, r1)) = joinClear empty l r
   
   in (Table flr $ INode $ joinInclude include j1, Table (fields r) (INode r1):(ml++mr))

joinFuzzy include maxDist empty l r = let
   (flr, j1) = joinFuzzy1 maxDist empty l r

   in Table flr $ INode $ joinInclude include j1

joinAux empty a b = {-trace (show il ++ " " ++ show ir) -}res
   where
      Table fieldsl il = a
      Table fieldsr ir = b

      INode rl = il
      INode rr = ir

      fieldnumsl = map snd $ M.toList fieldsl
      fieldnumsr = map snd $ M.toList fieldsr
      minl = minimum fieldnumsl
      maxl = maximum fieldnumsl
      minr = minimum fieldnumsr
      maxr = maximum fieldnumsr
      shift = maxl - minr + 1
      fieldsr1 = M.map (+shift) fieldsr
      fieldslr = appendFields fieldsl fieldsr1
      fl = joinL   shift empty fieldsr1
      fi = joinRec shift
      fr = joinR   shift empty fieldsl
      l1 = rl M.\\ rr
      r1 = rr M.\\ rl
      i = M.intersectionWith fi rl rr

      res = (fieldslr, i, l1, r1, fi, fl, fr)


--index xs = Table (M.fromList [("empty", 0)]) $ INode $ M.fromList $ map (, Rec $ T.fromElems [toDyn ""]) xs
index xs = Table (M.fromList [("empty", 0)]) $ INode $ M.fromList $ map (, Recs $ tz [Rec $ T.fromElems [toDyn ""]]) xs

appendFields fieldsl fieldsr = uniquify $ M.toList fieldsl ++ M.toList fieldsr

joinRec shift (Rec  l) (Rec  r) = Rec $ T.append shift l r
joinRec shift (Recs l) (Recs r) = Recs $ tz $ concat $ crossWith (\(Rec l1) (Rec r1) -> Rec $ T.append shift l1 r1) (T.toElems l) (T.toElems r)
joinRec shift l        r        = error $ "joinRec called with "++show l++" and "++show r

joinL shift z fr (Rec  rl) = Rec  $ T.append shift rl (blankRec z fr)
joinL shift z fr (Recs rl) = Recs $ T.map (joinL shift z fr) rl

joinR shift z fl (Rec  rr) = Rec  $ T.append shift (blankRec z fl) rr
joinR shift z fl (Recs rr) = Recs $ T.map (joinR shift z fl) rr

showKey (k, v) = (k, show k, v)

data Fuzzy dist lk rk v = Fuzzy dist lk rk v deriving (Show)

instance (Eq dist) => Eq (Fuzzy dist lk rk v) where
   Fuzzy a _ _ _ == Fuzzy b _ _ _ = a == b

instance (Ord dist) => Ord (Fuzzy dist lk rk v) where
   compare (Fuzzy a _ _ _) (Fuzzy b _ _ _) = compare a b

joinClear empty l r =
   let
      (flr, i, l1, r1, fi, fl, fr) = joinAux empty l r
   
   in (flr, (i, M.map fl l1, M.map fr r1))

joinFuzzy1 maxDist empty l r = let
   (flr, i, l1, r1, fi, fl, fr) = joinAux empty l r
   (a, b) = unzip $ fuzzyJoin1 maxDist (map showKey $ M.toList l1) (map showKey $ M.toList r1)
   (lks, rks) = unzip a
   --levs = sort $ concat $ crossWith (\(lk, ls, lv) (rk, rs, rv) -> Fuzzy (fuzzyMatch maxDist ls rs) lk rk (lv `fi` rv)) (map showKey $ M.toList l1) (map showKey $ M.toList r1)
   --(l2, i2, r2) = foldr (joinFuzzyAux maxDist) (l1, i, r1) levs
   
   l2 = l1 M.\\ (M.fromList $ map (, 0) lks)
   r2 = r1 M.\\ (M.fromList $ map (, 0) rks)
   i2 = M.fromList $ zipWith joinFuzzyAux lks rks

   joinFuzzyAux lk rk = let
      Just lv = M.lookup lk l1
      Just rv = M.lookup rk r1
      in (lk, lv `fi` rv)

   in trace (show (M.size i) ++ " " ++ show (M.size l1) ++ " " ++ show (M.size r1)) (flr, (i2, M.map fl l2, M.map fr r2))

joinInclude (ii, il, ir) (ri, rl, rr) =
   (if ii then M.union ri else id) $
   (if il then M.union rl else id) $
   (if ir then rr else M.empty)

addCalcField name func table = let
   i = (maximum $ map snd $ M.toList $ fields table) + 1
   
   in mapTable (\r@(Record fields rt) -> Record (M.insert name i fields) $ T.insert i (func r) rt) table

blankRec z f = T.fromList $ map (\(_, n) -> (n, z)) $ M.toList f

appendTable joinType (Table tfs tr) (Table bfs br) = let
   fmap = joinFields joinType tfs bfs
   in Table tfs $ appendTableG tr $ translateTableG fmap br -- fields are similar enough

joinFields joinType tfs bfs = let
   tfs1 = M.fromList $ map (\((a, b, c, d, e, f, g, h, i, j), k) -> ((a, b, c, d, e, f, g, j), k)) $ M.toList tfs
   bfs1 = M.fromList $ map (\((a, b, c, d, e, f, g, h, i, j), k) -> ((a, b, c, d, e, f, g, j), k)) $ M.toList bfs
   maxt = maximum $ map snd $ M.toList tfs
   fmap = map snd $ M.toList $ unionWith3A joinType (,) (-1,) (,-1) tfs1 bfs1
   (f, g) = partition ((< 0) . fst) fmap
   f1 = zip [maxt+1..] $ map snd f
   in M.fromList $ map tflip (f1 ++ g)

appendTableG (INode ts) (INode bs) = INode $ M.unionWith appendTableG ts bs
appendTableG (Recs  ts) (Recs  bs) = Recs  $ T.append (T.minKey bs - T.maxKey ts + 1) ts bs
appendTableG (Rec   ts) (Rec   bs) = Rec   $ T.union ts bs

translateTableG fmap (INode bs) = INode $ M.map (translateTableG fmap) bs
translateTableG fmap (Recs  bs) = Recs  $ T.map (translateTableG fmap) bs
translateTableG fmap (Rec   bs) = Rec   $ T.fromList $ M.toList $ M.compose (M.fromList $ T.toList bs) fmap

-- foldTable f z n t =
foldSubTable fs (Table flds g) = applyL fs $ Record flds $ foldSubTableG g

foldSubTableG g = T.untree $ toList2 g

-- foldSubTable1 fs (Table flds g) = applyL fs $ Record flds $ foldSubTable1G (T.fromElems fs) g
addTotals t = Table (fields t) $ INode $ M.insert (toDyn "ZZZZ") (Rec $ foldSubTable2 sum t) g where INode g = tgroup t
foldSubTable2 f t = foldSubTable2G f $ tgroup t
foldSubTable2G f g = T.map f $ T.untree $ toList2 g

--addTotals3 t = Table (fields t) $ foldSubTable3G sum $ tgroup t
foldSubTable3 f t = Table (fields t) $ snd $ foldSubTable3G (foldSubTable3R f) $ tgroup t

foldSubTable3G f (INode rs) = let
   (keys, vals) = unzip $ M.toList $ M.map (foldSubTable3G f) rs
   (totals, rebuild) = unzip vals
   newtotal = f totals
   
   in (newtotal, INode $ (if length totals > 1 then M.insert (toDyn "ZZZZ") (Rec newtotal) else id) $ M.fromList $ zip keys rebuild)

foldSubTable3G f (Recs rs) = let
   (totals, rebuild) = T.unzip $ T.map (foldSubTable3G f) rs
   newtotal = f $ T.toElems totals
   
   in (newtotal, Recs $ if T.size totals > 1 then T.insert ((1+) $ snd $ T.span rebuild) (Rec newtotal) rebuild else rebuild)
--foldSubTable3G f (Recs rs) = T.map f $ T.untree $ T.toElems $ T.map (foldSubTable3G f) rs

foldSubTable3G f r@(Rec fs) = (fs, r)

foldSubTable3R f rs = T.map f $ T.untree rs

p = unzip

mapSubTable f n (Table flds g) = Table (mapSubTableGF (fields . f . Table flds) n g) $ mapSubTableG (tgroup . f . Table flds) n g

mapSubTableG f 0 g = f g
mapSubTableG f n (INode m) = INode $ M.map (mapSubTableG f (n - 1)) m

mapSubTableGF f 0 g = f g
mapSubTableGF f n (INode m) = mapSubTableGF f (n - 1) $ snd $ fromMaybe (error "INode empty") $ M.lookupMin m

-- join2 :: (Record f r -> i) -> Table f i r -> Table f i r -> Table f i r
-- join2 f l r = mapTable (\re -> appendRec2 re $ lookupg2 0 $ lookupgk (f re) r) l -- probably very inefficient

-- mapTable fieldName f l = Table (M.fromList $ zip (fieldsUT l ++ [fieldName]) [0..]) $ M.map (\v -> (v ++) $ values $ f $ Record (fields l) v) $ records l

insertWith3 (k, v) m = M.insert (forFJ ("" : map show [2 ..]) (\n -> let k1 = reverse (dropWhile isDigit $ reverse k) ++ n in case M.lookup k1 m of Nothing -> Just k1; Just _ -> Nothing)) v m

insertWith4 (k, v) m =
   case M.lookup k m of
      Just j -> let
         k1 = takeWhile (/= convertChar '_') k -- reverse (dropWhile isDigit $ reverse k)
         n1 = readNum $ convertString $ drop (length k1 + 1) k
         k2 = head $ mapMaybe (\n2 -> let
            k3 = k1 ++ cons (convertChar '_') (c $ show n2)
            in case M.lookup k3 m of
               Just j2 -> Nothing
               Nothing -> Just k3) [n1+1..]
         in M.insert k2 v m
      Nothing -> M.insert k v m

rebalance t = mapTable (\(Record f r) -> Record f $ T.rebalance r) 

mapTable f (Table fields g) = Table (mapTableF f fields g) $ mapTableG f fields g

mapTableG f fields (INode m) = INode $ fmap (mapTableG f fields) m
mapTableG f fields (Recs t) = Recs $ fmap (mapTableG f fields) t
mapTableG f fields (Rec r) = Rec $ values $ f $ Record fields r
mapTableR f fields r = values $ f $ Record fields r

mapTableF f fields (INode m) = mapTableF f fields $ snd $ fromMaybe (error "INode empty") $ M.lookupMin m
mapTableF f fields (Recs r) = mapTableF f fields $ snd $ fromMaybe (error "Recs empty") $ T.lookupMin r
mapTableF f fields (Rec r) = fieldsr $ f $ Record fields r

mapFieldsR f flds r = fieldsr $ f $ Record flds r

{-}
delField fieldName t = if head (fields t) == fieldName
   then error ("can't delete index " ++ fieldName)
   else let
      Just fieldN = elemIndex fieldName $ fields t
      in Table (deleteIndex fieldN $ fields t) $ M.fromList $ map (\(k, v) -> (k, deleteIndex (fieldN-1) v)) $ M.toList $ records t
-}
class LookupR a b c | a b -> c, c -> a where
   lookup :: a -> b -> c

instance LookupR Dynamic (Table f Dynamic r) (Table f Dynamic r) where
   lookup = lookupgk

instance LookupR Int (Table f i r) (Record f r) where
   lookup = lookupg2

t ! k = fromMaybe (error "Table lookup failed") $ TableB.lookup k t

lookupgk k (Table f (INode m)) = Table f $ fromJust $ M.lookup k m

lookupg2 k (Table f (Recs m)) = Record f $ unrec $ fromJust $ T.lookup k m
lookupg2 k (Table f (Rec r)) = Record f r

-- lookupgf k (Table f (Rec r)) = fromJust $ T.lookup (fromJust (M.lookup k f)) r

-- unrec (Rec r) = r
lookupr :: (Ord f, Show f, Show t) => f -> Record f t -> t
lookupr k r =
   case M.lookup k $ fieldsr r of
      Nothing -> error $ "failed to find field name "++show k++" in record "++show r
      Just n ->
         case T.lookup n $ values r of
            Just j  -> j
            Nothing -> error $ let
               s = unlines $ T.showTree1 $ values r
               i = unsafePerformIO $ putStr s
      
               in if i == () then "failed to find field "++show k++" number "++show n++" in record "++show r else "UH OH"

-- lookupr k r = toDyn k

(?) :: (Ord f, Show f, Show t) => Record f t -> f -> t
(?) = flip lookupr

--(??) :: (Ord [f]) => Record [f] t -> [f] -> t
--r ?? k = fromJust $ (values r T.!) $ snd $ fromJust $ find (isInfixOf k . map toLower . fst) $ M.toList $ fieldsr r
