-- Copyright 2025 Brett Curtis
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
{-# LANGUAGE FunctionalDependencies #-}

module Table1G where

import Favs
import MyPretty2
import NumberParsers
import Https
import MyDynamic

import qualified Tree as T
import Data.Char
import Data.List

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.LocalTime

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator


data Type = M | To

type Amount = Int

ymd = YearMonthDay

data Trans = Trans { day::Day, amount::Amount, person::Person, typ::Type }

--go = putGridF =<< readfods

linux = True

importdir = if linux then "/home/brett/SOL/Office/" else "D:/Office/"

names = ["Tyrone", "Danny", "James"]

trans = [(ymd 2023 6 13, -20, Tyrone, M),
         (ymd 2023 6 13, -30, Tyrone, M),
         (ymd 2023 6 13, -20, James, M),
         (ymd 2023 6 13, -25, James, M),
         (ymd 2023 6 13,  -3, James, To),
         (ymd 2023 6 13, -20, Danny, M)]

--counts :: (Ord a, Show a) => [a] -> String
--counts = show . rsort . mode1
countUnique l = S.size $ S.fromList l

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
   Right r <- parseFromFile htmlP (importdir++"money.fods")
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
   t = cTextGrid table
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

dynGrid f = map2 (parse1 dynCell f)

dynCell = dynOfCell <$> fodscell

dynOfCell (String1  s) = toDyn s
dynOfCell (Int1     i) = toDyn i
dynOfCell (Integer1 i) = toDyn i
dynOfCell (Double1  d) = toDyn d
dynOfCell (Date     d) = toDyn d

fodscell = try dateExcel <|> try number <|> String1 <$> many anyChar

csvcell = try dateExcel <|> try number <|> txt

dateExcel = do
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

data Table f i r = Table { fields :: M.Map f Int, tgroup :: Group i r }

data Group i r = INode (M.Map i (Group i r)) | Recs (T.Tree (Group i r)) | Rec (T.Tree r) deriving Show

data Field = FieldStr String Int

data Record f r = Record { fieldsr :: M.Map f Int, values :: T.Tree r } deriving Show

unmap  (INode m) = m
unrecs (Recs  r) = r
unrec  (Rec   r) = r

instance Show a => Show (Table String a Dynamic) where
   show = showTable

--showTable t = showGrid $ transpose $ (("":fieldsUT t):) $ map (map show . uncurry (:)) $ M.toList $ records t

--showTable t = showGrid $ transpose $ (("":fieldsUT t):) $ map (\(i, r) -> show i : map (\(j, t) -> show t) r) $ M.toList $ (\(Recs r) -> r) $ tgroup t
showTable  t = showGrid $ zipWith (:) (fieldsUT t) $ map showColD $ transposez (toDyn "") $ ungroup $ tgroup t
showTable1 t = showGrid $ zipWith (:) (fieldsUT t) $ transposez "" $ map2 show $ ungroup $ tgroup t

showTableMeta (Table fields (INode records)) = show (M.size fields)++"x"++show (M.size records)++" "++show (fieldsU fields)

ungroup (INode m) = concatMap (ungroup . snd) $ M.toList m
ungroup (Recs r) = map (ungroup2 . snd) $ T.toList r

ungroup2 (Rec r) = map snd $ T.toList r

applygroup f (Table fs g) = Table fs $ f g

fieldsU x = map fst $ sortOn snd $ M.toList x
fieldsUT = fieldsU . fields
fieldsUR = fieldsU . fieldsr

inversePerm indices = map snd $ sort $ zip indices [0..]

pTerm text = let
   t = takeWhile isDigit $ filter (`notElem` ", ") text

   in if null t then String1 text else Int1 $ readInt t

--fromGridG (fields:recordl) = fromGridG1 fields recordl
fromGrid  g = fromGridH    $ transpose g
fromGridD f = fromGridHD f . transpose
fromGrid1 f = fromGridH1 f . transpose

fromGridH    (fs:rs) = fromGridH1 fs rs
fromGridHD f (fs:rs) = fromGridH1 (map (\x -> [f::String, x]) fs) $ map2 (parse1 dynCell f) rs
fromGridH1 :: (Ord f, Show r) => [f] -> [[r]] -> Table f Dynamic r
fromGridH1 fields recordl = Table (M.fromList $ zip fields [0..]) $ Recs $ T.fromElems $ map (Rec . T.fromElems) recordl

tz :: Show a => [a] -> T.Tree a
tz = T.fromList . zip ([0,1..]::[Int])

mz = M.fromList . zip ([0,1..]::[Int])
--fromGrid1 indexFunc g = Table (head g) $ M.fromList $ map (\row -> (indexFunc row, row)) $ map2 (right . parse csvcell "") g

--convert to list of records
toList (INode r) = concatMap (toList . snd) $ M.toList r
toList (Recs r) = map snd $ T.toList r

toList2 = map unrec . toList

byz func = mapFromList (:) [] . mapfxx func

--byy1 :: (Show j, Ord j, Show r) => (T.Tree r -> j) -> [T.Tree r] -> Group j r
byy1 f = INode . M.map (Recs . tz . map Rec) . byz f

byy2 f g = INode . M.map (byy1 g) . byz f

byyn []     = Recs . tz . map Rec
byyn (f:fs) = INode . M.map (byyn fs) . byz f

by f (Table fields tgroup) = Table fields $ byy1 (f . Record fields) $ toList2 tgroup

by2 f g (Table fields tgroup) = Table fields $ byy2 (f . Record fields) (g . Record fields) $ toList2 tgroup

thenby f (Table flds tgroup) = Table flds $ mapRecs f flds tgroup

mapRecs f flds (INode m) = INode $ M.map (mapRecs f flds) m
--mapRecs f flds g@(Recs r) = byy1 (f . Record flds) $ toList g

byl fs (Table flds tgroup) = Table flds $ byyn (map (. Record flds) fs) $ toList2 tgroup

--byw f tab = byx ()

putTable = putStr . showTable

--join f l r = Table (M.fromList $ zip (fieldsUT l ++ fieldsUT r) [0..]) $ M.map (\v -> (v ++) $ values $ lookupt (f $ Record (fields l) v) r) $ records l

--joinMulti f l r = Table (M.fromList $ zip (fieldsUT l ++ fieldsUT r) [0..]) $ M.map (\v -> (v ++) $ singleton $ Table1 $ map (r !) (f $ Record (fields l) v)) $ records l
appendRec shift (Rec l) (Rec r) = Rec $ T.union l $ shift r

join z (Table fl (INode rl)) (Table fr (INode rr)) = let
   maxl  = maximum $ map snd $ M.toList fl
   minr  = minimum $ map snd $ M.toList fr
   sh    = max 0 $ maxl - minr + 1
   shift = if sh == 0 then id else T.shift sh
   fr1   = M.fromList $ map (\(k, v) -> (k, v + sh)) $ M.toList fr
   flr   = appendFields2 fl fr1
   
   in Table flr $ INode $ joinFuzzy (appendL shift z fr, appendRec shift, appendR shift z fl) rl rr

appendL shift z fr (Rec rl) = Rec $ T.union rl $ shift (blankRec z fr)
appendR shift z fl (Rec rr) = Rec $ T.union (blankRec z fl) $ shift rr

showKey (k, v) = (k, show k, v)

data Lev dist lk rk v = Lev dist lk rk v

instance Eq dist => Eq (Lev dist lk rk v) where
   Lev a _ _ _ == Lev b _ _ _ = a == b

instance Ord dist => Ord (Lev dist lk rk v) where
   compare (Lev a _ _ _) (Lev b _ _ _) = compare a b

joinFuzzy (fl, fi, fr) l r = let
   l1 = l M.\\ r
   r1 = r M.\\ l
   i  = M.intersectionWith fi l r
   levs         = sort $ concat $ crossWith (\(lk, ls, lv) (rk, rs, rv) -> Lev (levenshtein ls rs) lk rk (lv `fi` rv)) (map showKey $ M.toList l1) (map showKey $ M.toList r1)
   (l2, i2, r2) = foldr (joinFuzzyAux 4) (l1, i, r1) levs 
   in M.map fl l2 `M.union` i2 `M.union` M.map fr r2

joinFuzzyAux maxDist (Lev dist lk rk a) (lks, res, rks) =
   if dist <= maxDist && M.member lk lks && M.member rk rks
      then (M.delete lk lks, M.insert lk a res, M.delete rk rks)
      else (lks, res, rks)


appendRecs (Recs l) (Recs r) = Recs $ T.fromElems [fromJust $ T.lookup 0 l, fromJust $ T.lookup 0 r]

blankRec z f = T.fromList $ map (\(_, n) -> (n, z)) $ M.toList f

--foldTable f z n t = 
foldSubTable fs (Table flds g) = applyL fs $ Record flds $ foldSubTableG g

foldSubTableG g = T.map toDyn $ T.untree $ toList2 g

--foldSubTable1 fs (Table flds g) = applyL fs $ Record flds $ foldSubTable1G (T.fromElems fs) g


foldSubTable2  f z t = foldSubTable2G f z $ tgroup t
foldSubTable2G f z g = T.map (foldl f z) $ T.untree $ toList2 g

p = unzip

mapSubTable f n (Table flds g) = Table (mapSubTableGF (fields . f . Table flds) n g) $ mapSubTableG (tgroup . f . Table flds) n g

mapSubTableG f 0 g       = f g
mapSubTableG f n (INode m) = INode $ M.map (mapSubTableG f (n-1)) m

mapSubTableGF f 0 g       = f g
mapSubTableGF f n (INode m) = mapSubTableGF f (n-1) $ snd $ fromMaybe (error "INode empty") $ M.lookupMin m

--join2 :: (Record f r -> i) -> Table f i r -> Table f i r -> Table f i r
--join2 f l r = mapTable (\re -> appendRec2 re $ lookupg2 0 $ lookupgk (f re) r) l -- probably very inefficient

appendRec2 l r = Record (appendFields2 (fieldsr l) (fieldsr r)) $ T.append (values l) $ values r
--mapTable fieldName f l = Table (M.fromList $ zip (fieldsUT l ++ [fieldName]) [0..]) $ M.map (\v -> (v ++) $ values $ f $ Record (fields l) v) $ records l

insertWith3 (k, v) m = M.insert (forFJ ("":map show [2..]) (\n -> let k1 = takeWhile (not . isDigit) k ++ n in case M.lookup k1 m of { Nothing -> Just k1; Just _ -> Nothing })) v m

uniquify list = foldl (flip insertWith3) M.empty $ zip list [0..]

appendFields2 l r = uniquify $ fieldsU l ++ fieldsU r



mapTable f (Table flds g) = Table (mapFields f flds g) $ mapTableG f flds g

mapTableG f fields (INode m) = INode  $ fmap (mapTableG f fields) m
mapTableG f fields (Recs  r) = Recs  $ fmap (mapTableG f fields) r
mapTableG f fields (Rec   r) = Rec   $ values $ f $ Record fields r 
mapTableR f fields r = values $ f $ Record fields r

mapFields f flds (INode m) = mapFields f flds $ snd $ fromMaybe (error "INode empty" ) $ M.lookupMin m
mapFields f flds (Recs  r) = mapFields f flds $ snd $ fromMaybe (error "Recs empty") $ T.lookupMin r
mapFields f flds (Rec   r) = fieldsr $ f $ Record flds r

mapFieldsR f flds r = fieldsr $ f $ Record flds r
{-}
delField fieldName t = if head (fields t) == fieldName
   then error ("can't delete index " ++ fieldName)
   else let
      Just fieldN = elemIndex fieldName $ fields t
      in Table (deleteIndex fieldN $ fields t) $ M.fromList $ map (\(k, v) -> (k, deleteIndex (fieldN-1) v)) $ M.toList $ records t
-}
deleteIndex i l = let (b, d:a) = splitAt i l in b ++ a

class LookupR a b c | a b -> c, c -> b where
   (!) :: a -> b -> c

instance LookupR (Table f Dynamic r) Dynamic (Table f Dynamic r) where
   (!) = flip lookupgk

instance LookupR (Table f i r) Int (Record f r) where
   (!) = flip lookupg2

lookupgk k (Table f (INode m)) = Table f $ fromJust $ M.lookup k m

lookupg2 k (Table f (Recs m)) = Record f $ unrec $ fromJust $ T.lookup k m
lookupg2 k (Table f (Rec  r)) = Record f r

--lookupgf k (Table f (Rec r)) = fromJust $ T.lookup (fromJust (M.lookup k f)) r

--unrec (Rec r) = r
lookupr :: Ord f => f -> Record f t -> t
lookupr k r = fromJust $ values r T.! (fieldsr r M.! k)
--lookupr k r = toDyn k


(?) :: Ord f => Record f t -> f -> t
(?) = flip lookupr

