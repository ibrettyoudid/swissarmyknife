-- Copyright 2025 Brett Curtis
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use first" #-}

module TableTuple where

import Favs hiding (levenshtein)
import MHashDynamic3 hiding (toList2, (?))
import MyPretty2
import NewTuple
import ShowTuple hiding (mapT)
import Show1
import qualified Tree
import BString
import HTML
import FuzzyMatch
import Colour
import Text.Parsec 
import qualified NumberParsers as NP

import Data.Char
import Prelude hiding (null, init, tail, head, elem, length, (++), (!!), toLower, split, last, take, drop, notElem, concat, takeWhile, dropWhile, putStrLn, putStr, readFile, writeFile, maximum)
import qualified Prelude
import Data.List (singleton, transpose, sort, elemIndex, findIndex, partition, unfoldr)
import qualified Data.List as L
import Maximum

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.LocalTime

--import Data.Attoparsec.Text hiding (take, takeWhile)

import Control.Monad hiding (join)

import Control.DeepSeq

import System.IO.Unsafe
import System.Process

import GHC.Generics
import GHC.Stack

import Debug.Trace
import Dataflow2 (TermType)

type Parser a = Parsec TString () a

linux = True

importdir = if linux then "/home/brett/SOL/Office/" else "D:/Office/"

data UID   = UID   deriving (Eq, Ord, Show, Read)
data PID   = PID   deriving (Eq, Ord, Show, Read)
data PPID  = PPID  deriving (Eq, Ord, Show, Read)
data STIME = STIME deriving (Eq, Ord, Show, Read)
data TTY   = TTY   deriving (Eq, Ord, Show, Read)
data TIME  = TIME  deriving (Eq, Ord, Show, Read)
data CMD   = CMD   deriving (Eq, Ord, Show, Read)

ps :: IO (Table (UID :- PID :- PPID :- NewTuple.C :- STIME :- TTY :- TIME :- ()) Int (String :- String :- String :- String :- String :- String :- String :- ()))
ps = do
   text <- readProcess "ps" ["-ef"] ""
   return $ fromGridH1 (UID :- PID :- PPID :- C :- STIME :- TTY :- TIME :- ()) $ transpose $ take 7 $ transpose $ cols $ lines text -- $ cols $ lines text

data NFileMode = NFileMode deriving (Eq, Ord, Show, Read)
data N         = N         deriving (Eq, Ord, Show, Read)
data User      = User      deriving (Eq, Ord, Show, Read)
data UGroup    = UGroup    deriving (Eq, Ord, Show, Read)
data Size      = Size      deriving (Eq, Ord, Show, Read) 
data ModTime   = ModTime   deriving (Eq, Ord, Show, Read)
data Name      = Name      deriving (Eq, Ord, Show, Read)

addCommas x = let
   l = length x
   il = l `mod` 3
   (b, a) = L.splitAt il x
   gs = if il == 0 then groupN 3 a else b : groupN 3 a
   in intercalate "," gs

j68 [c1, c2, c3, c4, c5, (t6, g6), (t7, g7), (t8, g8), c9] = [c1, c2, c3, c4, c5, (t6+g6+t7+g7+t8, g8), c9]
--ls :: IO (Table (NFileMode :- N :- User :- UGroup :- Size :- ModTime :- Name :- ()) Int (String :- Int :- String :- String :- Int :- String :- String :- ()))

lsD = do
   text <- readProcess "ls" ["-al", "--color=always"] ""
   let colourText = readColour text
   let lines1 = linesColour colourText
   let lines2 = tail lines1
   mapM_ (print . decolourStr) lines2
   let cspec0 = cspecC lines2
   print cspec0
   let cspec1 = j68 $ cspecC lines2
   --return $ fromGridH1 (NFileMode :- N :- User :- UGroup :- Size :- ModTime :- Name :- ()) $ 
   let x = transposez [] $ map (map trimC . slice cspec1) lines2
   editGrid x

ls = do
   text <- readProcess "ls" ["-al", "--color=always"] ""
   let colourText = readColour text
   let lines1 = linesColour colourText
   let lines2 = tail lines1
   let cspec1 = j68 $ cspecC lines2
   --return $ fromGridH1 (NFileMode :- N :- User :- UGroup :- Size :- ModTime :- Name :- ()) $ 
   let x = transposez [] $ map (map trimC . slice cspec1) lines2
   editGrid x

isSpaceC (c,  x) = isSpace1 x

trimC = trimtrailingC . L.dropWhile isSpaceC

trimtrailingC [] = []
trimtrailingC (x : xs) = let xst = trimtrailingC xs in if isSpaceC x && null xst then [] else x : xst

linesColour s = cons (case break (\(_, c) -> c == '\n') s of
                              (l, s') -> (l, case s' of
                                                []      -> []
                                                _:s''   -> linesColour s''))
   where
      cons ~(h, t) = h : t

cols lines1 = let
   cspec1 = cspec lines1
   in map (map trim . slice cspec1) lines1

slice cspec line1 = refold slice1 line1 cspec

slice1 line1 (text, gap) = let
   (t, g) = L.splitAt text line1
   (g1, r) = L.splitAt gap g
   in (t, r)

cspec lines1 = let
   linest    = transposez ' ' lines1
   le        = length lines1
   allspaces = map (all (== ' ')) linest
   f []        = Nothing
   f allspaces = let
      (text, rest1) = L.span not allspaces
      (gap , rest2) = L.span id rest1
      in Just ((length text, length gap), rest2)
   in unfoldr f allspaces

cspecC lines1 = let
   linest    = transposez (normal, ' ') lines1
   le        = length lines1
   allspaces = map (all (\(_, c) -> c == ' ')) linest
   f []        = Nothing
   f allspaces = let
      (text, rest1) = L.span not allspaces
      (gap , rest2) = L.span id rest1
      in Just ((length text, length gap), rest2)
   in unfoldr f allspaces

data Table f i r = Table { fields :: f, tgroup :: Group i r} deriving (Generic, NFData)

data Group i r = INode (M.Map i (Group i r)) | Recs (Tree.Tree (Group i r)) | Rec r deriving (Eq, Ord, Show, Generic, NFData)

data Record f r = Record {fieldsr :: f, values :: r} deriving (Show, Generic, NFData)

empty = Table $ Recs Tree.empty

unmap  (INode m) = m
unrecs (Recs  r) = r
unrec  (Rec   r) = r

type TString = String

instance (ShowFields f, ProperTuple r, ShowNewTuple r) => Show (Table f i r) where
   show = showTableNew

{-
showTable t =  showGridD colWidths1 width $ 
               zipWith (++) (map (map toDyn . showT) $ fieldsUT t) $ 
               transposez (toDyn (""::Text)) $ 
               ungroupFill (map (\(_, v) -> (v, toDyn (""::Text))) $ M.toList $ fields t) $
               table t
-}

class ShowField a where
   showField :: a -> [TString]

class ShowFields a where
   showFields :: a -> [[TString]] 

instance Show a => ShowField a where
   showField a = [show a]
{-}
instance ShowNewTuple a => ShowField a where
   showField a = showNewT a
-}
instance ShowFields () where
   showFields () = []

instance (ShowField a, ProperTuple b, ShowFields b) => ShowFields (a :- b) where
   showFields (a :- b) = showField a : showFields b

class ReadRec a where
   readRec :: [TString] -> a

instance ReadRec () where
   readRec [] = ()
   readRec _  = ()

instance (ReadLax a, ProperTuple b, ReadRec b) => ReadRec (a :- b) where
   readRec (a : b) = readLax a :- readRec b

class ReadLax a where
   readLax :: TString -> a

instance ReadLax String where
   readLax = id

instance ReadLax Int where
   readLax s = NP.parse1 int "instance" s

showTableNew :: (ShowFields f, ShowNewTuple r, HasCallStack) => Table f i r -> String
showTableNew t =  showGrid $ 
                  zipWith (++) (showFields $ fields t) $ 
                  transposez (""::String) $ 
                  map showNewT $
                  ungroup $
                  tgroup t

showTable t = showTable1 t
showTableC cs cn t = showGrid $ drop cs $ take cn $ showTable2 t
showTable1 t = showGrid $ zipWith (++) (map showField $ fieldsUT t) $ transposez "" $ map2 show $ ungroup $ tgroup t
showTable2 t = zipWith (++) (map showField $ fieldsUT t) $ transposez (""::String) $ map showT $ ungroup $ tgroup t

toCsv t = unlines $ map (intercalate ",") $ (transpose (map showField $ fieldsUT t)++) $ map2 show $ ungroup $ tgroup t

showTableMeta t@(Table f (INode records)) =                show (length $ fieldsUT t) ++ "x" ++ show (   M.size records) ++ " " ++ show (showTableMeta2 t)
showTableMeta t@(Table f (Recs  records)) = "NO INDEX " ++ show (length $ fieldsUT t) ++ "x" ++ show (Tree.size records) ++ " " ++ show (showTableMeta2 t)
showTableMeta t@(Table f (Rec   record )) = "NO INDEX " ++ show (length $ fieldsUT t) ++ "x" ++ show 1                   ++ " " ++ show (showTableMeta2 t)

showTableMeta1 t@(Table f (INode records)) =                show (length $ fieldsUT t) ++ "x" ++ show (   M.size records)
showTableMeta1 t@(Table f (Recs  records)) = "NO INDEX " ++ show (length $ fieldsUT t) ++ "x" ++ show (Tree.size records)
showTableMeta1 t@(Table f (Rec   record )) = "NO INDEX " ++ show (length $ fieldsUT t) ++ "x" ++ show 1

showTableMeta2 t = showNewT $ fieldsUT t

fieldsUT (Table f r) = f

--fieldsUG :: (Map ((a :- b) :- c -> a) ((a :- b) :- c) a, Map (c -> d) c d) => Group i ((a :- b) :- c) -> a :- d
{-
fieldsUG :: Map (c -> d) c d => Group i c -> d
fieldsUG (INode m) = fieldsUG (snd $ fromJust $ M.lookupMin m)
fieldsUG (Recs rs) = fieldsUG (snd $ fromJust $ Tree.lookupMin rs)
fieldsUG (Rec   r) = mapT headT r
-}
showStructure (Table f group) = foldr (++) "" $ map (++"\n") $ showStructureI 0 group

showStructureI i (INode   records) = (sp i ++ "INode") : concatMap (showStructureG (i+5)) (M.toList records)

showStructureG i (k, Recs records) = (sp i ++ "Recs"  ++ showt k) : concatMap (showStructureR (i+5)) (Tree.toList records)

showStructureR i (k, Rec  records) = [sp i ++ "Rec"   ++ showt k ++ "=" ++ showt (Tree.toList records)]

sp :: Int -> Text
sp n = convertString $ replicate n ' '

size t = length $ ungroup $ tgroup t

ungroup (INode m) = concatMap (ungroup . snd) $ M.toList m
ungroup (Recs r) = map (ungroup2 . snd) $ Tree.toList r
ungroup (Rec r) = [r]

ungroup2 (Rec r) = r

applygroup f (Table flds g) = Table flds $ f g

fieldsU x = map fst $ sortOn snd $ M.toList x

inversePerm indices = map snd $ sort $ zip indices [0 ..]

--fromGrid g = fromGridH $ transpose g
fromGrid1 f = fromGridH1 f . transpose

fromGridHBF (c, hs, bs, fs) = unsafePerformIO $ do
   print c
   print hs
   print bs
   print fs
   return $ fromGridH1 (zipWith (\h fn -> map clean h :- fn :- ()) (transpose hs) [0::Int ..]) bs



--fromGridH1 flds []      = TableTuple.empty
fromGridH1 flds recordl = Table flds $ Recs $ Tree.fromElems $ map (Rec . readRec) recordl

tz :: (Show a) => [a] -> Tree.Tree a
tz = Tree.fromList . zip [0..]

mz = M.fromList . zip [0..]

-- fromGrid1 indexFunc g = Table (head g) $ M.fromList $ map (\row -> (indexFunc row, row)) $ map2 (right . parse csvcell "") g

-- convert to list of records
toList (INode r) = concatMap (toList . snd) $ M.toList r
toList (Recs  r) = concatMap (toList . snd) $ Tree.toList r
toList r@(Rec _) = [r]

toList2 = map unrec . toList

clean :: TString -> TString
clean = id

scrub :: TString -> TString
scrub = smap (\c -> let x = ord c in if (x >= 65 && x <= 90) || (x >= 97 && x <= 122) then c else ' ')
{-
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
         $ map (\f -> (\r -> (M.size r, f))
         $ joinAux1 master
         $ byscrub (? f) tab) 
         $ map fst
         $ M.toList
         $ fields tab)
-}
byz func = mapFromList (:) [] . mapfxx func

-- byy1 :: (Show j, Ord j, Show r) => (Tree.Tree r -> j) -> [Tree.Tree r] -> Group j r
byy1 f = INode . M.map (Recs . tz . map Rec) . byz f

byy2 f g = INode . M.map (byy1 g) . byz f

byyn [] = Recs . tz . map Rec
byyn (f : fs) = INode . M.map (byyn fs) . byz f

byscrub f = by (scrub . convertString . show . f)

byf f = by (? f)

by f (Table flds table) = Table flds $ byy1 f $ toList2 table

by2 f g (Table flds table) = Table flds $ byy2 f g $ toList2 table

thenby f (Table flds table) = Table flds $ mapRecs f flds table

mapRecs f flds (INode m) = INode $ M.map (mapRecs f flds) m

-- mapRecs f flds g@(Recs r) = byy1 (f . Record flds) $ toList g

byl fs (Table fd table) = Table fd $ byyn fs $ toList2 table

-- byw f tab = byx ()

putTable x = putStr $ showTable x

join f (l, r) = let
   (i, l1, r1) = joinT f (l, r)
   in M.unions [i, l1, r1]

joinInner (l, r) = (M.intersectionWith joinRec l r, M.empty , M.empty )
joinLeft  (l, r) = (M.intersectionWith joinRec l r, l M.\\ r, M.empty )
joinRight (l, r) = (M.intersectionWith joinRec l r, M.empty , r M.\\ l)
joinOuter (l, r) = (M.intersectionWith joinRec l r, l M.\\ r, r M.\\ l)

applyLeft  f (i, l, r) = (i, f l, r)
applyRight f (i, l, r) = (i, l, f r)
applyBoth  fl fr (i, l, r) = (i, fl l, fr r) 

joinT f (Table lf l, Table rf r) = joinG f (l, r)

joinG f (INode l, INode r) = f (l, r)
--joinGroups (fi, fl, fr) (Rec   l) (Rec   r) = M.unions [fi l r, fl l r, fr l r]
--joinGroups (fi, fl, fr) (Recs  l) (Recs  r) = M.unions [fi l r, fl l r, fr l r]

foldlj f xs = foldl f (head xs, []) (tail xs)

miss x = (x, [])
{-}
joinAux1 a b = {-trace (show il ++ " " ++ show ir) -}res
   where
      Table fl il = a
      Table fr ir = b

      res = M.union il ir
-}

joinRec :: (Show i, Show rc, HasCallStack, Append ra rb rc) => Group i ra -> Group i rb -> Group i rc
joinRec (Rec  l) (Rec  r) = Rec $ appendT l r
{-}
joinRec shift (Recs l) (Recs r) = let
   ls = Tree.size l
   rs = Tree.size r
   in if ls <= 1 && rs <= 1 
      then Recs $ tz $ concat $ crossWith (\(Rec l1) (Rec r1) -> Rec $ Tree.append shift l1 r1) (Tree.toElems l) (Tree.toElems r) 
      else error $ "too many Recs l="++show ls++" r="++show rs
-- joinRec shift (Recs l) (Recs r) = Recs $ tz $ concat $ crossWith (\(Rec l1) (Rec r1) -> Rec $ Tree.append shift l1 r1) (Tree.toElems l) (Tree.toElems r)
-- joinRec shift l r = error $ "joinRec called with "++show (ungroup l)++" and "++show (ungroup r)
-}
joinL zr (Rec  rl) = Rec  $ appendT rl zr
joinL zr (Recs rl) = Recs $ Tree.map (joinL zr) rl

joinR zl (Rec  rr) = Rec  $ appendT zl rr
joinR zl (Recs rr) = Recs $ Tree.map (joinR zl) rr

showKey (k, v) = (k, show k, v)

data Fuzzy dist lk rk v = Fuzzy dist lk rk v deriving (Show)

instance (Eq dist) => Eq (Fuzzy dist lk rk v) where
   Fuzzy a _ _ _ == Fuzzy b _ _ _ = a == b

instance (Ord dist) => Ord (Fuzzy dist lk rk v) where
   compare (Fuzzy a _ _ _) (Fuzzy b _ _ _) = compare a b

joinFuzzy maxDist fi (i, l, r) = let
   (a, b) = unzip $ fuzzyJoin1 maxDist (map showKey $ M.toList l) (map showKey $ M.toList r)
   (lks, rks) = unzip a
   --levs = sort $ concat $ crossWith (\(lk, ls, lv) (rk, rs, rv) -> Fuzzy (fuzzyMatch maxDist ls rs) lk rk (lv `fi` rv)) (map showKey $ M.toList l1) (map showKey $ M.toList r1)
   --(l2, i2, r2) = foldr (joinFuzzyAux maxDist) (l1, i, r1) levs
   
   l2 = l M.\\ (M.fromList $ map (, 0) lks)
   r2 = r M.\\ (M.fromList $ map (, 0) rks)
   i2 = M.fromList $ zipWith joinFuzzyAux lks rks

   joinFuzzyAux lk rk = let
      Just lv = M.lookup lk l
      Just rv = M.lookup rk r
      in (lk, lv `fi` rv)

   in trace (show (M.size i) ++ " " ++ show (M.size l) ++ " " ++ show (M.size r)) (i2, l2, r2)

addCalcField name func table = mapTable (\r -> (name :- func r) :- r) table

blankRec z f = Tree.fromList $ map (\(_, n) -> (n, z)) $ M.toList f
{-}
appendTable joinType func (Table tfs tr) (Table bfs br) = let
   (tftobf2, fieldsj) = joinFields joinType func tfs bfs
   in Table fieldsj $ appendTableG tr $ translateTableG tftobf2 br

mapFields func (Table fs rs) = let
   (fs3, fs2) = mapFields1 func fs 
   in Table fs2 $ mapFieldsG func fs3 rs

mapFields1 func fs = let
   fs1 = M.fromList $ map (\(fld, k) -> (k, func fld)) $ M.toList fs
   fs2 = M.fromList $ map (\(fld, k) -> (func fld, k)) $ M.toList fs
   fs3 = M.fromList $ map tflip $ M.toList $ M.compose fs2 fs1
   in (fs3, fs2)

joinFields joinType func tfs bfs = let
   --y = map (\((a, b, c, d, e, f, g, h, i, j), k) -> let) $ M.toList tfs
   --tfs1 = M.fromList $ map (\(fld, k) -> (fld, func fld)) $ M.toList tfs
   --bfs1 = M.fromList $ map (\(fld, k) -> (fld, func fld)) $ M.toList bfs
   tfs2 = M.fromList $ map (\(fld, k) -> (func fld, k)) $ M.toList tfs
   bfs2 = M.fromList $ map (\(fld, k) -> (func fld, k)) $ M.toList bfs
   -- the field numbers in bfs must be changed to match those in tfs
   -- for the fields that are in bfs but not tfs, we need new numbers. 
   -- we can start numbering from maxt+1 because we can ignore the current bfs numbers,
   -- because they're going to be changed anyway#

   -- tf uname
   -- tf jname
   -- 3 types of fields numbers
   -- tf f# original
   -- tfn f# new -- a new number for a field that was previously not in tfs, only in bfs
   -- bf f#
   -- 3 types of pairs
   -- t = fields only in tf
   -- b = fields only in bf
   -- j = fields only in tf & bf
   -- bj = fields in b and j
   f8_tf_bf  = M.intersectionWith (,) tfs2 bfs2
   tf_bf_j   = map snd $ M.toList f8_tf_bf
   f8_bf_b   = bfs2 M.\\ tfs2
   tf_t      = map snd $ M.toList tfs
   max_tf    = if null tf_t then 0 else maximum tf_t
   tfn_bf_b  = zip [max_tf+1..] $ map snd $ M.toList f8_bf_b
   tfn_bf_bj = M.fromList (tfn_bf_b ++ tf_bf_j) -- updated field numbers for all of bfs

{-
   f8        = unionWith3A joinType (,) (,z) (z,) tfs2 bfs2
   tf_bf_a   = map snd $ M.toList f8 
   maxtf     = if null tf_bf_a then 0 else maximum $ map fst tf_bf_a
   tf_bf_b   = filter ((== z) . fst) tf_bf_a
   tfn_bf_b  = zip [maxtf+1..] $ map snd tf_bf_b                      -- new field numbers for fields in bfs not tfs
   tf_bf_j   = filter (\(tf, bf) -> tf >= 0 && bf >= 0) tf_bf_a
   tfn_bf_bj = M.fromList (tfn_bf_b ++ tf_bf_j) -- updated field numbers for all of bfs
-}
   -- M.compose Map b c -> Map a b -> Map a c
   compb = M.compose (M.fromList $ map tflip $ M.toList tfn_bf_bj) bfs
   fieldsj = union3A joinType tfs compb
   in  -- (tfn_bf_bj, fieldsj)
      
      unsafePerformIO $ do
      putStrLn $ t "tfs="
      mapM_ print $ M.toList tfs
      putStrLn $ t "bfs="
      mapM_ print $ M.toList bfs
      putStrLn $ t "tfs2="
      mapM_ print $ M.toList tfs2
      putStrLn $ t "bfs2="
      mapM_ print $ M.toList bfs2
      putStrLn $ t "f8_tf_bf="
      mapM_ print $ M.toList f8_tf_bf
      putStrLn $ t "tf_bf_j="
      mapM_ print tf_bf_j
      putStrLn $ t "f8_bf_b="
      mapM_ print $ M.toList f8_bf_b
      putStrLn $ t "tf_t="
      mapM_ print tf_t
      putStrLn $ "max_tf="++show max_tf
      putStrLn $ t "tfn_bf_b="
      mapM_ print tfn_bf_b
      putStrLn $ t "tfn_bf_bj="
      mapM_ print $ M.toList tfn_bf_bj
      putStrLn $ t "compb="
      mapM_ print $ M.toList compb
      putStrLn $ t "fieldsj="
      mapM_ print $ M.toList fieldsj
      return (tfn_bf_bj, fieldsj)

appendTableG (INode ts) (INode bs) = INode $ M.unionWith appendTableG ts bs
--appendTableG (Recs  ts) (Recs  bs) = Recs  $ Tree.append (Tree.minKey bs - Tree.maxKey ts + 1) ts bs
appendTableG (Rec   ts) (Rec   bs) = Rec   $ Tree.union ts bs

translateTableG tftobf2 (INode bs) = INode $ M.map (translateTableG tftobf2) bs
translateTableG tftobf2 (Recs  bs) = Recs  $ Tree.map (translateTableG tftobf2) bs
translateTableG tftobf2 (Rec   bs) = Rec   $ Tree.fromList $ M.toList $ M.compose (M.fromList $ Tree.toList bs) tftobf2

mapFieldsG func fs3 (INode rs) = INode $ M.map (mapFieldsG func fs3) rs
mapFieldsG func fs3 (Recs  rs) = Recs  $ Tree.map (mapFieldsG func fs3) rs
mapFieldsG func fs3 (Rec   rs) = Rec   $ Tree.fromList $ M.toList $ M.compose (M.fromList $ Tree.toList rs) fs3
-}
-- foldTable f z n t =
foldSubTable fs (Table flds g) = applyL fs $ Record flds $ foldSubTableG g

foldSubTableG g = Tree.untree $ toList2 g

-- foldSubTable1 fs (Table flds g) = applyL fs $ Record flds $ foldSubTable1G (Tree.fromElems fs) g
addTotals t = Table (fields t) $ INode $ M.insert "ZZZZ" (Rec $ foldSubTable2 sum t) g where INode g = tgroup t
foldSubTable2 f t = foldSubTable2G f $ tgroup t
foldSubTable2G f g = Tree.map f $ Tree.untree $ toList2 g

--addTotals3 t = Table (fields t) $ foldSubTable3G sum $ table t
foldSubTable3 f t = Table (fields t) $ snd $ foldSubTable3G (foldSubTable3R f) $ tgroup t

foldSubTable3G f (INode rs) = let
   (keys, vals) = unzip $ M.toList $ M.map (foldSubTable3G f) rs
   (totals, rebuild) = unzip vals
   newtotal = f totals
   
   in (newtotal, INode $ (if length totals > 1 then M.insert (toDyn ("ZZZZ"::Text)) (Rec newtotal) else id) $ M.fromList $ zip keys rebuild)

foldSubTable3G f (Recs rs) = let
   (totals, rebuild) = Tree.unzip $ Tree.map (foldSubTable3G f) rs
   newtotal = f $ Tree.toElems totals
   
   in (newtotal, Recs $ if Tree.size totals > 1 then Tree.insert ((1+) $ snd $ Tree.span rebuild) (Rec newtotal) rebuild else rebuild)
--foldSubTable3G f (Recs rs) = Tree.map f $ Tree.untree $ Tree.toElems $ Tree.map (foldSubTable3G f) rs

foldSubTable3G f r@(Rec fs) = (fs, r)

foldSubTable3R f rs = Tree.map f $ Tree.untree rs

p = unzip

mapSubTable f n (Table f1 g) = Table (mapSubTableGF (fields . f . Table) n g) $ mapSubTableG (tgroup . f . Table) n g

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
            k3 = k1 ++ cons (convertChar '_') (convertString $ show n2)
            in case M.lookup k3 m of
               Just j2 -> Nothing
               Nothing -> Just k3) [n1+1..]
         in M.insert k2 v m
      Nothing -> M.insert k v m

mapTable f (Table f1 g) = Table $ mapTableG f g

mapTableG f (INode m) = INode $ fmap (mapTableG f) m
mapTableG f (Recs  t) = Recs  $ fmap (mapTableG f) t
mapTableG f (Rec   r) = Rec   $ f r
mapTableR f fields r = values $ f $ Record fields r

mapFieldsR f flds r = fieldsr $ f $ Record flds r

{-}
delField fieldName t = if head (fields t) == fieldName
   then error ("can't delete index " ++ fieldName)
   else let
      Just fieldN = elemIndex fieldName $ fields t
      in Table (deleteIndex fieldN $ fields t) $ M.fromList $ map (\(k, v) -> (k, deleteIndex (fieldN-1) v)) $ M.toList $ records t
-}

(?) :: (NamedTuple a b c d, HasCallStack) => Record b c -> a -> d
(?) = flip lookupF

lookupF fld (Record flds val) = NewTuple.lookup fld flds val

--(??) :: (Ord [f]) => Record [f] t -> [f] -> t
--r ?? k = fromJust $ (values r Tree.!) $ snd $ fromJust $ find (isInfixOf k . map toLower . fst) $ M.toList $ fieldsr r

data Type = M | To

type Amount = Int

ymd = YearMonthDay

data Trans = Trans {day :: Day, amount :: Amount, person :: Person, typ :: Type}

-- go = putGridF =<< readfods

names = ["Tyrone", "Danny", "James"]

trans =
   [ (ymd 2023 6 13, -20, Tyrone,  M)
   , (ymd 2023 6 13, -30, Tyrone,  M)
   , (ymd 2023 6 13, -20,  James,  M)
   , (ymd 2023 6 13, -25,  James,  M)
   , (ymd 2023 6 13,  -3,  James, To)
   , (ymd 2023 6 13, -20,  Danny,  M)
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
convertGrid1 = map2 (NP.parse2 csvcell)
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
   return $ NP.parse2 csv f

csv = many csvline

csvline = sepBy csvcell $ char ','

txt x = let
   r = reads x
   in if null r then x else fst $ head r

dynCell s = case parse (choice [try (toDyn <$> dateExcel), try numberDyn]) "" s of
   Left l -> let r = reads $ convertString s in if null r then toDyn s else toDyn (fst $ head r :: Text)
   Right r -> r

fodscell = choice [try (toDyn <$> dateExcel), try numberDyn, toDyn <$> many anyChar]

csvcell  = choice [try (toDyn <$> dateExcel), try numberDyn]

wikicell = choice [try (toDyn <$> dateExcel), try numberDyn]

int = fromInteger <$> NP.integer

numberDyn = choice [try (toDyn <$> NP.forceFloatingC), try (toDyn <$> intC)]

intC = fromInteger <$> NP.integerC :: Parser Int

atleast n x r = try $ do
   z <- choice $ map (\t -> string $ take t x) [n..length x - 1]
   guard (length z >= n)
   return r

dateExcel = choice [try (do
   option ' ' (do
      try $ choice $ zipWith (atleast 2) (["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]) [1..7]
      char ' ')
   dom1 <- dom
   choice [char '/', char '-', try (do string " of "; return 'o'), char ' ']
   moy1 <- moy
   oneOf "/- "
   year <- NP.integer
   return $ ymd (if  | year <  40 -> year + 2000
                     | year < 100 -> year + 1900
                     | otherwise  -> year) moy1 dom1),
   try (do
      option ' ' (do
         try $ choice $ zipWith (atleast 2) ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"] [1..7]
         char ' ')
      moy1 <- moy
      choice [char '/', char '-', try (do string " the "; return 't'), char ' ']
      dom1 <- dom
      oneOf "/- "
      year <- NP.integer
      return $ ymd (if  | year <  40 -> year + 2000
                        | year < 100 -> year + 1900
                        | otherwise  -> year) moy1 dom1)]



dom = do
   dom1 <- int
   guard (dom1 <= 31)
   dmark <- choice [try (choice [string "st", string "nd", string "th", string "rd"]), string ""]
   return dom1


moy = do
   moy1 <- choice [try $ choice $ zipWith (atleast 3) ["january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"] [1..12], int]
   guard (moy1 <= 12)
   return moy1

