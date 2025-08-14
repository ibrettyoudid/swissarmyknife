{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Parser3 where

import Favs
import MyPretty2 hiding (format)
import MHashDynamic hiding (Apply)
import SyntaxCIPU
--import Syntax3 hiding (foldl, foldr)

import Control.Monad
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.IntMap qualified as I

data Rule tok =
   Seq [Rule tok]
   | Alt [Rule tok]
   | Name String (Rule tok)
   | Token tok
   | Range tok tok
   | Many (Rule tok)
   | Apply (Iso Dynamic Dynamic) (Rule tok)
   | StrRes String Dynamic

{-
parse (Token a) = a
parse (Ignore a) = 
format (Token a) = a
-}
--    EIso    :: Iso alpha beta -> Rule alpha -> Rule beta
tod f x = dynApply (toDyn f) x

isod a b = Iso (tod a) (tod b)

icons = isod (\(x, xs) -> Just (x:xs)) (\(x:xs) -> Just (x, xs))

instance Eq (Rule a) where
--   Seqd  a b == Seqd c d = (a, b) == (c, d)
   Seq   as  == Seq  bs  = as     == bs
   Alt   as  == Alt  bs  = as     == bs
   Name  a _ == Name  b _ = a == b
   Token a   == Token b   = a == b
   Range a b == Range c d = (a, b) == (c, d)
   _ == _ = False

instance Ord (Rule a) where
--   compare (Seqd a b) (Seqd c d) = compare (a, b) (c, d)
   compare (Alt as ) (Alt bs ) = compare as bs
   compare (Name a _) (Name b _) = compare a b
   compare (Token a) (Token b) = compare a b
   compare (Range a b) (Range c d) = compare (a, b) (c, d)
   compare (Name{}) _ = LT
   compare (Seq{}) (Name{}) = GT
   compare (Seq{}) _ = LT
   compare (Alt{}) (Name{}) = GT
   compare (Alt{}) (Seq{}) = GT
   compare (Alt{}) _ = LT
   compare (Token{}) (Name{}) = GT
   compare (Token{}) (Seq{}) = GT
   compare (Token{}) (Alt{}) = GT
   compare (Token{}) _ = LT
   compare (Range{}) (Name{}) = GT
   compare (Range{}) (Seq{}) = GT
   compare (Range{}) (Alt{}) = GT
   compare (Range{}) (Token{}) = GT
--   compare (Range{}) _ = LT

instance Show (Rule a) where
   show = outershow Nothing

innershow :: Maybe Int -> Rule a -> [Char]
--innershow d (Seqd  a b) = unwords $ ii d [innershow Nothing a, innershow Nothing b]
innershow d (Alt   as ) = unwords $ ii d [intercalate " | " $ map (innershow Nothing) as]
innershow d (Name  a _) = unwords $ ii d [a]
innershow d (Token a  ) = unwords $ ii d [show a]
innershow d (Range a b) = unwords $ ii d ["[" ++ show a ++ ".." ++ show b ++ "]"]

--outershow d r@(Seqd  a b) = "Seqd " ++ innershow d r
outershow d r@(Seq   as ) = "Seq " ++ innershow d r
outershow d r@(Alt   as ) = "Alt " ++ innershow d r
outershow d r@(Name  a b) = unwords $ ii d [a ++ " -> " ++ innershow Nothing b]
outershow d r@(Token a  ) = show a
outershow d r@(Range a b) = "[" ++ show a ++ ".." ++ show b ++ "]"

ii (Just d) = insertIndex d "."
ii Nothing = id

data Item r = Item Int (Rule r) deriving (Eq, Ord)

type ItemSet r = S.Set (Item r)

instance Show (Item r) where
   show (Item d r) = outershow (Just d) r

data EItem2 r = EItem2 Int Int (Item r) deriving (Eq, Ord)

data EItem x
   = EItem { r::Rule x, f::Int, t::Int, n::Int }
   deriving (Eq)

instance Show (EItem z) where
   show (EItem r j k d) = outershow (Just d) r ++ " " ++ unwords (map show [j, k])

instance Ord (EItem z) where
   compare (EItem e1 j1 k1 d1) (EItem e2 j2 k2 d2) = compare (j1, k1, d1, e1) (j2, k2, d2, e2)

digit = Range '0' '9'
lower = Range 'a' 'z'
upper = Range 'A' 'Z'
under = Token '_'
alpha = Alt [upper, lower]
--num = Many digit
--alnum = Alt [alpha, digit]
op = Alt [Token '+', Token '-', Token '*', Token '/']

--ident = Name "ident" $ cons (Alt [alpha, under]) ident1
--ident1 = Many (Alt [alpha, under, digit])

--expr = Name "expr" $ Alt [Seq [expr, Token '+', expr], Token 'a']

--test = p expr "a+a+a"

p e s = tree e $ parseE e s

pD e s = do
    states <- parseED e s
    table s states
    return $ tree e states

parseE e s =
   let
      items = predict (S.singleton $ EItem e 0 0 0)
    in
      parseE0 [items] s items [1 .. length s]

parseED e s =
   let
      items = predict (S.singleton $ EItem e 0 0 0)
    in
      parseE0D [items] s items [1 .. length s]

parseE0 states _ items [] = states
parseE0 states s items (k : ks) = let
   itemsnew = parseE1 states s items k
   in parseE0 (states ++ [itemsnew]) s itemsnew ks

parseE0D states _ items [] = return states
parseE0D states s items (k : ks) = do
   itemsnew <- parseE1D states s items k
   parseE0D (states ++ [itemsnew]) s itemsnew ks

-- scanM :: Monad m => (m [a] -> b -> m a) -> m [a] -> [b] -> m [a]
scanM f z = foldl (\a b -> do ra <- a; rr <- f (last ra) b; return $ ra ++ [rr]) (return [z])

parseE1 states s items k = let
   scan1 = scan k s items
   comp1 = complete states scan1
   in if k < length s
      then predict comp1
      else comp1

parseE1D states s items k = do
   let scan1 = scan k s items
   putStrLn $ "scan1=" ++ show scan1
   let comp1 = complete states scan1
   putStrLn $ "comp1=" ++ show comp1
   if k < length s
      then do
         let pred1 = predict comp1
         putStrLn $ "pred1=" ++ show pred1
         return pred1
      else return comp1

conseq (a : b : cs)
   | a == b = a
   | otherwise = conseq (b : cs)

closure f items = closure1 f items items

closure1 f done current =
   let
      new = f done current
      newdone = S.union done new
      newnew = new S.\\ done
    in
      if S.null new then done else closure1 f newdone newnew

process f old current = foldr S.union S.empty $ S.map (S.fromList . f) current

predict = closure (process predict1)

predict1 (EItem (Alt  as  ) j _ 0) = [EItem  a        j j 0 | a <-       as]
predict1 (EItem (Seq  as  ) j _ d) = [EItem (as !! d) j j 0 | d < length as]
predict1 (EItem (Name a  b) j _ 0) = [EItem  b        j j 0                ]
predict1 s = []

predict1a (EItem2 j _ (Item 0 (Alt   as  ))) = [EItem2 j j (Item 0  a       ) | a <-       as]
predict1a (EItem2 j _ (Item d (Seq   as  ))) = [EItem2 j j (Item 0 (as !! d)) | d < length as]
predict1a (EItem2 j _ (Item 0 (Name  a  b))) = [EItem2 j j (Item 0  b       )                ]
predict1a s = []

scan k s items = S.fromList $ mapMaybe (scan1 k (s !! (k - 1))) $ S.toList items

scan1 k ch (EItem r@(Token c   ) j _ 0) = ifJust (ch == c) (EItem r j k 1)
scan1 k ch (EItem r@(Range c d) j _ 0) = ifJust (ch >= c && ch <= d) (EItem r j k 1)
scan1 k ch s = Nothing

complete states = closure (\old -> process (complete1 (states ++ [old])) old)

complete1 states (EItem y j k p) = mapMaybe (\(EItem x i j1 q) -> ifJust (p == slength y && q < slength x && caux x q y) $ EItem x i k (q + 1)) $ S.toList $ states !! j

complete3 :: [S.Set (EItem z)] -> S.Set (EItem z) -> [S.Set (EItem z)]
complete3 states items = foldl complete4 states items

complete4 states (EItem y j k p) = foldl (complete5 p y) states $ S.toList $ states !! j

complete5 p y states (EItem x i j1 q) = states ++ 
   [ S.singleton $ EItem x i (length states) (q + 1) 
   | p == slength y && q < slength x && caux x q y ]
-- complete3 k states (EItem y j _ p) = mapMaybe (\(EItem x i j q) -> ifJust (p == slength y && q < slength x && caux x q y) $ EComplete3 (EItem x i j q) (EItem x i k (q + 1)) (EItem y j k p)) $ S.toList $ states !! j

{-
predict3 k (Item (Alt  as  ) 0) = [Item a 0 | a <- as]
predict3 k (Item (Seq  as  ) d) = [Item (as !! d) 0 | d < length as]
predict3 k (Item (Name a  b) 0) = [Item b 0]
predict3 k s = []
-}
makeLR e = let
   items = predict (S.singleton $ EItem e 0 0 0)
   in makeLRa [items] [items]

makeLRa old [] = old
makeLRa old current = let
   k = 0
   new = concatMap (makeLRb old) current
   oldcore = S.fromList $ map core old
   newnew = filter (\x -> not $ S.member (core x) oldcore) new
   in
   makeLRa (old ++ new) newnew

core = S.map core1

core1 (EItem r j k d) = Item d r

makeLRb states items = let
   states1 = scan2 (length states) items
   states2 = map (complete states) states1
   in if True --k < length s
      then map predict states2
      else states2

scan2 k items = map (S.fromList . catMaybes) $ crossWith
   (uncurry scan1)
   (zip [k..] $ S.toList $ S.fromList $ mapMaybe scan3 $ S.toList items)
   (S.toList items)

makeLRc states items = let
   tokens = scan5 items
   oldcore = S.fromList $ map core states
   in foldl (makeLRd items) states tokens

makeLRd items states token = let
   k = length states
   statescore = S.fromList $ map core states
   newstate = S.fromList $ mapMaybe (scan1 k token) $ S.toList items
   {-
   newstates = if S.member (core newstate) statescore
      then states
      else states ++ [newstate]
   -}
   in (states ++) $ map predict $ complete3 states newstate
   
data NumSet a b = NumSet (I.IntMap a) (S.Set b) (a -> b)

size (NumSet a b f) = fst $ fromJust $ I.lookupMax a

add a ns@(NumSet sa sb sf) = let
   b = sf a
   there = S.member b sb
   s = size ns
   in (not there, if not there 
                     then NumSet (I.insert s a sa) (S.insert b sb) sf
                     else ns)
{-
add1 a (ns, collect) = let
   (nt, ns1) = add a ns
-}
--addList as ns = foldr add ns as

scan5 items = S.toList $ S.fromList $ mapMaybe scan3 $ S.toList items

{-
scan2a k items = let
   toks = S.toList $ M.fromList $ mapMaybe (uncurry scan4) $ zip [k..] $ S.toList items
   map 
-}
scan3 (EItem r@(Token c   ) j _ 0) = Just c
--scan3 k (EItem r@(Range c d) j _ 0) = ifJust (ch >= c && ch <= d) (EItem r j k 1)
scan3 s = Nothing

scan4 k (EItem r@(Token c   ) j _ 0) = Just (c, EItem r j k 1)
--scan3 k (EItem r@(Range c d) j _ 0) = ifJust (ch >= c && ch <= d) (EItem r j k 1)
scan4 k s = Nothing


caux (Seq as) q y = as !! q == y
caux (Alt as) q y = y `elem` as
caux (Name a b) q y = b == y
caux _ _ _ = False

slength (Seq as) = length as
slength _ = 1

children states (EItem r f t n) = do
    s1@(EItem r1 f1 t1 n1) <- S.toList $ states !! t
    if caux r (n - 1) r1 && n1 == slength r1
         then do
             s2@(EItem r2 f2 t2 n2) <- S.toList $ states !! f1
             if r2 == r && f2 == f && n2 == n - 1
                  then if n2 > 0 then map (s1:) $ children states s2 else [[s1]]
             else []
         else []

data Tree z = Tree (EItem z) [Tree z] | Trees [Tree z] deriving (Eq, Ord)

tree start states =
   let
      end = EItem start 0 (length states - 1) (slength start)
      success = S.member end $ last states
    in
         tree1 states end

tree1 states end = Tree end $ tree2 $ reverse $ map reverse $ map2 (tree1 states) $ children states end

tree2 [x] = x

tree2 xs = map Trees xs

only [x] = x

instance Show (Tree z) where
    show tree = format1 1 $ convTree tree


convTree (Tree a b) = Data (show a) $ map convTree b
convTree (Trees b) = Data "TREES" $ map convTree b
{-
transeq :: Foldable t => S.Set ETrans -> t a -> Rule -> [[ETrans]]
>>> p expr "a+a+a"
-}

table str states =
   let
      (maxes, axes, fmts) = unzip3 $ zipWith (taux str $ 0 : maxes) states [0 ..]
      axis = Data.List.foldr Parser3.mergeStrs "" axes
    in
      do
         putStrLn $ unlines $ axis : concat fmts ++ [axis]
         return maxes

taux str maxes s k =
   let
      (ends, fmts) = unzip $ map (taux1 maxes max1 k) $ S.toList s
      (end1, num) = taux2 maxes max1 k k (show k) ' '
      (end2, tok) = if k > 0 then taux2 maxes max1 (k - 1) k (singleton $ str !! (k - 1)) ' ' else (end1, num)
      max1 = maximum (end1 : end2 : ends) + 4
    in
      (max1, Parser3.mergeStrs num tok, fmts)

taux1 ends max1 k state@(EItem e j _ d) = taux2 ends max1 j k (let s = show state in if j < k then s else "(" ++ s ++ ")") '-'

taux2 ends m j k sh fill =
   if
      | j < k ->
            let
               st = ends !! (j + 1)
               l1 = m - st - length sh
               l2 = div l1 2
               l3 = l1 - l2
             in
               (st + length sh, replicate st ' ' ++ replicate l2 fill ++ sh ++ replicate l3 fill)
      | j == k ->
            let
               st = ends !! j
               l = length sh
               l2 = div l 2
             in
               (st + l2, replicate (m - l2) ' ' ++ sh)

mergeStrs a b = zipWith (\x y -> if x == ' ' then y else x) a b ++ if length a > length b then drop (length b) a else drop (length a) b

makelr states rule k = makelr1 states (S.singleton $ EItem rule 0 0 0) k

makelr1 states items k = predict items
{-
scanlr k s states = S.fromList $ mapMaybe (scan1 k (s !! (k - 1))) $ S.toList states
scanlr k ch (EItem r@(Token c   ) j _ 0) = ifJust (ch == c) (EItem r j k 1)
scanlr k ch (EItem r@(Range c d) j _ 0) = ifJust (ch >= c && ch <= d) (EItem r j k 1)
-}
combinescans c@(Token cr, cs) d@(Token dr, ds) = if cr == dr then [(Token cr, cs ++ ds)] else [c, d]
combinescans c@(Token cr, cs) d@(Range d1 d2, ds) = if cr >= d1 && cr <= d2 then [(Range d1 (pred cr), ds), (Token cr, cs ++ ds), (Range (succ cr) d2, ds)] else [c, d]
combinescans c@(Range c1 c2, cs) d@(Range d1 d2, ds) =
   if c2 >= d1 && c1 <= d2 then
      let
         o1 = max c1 d1
         o2 = min c2 d2
      in if c1 < d1
               then [(Range c1 (pred o1), cs), (Range o1 o2, cs ++ ds), (Range (succ o2) d2, ds)]
               else [(Range d1 (pred o1), ds), (Range o1 o2, cs ++ ds), (Range (succ o2) c2, cs)]
   else
      [c, d]
-- start from start rule
-- thats your first kernel
-- closure it with predictions (if you have completions there's a problem)
-- scan all possible tokens into separate states
-- same token may have multiple meanings, combine them into one state
-- those are your new kernels
-- closure them with completions and predictions
-- 
-- how kernels are compared for equality defines what sort of parser it is
-- some record little context, some account for a lot, if it accounts for all context it can't do recursive grammars
