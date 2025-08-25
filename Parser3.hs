{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}
{-# HLINT ignore "Eta reduce" #-}

module Parser3 where

import Favs
import MyPretty2 hiding (format)
import MHashDynamic hiding (Apply, expr)
import SyntaxCIPU
import Rule
--import Syntax3 hiding (foldl, foldr)

import Control.Monad
import Control.Monad.State qualified as St
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.IntMap qualified as I
import Shell (ch)
{-
{-
data Rule tok =
   Seq [Rule tok]
   | Alt [Rule tok]
   | Name String (Rule tok)
   | Token tok
   | Range tok tok
   | Many (Rule tok)
   | ManyTill (Rule tok) (Rule tok)
   | Apply (Iso Rule Dynamic) (Rule tok)
   | StrRes String Dynamic
   | Not (Rule tok)
   | And (Rule tok)
-}
{-
parse (Token a) = a
parse (Ignore a) = 
format (Token a) = a
-}
--    EIso    :: Iso alpha beta -> Rule alpha -> Rule beta
tod :: (Typeable a, Typeable b) => (a -> Maybe b) -> Dynamic -> Maybe Dynamic
tod f x = toDyn <$> f (fromDyn1 x)

isod :: (Typeable a, Typeable b) => (a -> Maybe b) -> (b -> Maybe a) -> Iso Dynamic Dynamic
isod a b = Iso (tod a) (tod b)

--icons = isod (\(x, xs) -> Just (x:xs)) (\(x:xs) -> Just (x, xs))

instance Eq tok => Eq (Rule tok) where
--   Seqd  a b == Seqd c d = (a, b) == (c, d)
   Seq   as  == Seq   bs  = as     == bs
   Alt   as  == Alt   bs  = as     == bs
   Name  a _ == Name  b _ = a == b
   Token a   == Token b   = a == b
   Range a b == Range c d = (a, b) == (c, d)
   _ == _ = False

instance Ord tok => Ord (Rule tok) where
--   compare (Seqd a b) (Seqd c d) = compare (a, b) (c, d)
   compare (Alt   as ) (Alt   bs ) = compare as bs
   compare (Name  a _) (Name  b _) = compare a b
   compare (Token a  ) (Token b  ) = compare a b
   compare (Range a b) (Range c d) = compare (a, b) (c, d)
   compare (Name {}) _         = LT
   compare (Seq  {}) (Name {}) = GT
   compare (Seq  {}) _         = LT
   compare (Alt  {}) (Name {}) = GT
   compare (Alt  {}) (Seq  {}) = GT
   compare (Alt  {}) _         = LT
   compare (Token{}) (Name {}) = GT
   compare (Token{}) (Seq  {}) = GT
   compare (Token{}) (Alt  {}) = GT
   compare (Token{}) _         = LT
   compare (Range{}) (Name {}) = GT
   compare (Range{}) (Seq  {}) = GT
   compare (Range{}) (Alt  {}) = GT
   compare (Range{}) (Token{}) = GT
--   compare (Range{}) _ = LT

instance Show tok => Show (Rule tok) where
   show = outershow Nothing

--innershow d (Seqd  a b) = unwords $ ii d [innershow Nothing a, innershow Nothing b]
innershow :: Maybe Int -> (Rule tok) -> [Char]
innershow d (Seq   as ) = unwords $ ii d $ map (innershow Nothing) as
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

data Result = Running | Fail | Pass Dynamic deriving (Eq, Ord)

data Item tok  = Item   (Rule tok) Int 
               | ISeq   (Rule tok) Result Int 
               | IAlt   (Rule tok) Result Int
               | IName  (Rule tok) Result
               | ITerm  (Rule tok) Result --used for all terminal level ops
               | IMany  (Rule tok) Result
               | INot   (Rule tok) Result
               | IAnd   (Rule tok) Result
               | IGet   (Rule tok) Result
               | ISet   (Rule tok) Result
               | IApply (Rule tok) Result
               deriving (Eq, Ord)

result (ISeq   r re n) = re
result (IAlt   r re n) = re
result (IName  r re  ) = re
result (ITerm  r re  ) = re
result (IMany  r re  ) = re
result (INot   r re  ) = re
result (IAnd   r re  ) = re
result (ISet   r re  ) = re
result (IApply r re  ) = re

item2 (State2 b c i) = i
from2 (State2 b c i) = b
to2   (State2 b c i) = c

rule (ISeq   r re n) = r
rule (IAlt   r re n) = r
rule (IName  r re  ) = r
rule (ITerm  r re  ) = r
rule (IMany  r re  ) = r
rule (INot   r re  ) = r
rule (IAnd   r re  ) = r
rule (ISet   r re  ) = r
rule (IApply r re  ) = r

pos (ISeq r re n) = n
pos (IAlt r re n) = n

pass (Pass _) = True
pass _        = False

passi = pass . result

type ItemSet tok = S.Set (Item tok)

instance Show tok => Show (Item tok) where
   show (Item r n) = outershow (Just n) r

data State2 tok = State2 Int Int (Item tok) deriving (Eq, Ord)

data State tok
   = State { r::Rule tok, f::Int, t::Int, n::Int }
   deriving (Eq)

instance Show z => Show (State z) where
   show (State r b c d) = outershow (Just d) r ++ " " ++ unwords (map show [b, c, d])

instance Ord z => Ord (State z) where
   compare (State e1 b1 k1 d1) (State e2 j2 k2 d2) = compare (b1, k1, d1, e1) (j2, k2, d2, e2)

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

--expr = Name "expr" $ Alt [expr `Then` Token '+' `Then` expr], Token 'a']

--test = pD expr "a+a+a"

p s t = tree s $ parseE s t

pD s t = do
    states <- parseED s t
    tableD t $ concatMap S.toList states
    tableD t $ filter isCompleteZ $ concatMap S.toList states
    return $ tree s states

parseE s t =
   let
      items = predictZ (S.singleton $ State2 0 0 (start s))
    in
      parseE0 [items] t items [1 .. length t]

parseED s t =
   let
      items = predictZ (S.singleton $ State2 0 0 (start s))
    in
      parseE0D [items] t items [1 .. length t]

parseE0 states _ items [] = states
parseE0 states t items (c : ks) = let
   itemsnew = parseE1 states t c items
   in parseE0 (states ++ [itemsnew]) t itemsnew ks

parseE0D states _ items [] = return states
parseE0D states t items (c : ks) = do
   itemsnew <- parseE1D states t c items
   parseE0D (states ++ [itemsnew]) t itemsnew ks

-- scanM :: Monad m => (m [a] -> b -> m a) -> m [a] -> [b] -> m [a]
scanM f z = foldl (\a b -> do ra <- a; rr <- f (last ra) b; return $ ra ++ [rr]) (return [z])

data States t = States [State2 t] (I.IntMap (S.Set (State2 t)))

putss newlist = do
   States all bytok <- St.get
   St.put $ States (all ++ newlist) (foldr fu bytok newlist) 

fu s m = I.insert (to2 s) (S.insert s (fromMaybe S.empty (I.lookup (to2 s) m))) m

parseE1 states t c items = let
   scan1 = scanZ c t items
   comp1 = completeZ states $ S.fromList scan1
   in if c < length t
      then predictZ comp1
      else comp1

parseE1D states t c items = do
   let scan1 = scanZ c t items
   putStrLn $ "scan1=" ++ show scan1
   let comp1 = completeZ states scan1
   putStrLn $ "comp1=" ++ show comp1
   if c < length t
      then do
         let pred1 = predictZ comp1
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

predictZ = closure (process predict1Z)

predict1 (State r j c d) = [State a c c 0 | a <- paux r d]

predict1Z (State2 _ c (ISeq   (Seq   as ) t n)) = [State2 c c (start (as !! n))]
predict1Z (State2 _ c (IAlt   (Alt   as ) t n)) = [State2 c c (start a) | a <- as]
predict1Z (State2 _ c (IName  (Name  a b) t  )) = [State2 c c (start b)]
predict1Z (State2 _ c (IApply (Apply a b) t  )) = [State2 c c (start b)]
--predict1Z (State2 _ c (IGet   (Get   a  ) t  )) = [State2 c c (start a)]
predict1Z (State2 _ c (ISet   (Set   a b) t  )) = [State2 c c (start b)]

--predict1Z (State2 j c (ISeq (Seq as) t n)) = [State2 c c (start a) | a <- [as !! n]]

--paux (Seq  as ) q = [as !! q | q < length as]
paux (Alt  as ) 0 = as
paux (Name a b) 0 = [b]
--paux (ManyTill a b) 0 = [a, b]
paux _ _ = []

start r@(Seq   a  ) = ISeq  r Running 0
start r@(Alt   a  ) = IAlt  r Running 0
start r@(Name  a b) = IName r Running
start r@(Token a  ) = ITerm r Running

{-}
predict1 (State r@(Alt as) j c 0) = [State  a        c c 0 | a <-       as]
predict1 (State r@(Seq as) j c d) = [State (as !! d) c c 0 | d < length as]
predict1 (State (Name a b) j c 0) = [State  b        c c 0                ]
predict1 t = []

predict1Z (State2 j c (Item (Alt   as  ) 0)) = [State2 c c (Item  a        0) | a <-       as]
predict1Z (State2 j c (Item (Seq   as  ) d)) = [State2 c c (Item (as !! d) 0) | d < length as]
predict1Z (State2 j c (Item (Name  a  b) 0)) = [State2 c c (Item  b        0)                ]
predict1Z t = []
-}

--scan c t items = S.fromList $ mapMaybe (scan1 c (t !! (c - 1))) $ S.toList items
scan c t items = let
   sc    = map (scan1 c (t !! (c - 1))) $ S.toList items
   scf f = map snd $ filter (f . fst) sc
   in (scf id, scf not)

scanZ c t items = mapMaybe (scan1Y c (t !! (c - 1))) $ S.toList items


--scan1 c ch (State r j _ 0) = ifJust (saux ch r) (State r j c 1)
--scan1 c ch t = Nothing

scan1 c ch (State r j k1 n) = let pass = n == 0 && saux ch r in (pass, if pass then State r j c 1 else State r j k1 n)

scan1Y c ch (State2 j _ t) = scan1Z c ch j c t

scan1Z c ch j _ (ITerm r Running) = Just $ State2 j c (toDyn $ ITerm r (if saux ch r then Pass else Fail))
scan1Z c ch _ _ _ = Nothing

saux ch (Token c  ) = ch == c
saux ch (Range c d) = ch >= c && ch <= d
saux _ _ = False

complete states = closure (\old -> process (complete1 (states ++ [old])) old)

complete1 states (State y b c p) = mapMaybe 
   (\(State x a b1 q) -> 
      ifJust (p == slength y && q < slength x && caux x q y) 
         $ State x a c (q + 1)
   ) $ S.toList $ states !! b

completeZ states = closure (\old -> process (complete1Z (states ++ [old])) old)

complete1Z states (State2 b c i) =
   mapMaybe (complete2Z b c i) $ S.toList $ states !! b

complete2Z b c i (State2 a b1 (ISeq x q n)) =
      ifJust (result i /= Running && q == Running && caux x n (rule i)) $
         State2 a c (ISeq x (if result i == Fail then Fail else if n + 1 == slength x then result i else Running) (n + 1))

complete2Z b c i (State2 a b1 (IAlt x q n)) =
      ifJust (result i /= Running && q == Running && caux x 0 (rule i)) $
         State2 a c (if passi i then IAlt x (result i) n else if pos i > 0 then IAlt x Running (pos i-1) else IAlt x Fail 0)

complete2Z b c i (State2 a b1 (IName x q)) =
      ifJust (result i /= Running && q == Running && caux x 0 (rule i)) $
         State2 a c (IName x $ result i)

complete2Z b c i (State2 a b1 (IApply x q)) =
      ifJust (q == Running && caux x 0 (rule i)) $
         case result i of
            Pass res -> 
               case x of
                  Apply iso _ ->
                     case apply iso res of
                        Just ju -> State2 a c (IApply x (Pass ju))
                        Nothing -> State2 a c (IApply x Fail)
            Fail -> State2 a c (IApply x Fail)

complete3  states items = foldl complete4 states items

complete32 states items = foldl complete42 states items

complete4 states (State y b c p) = foldl (complete5 p y) states $ S.toList $ states !! b

complete42 states (State2 b c (Item y p)) = foldl (complete52 p y) states $ S.toList $ states !! b

complete5 p y states (State x a b1 q) = states ++
   [ S.singleton $ State x a (length states) (q + 1)
   | p == slength y && q < slength x && caux x q y ]

complete52 p y states (State2 a b1 (Item x q)) = states ++
   [ S.singleton $ State2 a (length states) (Item x (q + 1))
   | p == slength y && q < slength x && caux x q y ]
-- complete3 c states (State y b _ p) = mapMaybe (\(State x a b q) -> ifJust (p == slength y && q < slength x && caux x q y) $ EComplete3 (State x a b q) (State x a c (q + 1)) (State y b c p)) $ S.toList $ states !! b

caux (Seq as) q y = as !! q == y
caux (Alt as) q y = y `elem` as
caux (Name a b) q y = b == y
--caux (ManyTill a b) q y = a == y
caux _ _ _ = False

--iaux a q y = caux (rule a) q (rule y)

iaux (ISeq  (Seq  as ) re n) ch = as !! n == rule ch
iaux (IAlt  (Alt  as ) re n) ch = rule ch `elem` as
iaux (IName (Name a b) re  ) ch = b == rule ch 

slength (Seq as) = length as
slength _ = 1

isComplete (State r _ _ d) = d == slength r
isCompleteZ (State2 _ _ i) = result i /= Running
{-
predict3 c (Item (Alt  as  ) 0) = [Item a 0 | a <- as]
predict3 c (Item (Seq  as  ) d) = [Item (as !! d) 0 | d < length as]
predict3 c (Item (Name a  b) 0) = [Item b 0]
predict3 c t = []
-}
makeLR e = let
   items = predict (S.singleton $ State e 0 0 0)
   in makeLRa [items] [items]

makeLRa old [] = old
makeLRa old current = let
   c = 0
   new = concatMap (makeLRb old) current
   oldcore = S.fromList $ map core old
   newnew = filter (\x -> not $ S.member (core x) oldcore) new
   in
   makeLRa (old ++ new) newnew

core = S.map core1

core1 (State r b c d) = Item r d

makeLRb states items = let
   states1 = scan2 (length states) items
   states2 = map (complete states) states1
   in if True --c < length t
      then map predict states2
      else states2

scan2 c items = map (S.fromList . catMaybes) $ crossWith
   (uncurry scan1)
   (zip [c..] $ S.toList $ S.fromList $ mapMaybe scan3 $ S.toList items)
   (S.toList items)

makeLRc states items = let
   tokens = scan5 items
   oldcore = S.fromList $ map core states
   in foldl (makeLRd items) states tokens

makeLRd items states token = let
   c = length states
   statescore = S.fromList $ map core states
   newstate = S.fromList $ mapMaybe (scan1 c token) $ S.toList items
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
scan2a c items = let
   toks = S.toList $ M.fromList $ mapMaybe (uncurry scan4) $ zip [c..] $ S.toList items
   map 
-}
scan3 (State r@(Token c   ) b _ 0) = Just c
--scan3 c (State r@(Range c d) b _ 0) = ifJust (ch >= c && ch <= d) (State r b c 1)
scan3 t = Nothing

scan4 c (State r@(Token ch   ) b _ 0) = Just (ch, State r b c 1)
--scan3 c (State r@(Range c d) b _ 0) = ifJust (ch >= c && ch <= d) (State r b c 1)
scan4 c t = Nothing


children states (State2 f t i) = do
    s1@(State2 f1 t1 i1) <- S.toList $ states !! t
    if iaux i i1 && pass (result i1)
         then do
             s2@(State2 f2 t2 i2) <- S.toList $ states !! f1
             if rule i2 == rule i && f2 == f && pos i2 == pos i - 1
                  then if pos i2 > 0 then map (s1:) $ children states s2 else [[s1]]
             else []
         else []

data Tree z = Tree (State2 z) [Tree z] | Trees [Tree z] deriving (Eq, Ord)

tree start states =
   let
      [end] = filter ((0 ==) . from2) $ S.toList $ last states
    in
      tree1 states end

tree1 states end = Tree end $ tree2 $ reverse $ map reverse $ map2 (tree1 states) $ children states end

tree2 [x] = x

tree2 xs = map Trees xs

only [x] = x

instance Show z => Show (Tree z) where
    show tree = format1 1 $ convTree tree

convTree (Tree a b) = Data (show a) $ map convTree b
convTree (Trees b) = Data "TREES" $ map convTree b
{-
transeq :: Foldable t => S.Set ETrans -> t a -> (Rule tok) -> [[ETrans]]
>>> p expr "a+a+a"
-}

table str states =
   let
      (maxes, axes, fmts) = unzip3 $ zipWith (taux str $ 0 : maxes) states [0 ..]
      axis = Data.List.foldr Parser3.mergeStrs "" axes
    in
      do
         putStrLn $ unlines $ axis : map (' ':) (concat fmts) ++ [axis]
         return maxes

taux str maxes s c =
   let
      (ends, fmts) = unzip $ map (taux1 maxes max1 c) $ S.toList s
      (end1, num) = taux2 maxes max1 c c (show c) ' '
      (end2, tok) = if c > 0 then taux2 maxes max1 (c - 1) c (singleton $ str !! (c - 1)) ' ' else (end1, num)
      max1 = maximum (end1 : end2 : ends) + 5
    in
      (max1, Parser3.mergeStrs num tok, fmts)

taux1 ends max1 c state@(State e b _ d) = taux2 ends max1 b c (let s = show state in if b < c then s else "(" ++ s ++ ")") '-'

taux2 ends m b c sh fill =
   if
      | b < c ->
            let
               st = ends !! (b + 1)
               l1 = m - st - length sh
               l2 = div l1 2
               l3 = l1 - l2 - 1
             in
               (st + length sh, replicate st ' ' ++ replicate l2 fill ++ sh ++ replicate l3 fill)
      | b == c ->
            let
               st = ends !! b
               l = length sh
               l2 = div l 2
             in
               (st + l2, replicate (m - l2) ' ' ++ sh)

mergeStrs a b = zipWith (\x y -> if x == ' ' then y else x) a b ++ if length a > length b then drop (length b) a else drop (length a) b

t2 (State2 _ t _) = t

tableD str states =
   let
      shown = map show states
      range = [0..length str]
      nums  = map show [0..length str]
      numls = 0 : map length nums
      ends  = 0 : map (\a -> maximum $ zipWith (taux1D ends numls a) shown states) [0..length str]
      show1 = zipWith (taux2D ends numls) shown states 
      nums1 = map (taux3D ends nums ) [0..length str] 
      toks  = map (taux4D ends str  ) [0..length str] 
      axis  = Data.List.foldr Parser3.mergeStrs "" (nums1 ++ toks)
    in
      putStrLn $ unlines $ axis : show1 ++ [axis]
{- 
ends !! 0 = 0
ends !! 1 = 
-}
taux1D ends numls a sh st@(State2 f t i) = numls !! a + 
   if f == t 
      then let 
         l = length sh + 2
         in if | t     == a -> ends !!  f      +      div l 2  
               | t + 1 == a -> ends !! (f + 1) + (l - div l 2)
               | True       -> 0
      else let
         l = length sh + 4
         in if | t     == a -> ends !! (f + 1)
               | True       -> 0

taux2D ends numls sh st@(State2 f t i) = let
   l  = length sh
   in if f == t 
            then replicate (ends !! (f + 1) - div l 2) ' ' ++ "(" ++ sh ++ ")"
            else let
               l2 = ends !! (f + 2) - ends !! (f + 1)
               l3 = l2 - l
               l4 = div l 2
               l5 = l - l3
               in replicate (ends !! (f + 1) + numls !! (f + 1)) ' ' ++ replicate l4 '-' ++ sh ++ replicate l5 '-'

taux3D ends nums a = replicate (ends !! a) ' ' ++ nums !! a

taux4D ends str a = let
   sh = show $ str !! a
   l  = length sh
   in replicate (div (ends !! a + ends !! (a + 1) - l) 2) ' ' ++ sh

taux5D ends m b c sh fill =
   if
      | b < c ->
            let
               st = ends !! (b + 1)
               l1 = m - st - length sh
               l2 = div l1 2
               l3 = l1 - l2 - 1
             in
               (st + length sh, replicate st ' ' ++ replicate l2 fill ++ sh ++ replicate l3 fill)
      | b == c ->
            let
               st = ends !! b
               l = length sh
               l2 = div l 2
             in
               (st + l2, replicate (m - l2) ' ' ++ sh)

makelr states rule c = makelr1 states (S.singleton $ State rule 0 0 0) c

makelr1 states items c = predict items
{-
scanlr c t states = S.fromList $ mapMaybe (scan1 c (t !! (c - 1))) $ S.toList states
scanlr c ch (State r@(Token c   ) b _ 0) = ifJust (ch == c) (State r b c 1)
scanlr c ch (State r@(Range c d) b _ 0) = ifJust (ch >= c && ch <= d) (State r b c 1)
-}
combinescans c@(Token cr, cs) d@(Token dr, ds) =
   if cr == dr
      then [(Token cr, cs ++ ds)]
      else [c, d]
combinescans c@(Token cr, cs) d@(Range d1 d2, ds) =
   if cr >= d1 && cr <= d2
      then [(Range d1 (pred cr), ds), (Token cr, cs ++ ds), (Range (succ cr) d2, ds)]
      else [c, d]
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
-- closure i with predictions (if you have completions there's a problem)
-- scan all possible tokens into separate states
-- same token may have multiple meanings, combine them into one state
-- those are your new kernels
-- closure them with completions and predictions
-- 
-- how kernels are compared for equality defines what sort of parser i is
-- some record little context, some account for a lot, if i accounts for all context i can't do recursive grammars
pF e t = tree e $ parseE e t

pFD e t = do
    states <- parseED e t
    table t states
    table t $ map (S.filter isComplete) states
    return $ tree e states

parseF e t =
   let
      items = predict (S.singleton $ State e 0 0 0)
    in
      parseF0 [items] t items [1 .. length t]

parseFD e t =
   let
      items = predict (S.singleton $ State e 0 0 0)
    in
      parseF0D [items] t items [1 .. length t]

parseF0 states _ items [] = states
parseF0 states t items (c : ks) = let
   itemsnew = parseE1 states t items c
   in parseE0 (states ++ [itemsnew]) t itemsnew ks

parseF0D states _ items [] = return states
parseF0D states t items (c : ks) = do
   itemsnew <- parseF1D states t items c
   parseF0D (states ++ [itemsnew]) t itemsnew ks

-- scanM :: Monad m => (m [a] -> b -> m a) -> m [a] -> [b] -> m [a]
scanFM f z = foldl (\a b -> do ra <- a; rr <- f (last ra) b; return $ ra ++ [rr]) (return [z])

parseF1 states t items c = let
   (pass, fail) = scanZ c t items
   comp1 = completeZ states pass
   in if c < length t
      then predictZ comp1
      else comp1

parseF1D states t items c = do
   let scan1 = scan c t items
   putStrLn $ "scan1=" ++ show scan1
   let comp1 = complete states scan1
   putStrLn $ "comp1=" ++ show comp1
   if c < length t
      then do
         let pred1 = predict comp1
         putStrLn $ "pred1=" ++ show pred1
         return pred1
      else return comp1

closureF f items = closure1 f items items

closureF1 f done current =
   let
      new = f done current
      newdone = S.union done new
      newnew = new S.\\ done
    in
      if S.null new then done else closure1 f newdone newnew

processF f old current = foldr S.union S.empty $ S.map (S.fromList . f) current

predictF = closure (process predict1)

predictFZ = closure (process predict1Z)

predictF1 (State r j c d) = [State a c c 0 | a <- paux r d]

predictF1Z j c (ISeq   (Seq   as ) t n) = [State2 c c (start (as !! n))]
predictF1Z j c (IAlt   (Alt   as ) t n) = [State2 c c (start a) | a <- as]
predictF1Z j c (IName  (Name  a b) t  ) = [State2 c c (start b)]
predictF1Z j c (IApply (Apply a b) t  ) = [State2 c c (start b)]
predictF1Z j c (IGet   (Get   a  ) t  ) = [State2 c c (start a)]
predictF1Z j c (ISet   (Set   a b) t  ) = [State2 c c (start b)]

--predict1Z (State2 j c (ISeq (Seq as) t n)) = [State2 c c (start a) | a <- [as !! n]]

--paux (Seq  as ) q = [as !! q | q < length as]
pauxF (Alt  as ) 0 = as
pauxF (Name a b) 0 = [b]
--paux (ManyTill a b) 0 = [a, b]
pauxF _ _ = []

startF r@(Seq   a  ) = ISeq  r Running 0
startF r@(Alt   a  ) = IAlt  r Running $ length a
startF r@(Name  a b) = IName r Running
startF r@(Token a  ) = ITerm r Running

{-}
predict1 (State r@(Alt as) j c 0) = [State  a        c c 0 | a <-       as]
predict1 (State r@(Seq as) j c d) = [State (as !! d) c c 0 | d < length as]
predict1 (State (Name a b) j c 0) = [State  b        c c 0                ]
predict1 t = []

predict1Z (State2 j c (Item (Alt   as  ) 0)) = [State2 c c (Item  a        0) | a <-       as]
predict1Z (State2 j c (Item (Seq   as  ) d)) = [State2 c c (Item (as !! d) 0) | d < length as]
predict1Z (State2 j c (Item (Name  a  b) 0)) = [State2 c c (Item  b        0)                ]
predict1Z t = []
-}

--scan c t items = S.fromList $ mapMaybe (scan1 c (t !! (c - 1))) $ S.toList items
scanF c t items = let
   sc    = map (scan1 c (t !! (c - 1))) $ S.toList items
   scf f = map snd $ filter (f . fst) sc
   in (scf id, scf not)

scanFZ c t items = let
   sc    = mapMaybe (scan1Y c (t !! (c - 1))) $ S.toList items
   scf f = filter ((== f) . result . item2) sc
   in (scf Pass, scf Fail)


--scan1 c ch (State r j _ 0) = ifJust (saux ch r) (State r j c 1)
--scan1 c ch t = Nothing

scanF1 c ch (State r j k1 n) = let pass = n == 0 && saux ch r in (pass, if pass then State r j c 1 else State r j k1 n)

scanF1Y c ch (State2 j _ t) = scan1Z c ch j c t

scanF1Z c ch j _ (ITerm r Running) = Just $ State2 j c (toDyn $ ITerm r (if saux ch r then Pass else Fail))
scanF1Z c ch _ _ _ = Nothing

sauxF ch (Token c  ) = ch == c
sauxF ch (Range c d) = ch >= c && ch <= d
sauxF _ _ = False

completeF states = closure (\old -> process (complete1 (states ++ [old])) old)

completeF1 states (State y b c p) = mapMaybe 
   (\(State x a b1 q) -> 
      ifJust (p == slength y && q < slength x && caux x q y) 
         $ State x a c (q + 1)
   ) $ S.toList $ states !! b

completeF1Z1 states (State2 b c i) =
   mapMaybe (\(State2 a b1 (ISeq x q n)) ->
      ifJust (result i /= Running && q == Running && caux x n (rule i)) $
         State2 a c (ISeq x (if result i == Fail then Fail else if n + 1 == slength x then result i else Running) (n + 1))
   ) $ S.toList $ states !! b

completeF1Z2 states (State2 b c i) =
   mapMaybe (\(State2 a b1 (IAlt x q n)) ->
      ifJust (result i /= Running && q == Running && caux x 0 (rule i)) $
         State2 a c (if pass $ result i then IAlt x (result i) n else if pos i > 0 then IAlt x Running (pos i-1) else IAlt x Fail 0)
   ) $ S.toList $ states !! b

completeF1Z3 states (State2 b c i) =
   mapMaybe (\(State2 a b1 (IName x q)) ->
      ifJust (result i /= Running && q == Running && caux x 0 (rule i)) $
         State2 a c (IName x $ result i)
   ) $ S.toList $ states !! b

completeF1Z4 states (State2 b c i) =
   mapMaybe (\(State2 a b1 (IApply x q)) ->
      ifJust (q == Running && caux x 0 (rule i)) $
         case result i of
            Pass res -> 
               case x of
                  Apply iso _ ->
                     case apply iso res of
                        Just ju -> State2 a c (IApply x (Pass ju))
                        Nothing -> State2 a c (IApply x Fail)
            Fail -> State2 a c (IApply x Fail)
   ) $ S.toList $ states !! b

completeF3  states items = foldl complete4 states items

completeF32 states items = foldl complete42 states items

completeF4 states (State y b c p) = foldl (complete5 p y) states $ S.toList $ states !! b

completeF42 states (State2 b c (Item y p)) = foldl (complete52 p y) states $ S.toList $ states !! b

completeF5 p y states (State x a b1 q) = states ++
   [ S.singleton $ State x a (length states) (q + 1)
   | p == slength y && q < slength x && caux x q y ]

completeF52 p y states (State2 a b1 (Item x q)) = states ++
   [ S.singleton $ State2 a (length states) (Item x (q + 1))
   | p == slength y && q < slength x && caux x q y ]
-- complete3 c states (State y b _ p) = mapMaybe (\(State x a b q) -> ifJust (p == slength y && q < slength x && caux x q y) $ EComplete3 (State x a b q) (State x a c (q + 1)) (State y b c p)) $ S.toList $ states !! b

cauxF (Seq as) q y = as !! q == y
cauxF (Alt as) q y = y `elem` as
cauxF (Name a b) q y = b == y
--caux (ManyTill a b) q y = a == y
cauxF _ _ _ = False

slengthF (Seq as) = length as
slengthF _ = 1

-}