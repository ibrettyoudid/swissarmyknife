{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Parser3 where

import Favs
import MyPretty2 hiding (format)
import MHashDynamic hiding (Apply, expr)
import Iso hiding (foldl, (!!))
--import Rule
--import Syntax3 hiding (foldl, foldr)

import Control.Monad
import Control.Monad.State qualified as St
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.IntMap qualified as I
import Shell (ch)

data Rule tok =
   Seq [Rule tok]
   | Alt [Rule tok]
   | Name String (Rule tok)
   | Token tok
   | Range tok tok
   | Many (Rule tok)
   | ManyTill (Rule tok) (Rule tok)
   | Apply (Iso Dynamic Dynamic) (Rule tok)
   | Bind (Rule tok) (Dynamic -> Rule tok, Dynamic -> Rule tok -> Rule tok) -- parsing, the iso turns the result of the nested rule into a new rule. printing, we have the result of the new rule but not the new rule itself. we need to use the nested rule and the result to recreate it
   | Return Dynamic
   | StrRes String Dynamic
   | Not (Rule tok)
   | And (Rule tok)
   | Set String (Rule tok)
   | Get String

{-
parse (Token a) = a
parse (Ignore a) = 
format (Token a) = a
-}
--    EIso    :: Iso alpha beta -> Rule alpha -> Rule beta
fd f x = do d <- fromDynamic x; r <- f d; return $ toDyn r

isod (Iso f g) = Iso (fd f) (fd g)

totald f g = isod (total f g)

strOfChars :: [Dynamic] -> String
strOfChars = map fromDyn1

charsOfStr :: String -> [Dynamic]
charsOfStr = map toDyn

repl1 r = do n <- St.get; return $ Seq $ replicate (fromDyn n 0 :: Int) r

repl n r = Seq $ replicate (fromDyn n 0 :: Int) r

repl2 re r = Seq $ replicate (length (fromDyn re [] :: [Dynamic])) r

repl3 int r = int >>== (\n -> repl n r, repl2)

intiso :: Iso String Int
intiso = total read show 

intisod = isod intiso

int = Apply intisod $ Range '0' '9'

(>>==) = Bind

test = int >>== (\n -> repl n $ Range 'a' 'z', repl2)
--icons = isod (\(x, xs) -> Just (x:xs)) (\(x:xs) -> Just (x, xs))

{-
simulate this:
do
   n <- int
   rep n anychar

which is

int >>= (\n -> rep n anychar)

-}

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
innershow :: Show tok => Maybe Int -> Rule tok -> [Char]
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

data Item tok  = Item   (Rule tok) Result Item2 deriving (Eq, Ord)

data Item2     = Item2
               | ISeq  Int
               | IAlt  Int -- Int is how many have finished
               | IName
               | ITerm   --used for all terminal level ops
               | IMany [Dynamic]
               | INot
               | IAnd
               | IGet
               | ISet
               | IApply
               deriving (Eq, Ord)

item  (State2 b c i) = i
from2 (State2 b c i) = b
to2   (State2 b c i) = c

result (Item r re i) = re
rule   (Item r re i) = r
item2  (Item r re i) = i

pos2 (ISeq n) = n
pos2 (IAlt n) = n

pos (Item r s i) = pos2 i

pass (Pass _) = True
pass _        = False

passi = pass . result

type ItemSet tok = S.Set (Item tok)

instance Show tok => Show (Item tok) where
   show (Item r s1 (ISeq n)) = outershow (Just n) r
   show (Item r s1 (IAlt n)) = outershow (Just n) r
   show (Item r s1 _) = outershow Nothing r

data State tok
   = State { r::Rule tok, f::Int, t::Int, n::Int }
   deriving (Eq)

data State2 tok = State2 Int Int (Item tok) deriving (Eq, Ord)

instance Show z => Show (State z) where
   show (State r b c d) = outershow (Just d) r ++ " " ++ unwords (map show [b, c, d])

instance Ord z => Ord (State z) where
   compare (State e1 b1 k1 d1) (State e2 j2 k2 d2) = compare (b1, k1, d1, e1) (j2, k2, d2, e2)

instance Show z => Show (State2 z) where
   show (State2 b c r) = show r ++ " " ++ unwords (map show [b, c])
{-
instance Ord z => Ord (State2 z) where
   compare (State2 b1 k1 e1) (State2 j2 k2 e2) = compare (b1, k1, e1) (j2, k2, e2)
-}
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

--p s t = tree s $ parseE s t

{-
pD s t = do
    states <- parseED s t
    tableZ t $ concatMap S.toList states
    tableZ t $ filter isCompleteZ $ concatMap S.toList states
    return $ tree s states
-}
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
   let comp1 = completeZ states $ S.fromList scan1
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

predict :: Ord tok => S.Set (State tok) -> S.Set (State tok)
predict = closure (process predict1)

predictZ :: Ord tok => S.Set (State2 tok) -> S.Set (State2 tok)
predictZ = closure (process predict1Z)

predict1 (State r j c d) = [State a c c 0 | a <- paux r d]

--paux (Seq  as ) q = [as !! q | q < length as]
paux (Alt  as ) 0 = as
paux (Name a b) 0 = [b]
--paux (ManyTill a b) 0 = [a, b]
paux _ _ = []

predict1Z (State2 _ c i) = map (State2 c c) $ predict2Z i

predict2Z (Item (Seq    as   ) Running (ISeq n)) = [start (as !! n)]
predict2Z (Item (Alt    as   ) Running (IAlt 0)) = [start a | a <- as]
predict2Z (Item (Many   a    ) Running _       ) = [start a]
predict2Z (Item (Name   a b  ) Running _       ) = [start b]
predict2Z (Item (Apply  a b  ) Running _       ) = [start b]
predict2Z (Item (Set    a b  ) Running _       ) = [start b]
predict2Z (Item (Not    a    ) Running _       ) = [start a]
predict2Z (Item (Bind   a b  ) Running _       ) = [start a]
predict2Z (Item (Return a    ) Running _       ) = [Item (Return a) (Pass a) Item2]
--predict2Z (Item (Get   a  ) t _) = [Item (Get a) (Pass $ lookup a) Item2]
predict2Z (Item {}) = []

start r = Item r Running $ start1 r

start1 (Seq  a) = ISeq 0
start1 (Alt  a) = IAlt 0
start1 (Many a) = IMany []
start1 _ = Item2

scan c t items = let
   sc    = map (scan1 c (t !! (c - 1))) $ S.toList items
   scf f = map snd $ filter (f . fst) sc
   in (scf id, scf not)

scanZ c t items = mapMaybe (scan1Y c (t !! (c - 1))) $ S.toList items


--scan1 c ch (State r j _ 0) = ifJust (saux ch r) (State r j c 1)
--scan1 c ch t = Nothing

scan1 c ch (State r j k1 n) = let pass = n == 0 && saux ch r in (pass, if pass then State r j c 1 else State r j k1 n)

scan1Y c ch (State2 j _ t) = scan1Z c ch j c t

scan1Z c ch j _ (Item r Running i2) = Just $ State2 j c $ Item r (if saux ch r then Pass $ toDyn ch else Fail) i2
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

complete1Z states state =
   concatMap (complete2Z state) $ S.toList $ states !! from2 state

complete2Z (State2 b c sub) (State2 a b1 main) =
      if result sub /= Running && result main == Running && subitem main sub
         then map (State2 a c) $ complete3Z main sub
         else []

complete3Z main@(Item r@(Seq as) s (ISeq n)) sub
  | result sub == Fail = [Item r Fail         (ISeq  n     )]
  | n + 1 == length as = [Item r (result sub) (ISeq (n + 1))]
  | otherwise          = [Item r Running      (ISeq (n + 1))]

complete3Z main@(Item x@(Alt as) q (IAlt n)) sub
  | passi sub     = [Item x (result sub) (IAlt  n     )]
  | n < length as = [Item x Running      (IAlt (n + 1))]
  | otherwise     = [Item x Fail         (IAlt  n     )]

complete3Z main@(Item x@(Name d e) q i2) sub = [Item x (result sub) i2]

complete3Z main@(Item x@(Apply d e) q _) sub =
   case result sub of
      Pass res ->
         case x of
            Apply iso _ ->
               case apply iso res of
                  Just j  -> [Item x (Pass j) Item2]
                  Nothing -> [Item x  Fail    Item2]
      Fail -> [Item x Fail Item2]

complete3Z main@(Item x@(Not d) q i2) sub = [Item x (case result sub of { Pass p -> Fail; Fail -> Pass (toDyn ()) } ) Item2]

complete3Z main@(Item x@(Bind a (b, c)) q _) sub = 
   case result sub of
      Pass res -> 
         case b res of
            r2 -> [start r2]
      Fail -> [Item x Fail Item2]

complete3Z main@(Item x@(Many a) q (IMany done)) sub =
   Item x (Pass $ toDyn done) Item2 :
   case result sub of
      Pass res -> [Item x Running (IMany $ done ++ [res])]
      Fail     -> []
      


caux (Seq as) q y = as !! q == y
caux (Alt as) q y = y `elem` as
caux (Name a b) q y = b == y
--caux (ManyTill a b) q y = a == y
caux _ _ _ = False

subitem (Item (Seq   as ) _ (ISeq n)) ch = as !! n == rule ch
subitem (Item (Alt   as ) _ _       ) ch = rule ch `elem` as
subitem (Item (Name  a b) _ _       ) ch = b == rule ch
subitem (Item (Apply a b) _ _       ) ch = b == rule ch
subitem (Item (Not   a  ) _ _       ) ch = a == rule ch
subitem (Item (Many  a  ) _ _       ) ch = a == rule ch

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
{-
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
-}

children states (State2 f t i) = do
    s1@(State2 f1 t1 i1) <- S.toList $ states !! t
    if subitem i i1 && pass (result i1)
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
-- how kernels are compared for equality defines what sort of parser it is
-- some record little context, some account for a lot, if it accounts for all context it can't do recursive grammars

tableZ str states =
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
