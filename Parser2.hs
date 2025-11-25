{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Parser2 where

import Favs
import MyPretty2

import Control.Monad
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S

data Rule
   = Seq [Rule]
   | Alt [Rule]
   | RuleName String Rule
   | Token Char
   | RRang Char Char
   | Func String (State -> [State])

--   EIso   :: Iso alpha beta -> Rule alpha -> Rule beta

instance Eq Rule where
   Seq as == Seq bs = as == bs
   Alt as == Alt bs = as == bs
   RuleName a _ == RuleName b _ = a == b
   Token a == Token b = a == b
   RRang a c == RRang b d = a == b && c == d
   Func a _ == Func b _ = a == b
   _ == _ = False

instance Ord Rule where
   compare (Seq as) (Seq bs) = compare as bs
   compare (Alt as) (Alt bs) = compare as bs
   compare (RuleName a _) (RuleName b _) = compare a b
   compare (Token a) (Token b) = compare a b
   compare (RRang a c) (RRang b d) = compare (a, c) (b, d)
   compare (RuleName{}) _ = LT
   compare (Seq{}) (RuleName{}) = GT
   compare (Seq{}) _ = LT
   compare (Alt{}) (RuleName{}) = GT
   compare (Alt{}) (Seq{}) = GT
   compare (Alt{}) _ = LT
   compare (Token{}) (RuleName{}) = GT
   compare (Token{}) (Seq{}) = GT
   compare (Token{}) (Alt{}) = GT
   compare (Token{}) _ = LT
   compare (RRang{}) (RuleName{}) = GT
   compare (RRang{}) (Seq{}) = GT
   compare (RRang{}) (Alt{}) = GT
   compare (RRang{}) (Token{}) = GT
   compare (RRang{}) _ = LT
   compare (Func{}) _ = GT

instance Show Rule where
   show = outershow Nothing

innershow d (Seq   as) = unwords $ ii d $ map (innershow Nothing) as
innershow d (Alt   as) = unwords $ ii d [intercalate " | " $ map (innershow Nothing) as]
innershow d (RuleName a _) = unwords $ ii d [a]
innershow d (Token a  ) = unwords $ ii d [show a]
innershow d (RRang a b) = unwords $ ii d ["[" ++ [a] ++ ".." ++ [b] ++ "]"]

outershow d r@(Seq  as ) = "Seq " ++ innershow d r
outershow d r@(Alt  as ) = "Alt " ++ innershow d r
outershow d r@(RuleName a b) = unwords $ ii d [a ++ " -> " ++ innershow Nothing b]
outershow d r@(Token a  ) = show a
outershow d r@(RRang a b) = "[" ++ show a ++ ".." ++ show b ++ "]"

ii (Just d) = insertIndex d "."
ii Nothing = id

data State
   = State { r::Rule, f::Int, t::Int, n::Int }
   deriving (Eq)

instance Show State where
   show (State r j k d) = outershow (Just d) r ++ " " ++ unwords (map show [j, k])

instance Ord State where
   compare (State e1 j1 k1 d1) (State e2 j2 k2 d2) = compare (j1, k1, d1, e1) (j2, k2, d2, e2)

digit = RRang '0' '9'
lower = RRang 'a' 'z'
upper = RRang 'A' 'Z'
under = Token '_'
alpha = Alt [upper, lower]
num = Alt [digit, Seq [digit, num]]
alnum = Alt [alpha, num]
op = Alt [Token '+', Token '-', Token '*', Token '/']

ident = RuleName "ident" $ Seq [alpha, ident1]
ident1 = Alt [alnum, Seq [alnum, ident1]]

expr = RuleName "expr" $ Alt [Seq [expr, Token '+', expr], Token 'a']

test = p expr "a+a+a"

p e s = tree e $ parseE e s

pD e s = do
      states <- parseED e s
      table s states
      return $ tree e states

parseE e s = 
   let
      states = predict 0 (S.singleton $ State e 0 0 0)
      in
      parseE0 [states] s states [1 .. length s]

parseED e s =
   let
      states = predict 0 (S.singleton $ State e 0 0 0)
      in
      parseE0D [states] s states [1 .. length s]

parseE0 allstates _ states [] = allstates
parseE0 allstates s states (k : ks) = let
   statesnew = parseE1 allstates s states k
   in parseE0 (allstates ++ [statesnew]) s statesnew ks

parseE0D allstates _ states [] = return allstates
parseE0D allstates s states (k : ks) = do
   statesnew <- parseE1D allstates s states k
   parseE0D (allstates ++ [statesnew]) s statesnew ks

-- scanM :: Monad m => (m [a] -> b -> m a) -> m [a] -> [b] -> m [a]
scanM f z = foldl (\a b -> do ra <- a; rr <- f (last ra) b; return $ ra ++ [rr]) (return [z])

parseE1 allstates s states k = let
   scan1 = scan k s states
   comp1 = complete k allstates scan1
   in if k < length s
      then predict k comp1
      else comp1

parseE1D allstates s states k = do
   let scan1 = scan k s states
   putStrLn $ "scan1=" ++ show scan1
   let comp1 = complete k allstates scan1
   putStrLn $ "comp1=" ++ show comp1
   if k < length s
      then do
         let pred1 = predict k comp1
         putStrLn $ "pred1=" ++ show pred1
         return pred1
      else return comp1

conseq (a : b : cs)
   | a == b = a
   | otherwise = conseq (b : cs)

close f states = close1 f states states

close1 f done current =
   let
      new = f current
      newdone = S.union done new
      newnew = new S.\\ done
      in
      if S.null new then done else close1 f newdone newnew

close2 f states = close3 f states states

close3 f done current =
   let
      new = f done current
      newdone = S.union done new
      newnew = new S.\\ done
      in
      if S.null new then done else close3 f newdone newnew

process f current = foldr S.union S.empty $ S.map (S.fromList . f) current

predict k = close (process (predict1 k))

predict1 k (State (Alt  as ) j _ 0) = [State a k k 0 | a <- as]
predict1 k (State (Seq  as ) j _ d) = [State (as !! d) k k 0 | d < length as]
predict1 k (State (RuleName a b) j _ 0) = [State b k k 0]
predict1 k s@(State (Func n f) j _ 0) = f s
predict1 k s = []

scan k s states = S.fromList $ mapMaybe (scan1 k (s !! (k - 1))) $ S.toList states

scan1 k ch (State r@(Token c  ) j _ 0) = ifJust (ch == c) (State r j k 1)
scan1 k ch (State r@(RRang c d) j _ 0) = ifJust (ch >= c && ch <= d) (State r j k 1)
scan1 k ch s = Nothing

complete k allstates = close2 (\old -> process (complete1 k (allstates ++ [old])))

complete1 k allstates (State y j k1 p) = mapMaybe (\(State x i j1 q) -> ifJust (p == slength y && q < slength x && caux x q y) $ State x i k (q + 1)) $ S.toList $ allstates !! j

-- complete3 k allstates (State y j _ p) = mapMaybe (\(State x i j q) -> ifJust (p == slength y && q < slength x && caux x q y) $ EComplete3 (State x i j q) (State x i k (q + 1)) (State y j k p)) $ S.toList $ allstates !! j

caux (Seq as) q y = as !! q == y
caux (Alt as) q y = y `elem` as
caux (RuleName a b) q y = b == y
caux _ _ _ = False

slength (Seq as) = length as
slength _ = 1

children allstates (State r f t n) = do
      s1@(State r1 f1 t1 n1) <- S.toList $ allstates !! t
      if caux r (n - 1) r1 && n1 == slength r1
         then do
            s2@(State r2 f2 t2 n2) <- S.toList $ allstates !! f1
            if r2 == r && f2 == f && n2 == n - 1
                  then if n2 > 0 then map (s1:) $ children allstates s2 else [[s1]]
            else []
         else []

data Tree = Tree State [Tree] | Trees [Tree] deriving (Eq, Ord)

tree start allstates =
   let
      end = State start 0 (length allstates - 1) (slength start)
      success = S.member end $ last allstates
      in
         tree1 allstates end

tree1 allstates end = Tree end $ tree2 $ reverse $ map reverse $ map2 (tree1 allstates) $ children allstates end

tree2 [x] = x

tree2 xs = map Trees xs

only [x] = x

instance Show Tree where
      show tree = format1 1 $ conv tree
   
   
conv (Tree a b) = Data (show a) $ map conv b
conv (Trees b) = Data "TREES" $ map conv b
{-
transeq :: Foldable t => S.Set ETrans -> t a -> Rule -> [[ETrans]]
>>> p expr "a+a+a"
-}

table str allstates =
   let
      (maxes, axes, fmts) = unzip3 $ zipWith (taux str $ 0 : maxes) allstates [0 ..]
      axis = foldr mergeStrs "" axes
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
      (max1, mergeStrs num tok, fmts)

taux1 ends max1 k state@(State e j _ d) = taux2 ends max1 j k (let s = show state in if j < k then s else "(" ++ s ++ ")") '-'

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

makelr allstates rule k = makelr1 allstates (S.singleton $ State rule 0 0 0) k

makelr1 allstates states k = predict k states
{-
scanlr k s states = S.fromList $ mapMaybe (scan1 k (s !! (k - 1))) $ S.toList states
scanlr k ch (State r@(Token c  ) j _ 0) = ifJust (ch == c) (State r j k 1)
scanlr k ch (State r@(RRang c d) j _ 0) = ifJust (ch >= c && ch <= d) (State r j k 1)
-}
combinescans c@(Token cr, cs) d@(Token dr, ds) = if cr == dr then [(Token cr, cs ++ ds)] else [c, d]
combinescans c@(Token cr, cs) d@(RRang d1 d2, ds) = if cr >= d1 && cr <= d2 then [(RRang d1 (pred cr), ds), (Token cr, cs ++ ds), (RRang (succ cr) d2, ds)] else [c, d]
combinescans c@(RRang c1 c2, cs) d@(RRang d1 d2, ds) = 
   if c2 >= d1 && c1 <= d2 then 
      let
         o1 = max c1 d1
         o2 = min c2 d2
      in if c1 < d1
               then [(RRang c1 (pred o1), cs), (RRang o1 o2, cs ++ ds), (RRang (succ o2) d2, ds)]
               else [(RRang d1 (pred o1), ds), (RRang o1 o2, cs ++ ds), (RRang (succ o2) c2, cs)]
   else
      [c, d]
-- start from start rule
-- thats your first kernel
-- close it with predictions (if you have completions there's a problem)
-- scan all possible tokens into separate states
-- same token may have multiple meanings, combine them into one state
-- those are your new kernels
-- close them with completions and predictions
-- 
-- how kernels are compared for equality defines what sort of parser it is
-- some record little context, some account for a lot, if it accounts for all context it can't do recursive grammars
