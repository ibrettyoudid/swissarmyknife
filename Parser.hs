{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Parser where

import Favs
import MyPretty2

import Control.Monad
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S

data Rule
  = RSeq [Rule]
  | RAlt [Rule]
  | RName String Rule
  | RChar Char
  | RRang Char Char
  | REmpt -- epsilon, not used, use an empty RSeq instead

--   EIso   :: Iso alpha beta -> Rule alpha -> Rule beta

instance Eq Rule where
  RSeq as == RSeq bs = as == bs
  RAlt as == RAlt bs = as == bs
  RName a _ == RName b _ = a == b
  RChar a == RChar b = a == b
  RRang a c == RRang b d = a == b && c == d
  REmpt == REmpt = True
  _ == _ = False

instance Ord Rule where
  compare (RSeq as) (RSeq bs) = compare as bs
  compare (RAlt as) (RAlt bs) = compare as bs
  compare (RName a _) (RName b _) = compare a b
  compare (RChar a) (RChar b) = compare a b
  compare (RRang a c) (RRang b d) = compare (a, c) (b, d)
  compare (RName{}) _ = LT
  compare (RSeq{}) (RName{}) = GT
  compare (RSeq{}) _ = LT
  compare (RAlt{}) (RName{}) = GT
  compare (RAlt{}) (RSeq{}) = GT
  compare (RAlt{}) _ = LT
  compare (RChar{}) (RName{}) = GT
  compare (RChar{}) (RSeq{}) = GT
  compare (RChar{}) (RAlt{}) = GT
  compare (RChar{}) _ = LT
  compare (RRang{}) (RName{}) = GT
  compare (RRang{}) (RSeq{}) = GT
  compare (RRang{}) (RAlt{}) = GT
  compare (RRang{}) (RChar{}) = GT
  compare (RRang{}) _ = LT
  compare (REmpt{}) _ = GT

instance Show Rule where
  show = outershow Nothing

innershow d (RSeq   as) = unwords $ ii d $ map (innershow Nothing) as
innershow d (RAlt   as) = unwords $ ii d [intercalate " | " $ map (innershow Nothing) as]
innershow d (RName a _) = unwords $ ii d [a]
innershow d (RChar a  ) = unwords $ ii d [show a]
innershow d (RRang a b) = unwords $ ii d ["[" ++ [a] ++ ".." ++ [b] ++ "]"]

outershow d r@(RSeq  as ) = "Seq " ++ innershow d r
outershow d r@(RAlt  as ) = "Alt " ++ innershow d r
outershow d r@(RName a b) = innershow d r ++ " -> " ++ innershow Nothing b
outershow d r@(RChar a  ) = show a
outershow d r@(RRang a b) = "[" ++ show a ++ ".." ++ show b ++ "]"

ii (Just d) = insertIndex d "."
ii Nothing = id

data EState
  = EState { r::Rule, f::Int, t::Int, n::Int }
  | Predict
  | Scan
  | Complete
  | Complete3
  deriving (Eq)

data ETrans
  = EPredict   {from :: EState, to :: EState}
  | EScan      {from :: EState, to :: EState}
  | EComplete  {from :: EState, to :: EState}
  | EComplete3 {from :: EState, to :: EState, with :: EState}
  deriving (Eq, Ord, Show)

instance Show EState where
  show (EState r j k d) = outershow (Just d) r ++ " " ++ unwords (map show [j, k])
  show Predict   = "Predict"
  show Scan      = "Scan"
  show Complete  = "Complete"
  show Complete3 = "Complete3"

instance Ord EState where
  compare (EState e1 j1 k1 d1) (EState e2 j2 k2 d2) = compare (j1, k1, d1, e1) (j2, k2, d2, e2)
  compare a b = compare (findstate a) (findstate b)

findstate (EState{}) = 0
findstate Predict = 1
findstate Scan = 2
findstate Complete = 3
findstate Complete3 = 4

digit = RRang '0' '9'
lower = RRang 'a' 'z'
upper = RRang 'A' 'Z'
under = RChar '_'
alpha = RAlt [upper, lower]
num = RAlt [digit, RSeq [digit, num]]
op = RAlt [RChar '+', RChar '-', RChar '*', RChar '/']

ident = RName "ident"
expr = RName "expr" $ RAlt [RSeq [expr, RChar '+', expr], RChar 'a']

p e s = do
  (t, r) <- parseeD e s
  table s r
  let tr1 = transeq1 t r e
  putStrLn (show (length tr1) ++ " paths")
  --let Just (lev, _) = levenshteinMulti tr1
  --let tr2 = transpose lev
  --putGrid $ map (map (\case Just x -> show x; Nothing -> "")) tr2
  putGrid $ map2 show tr1
  putStrLn ""
  let tr = transeq t r e
  putStrLn (show (length tr) ++ " paths")
  mapM_ print tr
  putStrLn ""
  putStrLn (show (length t) ++ " transitions")
  mapM_ print t
  putStrLn ""
  putStrLn "splits"
  mapM_ print $ splits t
  putStrLn ""
  putStrLn "joins"
  mapM_ print $ joins t
  return $ tree e r

parseeD e s =
  let
    states = predict 0 (S.empty, S.singleton $ EState e 0 0 0)
   in
    parsee0D [snd states] s states [1 .. length s]

parsee0D allstates _ states [] = return (fst states, allstates)
parsee0D allstates s states (k : ks) = do
  statesnew <- parsee1D allstates s states k
  parsee0D (allstates ++ [snd statesnew]) s statesnew ks

-- scanM :: Monad m => (m [a] -> b -> m a) -> m [a] -> [b] -> m [a]
scanM f z = foldl (\a b -> do ra <- a; rr <- f (last ra) b; return $ ra ++ [rr]) (return [z])

parsee1D allstates s states k = do
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

close f (trans, states) = close1 f trans states states

close1 f oldtrans old new =
  let
    newtrans = f old new S.\\ oldtrans
    oldtrans1 = S.union oldtrans newtrans
    new1 = getStates newtrans S.\\ old
    old1 = S.union old new1
   in
    if S.null newtrans then (oldtrans, old) else close1 f oldtrans1 old1 new1

getStates trans = S.map to trans

process1 f current = foldr S.union S.empty $ S.map (S.fromList . f) current

process phase f new = foldr S.union S.empty $ S.map (\x -> S.fromList $ map (phase x) $ f x) new

predict k = close (\old new -> process EPredict (predict1 k) new)

predict1 k (EState (RAlt as) j _ 0) = [EState a k k 0 | a <- as]
predict1 k (EState (RSeq as) j _ d) = [EState (as !! d) k k 0 | d < length as]
predict1 k (EState (RName a b) j _ 0) = [EState b k k 0]
predict1 k s = []

scan k s (oldtrans, states) =
  let
    newtrans = S.fromList $ mapMaybe (\x -> EScan x <$> scan1 k (s !! (k - 1)) x) $ S.toList states
   in
    (S.union oldtrans newtrans, getStates newtrans)

scan1 k ch (EState (RChar c) j _ 0) = ifJust (c == ch) (EState (RChar c) j k 1)
scan1 k ch s = Nothing

scanEmpty = map scanEmpty1

scanEmpty1 (EState REmpt j _ 0) = EState REmpt j j 1
scanEmpty1 s = s

complete k allstates = close (\old new -> process EComplete (complete1 k (allstates ++ [old])) new)

complete1 k allstates (EState y j _ p) = mapMaybe (\(EState x i _ q) -> ifJust (p == slength y && q < slength x && caux x q y) $ EState x i k (q + 1)) $ S.toList $ allstates !! j

complete2 k allstates = close (\old new -> process1 (complete3 k (allstates ++ [old])) new)

complete3 k allstates (EState y j _ p) = mapMaybe (\(EState x i j q) -> ifJust (p == slength y && q < slength x && caux x q y) $ EComplete3 (EState y j k p) (EState x i k (q + 1)) (EState x i j q)) $ S.toList $ allstates !! j

-- complete3 k allstates (EState y j _ p) = mapMaybe (\(EState x i j q) -> ifJust (p == slength y && q < slength x && caux x q y) $ EComplete3 (EState x i j q) (EState x i k (q + 1)) (EState y j k p)) $ S.toList $ allstates !! j

caux (RSeq as) q y = as !! q == y
caux (RAlt as) q y = y `elem` as
caux (RName a b) q y = b == y
caux _ _ _ = False

slength (RSeq as) = length as
slength _ = 1

children allstates (EState r f t n) = do
   s1@(EState r1 f1 t1 n1) <- S.toList $ allstates !! t
   if caux r (n - 1) r1 && n1 == slength r1
      then do
         s2@(EState r2 f2 t2 n2) <- S.toList $ allstates !! f1
         if r2 == r && f2 == f && n2 == n - 1
            then if n2 > 0 then map (s1:) $ children allstates s2 else [[s1]]
         else []
      else []
--children allstates (EState (RAlt as) f t s) = allstates !! t
{-
children allstates (EState r@(RName a b) f t n) = do
   s1@(EState r1 f1 t1 n1) <- S.toList $ allstates !! t
   if r1 == b && n1 == slength r1
      then do
         s2@(EState r2 f2 t2 n2) <- S.toList $ allstates !! f1
         if r2 == r && f2 == f && n2 == n - 1
            then if n2 > 0 then map (s1:) $ children allstates s2 else [[s1]]
         else []
      else []
-} 
--children allstates (EState (RChar a) f t s) = allstates !! t

{-
predict ABC = CDE
predict DE = EF
predict F = []

queue ABC ABC
   old = ABC
   cur = ABC
   new = CDE
   new1 = DE
   res = DE + queue ABC DE

   cur = DE
   new = EF
   new1 = F
   res = F + queue ABCDE
-}
{-
table allstates = let
   a = concat $ zipWith (\s k -> zip (map show $ S.toList s) $ repeat k) allstates [0..]
   b = map (\(s, k) ->
-}
data Tree = Tree EState [Tree] | Branch [Tree] deriving (Eq, Ord)

tree start allstates =
  let
    end = EState start 0 (length allstates - 1) (slength start)
    success = S.member end $ last allstates
   in
      tree1 allstates end

tree1 allstates end = Tree end $ tree2 $ reverse $ map reverse $ map2 (tree1 allstates) $ children allstates end

tree2 [x] = x

tree2 xs = map Branch xs

only [x] = x

instance Show Tree where
   show tree = format1 1 $ conv tree
   
   
conv (Tree a b) = Data (show a) $ map conv b
conv (Branch b) = Data "branch" $ map conv b
{-
transeq :: Foldable t => S.Set ETrans -> t a -> Rule -> [[ETrans]]
>>> p expr "a+a+a"
-}

splits alltrans = filter (\(x, y) -> length y > 1) $ M.toList $ mapFromList (:) [] $ map (\x -> (from x, to x)) $ S.toList alltrans

joins alltrans = filter (\(x, y) -> length y > 1) $ M.toList $ mapFromList (:) [] $ map (\x -> (to x, from x)) $ S.toList alltrans

transeq alltrans allstates startrule =
  let
    transmap = multimapOn to $ S.toList alltrans
    start = EState startrule 0 0 0
    end = EState startrule 0 (length allstates - 1) (slength startrule)
    splits1 = S.insert start $ S.fromList $ map fst $ splits alltrans
    joins1 = S.insert end $ S.fromList $ map fst $ joins alltrans
    stops = S.union splits1 joins1
   in
    sort $ concatMap (\x -> ts2 transmap startrule [x] stops x) $ S.toList stops

{-
[. expr 0 0 0,Predict,. expr + expr | a 0 0 0]
[. expr + expr | a 0 0 0,Predict,. expr + expr 0 0 0,Predict,. expr 0 0 0]
[. expr + expr | a 0 0 0,Predict,. a 0 0 0,Scan,a . 0 1 1,Complete,expr + expr | a . 0 1 1,Complete,expr . 0 1 1,Complete,expr . + expr 0 1 1,Predict,. + 1 1 0,Scan,+ . 1 2 1,Complete,expr + . expr 0 2 2,Predict,. expr 2 2 0]
[. expr 2 2 0,Predict,. expr + expr | a 2 2 0]
[. expr + expr | a 2 2 0,Predict,. expr + expr 2 2 0,Predict,. expr 2 2 0]
[. expr + expr | a 2 2 0,Predict,. a 2 2 0,Scan,a . 2 3 1,Complete,expr + expr | a . 2 3 1,Complete,expr . 2 3 1]
[expr . 2 3 1,Complete,expr + expr . 0 3 3,Complete,expr + expr | a . 0 3 1,Complete,expr . 0 3 1,Complete,expr . + expr 0 3 1,Predict,. + 3 3 0]
[expr . 2 3 1,Complete,expr . + expr 2 3 1,Predict,. + 3 3 0]
[. + 3 3 0,Scan,+ . 3 4 1]
[+ . 3 4 1,Complete,expr + . expr 0 4 2,Predict,. expr 4 4 0]
[+ . 3 4 1,Complete,expr + . expr 2 4 2,Predict,. expr 4 4 0]
[. expr 4 4 0,Predict,. expr + expr | a 4 4 0]
[. expr + expr | a 4 4 0,Predict,. expr + expr 4 4 0,Predict,. expr 4 4 0]
[. expr + expr | a 4 4 0,Predict,. a 4 4 0,Scan,a . 4 5 1,Complete,expr + expr | a . 4 5 1,Complete,expr . 4 5 1]
[expr . 4 5 1,Complete,expr + expr . 2 5 3,Complete,expr + expr | a . 2 5 1,Complete,expr . 2 5 1]


[expr . 2 5 1,Complete,expr + expr . 0 5 3]
[expr . 4 5 1,Complete,expr + expr . 0 5 3]
[expr + expr . 0 5 3,Complete,expr + expr | a . 0 5 1,Complete,expr . 0 5 1]
-}

transeq1 alltrans allstates start =
  let
    transmap = multimapOn to $ S.toList alltrans
    end = EState start 0 (length allstates - 1) (slength start)
   in
    ts3 transmap start [end] S.empty end

ts1 transmap start done stops s0 = do
  tr <- transmap M.! s0
  let s1@(EState r i j k) = with tr
  if
    | S.member s1 stops -> [s1 : done]
    | otherwise -> ts2 transmap start (s1 : done) stops s1

ts2 transmap start done stops s0 = do
  tr <- transmap M.! s0
  let s1@(EState r i j k) = from tr
  if
    | S.member s1 stops -> [tsaux tr done]
    | otherwise -> ts2 transmap start (tsaux tr done) stops s1

ts3 transmap start done set s0 = do
  tr <- transmap M.! s0
  let s1@(EState r i j k) = from tr
  if
    | r == start && j == 0 -> [tsaux tr done]
    | S.member s1 set -> []
    | otherwise -> ts3 transmap start (tsaux tr done) (S.insert s1 set) s1

tsaux (EPredict from to) done = from : Predict : done
tsaux (EScan from to) done = from : Scan : done
tsaux (EComplete from to) done = from : Complete : done
tsaux (EComplete3 from to with) done = from : with : Complete3 : done

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

taux1 ends max1 k state@(EState e j _ d) = taux2 ends max1 j k (let s = show state in if j < k then s else "(" ++ s ++ ")") '-'

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
