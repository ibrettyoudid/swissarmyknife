{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Parser where

import Favs

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
  | REmpt

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
  show (RSeq as) = unwords $ map show as
  show (RAlt as) = intercalate " | " $ map show as
  show (RName a _) = a
  show (RChar a) = singleton a
  show (RRang a b) = "[" ++ [a] ++ ".." ++ [b] ++ "]"

data EState = EState Rule Int Int Int deriving (Eq)

data ETrans
  = EPredict {from :: EState, to :: EState}
  | EScan {from :: EState, to :: EState}
  | EComplete {from :: EState, to :: EState}
  deriving (Eq, Ord, Show)

data Phase = Predict | Scan | Complete deriving (Eq, Ord, Show)

instance Show EState where
  show (EState e j k d) = case e of
    RSeq as -> unwords $ insertIndex d "." (map show as) ++ [show j, show k, show d]
    _ -> unwords $ insertIndex d "." [show e] ++ [show j, show k, show d]

instance Ord EState where
  compare (EState e1 j1 k1 d1) (EState e2 j2 k2 d2) = compare (j1, k1, d1, e1) (j2, k2, d2, e2)

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
  mapM_ (\x -> do putStrLn ""; mapM_ print x) $ transeq t r e

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

process f current = foldr S.union S.empty $ S.map (S.fromList . f) current

process1 phase f new = foldr S.union S.empty $ S.map (\x -> S.fromList $ map (phase x) $ f x) new

predict k = close (\old new -> process1 EPredict (predict1 k) new)

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

complete k allstates = close (\old new -> process1 EComplete (complete1 k (allstates ++ [old])) new)

complete1 k allstates (EState y j _ p) = mapMaybe (\(EState x i _ q) -> ifJust (p == slength y && q < slength x && caux x q y) $ EState x i k (q + 1)) $ S.toList $ allstates !! j

-- complete k allstates = close (\old new -> process (complete2 k (allstates ++ [old])) new)

-- complete2 k allstates (EState y j _ p) = mapMaybe (\(EState x i j q) -> ifJust (p == slength y && q < slength x && caux x q y) $ EComplete (EState x i j q) (EState x i k (q + 1)) (EState y j k p)) $ S.toList $ allstates !! j

caux (RSeq as) q y = as !! q == y
caux (RAlt as) q y = y `elem` as
caux (RName a b) q y = b == y
caux _ _ _ = True

slength (RSeq as) = length as
slength _ = 1

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
data Tree = Tree EState [Tree]

tree start allstates =
  let
    all1 = S.member (EState start 0 (length allstates - 1) (slength start)) $ last allstates
   in
    all1

transeq alltrans allstates start =
  let
    transmap = multimapOn to $ S.toList alltrans
    end = EState start 0 (length allstates - 1) (slength start)
   in
    ts2 transmap start [] S.empty end

ts1 _ x@(EState _ 0 0 _) = [x]
ts1 transmap x = x : maybe [] (ts1 transmap . from) (M.lookup x transmap)

ts2 transmap start done set s0 = do
  tr <- transmap M.! s0
  let s1@(EState r i j k) = from tr
  if| r == start && j == 0 -> [tr : done]
    | S.member s1 set      -> []
    | otherwise            -> ts2 transmap start (tr : done) (S.insert s1 set) s1

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
