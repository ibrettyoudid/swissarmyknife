-- Copyright 2025 Brett Curtis
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{- HLINT ignore "Use map once" -}

module Anagram where

import Favs
import MyPretty2
import Shell

import Debug.Trace

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Map.Internal as M
import System.Directory
import System.IO.Unsafe
import Data.Char
import Data.List
import Data.Maybe

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

{-

USAGE: 

go "letterstostartwith" "letterstoremove"

"letterstoremove" can be ""

it will tell you what letters are left, and list words that can be made with them

the idea is that you then use the history to call go again, with a chosen subgram
added to the letters to remove

-}

go letterstostartwith letterstoremove = let
   letters = letterstostartwith \\ letterstoremove
   checknull = letterstoremove \\ letterstostartwith
   subgrams = subgramsL letters
   in do
      putStrLn $ "letters left: "++letters
      unless (null checknull) $
         putStrLn $ "letters attempted to remove that weren't there:"++checknull
      putStrLn "subgrams:"
      print subgrams

anagrams letters = anagramsmwd letters

path = "/home/brett/swissarmyknife/words/" -- path to words directory

vocab = vocab0

-- filter reversible words (argument is a list of words)
vocabr v = let forwards  = S.fromList v
               backwards = S.fromList $ map reverse v
            in  S.toList $ S.intersection forwards backwards

vocab0 = voc 100

vocab1 = filter (notElem '\'') $ words $ map toLower $ readFileU "d:/code/bcb/anagram/words/english-words.50"

vocab2 = words $ map toLower $ readFileU (path ++ "scrabble.35")

vocab3 = words $ map toLower $ readFileU "/home/brett/code/haskell/swissarmyknife/scrabble.txt"

-- list english words up to a particular number
voc n = nubSet $ (["a","i","o"]++) $ concatMap readf $ filter (vfilt n) $ names path

readf fi = nubSet $ filter (\w -> length w > 1 && notElem '-' w) $ words $ map toLower $ readFileU (path++fi)

vfilt n f = let lst = split "." f in case lst of
                                          -- [a, b]    -> readInt b <= n
                                          -- [a, b]    -> "english" `isPrefixOf` a && readInt b <= n
                                          -- [a, b]    -> "words" `isInfixOf` a && readInt b <= n
                                          [a, b]    -> a == "english-words" && readInt b <= n
                                          _         -> False

readFiles dir filt = concatMap (\f -> readFileU (dir++f))
                  $ filter filt $ filter (\a -> a /= "." && a /= "..")
                  $ unsafePerformIO $ getDirectoryContents dir

match1 ' ' c = True
match1 a   b = a == b

match m w = length m == length w && and (zipWith match1 m w)

vocm m = filter (match m) vocab

type LetterCount = M.Map Char Integer
type Subgrams = M.Map (M.Map Char Integer) [(String, M.Map Char Integer)]

newtype Dag = Dag [(String, Dag)]

letters = "quginhrjbtvco"

reversible = S.toList (S.fromList vocab  `S.intersection` S.fromList (map reverse vocab))

mm l = mapFromList (+) 0 $ map (,1) l

unmm m = smv M.! m

unmm1 m = concatMap (uncurry $ flip replicate) $ M.toList m

mvocab = map snd mvocab1

mvocab1 :: [([Char], M.Map Char Int)]
mvocab1 = mapxfx mm vocab

svocab = S.fromList mvocab

msv = M.fromList mvocab1

-- a map from letter frequencies to words with those letter frequencies
smv = mapFromList (:) [] $ map tflip mvocab1

filterRepeats l = filterRepeats1 l S.empty

filterRepeats1 []     _ = []
filterRepeats1 (x:xs) s = if S.member x s then filterRepeats1 xs s else x : filterRepeats1 xs (S.insert x s)

anagramsmw l = anagramsmw1 svocab (mm l) []

anagramsmw1 s m w
   | M.size m == 0 = [w]
   | otherwise = let
      s1 = S.intersection s $ subletters m

      in concatMap (\m1 -> anagramsmw1 (snd $ S.split m1 s1) (fromJust $ subtract1 m m1) (unmm m1 : w)) $ S.toList s1

anagramsmwa l = anagramsmwa1 svocab (mm l) []

anagramsmwa1 s m w
   | M.size m == 0 = [w]
   | otherwise = let
      s1 = S.intersection s $ subletters m

      in concatMap (\m1 -> anagramsmw1 (S.fromList m1) (fromJust $ subtract1 m (head m1)) (unmm (head m1) : w)) $ init $ tails $ S.toList s1

anagramsmwb le = let
   lm = mm le
   sl = subletters lm
   li = S.toList sl
   ma = M.fromList $ mapxfx subgrams1 li
   in anagramsmwb1 ma lm lm []

anagramsmwb1 ma mi ml w
   | M.size ml == 0 = [w]
   | otherwise = let
      s1 = snd $ S.split mi (ma M.! ml)

      in concatMap (\m1 -> anagramsmwb1 ma m1 (fromJust $ subtract1 ml m1) (unmm m1 : w)) $ S.toList s1

data Tree = Tree (M.Map (M.Map Char Int) Tree) | Null deriving (Eq, Ord, Show)

untree (Tree t) = t

showDepth d w i (Tree t) = unlines $ 
                           map (replicate i ' '++) $ 
                           map (\(k, v) -> (head $ unmm k) ++ "=" ++ showDepth (d-1) w (i+3) v) $
                           take w $
                           M.toList t

anagramsmwc le = let
   lm = mm le
    -- ma is a self referential Tree containing all the vocab along with links to the words still able to
    -- be made after the particular word has been done
   ma = M.fromList -- presumably the Map can have its structure defined from the keys alone, without evaluating the (\m -> Tree part)
      $ mapxfx 
         (\m -> Tree 
            $ M.fromList 
            $ mapxfx (
                  fromMaybe Null .
                  (`M.lookup` ma) . 
                  fromMaybe (error "subtract1 returned Nothing") . 
                  subtract1 m) 
            $ S.toList 
            $ subgrams1 m)
      $ S.toList
      $ subletters lm
   in anagramsmwc1 (ma M.! lm) lm lm []

anagramsmwc1 (Tree branch) split left done
   | M.size left == 0 = done
   | otherwise = concatMap (\(next, newtree) -> anagramsmwc1 
                                                newtree
                                                next
                                                (fromMaybe (error "subtract1 returned Nothing") $ subtract1 left next)
                                                (unmm next : done)) 
      $ M.toList 
      $ snd 
      $ M.split split branch
anagramsmwc1 Null _ _ _ = []

anagramsmwd l = sortOn length <$> anagramsmwd1 svocab (mm l) []

anagramsmwd1 s m w
   | empty2 m = return [w]
   | otherwise = do
      --print w
      let s1 = S.intersection s $ subletters m

      concat <$> (mapM (\m1 -> anagramsmwd1 s1 (fromJust $ subtract1 m m1) (w ++ [unmm m1])) $ {- rsortBy length $ -} S.toList s1)

empty2 m = (==0) $ sum $ map snd $ M.toList m

anagramsmwe le = reverse $ sortOn length $ subgrams le 

subletters l = let -- take a map of letter frequencies
   l1 = M.toList l -- turn it into a list
   cs = map fst l1 -- the letters
   ns = map snd l1 -- the frequencies
   ss = crossList $ map (\x -> [0..x]) ns -- all the combinations of frequencies from 0 to the original
   rs = map (zipWith (-) ns) ss           -- the remainders when these are subtracted (not used)
   f  = map (M.fromList . filter ((>0) . snd) . zip cs) -- add the letters back and create Maps
   in S.fromList $ f ss -- a set of Maps of all frequencies from 0 up to and including the original

subgramsL l = sortOn length $ sort $ subgrams l 

subgrams l = concatMap unmm $ S.toList $ subgrams1 $ mm l

-- this is subgram words in set of maps form, the func subgrams turns it back into strings using unmm
subgrams1 m = S.intersection svocab $ subletters m
{-
subgrams1a m = let
   s = subgrams1 m
   r = map subtract1 $ S.toList s
-}
subgrams2 l = mapMaybe (subtract1 l)

--subtract2 a b = 

some = or .: map

subtract2 a b = let
   i = M.intersectionWith (-) a b
   amb = M.difference a b
   bma = M.difference b a
   ns = map snd $ M.toList i
   in if bma == M.empty && all (>= 0) ns
         then Just $ M.union i amb
         else Nothing

subtract1 a b = let
   r = M.merge M.preserveMissing M.dropMissing (M.zipWithMaybeMatched combine2) a b
   in if all ((>= 0) . snd) (M.toList r) then Just r else Nothing

subtracts a b = unmm1 $ fromJust $ subtract1 (mm a) (mm b)

combine2 k a b = Just $ a - b

newtype Memo k a r = Memo (k -> State (Memo k a r, k, M.Map k a) r)

recurse :: Ord k => k -> State (Memo k a a, k, M.Map k a) a
recurse k = do
   (m, _, ma) <- get
   case M.lookup k ma of
      Just j  -> return j
      Nothing -> do
         let Memo f = m
         --lift $ putStrLn ("call "++show k++" "++show ma)
         fk <- f k
         (_, _, ma2) <- get
         --lift $ putStrLn ("return "++show k++" "++show ma2)
         put (m, k, M.insert k fk ma2)
         return fk

recurse1 fk j@(Just v) = j
recurse1 fk n@Nothing  = Just fk

memo f k = evalState (f k) (Memo f, k, M.empty)

memo1 f k = (\(a, b, c) -> c) $ execState (f k) (Memo f, k, M.empty)

fibs k = memo fibs1 k

fibs2 = (fibs3 M.!)

fibs3 = memo1 fibs1 1000


fibs1 :: (Num k, Num a, Ord k) => k -> State (Memo k a a, k, M.Map k a) a
fibs1 0 = return 0
fibs1 1 = return 1
fibs1 n = recurse (n - 1) <*$ (+) $*> recurse (n - 2)
   --(+) <$> recurse (n - 1) <*> recurse (n - 2)
   {-
   a <- recurse (n - 1)
   b <- recurse (n - 2)
   return $ a + b
   -}
infixl 4 <*$
a <*$ b = b <$> a

infixl 4 $*>
b $*> c = b <*> c


load f = filter (`elem` " abcdefghijklmnopqrstuvwxyz\n") . map toLower $ readFileU $ path ++ f

linesL = nubSet . lines . load

words1 = mapFromList (+) 0 . map (,1) . words 

sl = linesL "slayer.txt"

ce = linesL "celine.txt"

--v5 = words1 $ load "war_of_the_worlds.txt"
v5 = words1 $ load "shakespeare.txt"

linw f = foldr (M.unionWith (++)) M.empty $ map (\l -> M.fromList $ map (,[l]) $ words l) f

m = sortOn (\(n, w, l) -> n) $ map (\(w, l) -> (fromMaybe 0 (M.lookup w v5), w, l)) $
   M.toList $ M.intersectionWith (\a b -> a ++ ["!!!"] ++ b) (linw ce) (linw sl)

merge5 xs ys = let
   xl = length xs
   yl = length ys
   in (xl*yl, xs ++ ys)

mergeWith f xs ys = case zipWith f xs ys of
   [] -> []
   [x] -> [x]
   zs -> let [as, bs] = transpose $ groupN 2 xs in mergeWith f as bs

merge6 f xs = map (\[a, b] -> f a b) $ groupN 2 xs
