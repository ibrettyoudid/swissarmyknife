{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
-- Copyright 2025 Brett Curtis
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Redundant multi-way if" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use map once" #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use ++" #-}

import Control.Applicative
import Control.DeepSeq
import Control.Monad.State.Lazy
import Data.Binary hiding (get, put)
import Data.Bits
import Data.Char
import Data.HashTable qualified as H
import Data.IORef
import Data.Int
import Data.List
import Data.Ratio
import Data.Time.Calendar
import Favs hiding (mean)
import GHC.Generics (Generic, Generic1)
import MyPretty2
import Numeric hiding (readInt)
import Prob
import ShowTuple
import System.IO
import System.Random.Stateful hiding (split)

{-
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
-}
import Control.Concurrent
import Control.Exception qualified as E
import Control.Monad
import Data.ByteString qualified as B
import Network.Socket
import Network.Socket.ByteString

import Data.Map qualified as M
import Data.Set qualified as S

import Data.Array.IArray qualified as A

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Enum, Show, Read)

data Card = Card {value :: Int, suit :: Int} deriving (Eq, Ord, Read)

data HandDesc = HandDesc !HandCat ![Int] deriving (Eq, Ord, Generic)

newtype HandRank = HandRank Int32 deriving (Eq, Ord, Show)

{-
                                             High  [Int]
                                       | Pair Int [Int]
                                       | Pairs Int Int Int
                                       | Three Int [Int]
                                       | Straight Int
                                       | Flush [Int]
                                       | FullHouse Int Int
                                       | Four Int Int
                                       | StraightFlush Int
                                       deriving (Eq, Ord)
-}
instance Show HandDesc where
      show = showHandVal

data HandCat = Invalid | High | Pair | Pairs | Three | Straight | Flush | FullHouse | Four | StraightFlush deriving (Eq, Ord, Enum, Read, Generic)

instance NFData HandCat

instance NFData HandDesc

instance Show Card where
      show = showCard

instance Enum Card where
      toEnum = cardOfNum
      fromEnum = numOfCard
      succ (Card v s)
            | v < 14 = Card (v + 1) s
            | s < 3 = Card 2 (s + 1)
            | otherwise = Card 2 0

      pred (Card v s)
            | v > 2 = Card (v - 1) s
            | s > 0 = Card 14 (s - 1)
            | otherwise = Card 14 3

instance Show HandCat where
      show = showHandCat

handCat (HandDesc c ns) = c
handNums (HandDesc _ ns) = ns

showCard (Card n s) = [showVal n, showSuit s]
showCards = map showCard
showHand aaa = unwords $ map showCard aaa

showVal n = " 123456789XJQKA" !! n
showVals = map showVal

showSuit s = "scdh" !! s

showHandCat StraightFlush = "*** STRAIGHT FLUSH ***"
showHandCat Four = "FOUR OF A KIND ***"
showHandCat FullHouse = "FULL HOUSE!!"
showHandCat Flush = "FLUSH!!!"
showHandCat Straight = "Strait"
showHandCat Three = "Three"
showHandCat Pairs = "2 2s"
showHandCat Pair = "2"
showHandCat High = "-"
showHandCat Invalid = "INVALID INVALID INVALID"

showHandVal v = showHandCat (handCat v) ++ "  " ++ showHandVal1 v

showHandVal1 (HandDesc StraightFlush [n]) = showVals [n, n - 1 .. n - 4]
showHandVal1 (HandDesc Four [n, k]) = showVals [n, n, n, n, 0, k]
showHandVal1 (HandDesc FullHouse [n1, n2]) = showVals [n1, n1, n1, 0, n2, n2]
showHandVal1 (HandDesc Flush ns) = showVals ns
showHandVal1 (HandDesc Straight [n]) = showVals [n, n - 1 .. n - 4]
showHandVal1 (HandDesc Three [n, k1, k2]) = showVals [n, n, n, 0, k1, k2]
showHandVal1 (HandDesc Pairs [n1, n2, k]) = showVals [n1, n1, 0, n2, n2, 0, k]
showHandVal1 (HandDesc Pair (n : ks)) = showVals (n : n : 0 : ks)
showHandVal1 (HandDesc High ks) = showVals ks
showHandVal1 (HandDesc Invalid ks) = showVals ks

allEqual a = and $ zipWith (==) a (tail a)

groupR a = group $ rsort a

groupByR f = groupBy (\l r -> f r l == EQ) . sortBy (flip f)

rsortBy f = sortBy (flip f)

pairs = filter ((>= 2) . length) . group

threes = filter ((== 3) . length) . group

fours = filter ((== 4) . length) . group

invalid = filter ((>= 5) . length) . group

straightFlushes = concatMap (straights . map value)

-- straights = filter ((== 5) . length) . map (take 5 . straight) . tails . nub

acesHigh = map (\case 1 -> 14; x -> x)

acesLow = map (\case 14 -> 1; x -> x)

straights x = if elem 14 x then straightsF x ++ straightsF (acesLow x) else straightsF x

straightsF x = filter ((== 5) . length) $ map (take 5) $ groupByA (\x y -> x == y + 1) $ rsort $ nubSet x

groupByA f (x : y : zs) =
      let
            (a : b) = groupByA f (y : zs)
         in
            if f x y then (x : a) : b else [x] : a : b
groupByA f x = [x]

straightsG x =
      filter ((== 5) . length) $
            map (take 5 . map fst . snd) $
                  groupOn snd $
                        zip x $
                              zipWith (+) x [0 ..]

{-
straight (a:bs) = a : straight1 (a:bs)
straight [] = []

straight1 (a:b:cs)
         | a == b + 1 = b : straight1 (b:cs)
         | otherwise  = []
straight1 _ = []
-}
flushes = filter ((>= 5) . length) . map snd . groupOn suit

flushesA = map (take 5 . rsort . map value)

flushesB = filter ((>= 5) . length) . group

-- flushesC values pattern = zipWith (\v n -> ifJust (pattern .&. shift 1 n /= 0) values [0..6]

handDescL hand =
      let
            values = rsort $ map value hand

            flushes1 = flushes hand
            straightFlushes1 = straightFlushes flushes1
            flushes2 = flushesA flushes1
            straights1 = straights values

            fours1 = fours values
            threes1 = threes values
            pairs1 = pairs values

            pairs2 = pairs $ deleteMulti (head threes1) values
            --   in (values, straightFlushes1, fours1, flushes3, straights1, threes1, pairs1)
            -- {-
         in
            if
                  | flushes1 /= [] && straightFlushes1 /= [] ->
                              HandDesc StraightFlush [head $ head straightFlushes1]
                  | fours1 /= [] ->
                              HandDesc Four [head $ head fours1, head $ deleteMulti (head fours1) values]
                  | threes1 /= [] && pairs2 /= [] ->
                              HandDesc FullHouse [head $ head threes1, head $ head pairs2]
                  | flushes2 /= [] ->
                              HandDesc Flush $ head flushes2
                  | straights1 /= [] ->
                              HandDesc Straight [head $ head straights1]
                  | threes1 /= [] ->
                              HandDesc Three ((head $ head threes1) : take 2 (deleteMulti (head threes1) values))
                  | length pairs1 >= 2 ->
                              HandDesc Pairs [head $ head pairs1, head $ pairs1 !! 1, head $ deleteMulti (concat $ take 2 pairs1) values]
                  | pairs1 /= [] ->
                              HandDesc Pair ((head $ head pairs1) : (take 3 $ deleteMulti (head pairs1) values))
                  | otherwise ->
                              HandDesc High $ take 5 values

handRank desc =
      let
            nums = handNums desc
         in
            HandRank $ fromIntegral $ case handCat desc of
                  StraightFlush -> 2598900 + frank nums 0 * 4
                  Four -> 2598296 + frank nums 1 * 4
                  FullHouse -> 2594552 + frank nums 1 * 24
                  Flush -> 2589444 + frank nums 4 * 4
                  Straight -> 2579244 + frank nums 0 * 1020
                  Three -> 2524332 + frank nums 2 * 64
                  Pairs -> 2400780 + frank nums 2 * 144
                  Pair -> 1302540 + frank nums 3 * 384
                  High -> frank nums 4 * 1020
                  Invalid -> -1

frank rs n =
      let
            v = frank1 (map (- 2) rs) []
         in
            sum $ zipWith (*) v $ scanr (*) 1 $ take n [12, 11 .. 1]

frank1 [] _ = []
frank1 (r : rs) zs = (r - length (filter (< r) zs)) : frank1 rs (r : zs)

-- }
deleteMulti as bs = foldr delete bs as

insSort xs = foldl (flip insert) [] xs

-- bitsOfCard (Card 14 s) = shift (shift 1 14 + 2) (s * 15)
-- bitsOfCard (Card 1  s) = shift (shift 1 14 + 2) (s * 15)
numOfCard (Card v s) = v + s * 15

bitsOfCard c = shift 1 $ numOfCard c

bitsOfHand :: [Card] -> Int
bitsOfHand = foldr (.|.) 0 . map bitsOfCard

cardOfNum x =
      let
            (s, n) = divMod x 15
         in
            Card n s

handOfBits = filter (\c -> value c /= 1) . map cardOfNum . numsOfBits

bitsOfNum :: Int -> Int
bitsOfNum = shift 1

firstBit :: Int -> Int
-- firstBit 0 = error "no bits!"
firstBit 0 = 0
firstBit 1 = 0
firstBit x = firstBit (shift x -1) + 1

firstBitRank x = firstBit (combineRanks x .&. wholeSuit)

lastBit :: Int -> Int
lastBit 0 = error "no bits!"
lastBit x
      | x .&. 1 /= 0 = 0
      | otherwise = lastBit (shift x -1) + 1

lastBitRank x = lastBit x `mod` 15

numsOfBits 0 = []
numsOfBits x = f : numsOfBits (x - shift 1 f) where f = firstBit x

allRanks x = numsOfBits (combineRanks x .&. wholeSuit)

combineRanks x = x .|. shift x -15 .|. shift x -30 .|. shift x -45

countBits = length . numsOfBits

wholeSuit :: Int
wholeSuit = shift 1 15 - 1

wholeRank x = shift x 0 .|. shift x 15 .|. shift x 30 .|. shift x 45

delRank r x = x .&. complement (wholeRank $ bitsOfCard $ Card r 0)

straightsB hand =
      let
            s0 = acesLowB hand
            s1 = s0 .&. shift s0 -1
            s2 = s1 .&. shift s1 -2
         in
            s2 .&. shift hand -4

flushB hand = ifJust (countBits hand1 >= 5) $ take 5 $ numsOfBits hand1 where hand1 = hand .&. wholeSuit

pairsB hand =
      let
            pairs1 = hand .&. shift hand -15
            pairs2 = hand .&. shift hand -30
            pairs3 = hand .&. shift hand -45
         in
            pairs1 .|. pairs2 .|. pairs3

threesB pairs hand =
      let
            threes1 = pairs .&. shift hand -30
            threes2 = pairs .&. shift hand -45
         in
            threes1 .|. threes2

threesB1 hand =
      let
            pairs = pairsB hand
         in
            threesB pairs hand

threesB2 hand =
      let
            threes1 = hand .&. shift hand -15 .&. shift hand -30
            threes2 = hand .&. shift hand -15 .&. shift hand -45
            threes3 = hand .&. shift hand -30 .&. shift hand -45
         in
            threes1 .|. threes2 .|. threes3

handDescB hand =
      let
            pairs1 = hand .&. shift hand -15
            pairs2 = hand .&. shift hand -30
            pairs3 = hand .&. shift hand -45
            pairs = pairs1 .|. pairs2 .|. pairs3
            pairsRank = firstBitRank pairs

            pairs4 = pairsB $ delRank pairsRank hand
            pairsRank2 = firstBitRank pairs4

            threes1 = pairs1 .&. shift hand -30
            threes2 = pairs1 .&. shift hand -45
            threes3 = pairs2 .&. shift hand -45
            threes = threes1 .|. threes2 .|. threes3
            threesRank = firstBitRank threes

            two = hand .&. shift hand -15
            four = two .&. shift two -30
            fourRank = firstBitRank four

            fullHouse =
                  if threes > 0
                        then pairsB (delRank threesRank hand)
                        else 0

            straightFlush =
                  straightsB (hand .&. wholeSuit)
                        .|. straightsB (shift hand -15 .&. wholeSuit)
                        .|. straightsB (shift hand -30 .&. wholeSuit)
                        .|. straightsB (shift hand -45 .&. wholeSuit)
            straightFlushRank = firstBit straightFlush

            straight1 = straightsB $ (hand .|. shift hand -15 .|. shift hand -30 .|. shift hand -45) .&. wholeSuit

            flush = flushB hand <|> flushB (shift hand -15) <|> flushB (shift hand -30) <|> flushB (shift hand -45)
         in
            if
                  | straightFlush /= 0 -> HandDesc StraightFlush [straightFlushRank + 4]
                  | four /= 0 -> HandDesc Four [fourRank, firstBitRank $ delRank fourRank hand]
                  | fullHouse /= 0 -> HandDesc FullHouse [threesRank, firstBitRank fullHouse]
                  | isJust flush -> HandDesc Flush $ fromJust flush
                  | straight1 /= 0 -> HandDesc Straight [firstBitRank straight1 + 4]
                  | threes /= 0 -> HandDesc Three (threesRank : take 2 (allRanks $ delRank threesRank hand))
                  | pairs4 /= 0 -> HandDesc Pairs [pairsRank, pairsRank2, firstBitRank $ delRank pairsRank2 $ delRank pairsRank hand]
                  | pairs /= 0 -> HandDesc Pair (pairsRank : take 3 (allRanks $ delRank pairsRank hand))
                  | otherwise -> HandDesc High $ take 5 $ allRanks hand

acesLowB :: Int -> Int
acesLowB hand = hand .|. shift (hand .&. wholeRank (shift 1 14)) -13

bstr vb1 =
      let
            vb2 = vb1 .&. shiftL vb1 1
            vb4 = vb2 .&. shiftL vb2 2
            vb5 = vb4 .&. shiftL vb1 4
         in
            (vb2, vb4, vb5)

intToBin :: Int -> String
intToBin n = (if n > 1 then intToBin (div n 2) else "") ++ show (mod n 2)

binToInt :: String -> Int
binToInt "" = 0
binToInt s = binToInt (init s) * 2 + (if last s == '0' then 0 else 1)

bstrb vb1 =
      let
            (vb2, vb4, vb5) = bstr $ binToInt vb1
         in
            (intToBin vb2, intToBin vb4, intToBin vb5)

handDescLT1 valuesu =
      let
            values = rsort valuesu
            invalid1 = invalid values
            straights1 = straights values
            fours1 = fours values
            threes1 = threes values
            pairs1 = pairs values
            pairs2 = pairs $ deleteMulti (head threes1) values
         in
            if
                  | invalid1 /= [] ->
                              HandDesc Invalid values
                  | fours1 /= [] ->
                              HandDesc Four [head $ head fours1, head $ deleteMulti (head fours1) values]
                  | threes1 /= [] && pairs2 /= [] ->
                              HandDesc FullHouse [head $ head threes1, head $ head pairs2]
                  | straights1 /= [] ->
                              HandDesc Straight [head $ head straights1]
                  | threes1 /= [] ->
                              HandDesc Three ((head $ head threes1) : take 2 (deleteMulti (head threes1) values))
                  | length pairs1 >= 2 ->
                              HandDesc Pairs [head $ head pairs1, head $ pairs1 !! 1, head $ deleteMulti (concat $ take 2 pairs1) values]
                  | pairs1 /= [] ->
                              HandDesc Pair ((head $ head pairs1) : (take 3 $ deleteMulti (head pairs1) values))
                  | otherwise ->
                              HandDesc High $ take 5 values

handDescLT2 :: [Int] -> Int
handDescLT2 suits =
      let
            flushes1 = flushesB suits
         in
            if
                  | flushes1 /= [] ->
                              let suit = head $ head flushes1 in sum $ zipWith (\s n -> shift 1 n) suits [0 .. 6]
                  | otherwise ->
                              0

blah [a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)

lookupTable1 :: A.Array (Int, Int, Int, Int, Int, Int, Int) HandDesc
lookupTable1 = A.accumArray (\hv1 hv2 -> hv2) (HandDesc Invalid []) ((2, 2, 2, 2, 2, 2, 2), (14, 14, 14, 14, 14, 14, 14)) $ map (\h -> (blah h, HandDesc Invalid h)) $ crossList $ replicate 7 [2 .. 14]

lookupTable2 :: A.Array (Int, Int, Int, Int, Int, Int, Int) Int
lookupTable2 = A.array ((0, 0, 0, 0, 0, 0, 0), (3, 3, 3, 3, 3, 3, 3)) $ map (\h -> (blah h, handDescLT2 h)) $ crossList $ replicate 7 [2 .. 14]

{-
handDescLT3 cards = let
         values1 = map value cards
         suits1  = map suit  cards
         hv1 = lookupTable1 A.! blah values1
         hv2 = lookupTable2 A.! blah suits1
         flushes1 = flushes cards
         straightFlushes1 = straightFlushes flushes1
         in if
                  | hv2 /= 0 && handCat hv1 == Straight && straightFlushes1 /= [] -> HandDesc StraightFlush [head $ head straightFlushes1]
                  | hv2 /= 0 && handCat hv1 > Flush -> hv1
                  | hv2 /= 0 -> HandDesc Flush $ head $ flushesA flushes1
                  | otherwise -> hv1
-}
-- lookupTable3 :: M.Map (S.Set Card) HandDesc
lookupTable3 = M.fromList $ concatMap (\card0 -> concatMap (\card1 -> concatMap (\card2 -> concatMap (\card3 -> concatMap (\card4 -> concatMap (\card5 -> map (\card6 -> let h = [card0, card1, card2, card3, card4, card5, card6] in (S.fromList h, handDescL h)) (zorg 5 card5 allcards)) (zorg 4 card4 allcards)) (zorg 3 card3 allcards)) (zorg 2 card2 allcards)) (zorg 1 card1 allcards)) (zorg 0 card0 allcards)) (take 45 allcards)

handDescLT3 cards = lookupTable3 M.! S.fromList cards

-- }
zorg n card = dropWhile (<= card) . take 45 . drop n

concatMapM f xs = sequence $ xs

lookupTable4 = do
      ht <- H.newWithDefaults $ 2 ^ 32
      mapM_ (\card0 -> mapM_ (\card1 -> mapM_ (\card2 -> mapM_ (\card3 -> mapM_ (\card4 -> mapM_ (\card5 -> mapM_ (\card6 -> let h = [card0, card1, card2, card3, card4, card5, card6] in H.insert ht (bitsOfHand h) (handRank $ handDescL h)) (zorg 5 card5 allcards)) (zorg 4 card4 allcards)) (zorg 3 card3 allcards)) (zorg 2 card2 allcards)) (zorg 1 card1 allcards)) (zorg 0 card0 allcards)) (take 45 allcards)
      return ht

handRankIO ht cards = do
      m <- H.lookup ht (bitsOfHand cards)
      case m of
            Just j -> return j
            Nothing -> error $ "Hand " ++ showHand cards ++ " not in hash table"

ns (Card n s) = (n, s)

straightFlushesX1 valsT cards left =
      map
            ( \((n, s), got) ->
                        let
                              toget = max 0 (4 - got)
                              extra = left - toget
                           in
                              ( HandDesc StraightFlush [n]
                              , if toget <= left
                                          then comb (45 + extra) extra
                                          else 0
                              )
            )
            $ A.assocs (A.accumArray (+) 0 ((2, 0), (14, 3)) $ concatMap (\(Card n s) -> map (\x -> ((n + x, s), 1 :: Int)) [0 .. 4]) cards :: A.Array (Int, Int) Int)

straightFlushesX gotvaluesA yourhand nyourcards ntablecards n1 s1 =
      let
            -- nyourcards is set to zero for their hands
            gotcards1 = gotvaluesA A.! (n1, s1) + gotvaluesA A.! (n1 - 1, s1) + gotvaluesA A.! (n1 - 2, s1) + gotvaluesA A.! (n1 - 3, s1) + gotvaluesA A.! (n1 - 4, s1)
            gotcardsextra = (if yourhand then nyourcards else 0) + ntablecards - gotcards1
         in
            ( HandDesc Four [n1]
            , comb (52 - (if yourhand then 0 else nyourcards) - ntablecards) (2 - gotcardsextra)
            )

foursX takenvaluesA gotvaluesA yourhand nyourcards ntablecards n1 =
      let
            -- nyourcards is set to zero for their hands
            gotcards1 = gotvaluesA A.! n1
            gotcardsextra = (if yourhand then nyourcards else 0) + ntablecards - gotcards1

            takencards1 = takenvaluesA A.! n1
            takenvalues = 1 + gotcardsextra
         in
            ( HandDesc Four [n1]
            , comb (4 - takencards1) (4 - gotcards1)
                        * comb (52 - (if yourhand then 0 else nyourcards) - 4 * takenvalues) (3 - gotcardsextra)
            )

foursX1 valsT vals left =
      map
            ( \(n, got) ->
                        let
                              toget = max 0 (4 - got)
                              extra = left - toget
                           in
                              ( HandDesc Four [n]
                              , if toget <= left
                                          then comb (45 + extra) extra
                                          else 0
                              )
            )
            vals

-- 3 ways of making a full house
-- two threes
-- one three and two pairs
-- one three, one pair and two randoms
fullhousesX1 takenvaluesA gotvaluesA yourhand nyourcards ntablecards n1 n2 =
      let
            -- nyourcards is set to zero for their hands
            gotcards1 = gotvaluesA A.! n1
            gotcards2 = gotvaluesA A.! n2
            gotcardsextra = (if yourhand then nyourcards else 0) + ntablecards - gotcards1 - gotcards2

            takencards1 = takenvaluesA A.! n1
            takencards2 = takenvaluesA A.! n2
            takenvalues = 2 + gotcardsextra
         in
            ( HandDesc FullHouse [n1, n2]
            , comb (4 - takencards1) (3 - gotcards1)
                        * comb (4 - takencards2) (3 - gotcards2)
                        * comb (13 - takenvalues) (1 - gotcardsextra)
                        * 4 ^ (1 - gotcardsextra)
            )

fullhousesX2 takenvaluesA gotvaluesA yourhand nyourcards ntablecards n1 n2 n3 =
      let
            -- nyourcards is set to zero for their hands
            gotcards1 = gotvaluesA A.! n1
            gotcards2 = gotvaluesA A.! n2
            gotcards3 = gotvaluesA A.! n3
            gotcardsextra = (if yourhand then nyourcards else 0) + ntablecards - gotcards1 - gotcards2 - gotcards3

            takencards1 = takenvaluesA A.! n1
            takencards2 = takenvaluesA A.! n2
            takencards3 = takenvaluesA A.! n3
            takenvalues = 3 + gotcardsextra
         in
            ( HandDesc FullHouse [n1, n2]
            , comb (4 - takencards1) (3 - gotcards1)
                        * comb (4 - takencards2) (2 - gotcards2)
                        * comb (4 - takencards3) (2 - gotcards3)
            )

fullhousesX3 takenvaluesA gotvaluesA yourhand nyourcards ntablecards n1 n2 =
      let
            -- nyourcards is set to zero for their hands
            gotcards1 = gotvaluesA A.! n1
            gotcards2 = gotvaluesA A.! n2
            gotcardsextra = (if yourhand then nyourcards else 0) + ntablecards - gotcards1 - gotcards2

            takencards1 = takenvaluesA A.! n1
            takencards2 = takenvaluesA A.! n2
            takenvalues = 2 + gotcardsextra
         in
            ( HandDesc FullHouse [n1, n2]
            , comb (4 - takencards1) (3 - gotcards1)
                        * comb (4 - takencards2) (2 - gotcards2)
                        * comb (13 - takenvalues) (2 - gotcardsextra)
                        * 4 ^ (2 - gotcardsextra)
            )

fullhousesX valsT vals left =
      concat
            $ map2
                  ( \((n1, got1), (n2, got2)) ->
                              let
                                    toget1 = max 0 (3 - got1)
                                    toget2 = max 0 (2 - got2)
                                    toget = toget1 + toget2
                                    extra = left - toget
                                 in
                                    ( HandDesc FullHouse [n1, n2]
                                    , if n1 /= n2 && toget <= left
                                                then comb (45 + extra) extra * comb 4 toget1 * comb 4 toget2
                                                else 0
                                    )
                  )
            $ cross vals vals

fullhousesX1A valsT vals left =
      concat
            $ map2
                  ( \((n1, got1), (n2, got2)) ->
                              let
                                    toget1 = max 0 (3 - got1)
                                    toget2 = max 0 (3 - got2)
                                    toget = toget1 + toget2
                                    extra = left - toget
                                 in
                                    ( HandDesc FullHouse $ rsort [n1, n2]
                                    , if n1 /= n2 && toget <= left
                                                then comb (43 + extra) extra * comb 4 toget1 * comb 4 toget2 -- the extra is chosen from two less than the usual 45 to avoid a four
                                                else 0
                                    )
                  )
            $ cross vals vals

fullhousesX2A valsT vals left =
      concat
            $ concat
            $ map3
                  ( \((n1, got1), (n2, got2), (n3, got3)) ->
                              let
                                    toget1 = max 0 (3 - got1)
                                    toget2 = max 0 (2 - got2)
                                    toget3 = max 0 (2 - got3)
                                    toget = toget1 + toget2
                                    extra = left - toget
                                 in
                                    ( HandDesc FullHouse [n1, max n2 n3]
                                    , if n1 /= n2 && n2 /= n3 && n1 /= n3 && toget <= left
                                                then comb 4 toget1 * comb 4 toget2 * comb 4 toget3
                                                else 0
                                    )
                  )
            $ cross3 vals vals vals

fullhousesX3A valsT vals left =
      concat
            $ map2
                  ( \((n1, got1), (n2, got2)) ->
                              let
                                    toget1 = max 0 (3 - got1)
                                    toget2 = max 0 (2 - got2)
                                    toget = toget1 + toget2
                                    extra = left - toget
                                 in
                                    ( HandDesc FullHouse [n1, n2]
                                    , if n1 /= n2 && toget <= left
                                                then comb (9 + extra) extra * 4 ^ extra * comb 4 toget1 * comb 4 toget2
                                                else 0
                                    )
                  )
            $ cross vals vals

flushesX suitsT suits left =
      [
            ( HandDesc Flush []
            , sum $
                        map
                              ( \(s, got) ->
                                          sum $
                                                map
                                                      ( \total ->
                                                                  let
                                                                        toget = max 0 (total - got)
                                                                        extra = left - toget
                                                                     in
                                                                        if toget <= left
                                                                              -- then sum $ map (\x -> comb 13 x * comb 39 (7-x)) [5..7]
                                                                              then comb 13 toget * comb 39 extra
                                                                              else 0
                                                      )
                                                      [5 .. 7]
                              )
                              suits
            )
      ]

threesX valsT vals left =
      map
            ( \(n, got) ->
                        let
                              toget = max 0 (3 - got)
                              extra = left - toget
                           in
                              ( HandDesc Three [n]
                              , if toget <= left
                                          then comb (45 + extra) extra * comb 4 toget
                                          else 0
                              )
            )
            vals

{-
your cards = A K

for your hands, 4 cards are already chosen (the two pairs)
         all cards taken are got
         you may have got more than them
         the pairs:
                  the values are chosen by being mapped across
                  got includes your cards, and the tablecards
                  choose the suits = comb (4-taken1) (2-taken1) * comb (4-taken2) (2-taken2)
                  was thinking of making got the same as taken to use same function as for them but that messes other things up
                  yes should do that

         the three extra cards:
                  must choose 3 cards excluding the whole values that are on the table or in the pairs or in your hand
                  comb (52-4*yourvaluestaken) (3-gotextra)
                  yourvaluestaken is 2 + gotextra (but gotextra is different for you? thats ok)
                  gotextra is nyourcards + ntablecards - got1 - got2

for their hands:
         2 less cards are got than taken
         2 cards are chosen (yours) and 2 suits x 2 values
         the pairs:
                  the values are chosen by being mapped across
                  choose the suits = comb (4-taken1) (2-got1) * comb (4-taken2) (2-got2)
                  this is the same as for your hands just with got1/2 in place of taken1/2

         the three extra cards:
                  BEST:
                           must choose 3 excluding the cards in your hand and the whole values that are on the table or in the pairs

                           comb (52-nyourcards-4*valuestaken) (3-gotextra)
                           valuestaken is values on the table or in the pairs (some may appear more than once)
                           (it's ok if they have the same value as one of yours, we're already excluding your 2)
                           so takenvalues = 2 + gotextra
                           gotextra is ntablecards - got1 - got2

-}

-- make your hand true if this is your hand!
pairsX takenvaluesA gotvaluesA yourhand nyourcards ntablecards n1 n2 n3 =
      let
            -- nyourcards is set to zero for their hands
            gotcards1 = gotvaluesA A.! n1
            gotcards2 = gotvaluesA A.! n2
            gotcards3 = gotvaluesA A.! n3
            gotcardsextra = (if yourhand then nyourcards else 0) + ntablecards - gotcards1 - gotcards2 - gotcards3

            takencards1 = takenvaluesA A.! n1
            takencards2 = takenvaluesA A.! n2
            takencards3 = takenvaluesA A.! n3
            takenvalues = 3 + gotcardsextra
         in
            ( HandDesc Pairs [n1, n2]
            , if n1 > n2 && n2 > n3
                        then
                              comb (4 - takencards1) (2 - gotcards1)
                                    * comb (4 - takencards2) (2 - gotcards2)
                                    * comb (4 - takencards3) (2 - gotcards3)
                                    * comb (52 - (if yourhand then 0 else nyourcards) - 4 * takenvalues) (1 - gotcardsextra)
                        else 0
            )

pairsX1 takenvaluesA gotvaluesA yourhand nyourcards ntablecards n1 n2 =
      let
            -- nyourcards is set to zero for their hands
            gotcards1 = gotvaluesA A.! n1
            gotcards2 = gotvaluesA A.! n2
            gotcardsextra = (if yourhand then nyourcards else 0) + ntablecards - gotcards1 - gotcards2

            takencards1 = takenvaluesA A.! n1
            takencards2 = takenvaluesA A.! n2
            takenvalues = 2 + gotcardsextra
         in
            ( HandDesc Pairs [n1, n2]
            , if n1 > n2
                        then
                              comb (4 - takencards1) (2 - gotcards1)
                                    * comb (4 - takencards2) (2 - gotcards2)
                                    *
                                    -- perm (13-takenvalues) (3-gotcardsextra) * 4^(3-gotcardsextra) * 10 `div` product [1..5-ntablecards]
                                    comb (13 - takenvalues) (3 - gotcardsextra)
                                    * 4 ^ (3 - gotcardsextra)
                        -- comb (52-(if yourhand then 0 else nyourcards)-4*takenvalues) (3-gotcardsextra)
                        else 0
            )

-- comb 13 2 * comb 11 5 = comb 13 5 * comb 5 2
pairX valsT vals left =
      map
            ( \(n, got) ->
                        let
                              toget = max 0 (2 - got)
                              extra = left - toget
                           in
                              ( HandDesc Pair [n]
                              , if toget <= left
                                          then comb (45 + extra) extra * comb 4 toget
                                          else 0
                              )
            )
            vals

suitsX cards = A.accumArray (+) 0 (0, 3) $ map ((,1 :: Int) . suit) cards :: A.Array Int Int

valuesX cards = A.accumArray (+) 0 (2, 14) $ map ((,1 :: Int) . value) cards :: A.Array Int Int

handDescX yourcards tablecards =
      let
            takencards = yourcards .+. tablecards
            left = 7 - length takencards
            gotsuits = suitsX tablecards
            gotvalues = valuesX tablecards
            takensuits = suitsX takencards
            takenvalues = valuesX takencards
            sfcards = A.accumArray (+) 0 ((1, 0), (14, 3)) $ map (\(Card n s) -> ((n, s), 1)) tablecards :: A.Array (Int, Int) Int
            -- totals = [map show [Flush, Four, FullHouse, Three], map show [sum $ flushesX suits left, sum $ foursX values left, sum $ map sum $ fullhouseX values left, sum $ threesX values left]]
            cats =
                  concat
                        [ straightFlushesX sfcards False (size yourcards) (size tablecards) <$> [5 .. 14] <*> [0 .. 3]
                        , foursX takenvalues gotvalues False (size yourcards) (size tablecards) <$> [2 .. 14]
                        , fullhousesX1 takenvalues gotvalues False (size yourcards) (size tablecards) <$> [2 .. 14] <*> [2 .. 14]
                        , fullhousesX2 takenvalues gotvalues False (size yourcards) (size tablecards) <$> [2 .. 14] <*> [2 .. 14] <*> [2 .. 14]
                        , fullhousesX3 takenvalues gotvalues False (size yourcards) (size tablecards) <$> [2 .. 14] <*> [2 .. 14]
                        , pairsX takenvalues gotvalues False (size yourcards) (size tablecards) <$> [2 .. 14] <*> [2 .. 14] <*> [2 .. 14]
                        , pairsX1 takenvalues gotvalues False (size yourcards) (size tablecards) <$> [2 .. 14] <*> [2 .. 14]
                        ]
         in
            cats

showDescX yourcards tablecards = putGrid $ transpose $ map showT $ groupC handRank $ handDescX yourcards tablecards

groupC f = combine (+) 0 . map (\(a, b) -> (f a, b))

comparePlayers tablecards =
      let
            left = fromList allcards .-. fromList tablecards
            m =
                  ( \pc1 pc2 ->
                              let
                                    playercards = fromList [pc1, pc2]
                                    left1 = left .-. playercards
                                    cards = tablecards .+. playercards
                                 in
                                    map (\(r, n) -> (r, n, playercards)) $ groupC handRank $ handDescX [] cards
                  )
                        <$> left
                        <*> left
         in
            concat m

testAccuracyX tablecards = do
      let yourcards = []
      let ni = 100000
      let nx = comb 52 (7 - length tablecards)
      -- let tablecards = [Card 2 1, Card 2 2, Card 3 3] -- 133784560
      (yih :: [Int], oih :: [[Int]]) <- unzip <$> (replicateM ni $ trygameA $ trygame1 yourcards tablecards 5)
      let ylh = map fromBits yih :: [[Card]]
      let yid = map (,1) $ map handDesc yih
      let yld = map (,1) $ map handDesc ylh
      let yxd = handDescX yourcards tablecards
      let correct = zipWith (==) yid yld
      let f o = mapFromList (+) 0 . map (\(a, b) -> ((handCat a, take 0 $ handNums a), fromIntegral b / fromIntegral o))
      let results = map (\(a, (b, c)) -> (a, b, c)) $ M.toList $ joinMapsOuter (f ni yid) (f nx yxd)
      putGrid $ transpose $ map showT results
      putStrLn $ "out of " ++ show (comb (52 - length tablecards) (7 - length tablecards))

-- putGrid [map showHand yls, map show yiv, map show ysv, map (\x -> if x then "" else "***WRONG***") correct]
-- putStrLn $ "correct: "++show (length $ filter id correct)
{-
trygameX yourcards tablecards = let
         holeMap = comparePlayers tablecards

countRanks yourcards ((r, n, playercards):rs) you them =
         if playercards == yourcards
                  then them * n + countRanks yourcards rs (you+n) them
                  else countRanks yourcards rs you (them+n)
-}
yourcards = [1, 2, 3]
yourbets = [1, 2, 3]

hiscards = yourcards
hisbets = yourbets

gethisstrat =
      map
            ( \yourbet ->
                        map
                              ( \hiscard ->
                                          minel $
                                                map
                                                      ( \hisbet ->
                                                                  mean $
                                                                        map (\yourcard -> payoff yourcard yourbet hiscard hisbet) yourcards
                                                      )
                                                      hisbets
                              )
                              hiscards
            )
            yourbets

{-
calcyourstrat :: [[Int]] -> [Int]
calcyourstrat hisstrat =
         map (\yourcard -> maxel $
                  map (\yourbet -> mean $
                           map (\hiscard -> let hisbet = hisstrat !! yourbet !! hiscard in payoff yourcard yourbet hiscard hisbet) hiscards) yourbets) yourcards
-}
calchisstrat yourstrat =
      map
            ( \yourbet ->
                        map
                              ( \hiscard ->
                                          minel $
                                                map
                                                      ( \hisbet ->
                                                                  mean $
                                                                        mapMaybe (\yourcard -> ifJust (yourstrat !! yourcard == yourbet) $ payoff yourcard yourbet hiscard hisbet) yourcards
                                                      )
                                                      hisbets
                              )
                              hiscards
            )
            yourbets

probmult = 1 % 3

gethisstratp =
      map
            ( \yourbet ->
                        map
                              ( \hiscard ->
                                          minel $
                                                map
                                                      ( \hisbet ->
                                                                  mean $
                                                                        map (\yourcard -> payoff yourcard yourbet hiscard hisbet) yourcards
                                                      )
                                                      hisbets
                              )
                              hiscards
            )
            yourbets

{-
calcyourstratp hisstrat = -- calculate your strategy from his probabilistic strategy
         map (\yourcard -> maxel $
                  map (\yourbet -> mean $
                           map (\hiscard -> sum $
                                    map (\hisbet -> hisstrat !! yourbet !! hiscard !! hisbet * fromIntegral (payoff yourcard yourbet hiscard hisbet)) hisbets) hiscards) yourbets) yourcards
-}
calcyourstratp1 hisstrat =
      -- try a bunch of different ideas for your strategy and see the outcome
      snd $ maxOn (`stratpayoff` hisstrat) yourstrats

-- yourstrat !! yourcard !! yourbet = P(you bet yourbet | yourcard)

-- P(yourcard | yourbet) = P(yourbet | yourcard) * P(yourcard) / P(yourbet)
--                      = P(yourbet | yourcard) * P(yourcard) / Sum over all possible yourcards(P(yourbet | yourcard) * P(yourcard))
cardprob = 1 % 3

calchisstratp yourstrat =
      let
            yourbetprobs = map (\yourbet -> sum $ map (\yourcard -> yourstrat !! yourcard !! yourbet * cardprob) yourcards) yourbets
            yourcardprobs = map (\yourbet -> map (\yourcard -> yourstrat !! yourcard !! yourbet * cardprob / yourbetprobs !! yourbet) yourbets) yourcards
         in
            map
                  ( \yourbet ->
                              map
                                    ( \hiscard ->
                                                minel $
                                                      map
                                                            ( \hisbet ->
                                                                        sum $
                                                                              map (\yourcard -> yourcardprobs !! yourbet !! yourcard * fromIntegral (payoff yourcard yourbet hiscard hisbet)) yourcards
                                                            )
                                                            hisbets
                                    )
                                    hiscards
                  )
                  yourbets

calchisstratp1 yourstrat =
      snd $ minOn (stratpayoff yourstrat) hisstrats

-- stratpayoff :: (Real a) => [[a]] -> [[[a]]] -> [[a]]
-- stratpayoff :: [[Ratio Int]] -> [[[Ratio Int]]] -> [[Ratio Int]]
stratpayoff yourstrat hisstrat =
      mean $
            map
                  ( \yourcard ->
                              mean $
                                    map
                                          ( \hiscard ->
                                                      mean $
                                                            map
                                                                  ( \yourbet ->
                                                                              mean $
                                                                                    map
                                                                                          (\hisbet -> yourstrat !! yourcard !! yourbet * hisstrat !! yourbet !! hiscard !! hisbet * fromIntegral (payoff yourcard yourbet hiscard hisbet))
                                                                                          hisbets
                                                                  )
                                                                  yourbets
                                          )
                                          hiscards
                  )
                  yourcards

snoophisstrat games =
      A.amap wdist $
            A.accumArray (flip (:)) [] ((1, 1), (3, 3)) $
                  zipWith (\(yourcard, yourbet, hiscard, hisbet) weight -> ((yourbet, hiscard), (hisbet, weight))) games $
                        iterate (* 0.99) 1

yourstrats :: [[[Ratio Int]]]
yourstrats = crossList $ replicate 3 $ possprobs 3

hisstrats = map (groupN 3) $ crossList $ replicate 9 possprobs5 -- 1 3 0 0

possprobs5 :: [[Ratio Int]]
possprobs5 = adjprobs [[3, 0, 0], [0, 3, 0], [0, 0, 3], [1, 1, 1]]

adjprobs ys = map (\xs -> map (% sum xs) xs) ys

possprobs ncards = adjprobs $ possprobs1 ncards 0 0

possprobs1 ncards card done
      | card >= ncards || done >= ncards = [replicate ncards 0]
      | otherwise = concatMap (\c -> map (addat c 1) $ possprobs1 ncards c (done + 1)) [card .. ncards - 1]

addat at add recursed = let (before, a : after) = splitAt at recursed in before ++ a + add : after

mean xs = realToFrac (sum xs) / fromIntegral (length xs)

wmean xs ws = sum (zipWith (*) xs ws) / sum ws

wdist xws = let (xs, ws) = unzip xws in A.amap (/ sum ws) $ A.accumArray (+) 0 (minimum xs, maximum xs) xws

payoff1 (yc, (yb, (hc, (hb, ())))) = payoff yc yb hc hb

payoff :: (Num a1, Ord a2) => a2 -> a1 -> a2 -> a1 -> a1
payoff yourcard yourbet hiscard hisbet =
      (yourbet + hisbet)
            * if
                  | yourcard > hiscard -> 1
                  | yourcard < hiscard -> -1
                  | otherwise -> 0

convHexDigit x = elemIndex x "0123456789abcdef"

convHexDigit1 x = if x == ' ' then Just 0 else elemIndex x "0123456789abcdef"

convHex xs = sum . zipWith (*) (iterate (* 16) 1) . reverse <$> mapM convHexDigit xs

convHex1 xs = sum . zipWith (*) (iterate (* 16) 1) . reverse <$> mapM convHexDigit1 xs

a <.> b = (\x -> a <$> b x)

convBytes xs = mapM (\x -> mychr <$> (convHex1 $ take 2 x)) $ groupN 3 xs

mychr x =
      if
            | x == 10 -> '\n'
            | x < 32 || x >= 127 -> '.'
            | otherwise -> chr x

convLine xs = do
      let (xs1, xs2) = splitAt 4 xs
      addr <- convHex xs1
      let xs3 = take 48 $ drop 2 xs2
      convBytes xs3

{-
main = do
         l <- getLine
         case convLine l of
                  Just  j -> putStr j
                  Nothing -> putStrLn ('\n':l)
         main

INDEPENDENT HANDS

if the hands are independent (which I don't think they are),

maximise            ( chips + pot + n*raise )*p+(chips - raise)*(1-p)
                                                                  chips*p + pot*p + n*raise*p + raise*p - chips*p + chips - raise
                                                                  chips*(p+1) + pot*p + (n+1)*raise*p - chips*p - raise
                                                                  ((n+1)*p-1)*raise + chips*(p+1) + pot*p + chips*p

differentiate with respect to raise

                                                               (n+1)p-1 = 0

it doesn't have raise in it so there are no maxima, so:

                                                      so if p > 1/(n+1) then go all in
                                                                                                            else check or flop

DEPENDENT HANDS

the hands are not independent because the number of chips is carried across to the next hand
want to maximise the expected value of the ratio of chipsafterhand to chipsbeforehand
so we have to maximise the ratio of chips, and losing all your chips is very bad

if win probability is p between 0 and 1, or a ratio of a:b, this is the least you want to win, if you want to win overall, you want a better ratio
n is the number of players that call your raise
must be at least 1 because if no-one calls your raise you win anyway (unless some people are all-in)

                                    ( chipsNow + pot + n*raise )^a*(chipsNow - raise)^b  >  chipsBefore^(a+b)
                                                                                                                     c^a*d^b  >  e^(a+b)

set a=p
b=1-p

                                    ( chipsNow + pot + n*raise )^p*(chipsNow - raise)^(1-p)  >  chipsBefore
-}

expvalue chips pot nplayers winprob raise = (fromIntegral chips + fromIntegral pot + fromIntegral nplayers * raise) ** winprob * (fromIntegral chips - raise) ** (1 - winprob)

{-
I think that tends towards the additive equation above for independent hands when chipsNow & chipsBefore are large compared to pot and raise

if x is totalChipsBeforeThisHand - yourpreviousbetsthihand + pot, then y is totalChipsBeforeThisHand - yourpreviousbetsthishand
if x is totalChipsNow + pot, y = totalChipsNow

substitute x = chips + pot, y = chips

(x+nr)^p*(y-r)^(1-p)     differentiate and set = 0
n(py-r) + (p-1)x = 0
npy - nr + (p-1)x = 0
nr = npy + (p-1)x
r = py + (p-1)x/n
npy

generalise                   r            = (ayp+bop+cp+dy+eo+f)/n+gyp+hop+ip+jy+kp+l

if p = 0.5
         r = 1.5x/n+0.5y

if p = 0.25
         r = 1.25x/n+0.25y

maximise           (x+nr)^a*(y-r)^b
differentiate      an(y-r)^b*(x+nr)^(a-1)-b(y-r)^(b-1)*(x+nr)^a = 0
                                                         an(y-r)^b*(x+nr)^(a-1) = b(y-r)^(b-1)*(x+nr)^a             divide by (x+nr)^(a-1)
                                                         an(y-r)^b              = b(y-r)^(b-1)*(x+nr)               divide by (y-r)^(b-1)
                                                         an(y-r)                = b(x+nr)

                                                         an     x + nr      np
                                                         --  =  ------  =  -----
                                                         b      y - r      1 - p

                                                         a     x/n + r       p
                                                         -  =  -------  =  -----
                                                         b     y   - r     1 - p

                                                         a     x + nr        p
                                                         -  =  -------  =  -----
                                                         b     ny - nr     1 - p

                                                         basically, pot odds. at least we've proved it though

                                                         any - anr - bx - bnr   = 0
                                                         (-an-bn)r + any - bx   = 0
                                                                                             any - bx   = (a+b)nr
                                                                                                                  r   = (any - bx) / n(a + b)

if a = p and b = 1 - p

         r = (pny + (p-1)x)/n = x(p-1)/n+yp

c^p*d^(1-p) = c^p/d^p * d = (c/d)^p * d

differentiate e^p * d

d(e^p)/dx * d + e^p * dd/dx = 0
d(e^p)/dx         * d = - e^p * dd/dx
   pe^(p-1) * de/dx * d = - e^p * dd/dx           divide both sides by e^(p-1)
                        p * de/dx * d = - e   * dd/dx

differentiate c^p*d^(1-p)

d(c^p)/dx * d^(1-p) + c^p * d(d^(1-p)/dx             = 0
pc^(p-1)*dc/dx * d^(1-p) + c^p * (1-p)d^(-p) * dd/dx = 0
p(c/d)^(p-1)*dc/dx + (c/d)^p * (1-p) * dd/dx         = 0
pe    ^(p-1)*dc/dx = e    ^p * (p-1) * dd/dx         = 0
p           *dc/dx = e       * (p-1) * dd/dx
(dc/dx * p) / (dd/dx * (p-1))  = e
(dc/dx / dd/dx) * p/(p-1)      = e

dc/dx = n
dd/dx = -1

-}
-- ideal amount to bet
raise chips pot nplayers winprob = round $ fromIntegral (chips + pot) * (winprob - 1) / fromIntegral nplayers + fromIntegral chips * winprob

potodds nplayers winprob = fromIntegral nplayers * winprob / (1 - winprob)

{-
raise chips pot nplayers winprob = round $ fromIntegral (chips + pot) * winprob / fromIntegral nplayers + fromIntegral chips * winprob - (chips + pot) / nplayers
raise chips pot nplayers winprob = (chips + pot) * winprob / nplayers + chips * winprob * nplayers / nplayers - (chips + pot) / nplayers
raise chips pot nplayers winprob = ((chips + pot) * winprob + chips * winprob * nplayers - (chips + pot)) / nplayers
-}
toPotodds chips pot nplayers raise = (chips + pot + nplayers * raise) % (chips - raise)

winodds winprob = winprob / (1 - winprob)

{-
(chips + pot + raise) / (chips - raise) = potodds
   chips + pot + raise                    = potodds * (chips - raise)
   chips + pot + raise                    = potodds * chips - raise * chips
                                             raise + raise * chips    = potodds - chips - pot
                                                   (1 + chips) * raise    = potodds - chips - pot
                                                                                             raise    = (potodds - chips - pot) / (chips + 1)
                                                                                                                        ~ (potodds - pot) / chips
-}
raise1 chips pot potodds = round $ (potodds - fromIntegral chips - fromIntegral pot) / fromIntegral (chips + 1)

{-
if you fold, you get y
-}
-- expvalue chips pot nplayers winprob raise = (fromIntegral chips + fromIntegral pot + fromIntegral nplayers*raise)**winprob * (fromIntegral chips - raise)**(1-winprob)
-- max amount to bet, otherwise fold

call chips pot nplayers winprob = secantMethod (expvalue chips pot nplayers winprob) 10 0 (fromIntegral chips) (fromIntegral chips)

{-
maximin chipsnow pot yourcards tablecards opponents = maxOn (\yourbet -> minimax chipsnow pot yourbet yourcards tablecards opponents) $ take 10 $ iterate (*2) 10 --winprob 10 $ trygame yourcards tablecards opponents

minimax chipsnow pot yourbet yourcards tablecards opponents = minimum $ map (\hisbet -> maximin1 chipsnow (pot + bet) yourcards tablecards opponents) $ take 10 $ iterate (*2) 10 --winprob 10 $ trygame yourcards tablecards opponents

maximin1 chipsnow pot yourcards tablecards opponents = maximum $ map (\bet -> maximin1 chipsnow (pot + bet) yourcards tablecards opponents) $ take 10 $ iterate (*2) 10 --winprob 10 $ trygame yourcards tablecards opponents
-}
{-

HOW TO HANDLE MULTIPLE ROUNDS

you want it so that after all 4 rounds:

number of times to win      what you will have if you win the entire hand
-----------------------  =  ----------------------------------------------
number of times to lose     what you will have if you lose the entire hand

number of times to win      chipsBefore + pot + yourBets
-----------------------  =  ----------------------------
number of times to lose        chipsBefore - yourBets

so the pot odds that are important are not winnings : losings but totalafterwinning : totalafterlosing
this seems different to the standard theory
you must include your total chips (your total net worth)

if everyone is still in:

what you will have if you win the entire hand      number of times to win      chipsBefore + nplayers * yourBets    (nplayers excludes you)
----------------------------------------------  =  -----------------------  =  ---------------------------------
what you will have if you lose the entire hand     number of times to lose          chipsBefore - yourBets

choose the overall ratio, then split it up how people are least likely to fold, i.e. take the fourth root because there's four betting rounds

                                                         an     x + nr
                                                         --  =  ------
                                                         b       y - r

                                                         an     x + nr + ms     x + nr
                                                         --  =  -----------  =  ------
                                                         b       y - r - s      y - r

(x + nr + ms)(y - r) = (x + nr)(y - r - s)
xy + nyr + mys - xr - nr^2 - mrs = xy + nyr - xr - nr^2 - xs - nrs
                                 mys             - mrs =                      - xs - nrs
                                 mys - mrs +  xs + nrs = 0
                                       s(my - mr + x + nr) = 0
                                          s(my + x + (n-m)r) = 0
                                                   my + x + (n-m)r = 0 or s = 0
                                                                                             r = (x + my)/(n-m)

set c = x + nr, d = y - r, z = number of betting rounds left

a/b = (c/d)^4
(a/b)^(1/z) = c/d

ry     s(y-r)
--  =  ------
x       x+nr

ry(x+nr) = sx(y-r)
                           = sxy - sxr
xyr+nyr^2+xrs

                                                         an(y - r - s)   = b(x + nr + ms)
                                                         any - anr + ams = bx + bnr + bms
                                                         any

this is the total raise over all 4 rounds of betting
still need to account for bluffing

we can account for bluffing by the probability of the opponents folding
if we choose our raise so that it makes no difference to either player whether they call or fold (but they could re-raise/go all in! need to seriously consider that)
don't forget to check the value is higher than chipsBefore

                           (chipsNow + pot) = (chipsNow + pot + n*raise)^p * (chipsNow - raise)^(1-p)  >  chipsBefore

                                                                                    (chipsNow + pot + n*raise)^p * (chipsNow - raise)  >  chipsBefore
                           (chipsNow + pot) = (------------------------)
                                                                                    (    chipsNow - raise    )

                              chipsNow + pot    (chipsNow + pot + n*raise)^p  >  chipsBefore
                           ---------------- = (------------------------)
                           chipsNow - raise   (    chipsNow - raise    )

                              chipsNow + pot    (chipsNow + pot + n*raise)^p  >  chipsBefore
                           ---------------- = (------------------------)
                           chipsNow - raise   (    chipsNow - raise    )

                              ----------------
                           / chipsNow + pot      chipsNow + pot + n*raise  >  chipsBefore
                  p / ----------------  =  ------------------------
                  \/  chipsNow - raise         chipsNow - raise

                           (chipsNow - raise)^p       chipsNow - raise
                           --------------------  =  ------------------------
                                 chipsNow + pot         chipsNow + pot + n*raise

                                                                                             =         chipsNow                         raise
                                                                                                      ------------------------  -  --------------------------
                                                                                                      chipsNow + pot + n*raise     chipsNow + pot + n*raise

can't find a way to solve that for raise so do it numerically
-}
-- expvalue chips pot nplayers winprob raise = (chips + pot + nplayers*raise)**winprob * (chips - raise)**(1-winprob)

bluff chips pot nplayers winprob = secantMethod (expvalue chips pot nplayers winprob) 20 0 (fromIntegral chips) (fromIntegral chips / 2)

{-
when you bluff you are trying to change their perception of the strength of your hand

generalised binomial theorem
                           chipsNow + pot   = (x+nr)^p * (y-r)^(1-p)

we can solve this your strategy/their strategy recursion by searching for the fixed points, where neither strategy is improved
         may as well assume you and he have the same strategy
                  so then neither side can win
we have two functions, f calculates your strategy given his and g calculates his given yours

f(g(s)) = s

raise chips pot nplayers winprob = (chips + pot) * (winprob / nplayers + 1 / nplayers) + chips * winprob
raise chips pot nplayers winprob = (chips + pot) * (winprob / nplayers + 1 / nplayers) + chips * winprob
raise chips pot nplayers winprob = (chips + pot) * (winprob + 1) / nplayers + chips * winprob

a function of 4 variables

or with probability density function for bluffing
should you ever bet lower than the ideal? i don't think so
should you raise on a scare card? i like that idea
sklansky says you should bet so that your opponents pot odds match your win odds

pdf raise chips pot nplayers winprob

a function of 5 variables

could make it proportional to chips, then it's 4 again

pdf ypx nplayers winprob raisepx

could assume he uses a normal dist of mean
from our raise equation (perhaps we fit a quadratic function to this part from his bets)
and variance that we calculate from watching his bets

pdf = normal (\let x = raise ... in a*x^2+b*x+c) sd

you know your cards and the table cards so you have yourwinprob
he knows his cards and the table cards so he has hiswinprob
you know the tablecards but not his so for him you have yourhiswinprob
he knows the tablecards but not yours so for you he has hisyourwinprob
if it's only you and him then yourhiswinprob = 1 - yourwinprob and hisyourwinprob = 1 - hiswinprob
but not necessarily yourwinprob = 1 - hiswinprob

if we want a strategy that beats any other strategy,
then we don't know what that strategy is
so we can't guess their cards from their betting
and they may know ours (although ours may be probabilistic)

so after the river

(x+nr)^p*(y-r)^(1-p)
-}

draw c = do cl <- lift get; lift $ put $ delete c cl

draw1 c = lift $ do cl <- get; put $ delete c cl

pickCard :: (MonadIO m) => Int -> StateT Int m Int
pickCard cardsLeft = do
      cn <- lift $ uniformRM (0 :: Int, 59) globalStdGen
      if shift 1 cn .&. cardsLeft == 0
            then pickCard cardsLeft
            else return cn

randCardB :: (MonadIO m) => StateT Int m Int
randCardB = do
      cardsleft <- get
      cn <- pickCard cardsleft
      let c :: Int = bitsOfCard $ cardOfNum cn
      put $ cardsleft .&. complement c
      return c

randCardS :: (MonadIO m, Eq b) => StateT (S.Set b) m b
randCardS = do
      cardsleft <- get
      cn <- lift $ uniformRM (0, S.size cardsleft - 1) globalStdGen
      let c = S.elemAt cn cardsleft
      put $ S.deleteAt cn cardsleft
      return c

randCardL :: (MonadIO m, Eq b) => StateT [b] m b
randCardL = do
      cardsleft <- get
      cn <- lift $ uniformRM (0, length cardsleft - 1) globalStdGen
      let c = cardsleft !! cn
      put $ delete c cardsleft
      return c

randCardsB n = foldr (.|.) 0 <$> replicateM n randCardB
randCardsS n = S.fromList <$> replicateM n randCardS
randCardsL n = replicateM n randCardL

{-
pickCard :: (MonadIO m, StatefulGen g m) => g -> Int -> StateT Int m Int
pickCard rgen cardsLeft = do
         cn <- lift $ uniformRM (0::Int, 59) rgen --globalStdGen
         if shift 1 cn .&. cardsLeft == 0
                  then pickCard rgen cardsLeft
                  else return cn

randCardB :: (MonadIO m, StatefulGen g m) => g -> StateT Int m Int
randCardB rgen = do
         cardsleft <- get
         cn <- pickCard rgen cardsleft
         let c :: Int = shift 1 cn
         put $ cardsleft .&. complement c
         return c

randCardS :: (MonadIO m, StatefulGen g m, Eq b) => g -> StateT (S.Set b) m b
randCardS rgen = do
         cardsleft <- get
         cn <- lift $ uniformRM (0, S.size cardsleft - 1) rgen --globalStdGen
         let c = S.elemAt cn cardsleft
         put $ S.deleteAt cn cardsleft
         return c

randCardL :: (MonadIO m, StatefulGen g m, Eq b) => g -> StateT [b] m b
randCardL rgen = do
         cardsleft <- get
         cn <- lift $ uniformRM (0, length cardsleft - 1) rgen --globalStdGen
         let c = cardsleft !! cn
         put $ delete c cardsleft
         return c

-}
{-test :: (StatefulGen AtomicGenM m) => StateT [b] m b
test = do
         lift $ put [1,2,3,4]
         randCard globalStdGen
-}
test1 :: (MonadIO m, Ord a, Eq a, Num a) => StateT (S.Set a) m a
test1 = do
      put $ S.fromList [1, 2, 3, 4]

      randCardS

allcards = Card <$> [2 .. 14] <*> [0 .. 3]

replicateS n m = fromList <$> replicateM n m

class Hand a where
      (.+.) :: a -> a -> a
      (.-.) :: a -> a -> a
      fromList :: [Card] -> a
      fromBits :: Int -> a
      toBits :: a -> Int
      size :: a -> Int
      handDesc :: a -> HandDesc
      randCards :: (MonadIO m) => Int -> StateT a m a

instance Hand [Card] where
      (.+.) = (++)
      (.-.) = flip deleteMulti
      fromList = id
      fromBits = handOfBits
      toBits = bitsOfHand
      size = length
      handDesc = handDescL
      randCards = randCardsL

instance Hand (S.Set Card) where
      (.+.) = S.union
      (.-.) = (S.\\)
      fromList = S.fromList
      fromBits = S.fromList . handOfBits
      toBits = bitsOfHand . S.toList
      size = S.size
      handDesc = handDescL . S.toList
      randCards = randCardsS

instance Hand Int where
      (.+.) = (.|.)
      (.-.) a b = a .&. complement b
      fromList = bitsOfHand
      fromBits = id
      toBits = id
      size = countBits
      handDesc = handDescB
      randCards = randCardsB

sp m = do
      r <- m
      return $ deepseq r 0

game1 :: (MonadIO m, Hand b) => m (b, [b])
game1 = trygame1 [] [] 2

game :: IO (HandDesc, Int, HandDesc, Bool)
game = trygame game1

testSpeed n = unzip <$> (replicateM n $ trygameA $ trygame1 [] [] 5)
testSpeedB = testSpeed :: Int -> IO ([Int], [[Int]])
testSpeedL = testSpeed :: Int -> IO ([[Card]], [[[Card]]])
testSpeedS = testSpeed :: Int -> IO ([S.Set Card], [[S.Set Card]])

testAccuracy = do
      (yis :: [Int], ois :: [[Int]]) <- unzip <$> (replicateM 10000 $ trygameA $ trygame1 [] [] 5)
      let yls = map fromBits yis :: [[Card]]
      let yss = map fromBits yis :: [S.Set Card]
      let yiv = map handDesc yis
      let ylv = map handDesc yls
      let ysv = map handDesc yss
      let correct = zipWith (==) yiv ysv
      -- putStrLn $ "correct: "++show (length $ filter id correct)
      putGrid [map showHand yls, map show yiv, map show ysv, map (\x -> if x then "" else "***WRONG***") correct]
      putStrLn $ "correct: " ++ show (length $ filter id correct)

-- trygame :: (MonadIO m, Hand b c) => [Card] -> [Card] -> Int -> c -> m (HandDesc, b, HandDesc, Bool)

trygame game = do
      (yourhand, opphands) <- game
      let maxopp = maximum $ map handDesc opphands
      let yourval = handDesc yourhand
      return (yourval, yourhand, maxopp, yourval > maxopp)

trygameA game = do
      (yourhand, opphands) <- game
      return (fromBits yourhand, map fromBits opphands)

trygame1 yourcards tablecards opponents =
      let
            yourcardS = fromList yourcards
            tablecardS = fromList tablecards
            allcardS = fromList allcards
         in
            flip evalStateT (allcardS .-. yourcardS .-. tablecardS) $
                  trygame2 yourcardS tablecardS opponents

trygame2 yourcards tablecards opponents = do
      let opp = randCards 2
      oppcards1 <- replicateM opponents opp
      tablecards1 <- randCards (5 - size tablecards)
      let tablecards2 = tablecards .+. tablecards1
      let opphands = map (tablecards2 .+.) oppcards1
      yourcards2 <- randCards (2 - size yourcards)
      let yourhand = tablecards2 .+. yourcards .+. yourcards2
      return (yourhand, opphands)

handCat1 h = (handCat h, h)
groupCount g = mapFromList (insertWith2 (+) 0) M.empty . map (\x -> (g x, (x, 1)))
medianMap xs = M.elemAt (div (M.size xs) 2) xs

stats0 x y =
      let
            l = M.toList y
            (d, (a, _)) = maxOn snd l
         in
            (x, fst $ medianMap y, sum $ map snd $ M.toList y, a, d)

stats1 xs = map (uncurry stats0) $ M.toList $ groupCount handCat xs

stats n game = do
      (you, _, opp, wins) <- unzip4 <$> replicateM n game
      return (stats1 you, stats1 opp, (fromIntegral $ length $ filter id wins) / fromIntegral n)

showStats1 n game = do
      (yourhands, opphands :: [[Int]]) <- unzip <$> replicateM n game
      putGrid $ transposez "" $ map (\((c, v), n) -> showT (c, v, n)) $ combine (+) 0 $ map (\h -> let HandDesc c v = handDesc h in ((c, take 1 v), 1 :: Int)) yourhands

winprob n game = do
      (_, _, _, wins) <- unzip4 <$> replicateM n game
      return $ (fromIntegral $ length $ filter id wins) / fromIntegral n

data Var = Var
      { tablecards :: M.Map Int Card
      , players :: M.Map Int Player
      , yourseat :: Int
      , yourwinprob :: Double
      , pot :: Int
      }

data Player = Player
      { pname :: String
      , pid :: Int
      , pnameid :: String
      , plevel :: Int
      , plevelName :: String
      , pplayerSince :: Day
      , pchipsTotal :: Int
      , pchipsHand :: Int
      , pchipsTable :: Int
      , pamount :: Int
      , pcards :: [Card]
      , pbets :: [Trial]
      , pbetsCur :: M.Map Int Trial
      }

data Trial = Trial
      { twinProb :: Double
      , tncards :: Int
      , tamount :: Int
      , tchipsTotal :: Int
      , tchipsHand :: Int
      , tchipsTable :: Int
      }

emptyPlayer = Player "" 0 "" 0 "" (YearMonthDay 0 0 0) 0 0 0 0 [] [] M.empty

readCard xs =
      let
            [n, s] = split "of" xs
         in
            Card (readInt n) (fromJust $ elemIndex (head s) "msck") -- m = spades, s = clubs, c = diamonds, k = hearts

showPlayer (s, p) = [show s, pnameid p, showComma $ pchipsTotal p, showComma $ pchipsHand p, showComma $ pchipsTable p, showComma $ pamount p]

showPlayers players = do
      putStrLn "players:"
      putGrid $ transpose $ (["seat", "nameID", "total", "hand", "table", "amount"] :) $ map showPlayer $ M.toList players

showWinProb ioref = do
      var <- readIORef ioref
      wp <- showWinProb1 (map snd $ M.toList $ tablecards var) (players var) (yourseat var)
      writeIORef ioref var{yourwinprob = wp}

showWinProb1 t players1 yourseat1 =
      case M.lookup yourseat1 players1 of
            Just you -> do
                  showStats 10000 $ trygame $ (trygame1 :: (MonadIO m) => [Card] -> [Card] -> Int -> m (Int, [Int])) (pcards you) t (M.size players1 - 1)
            Nothing -> return 0

showStats n game = do
      (statsyou, statsopp, winprob) <- stats n game
      let (ycat, yval, ycount, yv1, yc1) = unzip5 statsyou
      let (ocat, oval, ocount, ov1, oc1) = unzip5 statsopp
      putGrid $ transposez "" $ ["You", "median", "count", "better", "worse", "mode", "count", "them", "median", "count", "better", "worse", "mode", "count"] : transposez "" [map show ycat, map showHandVal1 yval, map show ycount, map show $ tail $ scanr (+) 0 ycount, map show $ init $ scanl (+) 0 ycount, map showHandVal1 yv1, map show yc1, map show ocat, map showHandVal1 oval, map show ocount, map show $ tail $ scanr (+) 0 ocount, map show $ init $ scanl (+) 0 ocount, map showHandVal1 ov1, map show oc1]
      putStrLn $ ("win prob: " ++) $ showFFloat (Just 8) winprob ""
      return winprob

showPot v = do
      let tab = sum $ table $ players v
      let ttl = tab + pot v
      putStrLn $ "pot: " ++ show (pot v) ++ " table: " ++ show tab ++ " total: " ++ show ttl

showYourBets v = do
      let pot1 = pot v + (sum $ table $ players v)
      putStrLn $ "POT: " ++ showComma (pot v)
      let s = yourseat v
      let y = players v M.! s
      showYourBets1 (pchipsHand y) pot1 (M.size $ players v) ([0, 0, 0, 1, 2, 3] !! (M.size $ tablecards v)) (yourwinprob v)

showYourBets1 chips pot nplayers roundN winprob = do
      -- let bestBet = strategise chips pot nplayers roundN $ raise chips pot nplayers winprob
      let bestBet = raise1 chips pot $ (** (1 / fromIntegral (4 - roundN))) $ potodds nplayers winprob
      let maxBet = call chips pot nplayers winprob
      putStrLn $ "POT: " ++ showComma pot ++ "     best bet: " ++ showComma bestBet ++ "     max bet: " ++ showComma maxBet

table players = map (pchipsTable . snd) $ M.toList players

shiftChipsToPot ioref = do
      var <- readIORef ioref
      let players1 = players var
      let players2 = M.map (\p -> p{pchipsTable = 0}) players1
      let pot1 = pot var + sum (table players1)
      putStrLn $ "pot: " ++ showComma pot1
      writeIORef ioref var{players = players2, pot = pot1}

-- return pot1

showComma n = reverse $ intercalate "," $ groupN 3 $ reverse $ show n

talk ioref s = loop
   where
      loop = do
            dat <- recv s 1024
            let headers = B.split 10 dat
            let url = (!! 1) $ split " " $ sofbs $ head headers
            let terms = tail $ split "/" url
            print terms
            var <- readIORef ioref
            case terms of
                  ["you_are", readInt . drop 1 -> seat] -> do
                        let v = var{yourseat = seat}
                        writeIORef ioref v
                        putStrLn $ ("your seat: " ++) $ show $ yourseat v
                  ["sitplayer", readInt -> seat, name, readInt -> id, readInt -> level, levelName, readInt -> chipsTotal, readInt -> chipsHand, readInt -> chipsTable, readInt -> month, readInt -> day, readInt -> year] -> do
                        let v = var{players = M.alter (\x -> Just $ let player = fromMaybe emptyPlayer x in player{pname = name, pid = id, pnameid = name ++ take 4 (show id), plevel = level, plevelName = levelName, pchipsTotal = chipsTotal, pchipsHand = chipsHand, pchipsTable = chipsTable, pplayerSince = YearMonthDay (fromIntegral year) month day}) seat $ players var}
                        writeIORef ioref v
                        showPlayers $ players v
                  ["leave", readInt -> seat] -> do
                        let v = var{players = M.alter (\x -> Nothing) seat $ players var}
                        writeIORef ioref v
                        showPlayers $ players v
                  ["tablecard", readInt -> seat, readCard -> card] -> do
                        let t = M.insert seat card $ tablecards var
                        writeIORef ioref var{tablecards = t}
                        let c = map snd $ M.toList t
                        let cs = rsort c
                        putStrLn $ ("table cards: " ++) $ concatMap showCard c ++ " sorted: " ++ concatMap showCard cs
                        showWinProb ioref
                        when (elem (length c) [3, 4, 5]) $ shiftChipsToPot ioref
                  ["bet", readInt -> seat, readInt -> amount, readInt -> allIn, readInt -> chipsTotal, readInt -> chipsHand, readInt -> chipsTable] -> do
                        -- let v = var { players = M.alter (\x -> Just $ let player = fromMaybe emptyPlayer x in player { pbetsCur = M.insert (M.size $ tablecards var) (amount) $ pbetsCur player }) (seat) $ players var }
                        let trial = Trial 0 (M.size $ tablecards var) amount chipsTotal chipsHand chipsTable
                        let v = var{players = M.alter (\x -> Just $ let player = fromMaybe emptyPlayer x in player{pchipsTotal = chipsTotal, pchipsHand = chipsHand, pchipsTable = chipsTable, pamount = amount, pbetsCur = M.insert (tncards trial) trial $ pbetsCur player}) seat $ players var}
                        writeIORef ioref v
                        putStrLn $ intercalate "     " $ showPlayer $ (seat,) $ players v M.! seat
                        showYourBets v
                        hWaitForInput stdin 100
                        return ()
                  ("playercards" : seat1 : cards) -> do
                        let seat = if seat1 == "undefined" then yourseat var else readInt seat1
                        let playercards = map readCard cards :: [Card]
                        putStrLn $ "player " ++ show seat ++ " has " ++ concatMap showCard playercards
                        when (seat >= 0) $ do
                              let player = fromMaybe emptyPlayer $ M.lookup seat (players var)
                              let player1 = player{pcards = playercards}
                              let v = var{players = M.alter (\x -> Just player1) seat $ players var}
                              writeIORef ioref v
                              if seat1 == "undefined" || seat == yourseat var
                                    then do
                                          showWinProb ioref
                                    else do
                                          let tablecards1 = map snd $ M.toList $ tablecards var
                                          winprobs <-
                                                mapM
                                                      ( \tbn -> do
                                                                  let tablecardsZ = fromList (take tbn tablecards1) :: Int
                                                                  let playercardsZ = fromList playercards :: Int
                                                                  (_, _, winprob) <- stats 1000 $ trygame $ (trygame1 :: (MonadIO m) => [Card] -> [Card] -> Int -> m (Int, [Int])) playercards (take tbn tablecards1) (M.size $ players var)
                                                                  return winprob
                                                      )
                                                      [0, 3, 4, 5]
                                          let bets = map (\bet -> bet{twinProb = winprobs !! tncards bet}) $ map snd $ M.toList $ pbetsCur player
                                          let playerbets = bets ++ pbets player
                                          let player2 = player1{pbets = playerbets}
                                          return ()
                  ["handend"] -> do
                        let v = var{pot = 0, tablecards = M.empty, players = M.map (\p -> p{pcards = [], pamount = 0, pchipsTable = 0}) $ players var}
                        writeIORef ioref v

            unless (B.null dat) $ do
                  -- sendAll s "HTTP/1.1 200 OK\n\n\n"
                  sendAll s $
                        B.intercalate
                              "\n"
                              [ "HTTP/1.1 200 OK"
                              , "Date: Sun, 12 May 2024 12:51:40 GMT"
                              , "Content-Type: text/html"
                              , "Content-Length: 1"
                              , -- "Connection: keep-alive",
                                    -- "Keep-Alive: timeout=120",
                                    "Cache-Control: public, max-age=3600, stale-while-revalidate=60, stale-if-error=86400"
                              , "req-svc-chain: GTM"
                              , "Location: https://www.bbc.co.uk/index.html"
                              , "Origin-Agent-Cluster: ?0"
                              , "nel: {\"report_to\":\"default\",\"max_age\":2592000,\"include_subdomains\":true,\"failure_fraction\":0.25}"
                              , "via: 1.1 BBC-GTM"
                              , "report-to: {\"group\":\"default\",\"max_age\":2592000,\"endpoints\":[{\"url\":\"https://default.bbc-reporting-api.app/report-endpoint\",\"priority\":1}],\"include_subdomains\":true}"
                              , "Server: BBC-GTM"
                              , "Strict-Transport-Security: max-age=31536000; preload"
                              , "Permissions-Policy: browsing-topics=(), join-ad-interest-group=(), run-ad-auction=()"
                              , ""
                              , "<html>"
                              , "<head><title>301 Moved Permanently</title></head>"
                              , "<body>"
                              , "<center><h1>301 Moved Permanently</h1></center>"
                              , "<hr><center>openresty</center>"
                              , "</body>"
                              , "</html>"
                              ]
                  loop

-- what parameters should a strategy have?
-- tightness, aggressiveness, randomness

-- m = spades, s = clubs, c = diamonds, k = hearts

-- from the "network-run" package.
{-
could loop over all possible strategies

could allow randomness in the strategies

there's no point in him having a probabilistic strategy?

I think if he didn't you could bluff him all the time
-}

minel = findel minimum

-- maxel = findel maximum

findel f xs = fromJust $ elemIndex (f xs) xs

-- l = x mean $ x minimum k

main = main0

main0 = do
      ht <- lookupTable4
      r <- handRankIO ht $ take 7 allcards
      print r

main1 :: IO ()
main1 = do
      ioref <- newIORef $ Var M.empty M.empty -1 0 0
      runTCPServer Nothing "8888" ioref talk

runTCPServer mhost port ioref server = withSocketsDo $ do
      addr <- resolve
      E.bracket (open addr) close loop
   where
      resolve = do
            let hints =
                              defaultHints
                                    { addrFlags = [AI_PASSIVE]
                                    , addrSocketType = Stream
                                    }
            head <$> getAddrInfo (Just hints) mhost (Just port)
      open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
            setSocketOption sock ReuseAddr 1
            withFdSocket sock setCloseOnExecIfNeeded
            bind sock $ addrAddress addr
            listen sock 128
            return sock
      loop sock = forever $
            E.bracketOnError (accept sock) (close . fst) $
                  \(conn, _peer) ->
                        void $
                              -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
                              -- but 'E.bracketOnError' above will be necessary if some
                              -- non-atomic setups (e.g. spawning a subprocess to handle
                              -- @conn@) before proper cleanup of @conn@ is your case
                              forkFinally (server ioref conn) (const $ gracefulClose conn 5000)

z = payoff <$> [1, 2, 3] <*> [1, 2, 3] <*> [1, 2, 3] <*> [1, 2, 3]

x = (,)

x1 f = map f [1, 2, 3]

x2 f = map f [1, 2, 3]

x3 f = map (\x -> map (f x) [1, 2, 3])

x4 = map (\x -> map (\y -> x + y) [1, 2, 3]) [10, 20, 30]

x5 = map (map (+)) [[0, 1, 2]]

a1 = (,)

{-
class Cross f a b where
         cross :: f -> a -> b

instance Cross (a -> b) (a :- c) (b :- d) where

instance Cross (a -> b) () () where
         cross a b = b
-}
data Tree a = Tree a [Tree a]

class Mip a b c where
      mip :: a b -> a c -> a (a c)

instance Mip [] b b where
      mip xs ys = map (\x -> ($) (x :) ys) xs

instance Mip [] b [b] where
      mip xs ys = map (\x -> map (x :) ys) xs

instance Mip [] b [[b]] where
      mip xs ys = map (\x -> map2 (x :) ys) xs

instance Mip [] b [[[b]]] where
      mip xs ys = map (\x -> map3 (x :) ys) xs

f xs ys = crossWith (:) xs ys

crossMap xs ys = map (\x -> ($) (x :) ys) xs

crossMap1 xs ys = map (\x -> map (x :) ys) xs

crossMap2 xs ys = map (\x -> map2 (x :) ys) xs

infixr 9 <:>
xs <:> ys = mip xs ys

list = [1, 2, 3] :: [Int]

kk = list <:> list <:> list

-- cm :: [a] -> ((a -> b) -> [a] -> [b], (a -> b) -> a -> b, [[a]]) -> ((a -> b) -> [a] -> [b], [a] -> [[a]] -> [[a]])
{-
cm :: [Int] -> ((Int -> [Int]) -> [Int] -> [[Int]], ([Int] -> [Int]) -> [Int] -> [Int], [Int]) ->
                                             ((Int -> [Int]) -> [Int] -> [[Int]], ([Int] -> [[Int]]) -> [Int] -> [[Int]], [Int])
-}

-- in map . $
-- . :: (b -> c) -> (a -> b) -> (a -> c)
-- map :: (d -> e) -> ([d] -> [e])
-- b = d -> e, c = [d] -> [e]

{- $   :: (f -> g) -> (f -> g)
f = a, g = b
but b = d -> e
ok, so g = d -> e = b
so the result of . is (a -> c) = f -> [d] -> [e]
we need it to be at least (d -> e) -> [d] -> [e]
-}

-- and also (d -> e) -> [[d]] -> [[e]]

-- it has to be forall z . d e -> z d -> z e

-- map .! map2 = \f x ->

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- map

-- (.!) :: ((a -> b) -> [a] -> [b]) -> ((a -> b) -> ([[a]] -> [[b]])) -> (a -> b) -> [[[a]]] -> [[[b]]]
map .! map2 = \f xsss -> map (\xss -> map2 f xss) xsss

{-
cm :: forall c d z . z c -> ((c   ->   c) ->    z c  ->    z c , forall y . ((c -> c) ->    y c  ->    y c ,    y c)) ->
                                                                                    ((z c -> z c) -> z (z c) -> z (z c), forall y . ((c -> c) -> z (y c) -> z (y c), z (y c)))
--}
{-
cm :: forall c d z . c -> ((c   ->   c) ->    z c  ->    z c , (c -> c) ->    z c  ->    z c ,    z c ) ->
                                                                              ((z c -> z c) -> z (z c) -> z (z c), (c -> c) -> z (z c) -> z (z c), z (z c))
--}
{-
cm :: [Int] -> ((c -> y c) -> z c -> z (y c), y c -> z (y c) -> z c -> z (y c), y c) ->
                                             ((c -> y c) -> z c -> z (y c), (z c -> z (z c)) -> z (z c) -> z (z c), z (z c))
--}
-- cm xs (f, g, ys) = let h = f . g in (f, h, map (\x -> g (x :) ys) xs)

cn xs (f, g, ys) = let h = f . g in (f, h, map (\x -> g (x :) ys) xs)

cp xs (f, g, ys) = let h = f . g in (f, h, map (\x -> g (x :) ys) xs)

-- zzx :: Num a => (([a] -> [a]) -> [[a]] -> [[a]], ([a] -> [a]) -> [[a]] -> [[a]],  [[a]])
-- zzx = cm [1, 2, 3] (map, ($), [])

zzy :: (Num a) => (([[a]] -> [[a]]) -> [[[a]]] -> [[[a]]], ([a] -> [a]) -> [[[a]]] -> [[[a]]], [[[a]]])
zzy = cn [1, 2, 3] (map, map, [[1]])

zzz :: (Num a) => (([[[a]]] -> [[[a]]]) -> [[[[a]]]] -> [[[[a]]]], ([a] -> [a]) -> [[[[a]]]] -> [[[[a]]]], [[[[a]]]])
zzz = cp [1, 2, 3] (map, map2, [[[2]]])

zz9 = zz9

thd (a, b, c) = c

zz = crossMap [1, 2, 3] []

y1 = crossMap2 [1, 2, 3] $ crossMap1 [1, 2, 3] $ crossMap [1, 2, 3] []

k :: Int -> Int -> Int
k = (+)

{-
kelly :: Double -> Double -> Double
kelly winProb potOdds = winProb - (1 - winProb) / potOdds

kellyTab = crossTabFF kelly [0,0.1 .. 1] [1.0..10]

powerTab = crossTabFF (**) [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.9415,0.9718,0.9978] [1.0..10]

showFF1 x = if x >= 0 then showFFloat (Just 3) x "" else ""

crossTabFF f xs ys  = let shownxs = map showFF1 xs
                                                                              shownys = map showFF1 ys
                                                                              tab1    = crossWith (showFF1 .: f) xs ys
                                                                              tab2    = ("" : shownxs) : zipWith (:) shownys tab1
                                                                              tab3    = map (\(a:b) -> a:"|":b) tab2
                                                                              (h:t)   = map concat $ padTable tab3
                                                                              tab5    = h : replicate (length h) '-' : t
                                                                  in
                                                                              putStr $ intercalate1 "\n" tab5

getHR :: IO (Ptr CInt)
getHR = do
         let size = 32487834 * 4
         p <- mallocBytes size
         h <- openBinaryFile "d:/code/lib/xpokereval/bin/handranks.dat" ReadMode
         hGetBuf h p size
         return p

untilN f x 0 = x
untilN f x n = f $ untilN f x (n-1)

lookupStep hr done card = done >>= \idx -> peekElemOff hr $ fromIntegral idx + card

lookupHand :: Ptr CInt -> [Int] -> IO Int
lookupHand hr cards = fromIntegral <$> foldl (lookupStep hr) (return 53) cards

lookupHand1 :: Ptr CInt -> [Int] -> IO CInt
lookupHand1 hr cards = do
         i1 <- peekElemOff hr (53 + cards !! 0)
         i2 <- peekElemOff hr (fromIntegral i1 + cards !! 1)
         i3 <- peekElemOff hr (fromIntegral i2 + cards !! 2)
         i4 <- peekElemOff hr (fromIntegral i3 + cards !! 3)
         i5 <- peekElemOff hr (fromIntegral i4 + cards !! 4)
         i6 <- peekElemOff hr (fromIntegral i5 + cards !! 5)
         i7 <- peekElemOff hr (fromIntegral i6 + cards !! 6)
         return i7

winProb hr nt hole comm np = let
         holeb = ilToBits hole
         commb = ilToBits comm
         ntodeal = 5 - length comm
         w = evalStateT (winProb1 hr holeb commb ntodeal np) (holeb .|. commb)

         in length <$> filter id <$> replicateM nt w

winProb1 hr hole comm ntodeal np = do
         rc2        <- randCards2 ntodeal
         let comm1  =  comm .|. rc2
         myRank     <- lift $ lookupHand hr $ bitsToIl $ hole .|. comm1
         otherRanks <- replicateM np $ winProb2 hr comm1
         return $ myRank > maximum otherRanks

--winProb2 :: Ptr CInt -> Int64 -> StateT Int64 IO Int
winProb2 hr comm = do
         otherHole1 <- randCard2
         otherHole2 <- randCard2
         lift $ lookupHand hr $ bitsToIl $ comm .|. otherHole1 .|. otherHole2

pack = [[a, b] | a <- "23456789TJQKA", b <- "cdhs"]

showCard n = pack !! n

showCards = map showCard

randCard = randomRIO (1::Int, 52::Int)

randCards :: Int -> Int64 -> IO Int64
randCards 0 done = return done
randCards n done = randCard1 done >>= randCards (n-1)

type PackIO = StateT Int64 IO Int64

randCards2 n = foldr (.|.) 0 <$> replicateM n randCard2

randCard2 :: StateT Int64 IO Int64
randCard2 = do
         done <- get
         rc <- lift randCard
         let rcb = bit rc
         if testBit done rc
                  then randCard2
                  else do put $ setBit done rc; return rcb

x = runStateT randCard2 0

randCard1 :: Int64 -> IO Int64
randCard1 done = do
         c <- randCard
         if testBit done c then randCard1 done else return $ setBit done c

bitsToIl :: Int64 -> [Int]
bitsToIl bits = filter (testBit bits) [1..52]

ilToBits :: [Int] -> Int64
ilToBits lst = foldr (flip setBit) 0 lst

{-
int LookupHand(int* pCards)
{
            int p = HR[53 + *pCards++];
            p = HR[p + *pCards++];
            p = HR[p + *pCards++];
            p = HR[p + *pCards++];
            p = HR[p + *pCards++];
            p = HR[p + *pCards++];
         return HR[p + *pCards++];
}
-}

{-
straight flush
four
full house
flush
straight
three
two pairs
one pair
highest card

HOLE CARDS:

pocket pair
         chance of best hand being that pair

         chance of three of a kind = 2 * comb 49 4 / 2598960 = 0.16
                  beats 85% of 7 card hands

ace-king
         chance of another ace = 3 * comb 49 4 / 2598960 = 0.24
         chance of another king = 0.24

two card same suit
         ->chance of getting a flush = 96/1024 = 1/10

         1 way of getting five of same suit
         1 1 1 1 1

         1/4^5 = 1/1024

         5 ways of getting four of same suit

         1 1 1 1 -
         1 1 1 - 1
         1 1 - 1 1
         1 - 1 1 1
         - 1 1 1 1

         5 * (1/4)^4*(3/4) = 5 * 3/1024

         10 ways of getting three of same suit

         1 1 1 - -
         1 1 - 1 -
         1 1 - - 1
         1 - 1 1 -
         1 - 1 - 1
         1 - - 1 1
         - 1 1 1 -
         - 1 1 - 1
         - 1 - 1 1
         - - 1 1 1

         10 * 9/1024 = 90/1024

         10 ways of getting

         1 1 - - -
         1 - 1 - -
         1 - - 1 -
         1 - - - 1
         - 1 1 - -
         - 1 - 1 -
         - 1 - - 1
         - - 1 1 -
         - - 1 - 1
         - - - 1 1

         10 * 27/1024 = 270/1024

         1 - - - -
         - 1 - - -
         - - 1 - -
         - - - 1 -
         - - - - 1

         5 * 81/1024 = 405/1024

AFTER FLOP:

four cards same suit
         chance of flush = 1 - 37/46 * 36/45 = 36%

odds to 1 = 1/p - 1

kelly betting
f* = (bp - q)/b = p - q/b = p + (p - 1)/b

if b = q/p = (1-p)/p

f* = p - q/(q/p) = p - p = 0

if b = w/l

f* = p - ql/w
-}

winProbTab =
         [
                  [
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.513, 0.000], [0.354, 0.389], [0.363, 0.397], [0.374, 0.408], [0.371, 0.405], [0.374, 0.409], [0.396, 0.429], [0.417, 0.449], [0.440, 0.471], [0.467, 0.495], [0.495, 0.522], [0.526, 0.553], [0.570, 0.592]],
                           [[0.000, 0.000], [0.000, 0.000], [0.354, 0.389], [0.546, 0.000], [0.382, 0.415], [0.394, 0.426], [0.391, 0.424], [0.395, 0.428], [0.403, 0.434], [0.426, 0.457], [0.450, 0.479], [0.476, 0.505], [0.504, 0.530], [0.535, 0.560], [0.579, 0.601]],
                           [[0.000, 0.000], [0.000, 0.000], [0.363, 0.397], [0.382, 0.415], [0.578, 0.000], [0.412, 0.443], [0.410, 0.442], [0.415, 0.447], [0.422, 0.454], [0.433, 0.462], [0.459, 0.488], [0.485, 0.514], [0.513, 0.540], [0.544, 0.569], [0.587, 0.610]],
                           [[0.000, 0.000], [0.000, 0.000], [0.374, 0.408], [0.394, 0.426], [0.412, 0.443], [0.610, 0.000], [0.429, 0.460], [0.433, 0.463], [0.440, 0.472], [0.452, 0.483], [0.466, 0.496], [0.495, 0.522], [0.523, 0.548], [0.555, 0.577], [0.596, 0.618]],
                           [[0.000, 0.000], [0.000, 0.000], [0.371, 0.405], [0.391, 0.424], [0.410, 0.442], [0.429, 0.460], [0.638, 0.000], [0.450, 0.479], [0.457, 0.487], [0.468, 0.496], [0.483, 0.511], [0.500, 0.527], [0.531, 0.556], [0.561, 0.584], [0.595, 0.616]],
                           [[0.000, 0.000], [0.000, 0.000], [0.374, 0.409], [0.395, 0.428], [0.415, 0.447], [0.433, 0.463], [0.450, 0.479], [0.666, 0.000], [0.474, 0.502], [0.485, 0.513], [0.500, 0.526], [0.516, 0.542], [0.537, 0.562], [0.570, 0.592], [0.606, 0.626]],
                           [[0.000, 0.000], [0.000, 0.000], [0.396, 0.429], [0.403, 0.434], [0.422, 0.454], [0.440, 0.472], [0.457, 0.487], [0.474, 0.502], [0.696, 0.000], [0.501, 0.528], [0.517, 0.541], [0.534, 0.556], [0.553, 0.577], [0.576, 0.598], [0.614, 0.634]],
                           [[0.000, 0.000], [0.000, 0.000], [0.417, 0.449], [0.426, 0.457], [0.433, 0.462], [0.452, 0.483], [0.468, 0.496], [0.485, 0.513], [0.501, 0.528], [0.725, 0.000], [0.533, 0.557], [0.549, 0.572], [0.569, 0.591], [0.592, 0.613], [0.620, 0.640]],
                           [[0.000, 0.000], [0.000, 0.000], [0.440, 0.471], [0.450, 0.479], [0.459, 0.488], [0.466, 0.496], [0.483, 0.511], [0.500, 0.526], [0.517, 0.541], [0.533, 0.557], [0.753, 0.000], [0.566, 0.589], [0.586, 0.608], [0.610, 0.630], [0.639, 0.658]],
                           [[0.000, 0.000], [0.000, 0.000], [0.467, 0.495], [0.476, 0.505], [0.485, 0.514], [0.495, 0.522], [0.500, 0.527], [0.516, 0.542], [0.534, 0.556], [0.549, 0.572], [0.566, 0.589], [0.778, 0.000], [0.594, 0.615], [0.617, 0.636], [0.646, 0.663]],
                           [[0.000, 0.000], [0.000, 0.000], [0.495, 0.522], [0.504, 0.530], [0.513, 0.540], [0.523, 0.548], [0.531, 0.556], [0.537, 0.562], [0.553, 0.577], [0.569, 0.591], [0.586, 0.608], [0.594, 0.615], [0.802, 0.000], [0.625, 0.644], [0.654, 0.672]],
                           [[0.000, 0.000], [0.000, 0.000], [0.526, 0.553], [0.535, 0.560], [0.544, 0.569], [0.555, 0.577], [0.561, 0.584], [0.570, 0.592], [0.576, 0.598], [0.592, 0.613], [0.610, 0.630], [0.617, 0.636], [0.625, 0.644], [0.827, 0.000], [0.662, 0.678]],
                           [[0.000, 0.000], [0.000, 0.000], [0.570, 0.592], [0.579, 0.601], [0.587, 0.610], [0.596, 0.618], [0.595, 0.616], [0.606, 0.626], [0.614, 0.634], [0.620, 0.640], [0.639, 0.658], [0.646, 0.663], [0.654, 0.672], [0.662, 0.678], [0.855, 0.000]]
                  ],
                  {- 3 players -}
                  [
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.314, 0.000], [0.217, 0.256], [0.226, 0.264], [0.234, 0.273], [0.227, 0.267], [0.224, 0.265], [0.238, 0.278], [0.251, 0.290], [0.268, 0.306], [0.285, 0.324], [0.307, 0.343], [0.333, 0.370], [0.375, 0.408]],
                           [[0.000, 0.000], [0.000, 0.000], [0.217, 0.256], [0.344, 0.000], [0.245, 0.283], [0.256, 0.292], [0.247, 0.286], [0.245, 0.283], [0.244, 0.283], [0.260, 0.298], [0.276, 0.313], [0.295, 0.332], [0.316, 0.352], [0.343, 0.378], [0.384, 0.417]],
                           [[0.000, 0.000], [0.000, 0.000], [0.226, 0.264], [0.245, 0.283], [0.374, 0.000], [0.274, 0.310], [0.268, 0.304], [0.266, 0.303], [0.264, 0.302], [0.266, 0.303], [0.286, 0.323], [0.303, 0.341], [0.325, 0.361], [0.352, 0.386], [0.395, 0.428]],
                           [[0.000, 0.000], [0.000, 0.000], [0.234, 0.273], [0.256, 0.292], [0.274, 0.310], [0.407, 0.000], [0.287, 0.323], [0.285, 0.320], [0.284, 0.322], [0.287, 0.324], [0.292, 0.329], [0.313, 0.349], [0.335, 0.369], [0.362, 0.395], [0.405, 0.436]],
                           [[0.000, 0.000], [0.000, 0.000], [0.227, 0.267], [0.247, 0.286], [0.268, 0.304], [0.287, 0.323], [0.437, 0.000], [0.304, 0.338], [0.304, 0.340], [0.306, 0.340], [0.311, 0.346], [0.318, 0.354], [0.344, 0.377], [0.370, 0.402], [0.400, 0.432]],
                           [[0.000, 0.000], [0.000, 0.000], [0.224, 0.265], [0.245, 0.283], [0.266, 0.303], [0.285, 0.320], [0.304, 0.338], [0.469, 0.000], [0.324, 0.356], [0.326, 0.360], [0.331, 0.365], [0.338, 0.372], [0.351, 0.384], [0.380, 0.412], [0.412, 0.443]],
                           [[0.000, 0.000], [0.000, 0.000], [0.238, 0.278], [0.244, 0.283], [0.264, 0.302], [0.284, 0.322], [0.304, 0.340], [0.324, 0.356], [0.504, 0.000], [0.345, 0.378], [0.353, 0.385], [0.360, 0.391], [0.371, 0.404], [0.388, 0.418], [0.423, 0.453]],
                           [[0.000, 0.000], [0.000, 0.000], [0.251, 0.290], [0.260, 0.298], [0.266, 0.303], [0.287, 0.324], [0.306, 0.340], [0.326, 0.360], [0.345, 0.378], [0.541, 0.000], [0.374, 0.405], [0.381, 0.411], [0.393, 0.423], [0.409, 0.439], [0.432, 0.462]],
                           [[0.000, 0.000], [0.000, 0.000], [0.268, 0.306], [0.276, 0.313], [0.286, 0.323], [0.292, 0.329], [0.311, 0.346], [0.331, 0.365], [0.353, 0.385], [0.374, 0.405], [0.580, 0.000], [0.405, 0.434], [0.417, 0.446], [0.434, 0.462], [0.458, 0.486]],
                           [[0.000, 0.000], [0.000, 0.000], [0.285, 0.324], [0.295, 0.332], [0.303, 0.341], [0.313, 0.349], [0.318, 0.354], [0.338, 0.372], [0.360, 0.391], [0.381, 0.411], [0.405, 0.434], [0.615, 0.000], [0.427, 0.456], [0.444, 0.471], [0.468, 0.494]],
                           [[0.000, 0.000], [0.000, 0.000], [0.307, 0.343], [0.316, 0.352], [0.325, 0.361], [0.335, 0.369], [0.344, 0.377], [0.351, 0.384], [0.371, 0.404], [0.393, 0.423], [0.417, 0.446], [0.427, 0.456], [0.653, 0.000], [0.457, 0.483], [0.480, 0.506]],
                           [[0.000, 0.000], [0.000, 0.000], [0.333, 0.370], [0.343, 0.378], [0.352, 0.386], [0.362, 0.395], [0.370, 0.402], [0.380, 0.412], [0.388, 0.418], [0.409, 0.439], [0.434, 0.462], [0.444, 0.471], [0.457, 0.483], [0.692, 0.000], [0.493, 0.517]],
                           [[0.000, 0.000], [0.000, 0.000], [0.375, 0.408], [0.384, 0.417], [0.395, 0.428], [0.405, 0.436], [0.400, 0.432], [0.412, 0.443], [0.423, 0.453], [0.432, 0.462], [0.458, 0.486], [0.468, 0.494], [0.480, 0.506], [0.493, 0.517], [0.738, 0.000]]
                  ],
                  {- 4 players -}
                  [
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.225, 0.000], [0.154, 0.195], [0.163, 0.203], [0.170, 0.210], [0.162, 0.203], [0.158, 0.201], [0.168, 0.210], [0.178, 0.218], [0.190, 0.231], [0.203, 0.244], [0.220, 0.260], [0.241, 0.281], [0.276, 0.314]],
                           [[0.000, 0.000], [0.000, 0.000], [0.154, 0.195], [0.247, 0.000], [0.180, 0.219], [0.189, 0.227], [0.181, 0.221], [0.176, 0.216], [0.174, 0.215], [0.185, 0.225], [0.198, 0.237], [0.211, 0.251], [0.227, 0.267], [0.249, 0.287], [0.284, 0.323]],
                           [[0.000, 0.000], [0.000, 0.000], [0.163, 0.203], [0.180, 0.219], [0.268, 0.000], [0.205, 0.243], [0.200, 0.237], [0.195, 0.235], [0.191, 0.232], [0.191, 0.230], [0.206, 0.246], [0.219, 0.259], [0.235, 0.274], [0.257, 0.295], [0.294, 0.332]],
                           [[0.000, 0.000], [0.000, 0.000], [0.170, 0.210], [0.189, 0.227], [0.205, 0.243], [0.295, 0.000], [0.217, 0.254], [0.214, 0.251], [0.211, 0.250], [0.210, 0.249], [0.212, 0.251], [0.227, 0.266], [0.243, 0.282], [0.265, 0.302], [0.302, 0.338]],
                           [[0.000, 0.000], [0.000, 0.000], [0.162, 0.203], [0.181, 0.221], [0.200, 0.237], [0.217, 0.254], [0.320, 0.000], [0.232, 0.267], [0.230, 0.267], [0.228, 0.264], [0.230, 0.267], [0.232, 0.270], [0.251, 0.288], [0.272, 0.309], [0.296, 0.333]],
                           [[0.000, 0.000], [0.000, 0.000], [0.158, 0.201], [0.176, 0.216], [0.195, 0.235], [0.214, 0.251], [0.232, 0.267], [0.348, 0.000], [0.249, 0.282], [0.248, 0.284], [0.250, 0.286], [0.251, 0.288], [0.258, 0.295], [0.282, 0.318], [0.308, 0.344]],
                           [[0.000, 0.000], [0.000, 0.000], [0.168, 0.210], [0.174, 0.215], [0.191, 0.232], [0.211, 0.250], [0.230, 0.267], [0.249, 0.282], [0.381, 0.000], [0.267, 0.301], [0.271, 0.306], [0.273, 0.307], [0.279, 0.315], [0.289, 0.324], [0.318, 0.353]],
                           [[0.000, 0.000], [0.000, 0.000], [0.178, 0.218], [0.185, 0.225], [0.191, 0.230], [0.210, 0.249], [0.228, 0.264], [0.248, 0.284], [0.267, 0.301], [0.416, 0.000], [0.293, 0.326], [0.295, 0.328], [0.301, 0.335], [0.312, 0.345], [0.327, 0.362]],
                           [[0.000, 0.000], [0.000, 0.000], [0.190, 0.231], [0.198, 0.237], [0.206, 0.246], [0.212, 0.251], [0.230, 0.267], [0.250, 0.286], [0.271, 0.306], [0.293, 0.326], [0.456, 0.000], [0.322, 0.354], [0.328, 0.360], [0.340, 0.371], [0.356, 0.388]],
                           [[0.000, 0.000], [0.000, 0.000], [0.203, 0.244], [0.211, 0.251], [0.219, 0.259], [0.227, 0.266], [0.232, 0.270], [0.251, 0.288], [0.273, 0.307], [0.295, 0.328], [0.322, 0.354], [0.495, 0.000], [0.339, 0.371], [0.351, 0.381], [0.368, 0.397]],
                           [[0.000, 0.000], [0.000, 0.000], [0.220, 0.260], [0.227, 0.267], [0.235, 0.274], [0.243, 0.282], [0.251, 0.288], [0.258, 0.295], [0.279, 0.315], [0.301, 0.335], [0.328, 0.360], [0.339, 0.371], [0.540, 0.000], [0.364, 0.394], [0.382, 0.411]],
                           [[0.000, 0.000], [0.000, 0.000], [0.241, 0.281], [0.249, 0.287], [0.257, 0.295], [0.265, 0.302], [0.272, 0.309], [0.282, 0.318], [0.289, 0.324], [0.312, 0.345], [0.340, 0.371], [0.351, 0.381], [0.364, 0.394], [0.586, 0.000], [0.396, 0.425]],
                           [[0.000, 0.000], [0.000, 0.000], [0.276, 0.314], [0.284, 0.323], [0.294, 0.332], [0.302, 0.338], [0.296, 0.333], [0.308, 0.344], [0.318, 0.353], [0.327, 0.362], [0.356, 0.388], [0.368, 0.397], [0.382, 0.411], [0.396, 0.425], [0.642, 0.000]]
                  ],
                  {- 5 players -}
                  [
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.182, 0.000], [0.120, 0.161], [0.128, 0.168], [0.134, 0.175], [0.125, 0.167], [0.121, 0.164], [0.129, 0.171], [0.137, 0.178], [0.148, 0.189], [0.158, 0.200], [0.171, 0.213], [0.188, 0.231], [0.219, 0.259]],
                           [[0.000, 0.000], [0.000, 0.000], [0.120, 0.161], [0.196, 0.000], [0.143, 0.182], [0.151, 0.190], [0.142, 0.183], [0.137, 0.177], [0.134, 0.175], [0.143, 0.184], [0.154, 0.195], [0.164, 0.206], [0.178, 0.218], [0.195, 0.235], [0.227, 0.267]],
                           [[0.000, 0.000], [0.000, 0.000], [0.128, 0.168], [0.143, 0.182], [0.211, 0.000], [0.166, 0.203], [0.159, 0.197], [0.155, 0.194], [0.150, 0.190], [0.148, 0.188], [0.161, 0.201], [0.170, 0.212], [0.183, 0.224], [0.202, 0.242], [0.234, 0.275]],
                           [[0.000, 0.000], [0.000, 0.000], [0.134, 0.175], [0.151, 0.190], [0.166, 0.203], [0.229, 0.000], [0.174, 0.212], [0.171, 0.209], [0.167, 0.206], [0.164, 0.205], [0.166, 0.206], [0.178, 0.218], [0.191, 0.231], [0.208, 0.248], [0.242, 0.280]],
                           [[0.000, 0.000], [0.000, 0.000], [0.125, 0.167], [0.142, 0.183], [0.159, 0.197], [0.174, 0.212], [0.249, 0.000], [0.186, 0.223], [0.184, 0.222], [0.181, 0.218], [0.182, 0.220], [0.182, 0.222], [0.198, 0.236], [0.215, 0.254], [0.234, 0.274]],
                           [[0.000, 0.000], [0.000, 0.000], [0.121, 0.164], [0.137, 0.177], [0.155, 0.194], [0.171, 0.209], [0.186, 0.223], [0.272, 0.000], [0.200, 0.236], [0.199, 0.237], [0.200, 0.238], [0.199, 0.238], [0.204, 0.242], [0.223, 0.262], [0.244, 0.283]],
                           [[0.000, 0.000], [0.000, 0.000], [0.129, 0.171], [0.134, 0.175], [0.150, 0.190], [0.167, 0.206], [0.184, 0.222], [0.200, 0.236], [0.299, 0.000], [0.217, 0.251], [0.220, 0.256], [0.220, 0.255], [0.223, 0.261], [0.230, 0.268], [0.254, 0.292]],
                           [[0.000, 0.000], [0.000, 0.000], [0.137, 0.178], [0.143, 0.184], [0.148, 0.188], [0.164, 0.205], [0.181, 0.218], [0.199, 0.237], [0.217, 0.251], [0.330, 0.000], [0.241, 0.276], [0.241, 0.276], [0.244, 0.279], [0.252, 0.287], [0.262, 0.300]],
                           [[0.000, 0.000], [0.000, 0.000], [0.148, 0.189], [0.154, 0.195], [0.161, 0.201], [0.166, 0.206], [0.182, 0.220], [0.200, 0.238], [0.220, 0.256], [0.241, 0.276], [0.368, 0.000], [0.268, 0.301], [0.272, 0.305], [0.280, 0.313], [0.291, 0.326]],
                           [[0.000, 0.000], [0.000, 0.000], [0.158, 0.200], [0.164, 0.206], [0.170, 0.212], [0.178, 0.218], [0.182, 0.222], [0.199, 0.238], [0.220, 0.255], [0.241, 0.276], [0.268, 0.301], [0.407, 0.000], [0.283, 0.316], [0.291, 0.324], [0.304, 0.336]],
                           [[0.000, 0.000], [0.000, 0.000], [0.171, 0.213], [0.178, 0.218], [0.183, 0.224], [0.191, 0.231], [0.198, 0.236], [0.204, 0.242], [0.223, 0.261], [0.244, 0.279], [0.272, 0.305], [0.283, 0.316], [0.452, 0.000], [0.305, 0.337], [0.318, 0.350]],
                           [[0.000, 0.000], [0.000, 0.000], [0.188, 0.231], [0.195, 0.235], [0.202, 0.242], [0.208, 0.248], [0.215, 0.254], [0.223, 0.262], [0.230, 0.268], [0.252, 0.287], [0.280, 0.313], [0.291, 0.324], [0.305, 0.337], [0.501, 0.000], [0.335, 0.365]],
                           [[0.000, 0.000], [0.000, 0.000], [0.219, 0.259], [0.227, 0.267], [0.234, 0.275], [0.242, 0.280], [0.234, 0.274], [0.244, 0.283], [0.254, 0.292], [0.262, 0.300], [0.291, 0.326], [0.304, 0.336], [0.318, 0.350], [0.335, 0.365], [0.563, 0.000]]
                  ],
                  {- 6 players -}
                  [
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.159, 0.000], [0.100, 0.140], [0.107, 0.146], [0.112, 0.152], [0.103, 0.144], [0.098, 0.140], [0.104, 0.146], [0.111, 0.152], [0.121, 0.162], [0.129, 0.171], [0.140, 0.182], [0.155, 0.198], [0.182, 0.223]],
                           [[0.000, 0.000], [0.000, 0.000], [0.100, 0.140], [0.168, 0.000], [0.120, 0.159], [0.128, 0.166], [0.118, 0.158], [0.112, 0.152], [0.109, 0.150], [0.116, 0.157], [0.126, 0.166], [0.134, 0.176], [0.145, 0.187], [0.160, 0.202], [0.188, 0.230]],
                           [[0.000, 0.000], [0.000, 0.000], [0.107, 0.146], [0.120, 0.159], [0.178, 0.000], [0.140, 0.178], [0.134, 0.171], [0.128, 0.167], [0.122, 0.163], [0.120, 0.160], [0.132, 0.172], [0.139, 0.181], [0.150, 0.192], [0.166, 0.207], [0.195, 0.237]],
                           [[0.000, 0.000], [0.000, 0.000], [0.112, 0.152], [0.128, 0.166], [0.140, 0.178], [0.190, 0.000], [0.147, 0.185], [0.143, 0.181], [0.139, 0.178], [0.135, 0.175], [0.136, 0.176], [0.146, 0.186], [0.157, 0.197], [0.172, 0.213], [0.202, 0.241]],
                           [[0.000, 0.000], [0.000, 0.000], [0.103, 0.144], [0.118, 0.158], [0.134, 0.171], [0.147, 0.185], [0.205, 0.000], [0.157, 0.193], [0.154, 0.191], [0.150, 0.187], [0.150, 0.189], [0.149, 0.189], [0.162, 0.202], [0.177, 0.218], [0.194, 0.235]],
                           [[0.000, 0.000], [0.000, 0.000], [0.098, 0.140], [0.112, 0.152], [0.128, 0.167], [0.143, 0.181], [0.157, 0.193], [0.222, 0.000], [0.168, 0.203], [0.166, 0.204], [0.167, 0.205], [0.165, 0.204], [0.167, 0.207], [0.184, 0.224], [0.202, 0.243]],
                           [[0.000, 0.000], [0.000, 0.000], [0.104, 0.146], [0.109, 0.150], [0.122, 0.163], [0.139, 0.178], [0.154, 0.191], [0.168, 0.203], [0.245, 0.000], [0.182, 0.217], [0.185, 0.221], [0.183, 0.220], [0.185, 0.224], [0.191, 0.229], [0.211, 0.250]],
                           [[0.000, 0.000], [0.000, 0.000], [0.111, 0.152], [0.116, 0.157], [0.120, 0.160], [0.135, 0.175], [0.150, 0.187], [0.166, 0.204], [0.182, 0.217], [0.271, 0.000], [0.205, 0.239], [0.203, 0.239], [0.204, 0.241], [0.211, 0.248], [0.219, 0.258]],
                           [[0.000, 0.000], [0.000, 0.000], [0.121, 0.162], [0.126, 0.166], [0.132, 0.172], [0.136, 0.176], [0.150, 0.189], [0.167, 0.205], [0.185, 0.221], [0.205, 0.239], [0.304, 0.000], [0.230, 0.264], [0.232, 0.266], [0.238, 0.273], [0.247, 0.283]],
                           [[0.000, 0.000], [0.000, 0.000], [0.129, 0.171], [0.134, 0.176], [0.139, 0.181], [0.146, 0.186], [0.149, 0.189], [0.165, 0.204], [0.183, 0.220], [0.203, 0.239], [0.230, 0.264], [0.339, 0.000], [0.242, 0.276], [0.249, 0.283], [0.259, 0.292]],
                           [[0.000, 0.000], [0.000, 0.000], [0.140, 0.182], [0.145, 0.187], [0.150, 0.192], [0.157, 0.197], [0.162, 0.202], [0.167, 0.207], [0.185, 0.224], [0.204, 0.241], [0.232, 0.266], [0.242, 0.276], [0.384, 0.000], [0.263, 0.295], [0.273, 0.306]],
                           [[0.000, 0.000], [0.000, 0.000], [0.155, 0.198], [0.160, 0.202], [0.166, 0.207], [0.172, 0.213], [0.177, 0.218], [0.184, 0.224], [0.191, 0.229], [0.211, 0.248], [0.238, 0.273], [0.249, 0.283], [0.263, 0.295], [0.433, 0.000], [0.290, 0.322]],
                           [[0.000, 0.000], [0.000, 0.000], [0.182, 0.223], [0.188, 0.230], [0.195, 0.237], [0.202, 0.241], [0.194, 0.235], [0.202, 0.243], [0.211, 0.250], [0.219, 0.258], [0.247, 0.283], [0.259, 0.292], [0.273, 0.306], [0.290, 0.322], [0.497, 0.000]]
                  ],
                  {- 7 players -}
                  [
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.145, 0.000], [0.086, 0.126], [0.093, 0.131], [0.098, 0.137], [0.088, 0.128], [0.083, 0.124], [0.088, 0.129], [0.093, 0.134], [0.102, 0.143], [0.109, 0.151], [0.119, 0.161], [0.131, 0.175], [0.156, 0.198]],
                           [[0.000, 0.000], [0.000, 0.000], [0.086, 0.126], [0.151, 0.000], [0.105, 0.143], [0.112, 0.149], [0.102, 0.141], [0.096, 0.135], [0.092, 0.132], [0.098, 0.138], [0.107, 0.147], [0.113, 0.155], [0.123, 0.164], [0.136, 0.178], [0.162, 0.204]],
                           [[0.000, 0.000], [0.000, 0.000], [0.093, 0.131], [0.105, 0.143], [0.157, 0.000], [0.123, 0.160], [0.116, 0.153], [0.111, 0.149], [0.104, 0.144], [0.101, 0.141], [0.112, 0.152], [0.118, 0.159], [0.127, 0.169], [0.141, 0.183], [0.167, 0.210]],
                           [[0.000, 0.000], [0.000, 0.000], [0.098, 0.137], [0.112, 0.149], [0.123, 0.160], [0.165, 0.000], [0.128, 0.166], [0.124, 0.162], [0.119, 0.158], [0.115, 0.155], [0.115, 0.156], [0.123, 0.164], [0.133, 0.174], [0.146, 0.188], [0.174, 0.213]],
                           [[0.000, 0.000], [0.000, 0.000], [0.088, 0.128], [0.102, 0.141], [0.116, 0.153], [0.128, 0.166], [0.177, 0.000], [0.136, 0.172], [0.132, 0.170], [0.128, 0.165], [0.128, 0.167], [0.126, 0.167], [0.137, 0.177], [0.150, 0.191], [0.165, 0.207]],
                           [[0.000, 0.000], [0.000, 0.000], [0.083, 0.124], [0.096, 0.135], [0.111, 0.149], [0.124, 0.162], [0.136, 0.172], [0.190, 0.000], [0.145, 0.180], [0.142, 0.180], [0.143, 0.181], [0.140, 0.179], [0.141, 0.181], [0.156, 0.196], [0.172, 0.213]],
                           [[0.000, 0.000], [0.000, 0.000], [0.088, 0.129], [0.092, 0.132], [0.104, 0.144], [0.119, 0.158], [0.132, 0.170], [0.145, 0.180], [0.207, 0.000], [0.156, 0.192], [0.159, 0.196], [0.157, 0.194], [0.157, 0.196], [0.162, 0.202], [0.180, 0.220]],
                           [[0.000, 0.000], [0.000, 0.000], [0.093, 0.134], [0.098, 0.138], [0.101, 0.141], [0.115, 0.155], [0.128, 0.165], [0.142, 0.180], [0.156, 0.192], [0.229, 0.000], [0.178, 0.213], [0.175, 0.211], [0.175, 0.212], [0.180, 0.218], [0.187, 0.227]],
                           [[0.000, 0.000], [0.000, 0.000], [0.102, 0.143], [0.107, 0.147], [0.112, 0.152], [0.115, 0.156], [0.128, 0.167], [0.143, 0.181], [0.159, 0.196], [0.178, 0.213], [0.257, 0.000], [0.201, 0.236], [0.202, 0.236], [0.207, 0.242], [0.214, 0.251]],
                           [[0.000, 0.000], [0.000, 0.000], [0.109, 0.151], [0.113, 0.155], [0.118, 0.159], [0.123, 0.164], [0.126, 0.167], [0.140, 0.179], [0.157, 0.194], [0.175, 0.211], [0.201, 0.236], [0.289, 0.000], [0.211, 0.245], [0.216, 0.252], [0.224, 0.259]],
                           [[0.000, 0.000], [0.000, 0.000], [0.119, 0.161], [0.123, 0.164], [0.127, 0.169], [0.133, 0.174], [0.137, 0.177], [0.141, 0.181], [0.157, 0.196], [0.175, 0.212], [0.202, 0.236], [0.211, 0.245], [0.330, 0.000], [0.230, 0.263], [0.238, 0.273]],
                           [[0.000, 0.000], [0.000, 0.000], [0.131, 0.175], [0.136, 0.178], [0.141, 0.183], [0.146, 0.188], [0.150, 0.191], [0.156, 0.196], [0.162, 0.202], [0.180, 0.218], [0.207, 0.242], [0.216, 0.252], [0.230, 0.263], [0.377, 0.000], [0.255, 0.288]],
                           [[0.000, 0.000], [0.000, 0.000], [0.156, 0.198], [0.162, 0.204], [0.167, 0.210], [0.174, 0.213], [0.165, 0.207], [0.172, 0.213], [0.180, 0.220], [0.187, 0.227], [0.214, 0.251], [0.224, 0.259], [0.238, 0.273], [0.255, 0.288], [0.440, 0.000]]
                  ],
                  {- 8 players -}
                  [
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.135, 0.000], [0.077, 0.115], [0.083, 0.120], [0.087, 0.125], [0.078, 0.117], [0.073, 0.112], [0.076, 0.116], [0.081, 0.120], [0.088, 0.129], [0.095, 0.136], [0.103, 0.145], [0.114, 0.158], [0.136, 0.179]],
                           [[0.000, 0.000], [0.000, 0.000], [0.077, 0.115], [0.139, 0.000], [0.094, 0.131], [0.101, 0.137], [0.091, 0.129], [0.084, 0.122], [0.080, 0.119], [0.084, 0.124], [0.093, 0.132], [0.098, 0.140], [0.106, 0.148], [0.118, 0.160], [0.141, 0.184]],
                           [[0.000, 0.000], [0.000, 0.000], [0.083, 0.120], [0.094, 0.131], [0.143, 0.000], [0.111, 0.147], [0.104, 0.140], [0.098, 0.135], [0.091, 0.130], [0.087, 0.127], [0.097, 0.137], [0.102, 0.143], [0.110, 0.152], [0.122, 0.164], [0.146, 0.190]],
                           [[0.000, 0.000], [0.000, 0.000], [0.087, 0.125], [0.101, 0.137], [0.111, 0.147], [0.149, 0.000], [0.115, 0.151], [0.111, 0.147], [0.105, 0.143], [0.100, 0.139], [0.100, 0.140], [0.107, 0.147], [0.115, 0.156], [0.126, 0.168], [0.152, 0.192]],
                           [[0.000, 0.000], [0.000, 0.000], [0.078, 0.117], [0.091, 0.129], [0.104, 0.140], [0.115, 0.151], [0.158, 0.000], [0.121, 0.157], [0.117, 0.153], [0.112, 0.149], [0.112, 0.150], [0.109, 0.150], [0.118, 0.159], [0.130, 0.172], [0.144, 0.186]],
                           [[0.000, 0.000], [0.000, 0.000], [0.073, 0.112], [0.084, 0.122], [0.098, 0.135], [0.111, 0.147], [0.121, 0.157], [0.167, 0.000], [0.128, 0.163], [0.125, 0.163], [0.125, 0.163], [0.121, 0.161], [0.122, 0.163], [0.135, 0.176], [0.149, 0.191]],
                           [[0.000, 0.000], [0.000, 0.000], [0.076, 0.116], [0.080, 0.119], [0.091, 0.130], [0.105, 0.143], [0.117, 0.153], [0.128, 0.163], [0.182, 0.000], [0.138, 0.173], [0.140, 0.177], [0.137, 0.174], [0.136, 0.176], [0.141, 0.181], [0.156, 0.197]],
                           [[0.000, 0.000], [0.000, 0.000], [0.081, 0.120], [0.084, 0.124], [0.087, 0.127], [0.100, 0.139], [0.112, 0.149], [0.125, 0.163], [0.138, 0.173], [0.199, 0.000], [0.157, 0.192], [0.154, 0.190], [0.153, 0.190], [0.157, 0.195], [0.162, 0.203]],
                           [[0.000, 0.000], [0.000, 0.000], [0.088, 0.129], [0.093, 0.132], [0.097, 0.137], [0.100, 0.140], [0.112, 0.150], [0.125, 0.163], [0.140, 0.177], [0.157, 0.192], [0.222, 0.000], [0.178, 0.213], [0.178, 0.214], [0.182, 0.218], [0.188, 0.226]],
                           [[0.000, 0.000], [0.000, 0.000], [0.095, 0.136], [0.098, 0.140], [0.102, 0.143], [0.107, 0.147], [0.109, 0.150], [0.121, 0.161], [0.137, 0.174], [0.154, 0.190], [0.178, 0.213], [0.250, 0.000], [0.186, 0.221], [0.191, 0.227], [0.197, 0.233]],
                           [[0.000, 0.000], [0.000, 0.000], [0.103, 0.145], [0.106, 0.148], [0.110, 0.152], [0.115, 0.156], [0.118, 0.159], [0.122, 0.163], [0.136, 0.176], [0.153, 0.190], [0.178, 0.214], [0.186, 0.221], [0.288, 0.000], [0.203, 0.237], [0.210, 0.246]],
                           [[0.000, 0.000], [0.000, 0.000], [0.114, 0.158], [0.118, 0.160], [0.122, 0.164], [0.126, 0.168], [0.130, 0.172], [0.135, 0.176], [0.141, 0.181], [0.157, 0.195], [0.182, 0.218], [0.191, 0.227], [0.203, 0.237], [0.332, 0.000], [0.227, 0.260]],
                           [[0.000, 0.000], [0.000, 0.000], [0.136, 0.179], [0.141, 0.184], [0.146, 0.190], [0.152, 0.192], [0.144, 0.186], [0.149, 0.191], [0.156, 0.197], [0.162, 0.203], [0.188, 0.226], [0.197, 0.233], [0.210, 0.246], [0.227, 0.260], [0.391, 0.000]]
                  ],
                  {- 9 players -}
                  [
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.128, 0.000], [0.069, 0.106], [0.076, 0.111], [0.080, 0.116], [0.070, 0.107], [0.065, 0.103], [0.068, 0.106], [0.071, 0.110], [0.078, 0.118], [0.083, 0.124], [0.091, 0.132], [0.100, 0.144], [0.120, 0.163]],
                           [[0.000, 0.000], [0.000, 0.000], [0.069, 0.106], [0.131, 0.000], [0.086, 0.121], [0.092, 0.128], [0.082, 0.119], [0.075, 0.112], [0.071, 0.109], [0.074, 0.113], [0.082, 0.121], [0.086, 0.128], [0.094, 0.135], [0.104, 0.146], [0.125, 0.168]],
                           [[0.000, 0.000], [0.000, 0.000], [0.076, 0.111], [0.086, 0.121], [0.133, 0.000], [0.102, 0.137], [0.095, 0.130], [0.088, 0.125], [0.081, 0.120], [0.077, 0.116], [0.086, 0.125], [0.090, 0.131], [0.096, 0.138], [0.108, 0.150], [0.129, 0.173]],
                           [[0.000, 0.000], [0.000, 0.000], [0.080, 0.116], [0.092, 0.128], [0.102, 0.137], [0.137, 0.000], [0.105, 0.141], [0.101, 0.136], [0.094, 0.131], [0.089, 0.127], [0.089, 0.128], [0.094, 0.134], [0.101, 0.142], [0.111, 0.153], [0.135, 0.175]],
                           [[0.000, 0.000], [0.000, 0.000], [0.070, 0.107], [0.082, 0.119], [0.095, 0.130], [0.105, 0.141], [0.145, 0.000], [0.110, 0.145], [0.106, 0.141], [0.100, 0.137], [0.099, 0.137], [0.096, 0.136], [0.104, 0.144], [0.115, 0.156], [0.127, 0.169]],
                           [[0.000, 0.000], [0.000, 0.000], [0.065, 0.103], [0.075, 0.112], [0.088, 0.125], [0.101, 0.136], [0.110, 0.145], [0.152, 0.000], [0.116, 0.150], [0.112, 0.149], [0.112, 0.149], [0.107, 0.146], [0.108, 0.148], [0.118, 0.160], [0.131, 0.174]],
                           [[0.000, 0.000], [0.000, 0.000], [0.068, 0.106], [0.071, 0.109], [0.081, 0.120], [0.094, 0.131], [0.106, 0.141], [0.116, 0.150], [0.163, 0.000], [0.124, 0.158], [0.126, 0.162], [0.121, 0.159], [0.120, 0.160], [0.124, 0.164], [0.137, 0.179]],
                           [[0.000, 0.000], [0.000, 0.000], [0.071, 0.110], [0.074, 0.113], [0.077, 0.116], [0.089, 0.127], [0.100, 0.137], [0.112, 0.149], [0.124, 0.158], [0.177, 0.000], [0.141, 0.176], [0.137, 0.173], [0.135, 0.173], [0.138, 0.177], [0.143, 0.184]],
                           [[0.000, 0.000], [0.000, 0.000], [0.078, 0.118], [0.082, 0.121], [0.086, 0.125], [0.089, 0.128], [0.099, 0.137], [0.112, 0.149], [0.126, 0.162], [0.141, 0.176], [0.197, 0.000], [0.160, 0.196], [0.159, 0.195], [0.162, 0.199], [0.167, 0.206]],
                           [[0.000, 0.000], [0.000, 0.000], [0.083, 0.124], [0.086, 0.128], [0.090, 0.131], [0.094, 0.134], [0.096, 0.136], [0.107, 0.146], [0.121, 0.159], [0.137, 0.173], [0.160, 0.196], [0.221, 0.000], [0.167, 0.201], [0.170, 0.207], [0.176, 0.212]],
                           [[0.000, 0.000], [0.000, 0.000], [0.091, 0.132], [0.094, 0.135], [0.096, 0.138], [0.101, 0.142], [0.104, 0.144], [0.108, 0.148], [0.120, 0.160], [0.135, 0.173], [0.159, 0.195], [0.167, 0.201], [0.254, 0.000], [0.181, 0.215], [0.187, 0.223]],
                           [[0.000, 0.000], [0.000, 0.000], [0.100, 0.144], [0.104, 0.146], [0.108, 0.150], [0.111, 0.153], [0.115, 0.156], [0.118, 0.160], [0.124, 0.164], [0.138, 0.177], [0.162, 0.199], [0.170, 0.207], [0.181, 0.215], [0.295, 0.000], [0.203, 0.237]],
                           [[0.000, 0.000], [0.000, 0.000], [0.120, 0.163], [0.125, 0.168], [0.129, 0.173], [0.135, 0.175], [0.127, 0.169], [0.131, 0.174], [0.137, 0.179], [0.143, 0.184], [0.167, 0.206], [0.176, 0.212], [0.187, 0.223], [0.203, 0.237], [0.350, 0.000]]
                  ],
                  {- 10 players -}
                  [
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000], [0.000, 0.000]],
                           [[0.000, 0.000], [0.000, 0.000], [0.122, 0.000], [0.064, 0.099], [0.070, 0.104], [0.074, 0.109], [0.064, 0.100], [0.059, 0.096], [0.061, 0.098], [0.064, 0.101], [0.071, 0.110], [0.075, 0.115], [0.081, 0.122], [0.089, 0.133], [0.107, 0.150]],
                           [[0.000, 0.000], [0.000, 0.000], [0.064, 0.099], [0.124, 0.000], [0.079, 0.114], [0.086, 0.120], [0.076, 0.111], [0.068, 0.105], [0.064, 0.101], [0.067, 0.104], [0.074, 0.112], [0.077, 0.118], [0.083, 0.124], [0.092, 0.134], [0.112, 0.155]],
                           [[0.000, 0.000], [0.000, 0.000], [0.070, 0.104], [0.079, 0.114], [0.126, 0.000], [0.095, 0.128], [0.088, 0.122], [0.081, 0.117], [0.073, 0.111], [0.069, 0.107], [0.077, 0.115], [0.081, 0.121], [0.086, 0.127], [0.096, 0.138], [0.115, 0.160]],
                           [[0.000, 0.000], [0.000, 0.000], [0.074, 0.109], [0.086, 0.120], [0.095, 0.128], [0.128, 0.000], [0.097, 0.132], [0.093, 0.127], [0.086, 0.122], [0.080, 0.118], [0.081, 0.119], [0.084, 0.124], [0.090, 0.131], [0.099, 0.141], [0.121, 0.161]],
                           [[0.000, 0.000], [0.000, 0.000], [0.064, 0.100], [0.076, 0.111], [0.088, 0.122], [0.097, 0.132], [0.135, 0.000], [0.101, 0.135], [0.097, 0.132], [0.091, 0.127], [0.090, 0.127], [0.086, 0.126], [0.093, 0.133], [0.102, 0.144], [0.113, 0.156]],
                           [[0.000, 0.000], [0.000, 0.000], [0.059, 0.096], [0.068, 0.105], [0.081, 0.117], [0.093, 0.127], [0.101, 0.135], [0.140, 0.000], [0.106, 0.140], [0.102, 0.139], [0.102, 0.138], [0.096, 0.135], [0.096, 0.136], [0.105, 0.147], [0.117, 0.160]],
                           [[0.000, 0.000], [0.000, 0.000], [0.061, 0.098], [0.064, 0.101], [0.073, 0.111], [0.086, 0.122], [0.097, 0.132], [0.106, 0.140], [0.149, 0.000], [0.112, 0.147], [0.115, 0.151], [0.109, 0.146], [0.107, 0.147], [0.110, 0.151], [0.122, 0.164]],
                           [[0.000, 0.000], [0.000, 0.000], [0.064, 0.101], [0.067, 0.104], [0.069, 0.107], [0.080, 0.118], [0.091, 0.127], [0.102, 0.139], [0.112, 0.147], [0.161, 0.000], [0.129, 0.163], [0.124, 0.160], [0.121, 0.159], [0.123, 0.162], [0.127, 0.168]],
                           [[0.000, 0.000], [0.000, 0.000], [0.071, 0.110], [0.074, 0.112], [0.077, 0.115], [0.081, 0.119], [0.090, 0.127], [0.102, 0.138], [0.115, 0.151], [0.129, 0.163], [0.177, 0.000], [0.146, 0.181], [0.144, 0.181], [0.146, 0.183], [0.150, 0.189]],
                           [[0.000, 0.000], [0.000, 0.000], [0.075, 0.115], [0.077, 0.118], [0.081, 0.121], [0.084, 0.124], [0.086, 0.126], [0.096, 0.135], [0.109, 0.146], [0.124, 0.160], [0.146, 0.181], [0.197, 0.000], [0.150, 0.186], [0.153, 0.190], [0.157, 0.194]],
                           [[0.000, 0.000], [0.000, 0.000], [0.081, 0.122], [0.083, 0.124], [0.086, 0.127], [0.090, 0.131], [0.093, 0.133], [0.096, 0.136], [0.107, 0.147], [0.121, 0.159], [0.144, 0.181], [0.150, 0.186], [0.227, 0.000], [0.163, 0.197], [0.168, 0.205]],
                           [[0.000, 0.000], [0.000, 0.000], [0.089, 0.133], [0.092, 0.134], [0.096, 0.138], [0.099, 0.141], [0.102, 0.144], [0.105, 0.147], [0.110, 0.151], [0.123, 0.162], [0.146, 0.183], [0.153, 0.190], [0.163, 0.197], [0.264, 0.000], [0.183, 0.217]],
                           [[0.000, 0.000], [0.000, 0.000], [0.107, 0.150], [0.112, 0.155], [0.115, 0.160], [0.121, 0.161], [0.113, 0.156], [0.117, 0.160], [0.122, 0.164], [0.127, 0.168], [0.150, 0.189], [0.157, 0.194], [0.168, 0.205], [0.183, 0.217], [0.315, 0.000]]
                  ]
         ]
-}
