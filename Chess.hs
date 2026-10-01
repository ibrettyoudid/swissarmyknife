{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use list comprehension" #-}
{- HLINT ignore "Use infix" -}
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use second" -}
module Chess where

import Favs
import Matrix2 hiding (one)

import Prelude hiding (lines)
import Data.List hiding (lines)
import Data.Char
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S

import Data.IORef
import Control.Monad.State

{-
simplest chess strategy: resign on first move

there is a tree of moves
   which is made up of sequences of moves, and where the initial subsequence is the same, they are on the same branch
   if you come up with a sequence of moves somehow, and every sequence your opponent can come up with performs worse, that is an advantage
      but that's not all. he doesn't know for sure what moves you will play until you play them
      if you come up with a partial tree of your moves, and every response tree of his is worse (and each response tree must only branch after your corresponding move)
      ie. he can guess what you'll do, but you might have different branches depending on what he does
      you only need an answer to every move up to that point, there's nothing other than check that forces you to make a particular move
      you *can* just choose a random answer to every move up to that point
      and then improve the answers bit by bit
      you only *need* one answer to each move he can make, but you might want to explore many
      you only *need* to predict the move he will actually make, but if you accept this tenet, you can get in trouble
      if you know you can increase your advantage at every point, that's all you need to win
         you choose your best final position at each of your nodes
      alpha-beta pruning:
      Whenever the maximum score that the minimizing player (i.e. the "beta" player) is assured of becomes less than the minimum score that the maximizing player (i.e., the "alpha" player) is assured of (i.e. beta < alpha), the maximizing player need not consider further descendants of this node, as they will never be reached in the actual play.
      an assured score is yo

take his most valuable piece with your least valuable one (or put your valuable pieces in least jeopardy)

chess heuristics:
aim for checkmate
more advantage in less or same time is better (probably in life too)
   unless somehow having advantage earlier means less later (choose your definition of advantage to rule that out)
you can also checkmate pieces other than the king
if he has definite checkmate on your rook in 5 half-moves
   but you have definite checkmate on a more valuable piece in the same or less time, or the same or more valueable piece in less time
   that is an advantage

life heuristics:
there is no checkmate
if you don't believe you can do something, you're unlikely to try
if you don't believe you can do something, then you probably won't be able to do it?
believing in yourself is necessary but not sufficient
-}

data S = S { z::Int, p :: S, q :: S } | O


{-
grid con off = gr
where
   gr     = crossWith gf [0..7] [0..7]
   gf y x = 
-}
data PC = PC { colour :: Colour, piece :: Piece } deriving (Eq)

instance Show PC where
   show (PC col piece) = show col ++ show piece ++ show Black

data Piece = Pawn | Knight | Bishop | Castle | Queen | King | Empty deriving (Eq)

instance Show Piece where
   show Empty  = " "
   show Pawn   = "p"
   show Knight = "N"
   show Bishop = "B"
   show Castle = "C"
   show Queen  = "Q"
   show King   = "K"

showl :: [[PC]] -> String
showl list = unlines $ reverse $ map ((++ "\27[40;37m") . concatMap show) list
showim im = showl $ lofim im

lofim im = groupN 8 $ map snd $ IM.toList im
imofl l = IM.fromList $ zip [0..63] $ concat l

putl l = putStr $ showl l
putim im = putStr $ showim im

data Colour = White | Black | EmptyC | Red | Green | Yellow | Blue | Magenta | Cyan deriving (Eq, Ord)

data FGBG = FGBG Colour Colour

opponent White = Black
opponent Black = White

instance Show Colour where
   show Black  = "\27[40;37m"
   show White  = "\27[47;30m"
   show EmptyC = "\27[40;37m"
   show Red    = "\27[41;37m"
   show Blue   = "\27[44;37m"
   show Green  = "\27[42;30m"

instance Show FGBG where
   show (FGBG f b) = "\27["++show (30 + coln f)++";"++show (40 + coln b)++"m"

coln Black   = 0
coln Red     = 1
coln Green   = 2
coln Yellow  = 3
coln Blue    = 4
coln Magenta = 5
coln Cyan    = 6
coln White   = 7
coln EmptyC  = 9

showfg c = let n = coln c in "\27["++show (30+n)++"m"
showbg c = let n = coln c in "\27["++show (40+n)++"m"
showfb fc bc = let
   f = coln fc
   b = coln bc
   in "\27["++show (30+f)++";"++show (40+b)++"m"
--07547396336

empty = PC EmptyC Empty

backrow c = map (PC c) [Castle, Knight, Bishop, Queen, King, Bishop, Knight, Castle]

r8 = replicate 8

pawnrow c = r8 $ PC c Pawn

startboardl = [
   backrow White,
   pawnrow White,
   r8 empty,
   r8 empty,
   r8 empty,
   r8 empty,
   pawnrow Black,
   backrow Black]

startboardim = imofl startboardl

type Vec = [Int]

castleV = [[ 1, 0],[-1, 0],[ 0, 1],[ 0,-1]]
bishopV = [[ 1, 1],[-1, 1],[ 1,-1],[-1,-1]]
queenV  = castleV ++ bishopV
knightV = [[ 2, 1],[-2, 1],[ 2,-1],[-2,-1],[ 1, 2],[-1, 2],[ 1,-2],[-1,-2]]

data Sq = Sq { pos :: Vec, n :: Int, step1 :: M.Map Vec Sq, steps :: M.Map Vec [Sq], knight :: [[Sq]], pawn :: M.Map Colour [(Bool, Bool, [Sq])] } | Off

instance Eq Sq where
   a@Sq{} == b@Sq{} = n a == n b
   Sq{}   == Off    = False
   Off    == Sq{}   = False
   Off    == Off    = True

instance Show Sq where
   show sq = case pos sq of
      [x, y] -> [chr (97 + x), chr (49 + y)] ++ show EmptyC

instance Ord Sq where
   compare a b = compare (n a) (n b)

vec x y = [x, y]

num x y = y * 8 + x

masterboard1 = mapfxx n $ concat $ crossWith createSquare [0..7] [0..7]

masterboard = IM.fromList masterboard1

masterboardl = map snd masterboard1

onboard [x, y] = x >= 0 && x <= 7 && y >= 0 && y <= 7

at1 sqMap [x, y] = if onboard [x, y] then sqMap IM.! num x y else Off

createSquare y x = let
   pos1 = vec x y
   in Sq {
      pos    = pos1,
      n      = num x y,
      step1  = M.fromList $ mapxfx (at1 masterboard . (pos1 <+>)) queenV,
      steps  = M.fromList $ mapxfx (\v -> takeWhile (/= Off) $ map (at1 masterboard) $ iterate (v <+>) pos1) queenV,
      knight = map singleton $ filter (/= Off) $ map (at1 masterboard . (pos1 <+>)) knightV,
      pawn   = M.fromList $ map (\(f, mkv) -> (f, 
                     map (\(m, k, v) -> (m, k, 
                     map (at1 masterboard) $ filter onboard $ map (pos1 <+>) v)) mkv))
               [(White, [(False, True , [[-1,  1]]),
                         (False, True , [[ 1,  1]]), 
                         (True , False, [[ 0,  1]] ++ if y == 1 then [[ 0,  2]] else [])]),
                (Black, [(False, True , [[-1, -1]]),
                         (False, True , [[ 1, -1]]),
                         (True , False, [[ 0, -1]] ++ if y == 6 then [[ 0, -2]] else [])])]

   }


pcpiece = piece . pc

pccolour = colour . pc

at pcMap square = pcMap IM.! n square

check sqpcMap sqpc = at sqpcMap (sq sqpc) == sqpc

data SqPC = SqPC { sq :: Sq, pc :: PC } deriving (Eq)

instance Show SqPC where
   show (SqPC sq pc) = show pc ++ show sq

showSqPC1 sq pc = showfb (colour pc) (if mod (sum $ pos sq) 2 == 0 then Red else Blue) ++ show (piece pc)
showSqPC sqpc = showSqPC1 (sq sqpc) (pc sqpc)

showBoard2 sqMap pcMap = let
   sqPCMap = IM.intersectionWith SqPC sqMap pcMap

   in [header] ++ zipWith (\y l -> [show y] ++ l ++ [show y]) [1..8] (groupN 8 $ map (showSqPC . snd) $ IM.toList sqPCMap) ++ [header]

showBoard1 pcMap = showBoard2 masterboard pcMap

showBoard pcMap = unlines $ map concat $ reverse $ showBoard1 pcMap

rotate180 xss = reverse $ map reverse xss

header = map singleton " abcdefgh "

type PCMap = IM.IntMap PC

type SqMap = IM.IntMap Sq

type SqPCMap = IM.IntMap SqPC

data Action = Start | MoveA | Block | Kill

data Line = Line { canmove :: Bool, cankill :: Bool, from :: Sq, moves :: [Move], last1 :: Sq, len :: Int, pc1 :: PC  }

data Move = Move { to :: Sq, board :: Board }

data Board = Board { pcMap :: PCMap, lines :: [Line], linesThru :: M.Map Sq [Line] }

instance Eq Line where
   a == b = from a == from b && last1 a == last1 b

moveTo sq = Move { to = sq }

linesSquare pcMap fromsquare = let
   from1 = at pcMap fromsquare
   fromcol = colour from1
   steps1 = steps fromsquare
   stepsl = map snd $ M.toList steps1
   in case piece from1 of
      Empty -> []      
      Pawn  -> map (\(m, k, s) -> 
            Line { 
               canmove = m, 
               cankill = k, 
               from = fromsquare,
               moves = map moveTo s}) $ 
                  pawn fromsquare M.! colour from1
      _     -> map (\x -> 
            Line { 
               canmove = True, 
               cankill = True, 
               from = fromsquare,
               moves = map moveTo x}) $
         case piece from1 of
            Queen  -> stepsl
            King   -> map (take 1) stepsl
            Castle -> map (steps1 M.!) castleV
            Bishop -> map (steps1 M.!) bishopV
            Knight -> knight fromsquare

updateLine pcMap line1 = let
   colours  = map (colour . at pcMap . to) $ moves line1
   mycol    = colour $ pc1 line1
   oppcol   = opponent mycol
   kills    = (+1) $ fromMaybe (length colours) $ elemIndex oppcol colours
   blocks   =        fromMaybe (length colours) $ elemIndex  mycol colours
   in line1 { len = min kills blocks }

linesSquare2 pcMap fromsquare = map (updateLine pcMap) $ linesSquare pcMap fromsquare

linesSquares pcMap = concatMap (linesSquaresAux pcMap)

linesSquaresAux pcMap (n, pc) = linesSquare2 pcMap $ masterboard IM.! n


linesBoard pcMap = linesSquares pcMap $ IM.toList pcMap

squares line = from line:map to (moves line)

extendMove colour1 oldboard line move = let
   from1    = from line
   to1      = to move
   oldmap   = pcMap oldboard
   squares1 = squares line
   piece    = at oldmap from1
   newmap   = IM.insert (n to1) piece $ IM.delete (n from1) oldmap
   oldthru  = linesThru oldboard
   deletedlines = oldthru M.! from1 ++ oldthru M.! to1
   newlines1    = map (updateLine newmap) (filter ((\x -> x /= from1 && x /= to1) . head . squares) deletedlines) ++ linesSquare newmap to1
   newlines2    = (lines oldboard \\ deletedlines) ++ newlines1
   newthru      = M.union (getLinesThru newlines1) $ M.difference oldthru $ getLinesThru deletedlines
   --unblocked  = filter (\m -> squares1 !! head (blocks m) == from1) $ lines board
   --newblocked = map (\l -> if elem to1 $ squares l then ) $ lines board
   in if colour piece == colour1
         then Board newmap newlines2 newthru
         else oldboard

-- chess is about the interference of possible moves
-- and about the interference of those interferences
-- and so on

getLinesThru lines = multimap $ concatMap (\l -> map (,l) $ squares l) lines

extendBoard colour oldboard = mapBoard (extendMove colour) oldboard

extendTree 0     _      oldboard = oldboard
extendTree depth colour oldboard = 
   mapBoard (\b l m -> extendTree (depth - 1) (opponent colour) $ extendMove colour b l m) oldboard
      
mapBoard f board1 = 
   board1 { lines = map (\line -> 
      line { moves = map (\move -> 
         move { board = f board1 line move }) $ moves line}) $ lines board1 }
      

--interfere linea lineb = let

{-
allowMove move col = 
if | col == EmptyC               -> canmove move
   | col == pccolour (from move) -> False
   | otherwise                   -> cankill move

promote move = let
t = to move
s = sq t
in map (\to1 -> move { to = to1 }) $ case pccolour t of
      White -> if pos s !! 1 == 7
      then [SqPC s $ PC White Queen, SqPC s $ PC White Knight]
      else [t]
      Black -> if pos s !! 1 == 0
      then [SqPC s $ PC Black Queen, SqPC s $ PC Black Knight]
      else [t]
-}
{-
CnbKQbnC
pppppppp


b  pp  b

ppp  ppp
Cn KQ nC
--------
CnbKQbnC
pppppppp

b      b
p      p
CC   
pppppp 
n KQ n 
--------
CnbKQbnC
pppppppp

        
        
        
pppppppp
CnbKQbnC
-}
{-
moves4 pcMap = concatMap (movespSquare pcMap) masterboardl

movesThru4 pcMap = multimap $ concatMap (\m@(m1, k, s) -> map (,m) s) $ concatMap (movespSquare pcMap) masterboardl

moves5 pcMap = map (\(canmove, cankill, sqs@(from:_)) -> let 
            fromcol = colour $ at pcMap from
            in spanAllow (colour . at pcMap) canmove cankill fromcol sqs) $ moves4 pcMap

movesThru5 pcMap = multimap $ concatMap (\m@(s1, s2) -> map (,m) $ s1 ++ s2) $ moves5 pcMap

moves6 pcMap = map (createLine pcMap) $ moves4 pcMap
-}
--movesOK6 pcMap = multimap $ concatMap (\m -> map (,m) $ tail $ ok m) pcMap

--movesBlocked6 pcMap = multimap $ map (\m -> (head $ blocked m, m)) pcMap
{-
movessq1a pcMap tosquare = let
steps1 = steps tosquare
stepsl = map snd $ N.toList steps1
M.map (elemIndex tosquare) stepsl
-}
{-}
spanAllow :: (Sq -> Colour) -> Bool -> Bool -> Colour -> [Sq] -> ([Sq], [Sq])
spanAllow f canmove cankill fromcol (x:xs) = let (b, a) = loop xs in (x:b, a)
   where 
      loop [    ] = ([], [])
      loop (x:xs)  
         | f x == EmptyC  = if canmove then recurse else fail
         | f x == fromcol = fail
         | otherwise      = if cankill then kill else fail
         where
            recurse = let (b, a) = loop xs in (x:b, a)
            fail    = ([], x:xs)
            kill    = ([x], xs)
-}
{-
lengthAllow move1 = loop actions1 0
where 
   canmove1 = canmove move1
   cankill1 = cankill move1
   actions1 = actions move1
   loop [    ] n = [n]
   loop (x:xs) n = case x of 
      Start -> loop xs 1
      MoveA -> if canmove1 then move else block
      Block -> block
      Kill  -> if cankill1 then kill else block
      where
      move  = loop xs (n+1)
      block = n : move
      kill  = n + 1 : move
-}
{-}
lengthAllow move1 = loop actions1 [0..7]
   where 
      canmove1 = canmove move1
      cankill1 = cankill move1
      actions1 = actions move1
      squares1 = squares move1
      loop [    ] _      = []
      loop _      [    ] = []
      loop (a:as) (s:t:ss) = case a of 
            Start -> loop as (s:ss)
            MoveA -> if canmove1 then move else block
            Block -> block
            Kill  -> if cankill1 then kill else block
         where
            move  = loop as ss
            block = (s, t) : move
            kill  = (t, t) : move

      -- s = where you can get to
      -- t = the square that stops you

-}
{-
one True  = 1
one False = 0

countBlocks pcMap line1 = let
   squares1 = squares line1
   from1    = head squares1
   colours  = map (colour . at pcMap) squares1
   mycol    = head colours
   oppcol   = opponent mycol
   kills    = map (one . (== oppcol)) colours
   blocks   = map (one . (== mycol )) colours
   sum1     = scanl1 (+) $ zipWith (+) kills (tail blocks)
   in line1 { moves = multimap $ zip sum1 $ zipWith3 (Move from1) squares1 kills blocks }
-}
{-
change pcMap from to chline 
   | aux chline = head $ filter aux $ linesSquare pcMap $ head $ squares chline
   | otherwise  = chline
      where
         aux chline = elem from squares1 || elem to squares1
            where squares1 = squares chline

-}
{-
movessq2 :: IM.IntMap SqPC -> Sq -> [[Sq]]
movessq2 sqpcMap fromsquare = let
   from1  = at sqpcMap fromsquare
   steps1 = steps fromsquare
   stepsl = map snd $ M.toList steps1

   in case pcpiece from1 of
      Pawn   -> map (\(canmove, cankill, sqs) -> sqs) $ pawn fromsquare M.! pccolour from1
      Empty  -> []
      Queen  -> stepsl
      King   -> map (singleton . head) stepsl
      Castle -> map (steps1 M.!) castleV
      Bishop -> map (steps1 M.!) bishopV
      Knight -> knight fromsquare

moves2 sqpcMap = concatMap (\x -> map (x :) $ movessq2 sqpcMap x) masterboardl

moves3 sqpcMap = map (\sqs@(from:_) -> let 
            fromcol = pccolour $ at sqpcMap from
            in spanAllow (pccolour . at sqpcMap) True True fromcol sqs) $ moves2 sqpcMap
-}
{-
getMovesOK pcMap movesb = do
concat <$> mapM (\ref -> do
   moveb <- readIORef ref
   let squares1 = squares moveb
   let from1    = head squares1
   let ton      = head $ blocks moveb
   return $ map (Move from1) $ take ton squares1) movesb

getMoves pcMap line = let
   squares1 = squares line
   from1    = head squares1
   ton      = head $ blocks line
   f        = map (Move from1)
   (b, a)   = splitAt ton squares1
   in (f b, f a)
-}
{-
checkLineBlocked1 pcMap line blocksquare = let
   squares1   = squares line
   fromsquare = head squares1
   in case elemIndex blocksquare squares1 of
         Just i  -> let
            fromcol  = colour $ at pcMap fromsquare
            blockcol = colour $ at pcMap blocksquare
            ton | blockcol == fromcol = i - 1
                | blockcol == EmptyC  = length squares1
                | otherwise           = i
            in 0
         Nothing -> 0

checkLineBlocked pcMap line blocksquare = let
   squares1   = squares line
   fromsquare = head squares1
   in if elem blocksquare squares1
         then Just $ linesSquare pcMap fromsquare
         else Nothing

-}