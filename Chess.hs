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

import Data.IORef
import Control.Monad.State
{-
class Square s where
   x :: s -> Int
   y :: s -> Int
   n :: s -> s
   s :: s -> s
   w :: s -> s
   e :: s -> s
-}

data S = S { z::Int, p :: S, q :: S } | O

line con off = li
   where
      li    = map sf [0..7]
      sf  z = con z (sf1 (z-1)) (sf1 (z+1))
      sf1 z | z >= 0 && z <= 7 = li !! z
            | otherwise = off

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

opponent White = Black
opponent Black = White

instance Show Colour where
   show Black  = "\27[40;37m"
   show White  = "\27[47;30m"
   show EmptyC = "\27[40;37m"
   show Red    = "\27[41;37m"
   show Blue   = "\27[44;37m"
   show Green  = "\27[42;30m"

coln = flip elemIndex [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White]
showfg c = let Just n = coln c in "\27["++show (30+n)++"m"
showbg c = let Just n = coln c in "\27["++show (40+n)++"m"
showfb fc bc = let
   Just f = coln fc
   Just b = coln bc
   in "\27["++show (30+f)++";"++show (40+b)++"m"
--07547396336

startboardl = [map (PC White) [Castle, Knight, Bishop, Queen, King, Bishop, Knight, Castle],
               replicate 8 $ PC White Pawn,
               replicate 8 $ PC EmptyC Empty,
               replicate 8 $ PC EmptyC Empty,
               replicate 8 $ PC EmptyC Empty,
               replicate 8 $ PC EmptyC Empty,
               replicate 8 $ PC Black Pawn,
               map (PC Black) [Castle, Knight, Bishop, Queen, King, Bishop, Knight, Castle]]

startboardim = imofl startboardl

type Vec = [Int]

castleV   = [[ 1, 0],[-1, 0],[ 0, 1],[ 0,-1]]
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

at1 boardMap [x, y] = if onboard [x, y] then boardMap IM.! num x y else Off

createSquare y x = let
   pos1 = vec x y
   in Sq {
      pos = pos1,
      n = y * 8 + x,
      step1 = M.fromList $ mapxfx (at1 masterboard . (pos1 <+>)) queenV,
      steps = M.fromList $ mapxfx (\v -> takeWhile (/= Off) $ map (at1 masterboard) $ iterate (v <+>) pos1) queenV,
      knight = map singleton $ filter (/= Off) $ map (at1 masterboard . (pos1 <+>)) knightV,
      pawn = M.fromList $ map (\(f, mkv) -> (f, 
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

at boardMap square = boardMap IM.! n square

check boardMap sqpc = at boardMap (sq sqpc) == sqpc

data SqPC = SqPC { sq :: Sq, pc :: PC } deriving (Eq)

instance Show SqPC where
   show (SqPC sq pc) = show pc ++ show sq

type BoardMap = IM.IntMap PC

data Action = Start | MoveA | Block | Kill

type LineP = (Bool, Bool, [Sq])

data Line = Line { canmove :: Bool, cankill :: Bool, squares :: [Sq], actions :: [Action], moves :: M.Map Int [Move] }

data Move = Move { from :: Sq, to :: Sq, kills :: Int, blocks :: Int }

data Board = Board { boardMap :: BoardMap, lines :: [Line] }

movessq2 :: IM.IntMap SqPC -> Sq -> [[Sq]]
movessq2 boardMap fromsquare = let
   from1  = at boardMap fromsquare
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

moves2 boardMap = concatMap (\x -> map (x :) $ movessq2 boardMap x) masterboardl

moves3 boardMap = map (\sqs@(from:_) -> let 
                     fromcol = pccolour $ at boardMap from
                     in spanAllow (pccolour . at boardMap) True True fromcol sqs) $ moves2 boardMap

linePSquare :: BoardMap -> Sq -> [LineP]
linePSquare boardMap fromsquare = let
   from1 = at boardMap fromsquare
   fromcol = colour from1
   steps1 = steps fromsquare
   stepsl = map snd $ M.toList steps1
   in case piece from1 of
      Empty -> []      
      Pawn  -> map (\(m, k, s) -> (m, k, fromsquare:s)) $ pawn fromsquare M.! colour from1
      _     -> map (\x -> (True, True, fromsquare:x)) $
         case piece from1 of
            Queen  -> stepsl
            King   -> map (take 1) stepsl
            Castle -> map (steps1 M.!) castleV
            Bishop -> map (steps1 M.!) bishopV
            Knight -> knight fromsquare
{-
movessq1a boardMap tosquare = let
   steps1 = steps tosquare
   stepsl = map snd $ N.toList steps1
   M.map (elemIndex tosquare) stepsl
-}
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

one True  = 1
one False = 0

countBlocks boardMap line1 = let
   squares1 = squares line1
   from1    = head squares1
   mycol    = head colours
   oppcol   = opponent mycol
   colours  = map (colour . at boardMap) squares1
   kills    = map (one . (== oppcol)) colours
   blocks   = map (one . (== mycol )) colours
   sum1     = zipWith (+) kills (tail blocks)
   in multimap $ zip sum1 $ zipWith3 (Move from1) squares1 kills blocks

createLine :: BoardMap -> LineP -> Line
createLine boardMap m@(canmove1, cankill1, x:xs) = let 
   fromcol = colour $ at boardMap x
   line1 = Line {
      canmove = canmove1,
      cankill = cankill1,
      squares = x:xs,
      actions = [],
      moves = M.empty }
   in line1 { moves = countBlocks boardMap line1 }

linePSquares boardMap = concatMap (linePSquaresAux boardMap)

linePSquaresAux boardMap (n, pc) = linePSquare boardMap $ masterboard IM.! n


lineSquares boardMap = map (createLine boardMap) . linePSquares boardMap


lineBoard boardMap = lineSquares boardMap $ IM.toList boardMap


lineSquare boardMap = map (createLine boardMap) . linePSquare boardMap
{-
getMovesOK boardMap movesb = do
   concat <$> mapM (\ref -> do
      moveb <- readIORef ref
      let squares1 = squares moveb
      let from1    = head squares1
      let ton      = head $ blocks moveb
      return $ map (Move from1) $ take ton squares1) movesb

getMoves boardMap line = let
      squares1 = squares line
      from1    = head squares1
      ton      = head $ blocks line
      f        = map (Move from1)
      (b, a)   = splitAt ton squares1
      in (f b, f a)
-}

checkLineBlocked1 boardMap line blocksquare = let
   squares1   = squares line
   fromsquare = head squares1
   in case elemIndex blocksquare squares1 of
         Just i  -> let
            fromcol  = colour $ at boardMap fromsquare
            blockcol = colour $ at boardMap blocksquare
            ton | blockcol == fromcol = i - 1
                | blockcol == EmptyC  = length squares1
                | otherwise           = i
            in 0
         Nothing -> 0

checkLineBlocked boardMap line blocksquare = let
   squares1   = squares line
   fromsquare = head squares1
   in if elem blocksquare squares1
         then Just $ lineSquare boardMap fromsquare
         else Nothing


doMove board line move = let
   squares1   = squares line
   from1      = from move
   to1        = to move
   piece      = at (boardMap board) $ from move
   newboard   = IM.insert (n to1) piece $ IM.delete (n from1) $ boardMap board
   --unblocked  = filter (\m -> squares1 !! head (blocks m) == from1) $ lines board
   --newblocked = map (\l -> if elem to1 $ squares l then ) $ lines board
   in Board newboard (lines board) 

-- chess is about the interference of possible moves
-- and about the interference of those interferences
-- and so on

change boardMap from to chline 
   | aux chline = head $ filter aux $ lineSquare boardMap $ head $ squares chline
   | otherwise  = chline
      where
         aux chline = elem from squares1 || elem to squares1
            where squares1 = squares chline

linesThru lines = M.fromList $ concatMap (\l -> map (,l) $ squares l) lines


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
moves4 boardMap = concatMap (movespSquare boardMap) masterboardl

movesThru4 boardMap = multimap $ concatMap (\m@(m1, k, s) -> map (,m) s) $ concatMap (movespSquare boardMap) masterboardl

moves5 boardMap = map (\(canmove, cankill, sqs@(from:_)) -> let 
                     fromcol = colour $ at boardMap from
                     in spanAllow (colour . at boardMap) canmove cankill fromcol sqs) $ moves4 boardMap

movesThru5 boardMap = multimap $ concatMap (\m@(s1, s2) -> map (,m) $ s1 ++ s2) $ moves5 boardMap

moves6 boardMap = map (createLine boardMap) $ moves4 boardMap
-}
--movesOK6 boardMap = multimap $ concatMap (\m -> map (,m) $ tail $ ok m) boardMap

--movesBlocked6 boardMap = multimap $ map (\m -> (head $ blocked m, m)) boardMap
