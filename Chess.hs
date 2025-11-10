{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use list comprehension" #-}
module Chess where

import Favs
import Matrix2

import Data.List
import Data.Char
import qualified Data.Map as M
import qualified Data.IntMap as IM
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
   show (PC col piece) = show col ++ show piece

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

data Colour = White | Black | EmptyC deriving (Eq)

instance Show Colour where
   show Black = "\27[40;37m"
   show White = "\27[47;30m"
   show EmptyC  = "\27[40;37m"

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

data Sq = Sq { pos :: Vec, n :: Int, step1 :: M.Map Vec Sq, steps :: M.Map Vec [Sq], knight :: [[Sq]], pawn :: [(Colour, Colour, [Sq])] } | Off

instance Eq Sq where
   a@Sq{} == b@Sq{} = pos a == pos b
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

at1 board [x, y] = if x >= 0 && x <= 7 && y >= 0 && y <= 7 then board IM.! num x y else Off

createSquare y x = let
   pos1 = vec x y
   in Sq {
      pos = pos1,
      n = y * 8 + x,
      step1 = M.fromList $ mapxfx (at1 masterboard . (pos1 <+>)) queenV,
      steps = M.fromList $ mapxfx (\v -> takeWhile (/= Off) $ map (at1 masterboard) $ iterate (v <+>) pos1) queenV,
      knight = map singleton $ filter (/= Off) $ map (at1 masterboard . (pos1 <+>)) knightV,
      pawn = map (\(f, t, v) -> (f, t, map (at1 masterboard) $ filter onboard $ map (pos1 <+>) v)) 
                        [(White, Black , [[-1,  1]]),
                         (White, Black , [[ 1,  1]]), 
                         (White, EmptyC, [[ 0,  1]] ++ if y == 1 then [[ 0,  2]] else []), 

                         (Black, White , [[-1, -1]]),
                         (Black, White , [[ 1, -1]]),
                         (Black, EmptyC, [[ 0, -1]] ++ if y == 6 then [[ 0, -2]] else [])]
      }


pcpiece = piece . pc

pccolour = colour . pc

at board square = SqPC square $ board IM.! n square

check board sqpc = at board (sq sqpc) == sqpc

data SqPC = SqPC { sq :: Sq, pc :: PC } deriving (Eq)

instance Show SqPC where
   show (SqPC sq pc) = show pc ++ show sq

instance Show Move where
   show m = show (from m) ++ (if pcpiece (kill m) == Empty
                                 then '-' : show (sq $ to m)
                                 else 'x' : show (kill m))

data Move = Move { from :: SqPC, kill :: SqPC, to :: SqPC, thru :: [Sq] }

movessq movesofsqs board fromsquare = let
   from1 = at board fromsquare
   steps1 = steps fromsquare
   stepsl = map snd $ M.toList steps1
   in case pcpiece from1 of
      Pawn  -> map (\(fromcol, killcol, sqs) -> if pccolour from1 == fromcol 
                                                   then 
                                                      concatMap promote $
                                                         movesofsqs board from1 (== killcol) sqs 
                                                   else []) 
                                                   $ pawn fromsquare
      Empty -> []
      _ -> let
         steps2 = case pcpiece from1 of
            Queen  -> stepsl
            King   -> map (singleton . head) stepsl
            Castle -> map (steps1 M.!) castleV
            Bishop -> map (steps1 M.!) bishopV
            Knight -> knight fromsquare
         in map (movesofsqs board from1 (/= pccolour from1)) steps2

takeWhile1 pred []     = []
takeWhile1 pred (x:xs) = if pred x then x : takeWhile1 pred xs else [x]

movesofsqs board from1 killcol tos1 =
   filter (killcol . pccolour . kill) $ 
   takeWhile1 ((== Empty) . pcpiece . kill) $ 
   movesofsqs1 board from1 killcol tos1

movesofsqs1 board from1 killcol tos1 =
   filter (killcol . pccolour . kill) $ 
   movesofsqs2 board from1 killcol tos1

movesofsqs2 board from1 killcol tos1 = 
   mapMaybe (\kills2 -> let
         kill1 = last kills2
         sq1   = sq kill1
         in ifJust (not $ null kills2) $ Move { from = from1, kill = kill1, to = SqPC sq1 (pc from1), thru = map sq kills2 }) $ tail $ inits $ map (at board) tos1

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

moves1 movesofsqs board = concatMap (concat . movessq movesofsqs board) masterboardl

moves board = moves1 movesofsqs board

movesThru board = multimap $ concatMap (\m -> map (,m) $ thru m) $ moves1 movesofsqs1 board

doMove move board = if (check board $ from move) && (check board $ kill move)
   then
      IM.insert (n $ sq $ to   move) (pc $ to move   ) $
      IM.insert (n $ sq $ from move) (PC EmptyC Empty) board
   else
      error $ "This move "++show move++" is invalid on this board"

newMoves board = let
   moves2 = movesThru board
   in moves2
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
-------
CnbKQbnC
pppppppp

        
        
        
pppppppp
CnbKQbnC
-}