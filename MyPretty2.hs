-- Copyright 2025 Brett Curtis
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# LANGUAGE MultiWayIf #-}
{- HLINT ignore "Move filter" -}
{- HLINT ignore "Avoid lambda using `infix`" -}
{- HLINT ignore "Use section" -}
{- HLINT ignore "Avoid lambda" -}

module MyPretty2 (
   parseT,
   reformat,
   format,
   format1,
   pp,
   findWidth,
   putGrid,
   showGridD,
   showGridF,
   tryGridF,
   takeWhileUnique,
   colWidthsF,
   colWidths1,
   colWidths5Test,
   --colWidths5,
   --colWidths5A,
   --colWidths5B,
   colWidths7,
   randGrid,
   randGridS,
   showGrid,
   showGrid1,
   showGridW,
   showGridWrap,
   showCol,
   showColD1,
   showRowD1,
   gridDriver,
   gridTester,
   forceLess,
   putGridW,
   width,
   wrapText,
   justify,
   justify1,
   GridH(..),
   GridV(..),
   Term (Int1, Integer1, Data, Double1, String1, Date, DateTime, Bool1, List, NDiffTime, ByteStr),
)
where

import Favs
import Numeric
import Show1
import {-# SOURCE #-} MHashDynamic3

import Data.Functor
import Prelude hiding (maximum)
import Data.List hiding (maximum)
import Maximum
import qualified Data.List
import Text.ParserCombinators.Parsec hiding (Column)
import Text.ParserCombinators.Parsec.Language hiding (Column)

import qualified Data.ByteString as B
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock

import Control.Monad
import Control.Monad.State
import System.Random
import System.Random.Stateful

import qualified System.Console.Terminal.Size as Term

import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec.Token qualified as T

import Debug.Trace
import GHC.IO (unsafePerformIO)
import GHC.Stack

width1 = 412

{-# NOINLINE width #-}
width = unsafePerformIO getWidth1

findWidth n = mapM_ putStrLn $ transpose $ padcol0 $ map show [1 .. n]

newtype GridH a = GridH [[a]]
newtype GridV a = GridV [[a]]

class GridHV a where
   h :: a b -> GridH b
   v :: a b -> GridV b

instance GridHV GridH where
   h = id
   v (GridH g) = GridV $ transpose g

instance GridHV GridV where
   h (GridV g) = GridH $ transpose g
   v = id

instance Show1 a => Show (GridV a) where
   show (GridV es) = showGrid $ map2 show1 es

instance Show1 a => Show (GridH a) where
   show g = showGrid $ map2 show1 es where GridV es = v g

data Term
   = Int1 Int
   | Integer1 Integer
   | Double1 Double
   | String1 String
   | ByteStr B.ByteString
   | ChLit Char
   | Ident String
   | List [Term]
   | Tuple [Term]
   | Data String [Term]
   | DataN String [(String, Term)]
   | Named String Term
   | Date Day
   | DateTime UTCTime
   | NDiffTime NominalDiffTime
   | Bool1 Bool
   | Map (M.Map Term Term)
   deriving (Eq, Ord, Show, Read)

instance Num Term where
   Int1 a + Int1 b = Int1 (a + b)
   Int1 a + Date b = Date $ addDays (fromIntegral a) b
   Date a + Int1 b = Date $ addDays (fromIntegral b) a
   Integer1 a + Integer1 b = Integer1 (a + b)
   Integer1 a + Date b = Date $ addDays a b
   Date a + Integer1 b = Date $ addDays b a
   Int1 a + Integer1 b = Integer1 (fromIntegral a + b)
   Integer1 a + Int1 b = Integer1 (a + fromIntegral b)
   a + b = Int1 0
   Int1 a - Int1 b = Int1 (a - b)
   Date a - Int1 b = Date $ addDays (negate $ fromIntegral b) a
   Integer1 a - Integer1 b = Integer1 (a - b)
   Date a - Integer1 b = Date $ addDays (negate b) a
   Date a - Date b = Integer1 $ diffDays a b
   Int1 a - Integer1 b = Integer1 (fromIntegral a - b)
   Integer1 a - Int1 b = Integer1 (a - fromIntegral b)
   Int1 a * Int1 b = Int1 (a * b)
   Integer1 a * Integer1 b = Integer1 (a * b)
   Int1 a * Integer1 b = Integer1 (fromIntegral a * b)
   Integer1 a * Int1 b = Integer1 (a * fromIntegral b)
   negate (Int1 a) = Int1 (negate a)
   negate (Integer1 a) = Integer1 (negate a)
   abs (Int1 a) = Int1 (abs a)
   abs (Integer1 a) = Integer1 (abs a)
   signum (Int1 a) = Int1 (signum a)
   signum (Integer1 a) = Integer1 (signum a)
   fromInteger a = Int1 (fromInteger a)

-- fromInteger a = Integer1 (fromInteger a)

{-instance Ord Term where
      compare (Int1 a) (Int1 b) = compare a b
-}
data Test = Test {a :: String, b :: String, c :: String} deriving (Eq, Ord, Show)

t = Test "the quick brown fox jumps over the lazy dog" "mr jock, tv quiz phd, bags few lynx" "abcdefghijklmnopqrstuvwxyz"

parseT str = right $ parse term "" str
reformat w str = format1 w $ parseT str
format w t = format1 w $ parseT $ show t
pp w t = putStrLn $ format w t

between a b c = do a; r <- b; c; return r

list = do symbol "["; r <- sepBy term (symbol ","); symbol "]"; return $ List r

tuple = do symbol "("; r <- sepBy term (symbol ","); symbol ")"; return $ Tuple r

datap = do i <- identifier; datanp i <|> data1p i

data1p i = do ts <- many term; return $ Data i ts

datanp i = do symbol "{"; r <- sepBy1 datatp (symbol ","); symbol "}"; return $ DataN i r

datatp = do i <- identifier; symbol "="; t <- term; return (i, t)

term =
   Int1 . fromInteger
      <$> integer
         -- <|> Integer1 <$> integer
         <|> Double1
      <$> float
         <|> String1
      <$> stringLiteral
         <|> ChLit
      <$> charLiteral
         <|> list
         <|> tuple
         <|> datap

symbol = T.symbol haskell
integer = T.integer haskell
float = T.float haskell
stringLiteral = T.stringLiteral haskell
charLiteral = T.charLiteral haskell
identifier = T.identifier haskell

format1 w (Int1 x) = show x
format1 w (Double1 x) = show x
format1 w (String1 x) = show x
format1 w (ChLit x) = show x
format1 w (Ident x) = x
format1 w (List x) = formatList w "[" ", " "]" x
format1 w (Tuple x) = formatList w "(" ", " ")" x
format1 w (Data n x) = formatList1 w (n ++ " ") " " "" x
format1 w (DataN n x) = formatList1 w (n ++ " {") ", " "}" $ map (uncurry Named) x
format1 w (Named n v) = n ++ " = " ++ format1 w v

clen (List x) = length x
clen (Tuple x) = length x
clen (Data n x) = length x
clen _ = 1

formatList width open sep close terms =
   let
      openind = pad 3 open
      ind = length openind
      fterms = map (format1 (width - ind)) terms

      res = open ++ intercalate sep fterms ++ close

   in if length res <= width
         then res
         else openind ++ indent1 ind (intercalate (sep ++ "\n") fterms) ++ close

formatList1 width open sep close terms =
   let
      ind = 3
      fterms = map (format1 (width - ind)) terms
      res = open ++ intercalate sep fterms ++ close

   in if length res <= width
         then res
         else open ++ "\n" ++ indent ind (intercalate (sep ++ "\n") fterms) ++ close

{-
else if allEqual clens
            then
            then open ++ indent1 (length open) (intercalate (sep ++ "\n") fterms) ++ close
-}

{-
formatList width sep terms = let
      fterms = map (format width) terms
      res    = intercalate sep fterms

      in

      if length res <= width
         then res
         else indent1 1 $ intercalate (sep ++ "\n") fterms
-}

{- not needed, defined in Favs
indent ('\n':str) = '\n':' ':indent str
indent (s   :str) = s       :indent str
indent []         = []
-}

class Divisible a where
   (//) :: a -> a -> a

{-
instance (Integral a, Integral b, Fractional c) => Divisible a b c where
      n // d = fromIntegral n / fromIntegral d
-}
instance Divisible Int where
   n // d = ceiling (fromIntegral n / fromIntegral d)

instance Divisible Double where
   (//) = (/)

checkNoNulls cellwcols = if not $ null $ filter null cellwcols then error "some columns are []" else cellwcols

colWidthsF cellLengthCols = map maximum $ checkNoNulls cellLengthCols

colWidthsFloating width cellLengthCols =
   let
      colWidths = map (sqrt . realToFrac . sum) cellLengthCols
      mult = realToFrac width / sum colWidths

   in map (mult *) colWidths

colWidthsIntegral width cellLengthCols =
   let
      colWidths = map (sqrt . realToFrac . sum) cellLengthCols
      mult = realToFrac width / sum colWidths

   in map (round . (mult *)) colWidths

-- make the column widths proportional to the total length of all cells in them
-- but no wider than the widest

-- shrink them until no need
colWidths1 width cellLengthCols = gridDriver width (colWidths1A width cellLengthCols)

colWidths1A :: (Integral a, Integral b) => a -> [[a]] -> ((a, a, [(a, b)], [a], [a]) -> (a, a, [(a, b)], [a], [a]), (a, a, [(a, b)], [a], [a]), (a, a, [(a, b)], [a], [a]) -> [a])
colWidths1A width cellLengthCols =
   let
      colwsF = map maximum $ checkNoNulls cellLengthCols
      colwsV = map sum cellLengthCols

   in
      -- mult      = fromIntegral width / fromIntegral (sum colwsV)
      -- colwsV1   = map (ceiling . (mult*) . fromIntegral) colwsV

      (colWidths1B width, (0, sum colwsV, [], colwsF, colwsV), colWidths1C)

colWidths1B width (wf, wv, _, colwsF, colwsV) =
   let
      mult = fromIntegral (width - wf) / fromIntegral wv
      colwsV1 = map (ceiling . (mult *) . fromIntegral) colwsV
      colwsMin = zipWith min (map (,1) colwsF) (map (,2) colwsV1)
      wf1 = sum [w | (w, x) <- colwsMin, x == 1]
      wv1 = sum [w | (w, x) <- colwsMin, x == 2]
      -- [(1, wf1), (2, wv1)] = combine (+) 0 $ map tflip colws5
      colwsV2 = map fst colwsMin

   in (wf1, wv1, colwsMin, colwsF, if wv1 == 0 then colwsF else colwsV2)

colWidths1C (wf, wv, _, colwsF, colwsV) = map fst $ zipWith min (map (,1) colwsF) (map (,2) colwsV)

--colWidths2 width cellLengthCols = 

-- this is a list of all the cell widths / heights worth trying
-- ie. the factors of length, plus all the naturals up to the square root

-- can be considered as (height, width) or (width, height)
factors :: [M.Map Int Int]
factors = map (\length ->
            M.fromList $ unfold1 (factors1 length) (0, 0)) [1..]

unfold1 f x = case f x of
   Just y  -> y:unfold1 f y
   Nothing -> []

factors1 length (prevHeight, prevWidth)
   | prevWidth <= 1 = Nothing
   | prevWidth * prevWidth < length = let
         height = prevHeight + 1

         in Just (height, length // height)

   | otherwise = let
         width = length // (prevWidth - 1)

         in Just (length // width, width)

-- a refinement of colwidths1. divide cell widths by total width of row first
cellHeightsRow a b = zipWith (//) b a

cellHeightsCol colWidth cellLengthsCol = map (// colWidth) cellLengthsCol

rowHeight colWidths cellLengthsRow = maximum $ cellHeightsRow colWidths cellLengthsRow

rowHeights colWidths cellLengthRows = map (rowHeight colWidths) cellLengthRows

rowHeightsRaise colWidth cellLengthsCol rowHeights = zipWith (\rh cl -> let ch = cl // colWidth in max rh ch) rowHeights cellLengthsCol

rowHeights1 colWidths cellLengthsRow = reverse $ nubSet $ cellHeightsRow colWidths cellLengthsRow

gridHeight cellLengthRows colWidths = sum $ map (rowHeight colWidths) cellLengthRows

maxColumns width colWidths = max 1 $ length $ takeWhile (< width) $ tail $ scanl (+) 0 $ map (+1) colWidths -- $ map (sqrt . fromIntegral) colWidths

cellHeightRows colWidths cellLengthRows = map (cellHeightsRow colWidths) cellLengthRows

cellHeightCols colWidths cellLengthCols = zipWith cellHeightsCol colWidths cellLengthCols

cellPatternRows rowHeights cellHeightRows = zipWith (\rh -> map (\ch -> if ch >= rh then 1 else 0)) rowHeights cellHeightRows

cellPatternCols rowHeights cellHeightCols = map (zipWith (<=) rowHeights) cellHeightCols

colourCols colWidths cellLengthRows = let
   cellHeightRows1 = cellHeightRows colWidths cellLengthRows

   in transpose $ map (\cellhs -> let rh = maximum cellhs in map (\cellh -> if cellh >= rh then 1 else 0) cellhs) cellHeightRows1

adjustElem f col colWidths = let
   (b, x:a) = splitAt col colWidths

   in b ++ f x : a

minWidthAux f colWidths cellMapsRow =
   let
      (hs, ws) = unzip $ zipWith (\colWidth cellMap -> tflip
                     $ fromJust $ f colWidth cellMap) colWidths cellMapsRow

   in (maximum hs, sum ws)

-- minimum increased widths to reduce height of row
minIncWidthToDecHeight currWidths cellMapsRow =
   let
      currHeights = zipWith (\colWidth cellMap ->
                     fromJust $ M.lookup colWidth cellMap) currWidths cellMapsRow
      currHeight  = maximum currHeights
      newHeights  = zipWith (\cellHeight cellMap -> fst
                     $ fromJust $ M.lookupLT cellHeight cellMap) currHeights cellMapsRow
      newHeight   = maximum newHeights
      newWidths   = map (snd
                     . fromJust . M.lookupLE newHeight) cellMapsRow
      newWidth    = sum newWidths

   in (newHeight, newWidth, newWidths)

minIncWidthToDecHeight2 currWidths newWidths = do
   minWidths <- get
   put $ zipWith3 (\c n m -> max c $ min n m) currWidths newWidths minWidths

--minIncHeightFromDecWidth

--maxDecWidthFromIncHeight

-- when column W is widening and column N is narrowing,
-- how much do we have to take off one and put on the other before the
-- pattern of bottomed out cells changes?
-- if it's negative, something is WRONG
cellChangeLimit colWidthW colWidthN cellLengthW cellLengthN rowHeight = let
   colWidthChangeW = cellLengthW // rowHeight - colWidthW
   colWidthChangeN = colWidthN - cellLengthN // rowHeight
{-
now work out the change that will make them equal

cellHeights are:

    cellLengthW           cellLengthN
------------------- = --------------------
widenWidth + change   narrowWidth - change

cellLengthN * (widenWidth + change) = cellLengthW * (narrowWidth - change)
cellLengthN * widenWidth + cellLengthN * change = cellLengthW * narrowWidth - cellLengthW * change
cellLengthN * change + cellLengthW * change = cellLengthW * narrowWidth - cellLengthN * widenWidth
(cellLengthN + cellLengthW) * change = cellLengthW * colWidthN - cellLengthN * colWidthW

         cellLengthW * colWidthN - cellLengthN * colWidthW
change = -------------------------------------------------
                      cellLengthN + cellLengthW
-}
   colWidthChange2 = (cellLengthW * colWidthN - cellLengthN * colWidthW) // (cellLengthN + cellLengthW)
   cellHeightChange2 = cellLengthW // (colWidthW + colWidthChange2)

-- in [colWidthChange2, colWidthChangeW, colWidthChangeN]

   in if cellHeightChange2 > rowHeight -- when the changing cells are equal, will they be higher than the height caused by other cells in the row?
         then [colWidthChange2]
         else [colWidthChangeW, colWidthChangeN]

cellFullAtWidth rowHeight cellLength = cellLength // rowHeight

cellWideningPressure rowHeight colWidth cellHeight = let
   advantage   = cellHeight // colWidth

   in if cellHeight >= rowHeight then advantage else 0

rowWideningPressure colWidths cellLengthsRow = let
   cellHeightsRow1 = cellHeightsRow colWidths cellLengthsRow
   rowHeight = maximum cellHeightsRow1

   in zipWith (cellWideningPressure rowHeight) colWidths cellHeightsRow1

--cellWideningPressure1 colWidth rowHeight cellLength cellHeight = if cellHeight >= rowHeight then cellHeight // colWidth else 0
colAdvantages cellPatternCols cellLengthCols colWidths = zipWith (//) (map sum $ zipWith (zipWith (*)) cellPatternCols cellLengthCols) colWidths

colWideningAdvantage rowHeights colWidth cellHeightsCol =
   sum (zipWith (\rh ch -> if ch >= rh then ch else 0) rowHeights cellHeightsCol)

colFullLength1 rowHeights colWidth cellHeightsCol cellLengthsCol =
   sum (zipWith3 (\rh ch cl -> if ch >= rh then cl else 0) rowHeights cellHeightsCol cellLengthsCol)

colWideningAdvantage2 rowHeights colWidthW colWidthN cellLengthsColW cellLengthsColN
   = sqrt (colFullLength1 rowHeights colWidthW cellLengthsColW // colFullLength1 rowHeights colWidthN cellLengthsColN)
--   sum (zipWith (\rh cl -> cl / cw / cw) rowHeights cellLengthsCol)

filterPattern cellsFullCol xs = catMaybes $ zipWith (\cf x -> if cf then Just x else Nothing) cellsFullCol xs

colWidths10 width cellLengthCols = let
   cellLengthRows = transpose cellLengthCols
   colWidths1 = colWidthsFloating width cellLengthCols :: [Double]
   rowHeights1 = rowHeights colWidths1 cellLengthRows
   cellsFullCols = map (zipWith (<=) rowHeights1) $ zipWith (\cw -> map (// cw)) colWidths1 cellLengthCols 
   colWidths = zipWith (\cfc clc -> sum $ filterPattern cfc $ zipWith (/) clc rowHeights1) cellsFullCols cellLengthCols
   in 0
{-

can't we just choose the column widths such that the advantage is equal across the whole table?
for all columns, sqrt (sum lengths) / width is equal (for the bottomed out cells)

then we get a pattern of full cells

then we do it again, only counting full cells?

the width of a column is the area of full cells divided by their height

colWidths = zipWith (\cfc clc -> sum $ filterPattern cfc $ zipWith (/) clc rh) cellLengthCols

filterPattern cfc xs = catMaybes $ zipWith (\cf x -> if cf then Just x else Nothing) cellsFullCol xs

the height of a grid is the sum of the row heights

gridHeight = sum rowHeights = sum $ filter full $ area / width

rowHeight = fullHeight

height = area / width

I think we need to consider a moving grid over a time span or differentiate over total grid height

dw/dh = - area / height^2

w = startwidth + area / height - area / startheight

say startwidth = 10, startheight = 10, so area = 100

if we plug in height = 5

w = 10 + 100 / 5 - 100 / 10

the next thing is to work out at what height the full pattern changes

for all nonfull cells, they become full when height = area / width where the height and width is from the full cells

imagine a grid with these lengths, the blank ones are zero
if you widen A, that lowers h, which widens d, which lowers e, which widens b, which lowers g

  a b c d 
e| |2| |2|1
f| | |1| |1
g| |7| | |2.33
h|1| | |2|1

  1 3 1 2 = widths
  width = 7
  height = 5.33
  
  a b c d 
e| |2| |2|1
f| | |1| |1.11
g| |7| | |2.25
h|1| | |2|1

  1 3 0 2 = widths sum = 7
    . .
    1 9

height = 5.36

  a b c d 
e| |2| |2|1
f| | |1| |0.909
g| |7| | |2.41379
h|1| | |2|1

  1 2 1 2 = widths sum = 7
    . .
    9 1

  a b c d 
e| |2| |2|1
f| | |1| |0.911437827766
g| |7| | |2.41143782777
h|1| | |2|1

  1 2 1 2 = widths sum = 7
    . .
    9 1

height = 5.32288

  a b c d 
e| |2| |2|1
f| | |1| |0.5
g| |7| | |3.5
h|1| | |2|1

  1 2 2 2 = widths sum = 7
     
height = 6

s      = sqrt (sum (cellLengthCols !! widenCol) // sum (cellLengthCols !! narrowCol)) = sqrt (1/7) = 0.378
change = (narrowWidth * s - widenWidth) / (s + 1)

3 * 0.378 - 1 / 1.378 = 


  a b c d 
e| | |2| |2| |
f| | | |1| |1|
g| | |7| | | |
h| |1| | |2| |
 |1| | | | |3|
  1 1 3 1 2 2 = sqrt (sum lengths)

widening a lowers e and h, which widens c and d, which widens b

  a b c d 
e|2| | |2|
f| |2|2| |
g| |2| |2|
h|2| |2| |

  1 3 1 2 = sqrt (sum lengths)
  1 3 1 2 = widths



OK, but how do we handle the changing of which ones are full?
-}
--patternChangeAtWidth rowHeights cellLengthsColW cellLengthsColN =
cellFullAtWidthCol rowHeights cellLengthsCol = zipWith cellFullAtWidth rowHeights cellLengthsCol

-- when column W is widening and column N is narrowing,
-- how much do we have to take off N and put on W before the
-- pattern of full cells changes?
colChangeLimit colWidthW colWidthN cellLengthsColW cellLengthsColN rowHeights = zipWith3 (cellChangeLimit colWidthW colWidthN) cellLengthsColW cellLengthsColN rowHeights

colWidths3 width tab =
   let
      ncols = length tab
      cellLengthRows = transposez 0 $ map2 length tab
      cellMapsRows = transposez M.empty $ map2 (factors !!) cellLengthRows
      colWidths = replicate ncols 1

   in (colWidths3A width cellLengthRows cellMapsRows, colWidths, id)

-- colWidths4A :: Int -> [[a]] -> [[Int]] -> [Int] -> [Int]
colWidths3A width cellLengthRows cellMapsRows colWidths =
   let
      th = gridHeight cellLengthRows colWidths
      tw = sum colWidths
      colWidths2 = snd
            $ minimum
            $ map
               (\(newHeight, newWidth, colWidths) -> let
                     gh = gridHeight cellLengthRows colWidths

                     in (gh * newWidth, colWidths))
            $ nubSet
            $ map (minIncWidthToDecHeight colWidths) cellMapsRows

   in colWidths2
{-
lookupNearest key map = let
   eq = M.lookup key map 
   Just lt = M.lookupLT key map
   Just gt = M.lookupGT key map
   in if
      | isJust eq                   -> fromJust eq
      | key - fst lt < fst gt - key -> snd lt
      | otherwise                   -> snd gt
-}
cellHeights cellFactorCol width = (width, map (snd . fromJust . M.lookupLE width) cellFactorCol)
{-
rowHeights cellFactorCols2 colWidths = let
   cellHeightCols = map fromJust $ zipWith M.lookup colWidths cellFactorCols2
   in map maximum $ transpose cellHeightCols
-}

-- choose column widths one by one, not counting row heights for columns not yet done

colWidths4Y colWidths rowHeights cellLengthCols changeCol = 
   if changeCol < length cellLengthCols then let
      cellLengthsCol   = cellLengthCols !! changeCol

      -- the range to try is from where the column has no effect on the row heights to where it is the only one that does
      sumColWidths = sum colWidths
   {-
      gridWidth5  changeWidth = sumColWidths5 + changeWidth
      gridHeight5 changeWidth = sum $ rowHeightsRaise changeWidth cellLengthsCol5 rowHeights
      gridArea5   changeWidth = gridHeight5 changeWidth * gridWidth5 changeWidth
   -}
      ratios       = sort $ zipWith3 (\x y z -> [x // y, x, y, z]) cellLengthsCol rowHeights [0..] -- a list of column widths for cells in changeCol5 that cause that cell to be full
      [rs, xs, ys, zs] = transpose ratios
      as = scanr (+) 0 xs
      bs = scanl (+) 0 ys

      in concat $ zipWith4 (\a b x y -> let
   {-
   we want the minimum area
   the cells go from not full to full in a particular order (for this combination of the other columns)
   the first to be full is the one with the greatest value of (cell length / row height)

   a = sum of full cell lengths
   all are full from the point we choose the column width down through the list (up through the ratios)

   b = sum of row heights for the other rows

   width of changing column = length of cell / max height of rest of row
   changeWidth = x / y
   height of full cell = length of full cell / changeWidth
   height of grid = (a / changeWidth) + b
   width of grid = width of changing column + rest of grid = changeWidth + sumColWidth5
   area of grid = (a / changeWidth + b) * (changeWidth + sumColWidth5)
   area = (a / changeWidth + b) * (changeWidth + sumColWidth5)
   area = a + b * changeWidth + a / changeWidth * sumColWidth5 + b * sumColWidth5

   minimise the area
   solve for differential of area wrt to changeWidth equal to 0

   0 = b - a * sumColWidth5 / changeWidth^2
   a * sumColWidth5 / changeWidth^2 = b
   a * sumColWidth5 = b * changeWidth^2
   a/b * sumColWidth5 = changeWidth^2
   changeWidth = sqrt (a/b * sumColWidth5)
   -}
         changeWidth = sqrt (a / b * sumColWidths)
         area = (a / changeWidth + b) * (changeWidth + sumColWidths)
         colWidths1 = colWidths ++ [changeWidth]
         rowHeights1 = rowHeightsRaise changeWidth cellLengthsCol rowHeights
         in colWidths4Y colWidths1 rowHeights1 cellLengthCols (changeCol + 1)) as bs xs ys
   else
      [colWidths]

{-
if you change a column through the entire range of widths (0 to infinity?), and scale the rest of the table to fit, is there always a single minimum

at any time, if you multiply all the widths by the same number, you will divide the grid total height by the same number

mind you, i think the apparent multiple minima I've seen were properties of multiple columns

methods I could use to search through all possible combinations, which is hard because it's continuous:

I think the different patterns are the key

if you can have ANY combination of row heights for the other columns, you can have ANY order of cells becoming full in the chosen column

at least one cell in each row must be full, can you have any combination you want? NO

what if you just choose the column widths one by one, for each one you just decide how many cells in that column are full
   dont include the row heights for columns not done yet
   will that get all possible combinations?
   you might want to pick a value to match something that happens later on

if you're narrowing a column and a cell bottoms out, you don't have to stop, you can continue by narrowing all the other columns that have full cells on that row

if you're widening a column and a cell bottoms out in a different column, you can continue by widening all the other columns that have full cells on that row

widening multiple columns at different rates so that full cells in the same row have equal height

keep rowHeight = cellLength / colWidth equal among the columns you're changing

cellLength1 / (colWidth1 + change1) = cellLength2 / (colWidth2 + change2) = cellLength3 / (colWidth3 + change3)
cellLength1 * (colWidth2 + change2) * (colWidth3 + change3) = cellLength2 * (colWidth1 + change1) * (colWidth3 + change3) = cellLength3 * (colWidth1 + change1) * (colWidth2 + change2)
cellLength1 * cc2 * cc3 = cellLength2 * cc1 * cc3 = cellLength3 * cc1 * cc2

      cellLength2 * cc1   
cc2 = -----------------
         cellLength1

      cellLength3 * cc1
cc3 = -----------------
         cellLength1

widen a column until a cell bottoms out or it reaches the same advantage as another column
   if it bottoms out, 
      either
         the cell length/advantage is removed from the widening column and the bottomed out column has its advantage increased
      or
         you widen both, keeping those cells the same height

   if it reaches the same advantage as another column, you want to widen both at the rates that keep them that way

widening multiple columns at different rates so that they keep the same advantage

formula for keeping the same advantage:

colFullLength1 / colWidth1^2 = colFullLength2 / colWidth2^2 

how do you work out the advantages when some rows have multiple full cells? does it matter?
   the vertical force on those cells must be equal 

   advantage = height / width = total full length / width^2
-}


colWidths4Z colWidths cellLengthCols changeCol = let
   changeWidth5      = colWidths !! changeCol
   cellLengthsCol5   = cellLengthCols !! changeCol
   deleteFunc        = deleteIndex changeCol
   colWidths5        = deleteFunc colWidths
   cellLengthRows    = transpose $ deleteFunc cellLengthCols
   rowHeights        = map (rowHeight colWidths5) cellLengthRows

   -- the range to try is from where the column has no effect on the row heights to where it is the only one that does
   sumColWidths5 = sum colWidths5
{-
   gridWidth5  changeWidth = sumColWidths5 + changeWidth
   gridHeight5 changeWidth = sum $ rowHeightsRaise changeWidth cellLengthsCol5 rowHeights
   gridArea5   changeWidth = gridHeight5 changeWidth * gridWidth5 changeWidth
-}
   ratios       = sort $ zipWith3 (\x y z -> [x // y, x, y, z]) cellLengthsCol5 rowHeights [0..] -- a list of column widths for cells in changeCol5 that cause that cell to be full
   [rs, xs, ys, zs] = transpose ratios
   as = scanr (+) 0 xs
   bs = scanl (+) 0 ys

   in zipWith4 (\a b x y -> let
{-
we want the minimum area
the cells go from not full to full in a particular order (for this combination of the other columns)
the first to be full is the one with the greatest value of (cell length / row height)

a = sum of full cell lengths
all are full from the point we choose the column width down through the list (up through the ratios)

b = sum of row heights for the other rows

width of changing column = length of cell / max height of rest of row
changeWidth = x / y
height of full cell = length of full cell / changeWidth
height of grid = (a / changeWidth) + b
width of grid = width of changing column + rest of grid = changeWidth + sumColWidth5
area of grid = (a / changeWidth + b) * (changeWidth + sumColWidth5)
area = (a / changeWidth + b) * (changeWidth + sumColWidth5)
area = a + b * changeWidth + a / changeWidth * sumColWidth5 + b * sumColWidth5

minimise the area
solve for differential of area wrt to changeWidth equal to 0

0 = b - a * sumColWidth5 / changeWidth^2
a * sumColWidth5 / changeWidth^2 = b
a * sumColWidth5 = b * changeWidth^2
a/b * sumColWidth5 = changeWidth^2
changeWidth = sqrt (a/b * sumColWidth5)
-}

      changeWidth = sqrt (a / b * sumColWidths5)
      area = (a / changeWidth + b) * (changeWidth + sumColWidths5)
      colWidths7 = insertIndex changeCol changeWidth colWidths5
      in (changeWidth, area, colWidths7)) as bs xs ys

colWidths4AM width tab =
   let
      cellLengthCols1 = map2 length tab
      cellLengthCols  = map2 fromIntegral cellLengthCols1
      cellLengthRows  = transposez 0 cellLengthCols
      colWidths       = colWidthsFloating (fromIntegral width) cellLengthCols1 -- do putStr $ showGrid 420 $ transpose $ map2 show celllsrows

   in colWidths4M (fromIntegral width) tab cellLengthRows cellLengthCols 0 1 colWidths True True

colWidths4M :: Double -> [[String]] -> [[Double]] -> [[Double]] -> Int -> Double -> [Double] -> Bool -> Bool -> IO [Double]
colWidths4M width tab cellLengthRows cellLengthCols changeCol stepWidth colWidths chooseColMode chooseStepMode = let
   cellHeightRows1   = cellHeightRows colWidths cellLengthRows
   rowHeights        = map maximum cellHeightRows1
   cellPatternRows1  = cellPatternRows rowHeights cellHeightRows1
   cellPatternCols1  = transpose cellPatternRows1
   colAdvantages1    = colAdvantages cellPatternCols1 cellLengthCols colWidths
   colAdvantages2    = sort $ zip colAdvantages1 [0..]

   change = 0

   stepWidth7   = if chooseStepMode then stepWidth else change
   changeCol7   = changeCol
   changeWidth7 = colWidths !! changeCol

   in do
      putGrid $ transpose [
         ["colWidths"         , show colWidths        ],
         ["cellLengthRows"    , show cellLengthRows   ],
         ["cellHeightRows"    , show cellHeightRows1  ],
         ["rowHeights"        , show rowHeights       ],
         ["sum rowHeights"    , show $ sum rowHeights ],
         ["colAdvantages1"    , show colAdvantages1   ],
         ["colAdvantages2"    , show colAdvantages2   ],
         ["change"            , show change           ],
         ["changeCol"         , show changeCol7       ],
         ["changeWidth"       , show changeWidth7     ],
         ["stepWidth"         , show stepWidth7       ]]
      putStrLn $ showGridColour1 (map round colWidths) (map2 round cellPatternCols1) (map2 round cellLengthCols) tab
      --putStrLn $ showGridColour (map round colWidths) (map2 round cellLengthCols) tab
      --putStrLn $ unlines graph
      c <- getChar
      let n = length colWidths
      (changeCol1, stepWidth1, colWidths1) <- case c of
         '\27' -> do
            c1 <- getChar
            c2 <- getChar
            case c2 of
               'D' -> return (changeCol7 - 1, stepWidth7, colWidths)
               'C' -> return (changeCol7 + 1, stepWidth7, colWidths)
         '+' -> return (changeCol7, stepWidth7     , replaceIndex changeCol7 (changeWidth7 + stepWidth7) colWidths)
         '-' -> return (changeCol7, stepWidth7     , replaceIndex changeCol7 (changeWidth7 - stepWidth7) colWidths)
         '*' -> return (changeCol7, stepWidth7 * 10, colWidths)
         '/' -> return (changeCol7, stepWidth7 / 10, colWidths)
         --'a' -> return (widenCol6, narrowCol6    , change, colWidths)
         'e' -> return (changeCol7, stepWidth7, replicate n (width // fromIntegral n))
         'r' -> do
            gen <- initStdGen
            let cols = runStateGen_ gen (replicateM n . uniformRM (1::Double, 1000))
            return (changeCol7, stepWidth7, map fromIntegral $ forceLess (round width) (map round cols))

         --' ' -> return (widenCol8, narrowCol8, stepWidth8, replaceIndices [widenCol8, narrowCol8] [widenWidth8 + change, narrowWidth8 - change] colWidths)
         _   -> return (changeCol7, stepWidth7, colWidths)
      let chooseColMode1  = if c == 'c' then not chooseColMode  else chooseColMode
      let chooseStepMode1 = if c == 's' then not chooseStepMode else chooseStepMode

      colWidths4M width tab cellLengthRows cellLengthCols changeCol1 stepWidth1 colWidths1 chooseColMode1 chooseStepMode1

unionWith3 :: Ord k => (a -> b -> c) -> (a -> c) -> (b -> c) -> M.Map k a -> M.Map k b -> M.Map k c
unionWith3 f fl fr l r =
   M.unions [M.intersectionWith f l r, M.map fl (l M.\\ r), M.map fr (r M.\\ l)]

-- new strategy:
-- start off by widening the column that reduces the total height the most per unit width you increase, narrowing all the others proportionately to make the space for it
-- at some point, either
-- 1. a cell in one of the narrowing columns will bottom out, in that case, just continue but leaving that column alone, OR
-- 2. a cell in one of the widening columns will reach the cusp of lifting off
-- 3. another column will have better height for width

-- 1&2 are the same thing, a widening cell can only lift off if a narrowing cell bottoms out in the same row

-- what if you're widening two columns at once?
-- what rates should you use?
-- the rate that maximises the height loss per width gain?

-- we want the pattern of bottomed out cells to change for as many cells as possible at the same time
-- so that we don't have to keep cycling
-- that's how to choose the rates

-- add columns to the widening group from maximum hlpwg downwards, until 

-- add columns to the narrowing group from minimum hgpwl upwards, until their height gain uses up all the height lost by the widening ones

deleteIndices indices list = let
   indicesSorted = sort indices

   in foldr deleteIndex list indicesSorted

replaceIndices indices values list = let
   indicesSorted = sort $ zip indices values

   in foldr (uncurry replaceIndex) list indicesSorted

colWidths5Test width tab = colWidths5AM width tab
{-}
colWidths5 width cellLengthCols = gridDriver width (colWidths5A width cellLengthCols)

colWidths5A width cellLengthCols1 =
   let
      cellLengthCols = map2 fromIntegral cellLengthCols1 :: [[Double]]
      cellLengthRows = transposez 0 cellLengthCols
      colWidths = colWidthsFloating (fromIntegral width) cellLengthCols1 -- do putStr $ showGrid 420 $ transpose $ map2 show celllsrows

   in (colWidths5B (fromIntegral width) cellLengthRows cellLengthCols, colWidths, map round)

-- repeatedly widen the column that has the most cells that are keeping their row from being lower

colWidths5B width cellLengthRows cellLengthCols colWidths = let
   --rowHeights      = map (rowHeight colWidths) cellLengthRows
   cellHeightCols23  = transpose $ cellHeightRows colWidths cellLengthRows
   --colAdvantages   = zipWith3 (colFullLength1 rowHeights) colWidths cellHeightCols cellLengthCols
   --widenCol        = fromJust $ elemIndex (maximum colAdvantages) colAdvantages
   --narrowCol       = fromJust $ elemIndex (minimum colAdvantages) colAdvantages
   --widenWidth      = colWidths !! widenCol
   narrowWidth     = colWidths !! narrowCol
   deleteFunc      = deleteIndices [widenCol, narrowCol]
   colWidths2      = deleteFunc colWidths
   --cellLengthRows2 = map deleteFunc cellLengthRows
   --rowHeights2     = map (rowHeight colWidths2) cellLengthRows2
{-
sum lengthsW / (widenWidth + change)^2 = sum lengthsN / (narrowWidth - change)^2
(widenWidth + change)^2 / sum lengthsW = (narrowWidth - change)^2 / sum lengthsN
(widenWidth + change)^2 / (narrowWidth - change)^2 = sum lengthsW / sum lengthsN
(widenWidth + change) / (narrowWidth - change) = sqrt (sum lengthsW / sum lengthsN)
(widenWidth + change) = sqrt (sumw / sumn) * (narrowWidth - change)
widenWidth + change = sqrt (sumw / sumn) * narrowWidth - sqrt (sumw / sumn) * change
widenWidth + change + sqrt (sumw / sumn) * change = sqrt (sumw / sumn) * narrowWidth
change + sqrt (sumw / sumn) * change = sqrt (sumw / sumn) * narrowWidth - widenWidth
(sqrt (sumw / sumn) + 1) * change = sqrt (sumw / sumn) * narrowWidth - widenWidth
         sqrt (sumw / sumn) * narrowWidth - widenWidth
change = ---------------------------------------------
                  (sqrt (sumw / sumn) + 1)
s = sqrt  (sum lengthsW / sum lengthsN)
t = widenWidth + narrowWidth
u = narrowWidth - change
t - u = widenWidth + change
(t - u) / u = s
t - u = s * u
t = s * u + u
t = (s + 1) * u
u = t / (s + 1)
change = narrowWidth - u
-}
   s = colWideningAdvantage2 rowHeights2 widenWidth narrowWidth (cellLengthCols !! widenCol) (cellLengthCols !! narrowCol)
   t = widenWidth + narrowWidth
   u = t / (s + 1)

   in replaceIndices [widenCol, narrowCol] [t - u, u] colWidths
-}
colWidths5AM width tab =
   let
      cellLengthCols1 = map2 length tab
      cellLengthCols  = map2 fromIntegral cellLengthCols1
      cellLengthRows  = transposez 0 cellLengthCols
      colWidths       = colWidthsFloating (fromIntegral width) cellLengthCols1 -- do putStr $ showGrid 420 $ transpose $ map2 show celllsrows

   in colWidths5M (fromIntegral width) tab cellLengthRows cellLengthCols 0 1 1 colWidths False False

colWidths5M :: Double -> [[String]] -> [[Double]] -> [[Double]] -> Int -> Int -> Double -> [Double] -> Bool -> Bool -> IO [Double]
colWidths5M width tab cellLengthRows cellLengthCols widenCol narrowCol stepWidth colWidths chooseColMode chooseStepMode = let
   cellHeightRows1   = cellHeightRows colWidths cellLengthRows
   rowHeights        = map maximum cellHeightRows1
   cellPatternRows1  = cellPatternRows rowHeights cellHeightRows1
   cellPatternCols1  = transpose cellPatternRows1
   colAdvantages1    = colAdvantages cellPatternCols1 cellLengthCols colWidths
   colWidths2        = deleteFunc colWidths
   cellLengthRows2   = map deleteFunc cellLengthRows
   rowHeights2       = map (rowHeight colWidths2) cellLengthRows2
   gridHeight widenWidth = let
                        narrowWidth = t - widenWidth
                        in sum $
                           rowHeightsRaise narrowWidth (cellLengthCols !! narrowCol) $
                           rowHeightsRaise  widenWidth (cellLengthCols !!  widenCol) rowHeights2
   graph             = map ((\x -> replicate (min x 200) '*' ++ replicate (200 - x) ' ') . round . gridHeight) [1..t-1]

   colAdvantages2    = sort $ zip colAdvantages1 [0..]
   widenCol6         = snd $ last colAdvantages2
   narrowCol6        = snd $ head colAdvantages2
   colAdvantages3    = init $ tail colAdvantages2
   widenAdv7         = fst $ last colAdvantages3
   narrowAdv7        = fst $ head colAdvantages3
   widenWidth6       = colWidths !!  widenCol6
   narrowWidth6      = colWidths !! narrowCol6
   deleteFunc        = deleteIndices [widenCol6, narrowCol6]
   colWidths6        = deleteFunc colWidths
   cellLengthRows6   = map deleteFunc cellLengthRows
   rowHeights6       = map (rowHeight colWidths6) cellLengthRows6
   cellHeightCols    = transpose cellHeightRows1
   
   sumw   = colFullLength1 rowHeights   widenWidth6 (cellHeightCols !!  widenCol6) (cellLengthCols !!  widenCol6)
   sumn   = colFullLength1 rowHeights  narrowWidth6 (cellHeightCols !! narrowCol6) (cellLengthCols !! narrowCol6)
   sumw6  = colFullLength1 rowHeights6  widenWidth6 (cellHeightCols !!  widenCol6) (cellLengthCols !!  widenCol6)
   sumn6  = colFullLength1 rowHeights6 narrowWidth6 (cellHeightCols !! narrowCol6) (cellLengthCols !! narrowCol6)
   t      = widenWidth6 + narrowWidth6
   s      = sqrt (sumw // sumn)
   sn     = negate s
   uvc1   = (s  * narrowWidth6 - widenWidth6) / (s  + 1)
   uvc2   = (sn * narrowWidth6 - widenWidth6) / (sn + 1)
   aw1    = sumw / ( widenWidth6 + uvc1)^2
   an1    = sumn / (narrowWidth6 + uvc1)^2
   aw2    = sumw / ( widenWidth6 + uvc2)^2
   an2    = sumn / (narrowWidth6 + uvc2)^2
   {-
   u      = t / (s + 1)
   v      = t - u
   uvc    = narrowWidth6 - u
   -}
   {-
   advw   = sum w / (widenWidth6 + change)^2
   sqrt advw = sqrt sumw / (widenWidth6 + change)
   sqrt advw * (widenWidth6 + change) = sqrt sumw
   change * sqrt advw = sqrt sumw - sqrt advw * widenWidth6
   change = sqrt (sumw / advw) - widenWidth6
   -}
   awc    = sqrt (sumw / widenAdv7) - widenWidth6
   {-
   advn   = sum n / (narrowWidth6 - change)^2
   sqrt advn = sqrt sumn / (narrowWidth6 - change)
   sqrt advn * (narrowWidth6 - change) = sqrt sumn
   sqrt advn * narrowWidth6 - sqrt advn * change = sqrt sumn
   sqrt advn * narrowWidth6 - sqrt sumn - sqrt advn * change = 0
   sqrt advn * narrowWidth6 - sqrt sumn = sqrt advn * change
   narrowWidth6 - sqrt (sumn / advn) = change
   -}
   anc    = narrowWidth6 - sqrt (sumn / narrowAdv7)
   cs     = [uvc1, uvc2, awc, anc] : colChangeLimit widenWidth6 narrowWidth6 (cellLengthCols !! widenCol6) (cellLengthCols !! narrowCol6) rowHeights6
   change = minimum $ filter (>0) $ concat cs

   widenCol8    = if chooseColMode then  widenCol else widenCol6
   narrowCol8   = if chooseColMode then narrowCol else narrowCol6
   stepWidth8   = if chooseStepMode then stepWidth else change
   widenWidth8  = colWidths !! widenCol8
   narrowWidth8 = colWidths !! narrowCol8
   totalWidth8  = widenWidth8 + narrowWidth8

   in do
      putGrid $ transpose [
         ["colWidths"         , show colWidths        ],
         ["cellLengthRows"    , show cellLengthRows   ],
         ["cellHeightRows"    , show cellHeightRows1  ],
         ["cellHeightCols"    , show cellHeightCols   ],
         ["rowHeights"        , show rowHeights       ],
         ["sum rowHeights"    , show $ sum rowHeights ],
         ["colWidths2"        , show colWidths2       ],
         ["cellLengthRows2"   , show cellLengthRows2  ],
         ["colAdvantages1"    , show colAdvantages1   ],
         ["colAdvantages2"    , show colAdvantages2   ],
         ["widenCol6"         , show widenCol6        ],
         ["narrowCol6"        , show narrowCol6       ],
         ["colAdvantages3"    , show colAdvantages3   ],
         ["widenAdv7"         , show widenAdv7        ],
         ["narrowAdv7"        , show narrowAdv7       ],
         ["rowHeights6"       , show rowHeights6      ],
         ["sum w"             , show sumw             ],
         ["sum n"             , show sumn             ],
         ["s"                 , show s                ],
         ["t"                 , show t                ],
         ["aw1"               , show aw1              ],
         ["an1"               , show an1              ],
         ["aw2"               , show aw2              ],
         ["an2"               , show an2              ],
         ["cs"                , show cs               ],
         ["change"            , show change           ],
         ["widenCol"          , show widenCol8        ],
         ["narrowCol"         , show narrowCol8       ],
         ["widenWidth"        , show widenWidth8      ],
         ["narrowWidth"       , show narrowWidth8     ],
         ["stepWidth"         , show stepWidth8       ]]
      putStrLn $ showGridColour1 (map round colWidths) (map2 round cellPatternCols1) (map2 round cellLengthCols) tab
      --putStrLn $ showGridColour (map round colWidths) (map2 round cellLengthCols) tab
      --putStrLn $ unlines graph
      c <- getChar
      let n = length colWidths
      (widenCol1, narrowCol1, stepWidth1, colWidths1) <- case c of
         '\27' -> do
            c1 <- getChar
            c2 <- getChar
            case c2 of
               'D' -> return (widenCol8 - 1, narrowCol8, stepWidth8, colWidths)
               'C' -> return (widenCol8 + 1, narrowCol8, stepWidth8, colWidths)
         ',' -> return (widenCol8, narrowCol8 - 1, stepWidth8, colWidths)
         '.' -> return (widenCol8, narrowCol8 + 1, stepWidth8, colWidths)
         '+' -> return (widenCol8, narrowCol8, stepWidth8, replaceIndices [widenCol8, narrowCol8] [widenWidth8 + stepWidth8, narrowWidth8 - stepWidth8] colWidths)
         '-' -> return (widenCol8, narrowCol8, stepWidth8, replaceIndices [widenCol8, narrowCol8] [widenWidth8 - stepWidth8, narrowWidth8 + stepWidth8] colWidths)
         '*' -> return (widenCol8, narrowCol8, stepWidth8 * 10, colWidths)
         '/' -> return (widenCol8, narrowCol8, stepWidth8 / 10, colWidths)
         'a' -> return (widenCol6, narrowCol6, change, colWidths)
         'e' -> return (widenCol8, narrowCol8, stepWidth8, replicate n (width // fromIntegral n))
         'r' -> do
            gen <- initStdGen
            let cols = runStateGen_ gen (replicateM n . uniformRM (1::Double, 1000))
            return (widenCol8, narrowCol8, stepWidth8, map fromIntegral $ forceLess (round width) (map round cols))

         ' ' -> return (widenCol8, narrowCol8, stepWidth8, replaceIndices [widenCol8, narrowCol8] [widenWidth8 + change, narrowWidth8 - change] colWidths)
         _   -> return (widenCol8, narrowCol8, stepWidth8, colWidths)
      let chooseColMode1  = if c == 'c' then not chooseColMode  else chooseColMode
      let chooseStepMode1 = if c == 's' then not chooseStepMode else chooseStepMode

      colWidths5M width tab cellLengthRows cellLengthCols widenCol1 narrowCol1 stepWidth1 colWidths1 chooseColMode1 chooseStepMode1


colWidths5C width tab celllrows colWidths = do
   let t = transpose $ map (rowWideningPressure colWidths) celllrows
   let ts = map2 show t
   -- putStr $ showGrid 420 ts
   let t2 = map sum t
   print t2
   let t3 = zip t2 [0 ..]
   print t3
   let t4 = maximum t3
   print t4
   let colws2 = adjustElem (+1) (snd t4) colWidths
   print colws2
   if sum colws2 < width && colws2 /= colWidths
      then colWidths5C width tab celllrows colws2
      else return colWidths

colWidths5Z width tab colWidths =
   let
      celllsrows = map2 length $ transpose tab -- do putStr $ showGrid 420 $ transpose $ map2 show celllsrows

   in colWidths5C width tab celllsrows colWidths

--colWidths5 width = colWidths1 width

colWidths6 width celllcols =
   let
      cellhcols = map (\celllcol -> mapxfx (\colw -> map (// colw) celllcol) [1 .. width]) celllcols
      colwscombs = crossList cellhcols
      tabhcomb = mapMaybe ((\(cw, rh) -> ifJust (sum cw == width) (sum rh, cw)) . (\colwscomb -> (map fst colwscomb, map maximum $ transpose $ map snd colwscomb))) colwscombs

   in tabhcomb

colWidths6A width = sort . colWidths6 width

colWidths62 width = putStr . unlines . map ((`replicate` '#') . min 470 . ceiling . (400 /) . fst) . colWidths6 width

data Column = Column { heights :: [Double], xw :: Double, number :: Int }
            | Merged { heights :: [Double], wasted :: Double, colX :: Column, colY :: Column }

foldl2 f xs = let
   xs1 = take 4 xs
   (x:xs2) = xs1
   xs3 = zipWith (\y i -> (f x y, deleteIndex i xs2)) xs2 [0..]
   x4 = snd $ minOn (wasted . fst) xs3
   in fst x4 : foldl2 f (snd x4 ++ drop 4 xs)

foldl3 f [] = error "must be at least 1 in foldl3"
foldl3 f xs =
   case foldl2 f xs of
      [] -> error "[] in foldl3"
      [x] -> x
      xsnew@(a:b) -> foldl3 f xsnew


colWidths7 width tab =
   (id, , map ceiling) $
   map xw $
   sortOn number $
   colWidths7D $
   foldl3 colWidths7B $
   zipWith (colWidths7A $ fromIntegral width) [0..] tab

colWidths7A width number1 lengths = let
   h = map ((width /) . fromIntegral) lengths

   in Column {
      heights  = h,
      xw       = width,
      number   = number1 }

colWidths7B colX1 colY1 = let
   rowRatios = sort $ zipWith (\x y -> [x / y, x, y]) (heights colX1) (heights colY1)
   [rs, xs, ys] = transpose rowRatios
   as = scanr (+) 0 xs
   bs = scanl (+) 0 ys
   (divs, totals) = unzip $ zipWith4 (\a b x y -> let
{-
div is BETWEEN x / (x + y) and y / (x + y)

minimum of a / div + b / (1 - div)
m = 1 / div
div = 1 / m = 1 / (n+1)

a * m + b * (1 / (1 - 1/m))
a * m + b * (m / (m - 1))

n = m - 1 = (1 / div) - 1
m = n + 1

a * (n + 1) + b * (n + 1)/n
an + a + b + b/n = 0

multiply by n
an^2 + an + bn + b = 0
n^2 + (1+b/a)n + b/a = 0
c = b/a
n^2 + (1+c)n + c = 0
(n + (1+c)/2)^2 = c^2/4 - c
d = c/2 = b/2a
n + d + 0.5 = sqrt (d^2 - 2d)
n = sqrt (d^2 - 2d) - d - 1/2
-}
      d = b / (2*a)
      n = sqrt (d^2 - 2*d) - d - 0.5
      div1 = 1 / (n+1)
      divmin = min (x / (x+y)) (y / (x + y))
      divmax = max (x / (x+y)) (y / (x + y))
      div = max divmin (min div divmax)
      total = a / div + b / (1 - div)

      in (div, total)) as bs xs ys

   sorted = sort $ zip totals [0..]
   index = snd $ head sorted
   divx = divs !! index
   divy = 1 - divx
   xw = recip divx
   yw = recip divy
   xh = map (xw *) xs
   yh = map (yw *) ys
   maxh = zipWith max xh yh
   waste w h maxh = w * (maxh - h)

   in Merged {
      heights = maxh,
      wasted  = sum $ zipWith3 (\xh yh maxh -> max (waste xw xh maxh) (waste yw yh maxh)) xh yh maxh,
      colX    = colWidths7C xw colX1,
      colY    = colWidths7C yw colY1 }

colWidths7C mult col@(Column {}) =
   col {
      --lengths = map (mult *) lengths1,
      xw      = mult * xw col }

colWidths7C mult col@(Merged {}) =
   col {
      --lengths = map (mult *) lengths1,
      --xw      = mult * xw1,
      colX    = colWidths7C mult $ colX col,
      colY    = colWidths7C mult $ colY col }

colWidths7D col@(Column {}) = [col]
colWidths7D col@(Merged {}) = colWidths7D (colX col) ++ colWidths7D (colY col)

instance Eq Column where
   a@(Column {}) == b@(Column {}) = number a == number b
   a@(Merged {}) == b@(Merged {}) = (colX a, colY a) == (colX b, colY b)
   _ == _ = False

colWidths7Z celllcols =
   let
      celllrows = transpose celllcols
      rowratios = sort $ map (\[x, y] -> [x / y, x, y]) celllrows
      [r, x, y] = transpose rowratios
      a = scanr (+) 0 x
      b = scanl (+) 0 y
      -- rowratios = M.fromList $ zip (map (\[x, y] -> x // y) celllrows) $ zip xacc yacc
      -- x = column X lengths
      -- y = column Y lengths
      -- xw = column X width   = x/(x+y)
      -- 1-xw = column Y width = y/(x+y)
      -- r = x/y

      -- for chosen row 
      -- r / (r+1) = xw
      -- (r+1) / r = 1/xw
      rp1   = map    (1+) r           -- r + 1
      xw    = zipWith (/) r    rp1    -- r / (r + 1) = xw = x/x+y
      aoxw  = zipWith (/) a    xw     -- a / xw
      yw    = map    (1-) xw          -- y / x+y
      boyw  = zipWith (/) b    yw     -- b / yw
      total = zipWith (+) aoxw boyw   -- a / xw + b / yw
      check = map (gridHeight celllrows) $ zipWith (\xw yw -> [xw, yw]) xw yw

   in putGrid $ z [r, x, y, xw, yw, a, b, aoxw, boyw, total, check]

z cols = zipWith (:) ["r=x/y", "x", "y", "xw = x/(x+y)", "yw = y/(x+y)", "a", "b", "a/xw", "b/yw", "total", "check"] $ map2 show cols

outersperse x l = x : intersperse x l ++ [x]

xyz [r, rp1or, xacc, rp1, yacc] =
   let
      xacci = intersperse Nothing $ map Just xacc
      yacci = intersperse Nothing $ map Just yacc
      ri = outersperse Nothing $ map Just r
      rp1i = outersperse Nothing $ map Just rp1
      rp1ori = outersperse Nothing $ map Just rp1or

   in putGrid $ map2 showm [ri, rp1ori, xacci, rp1i, yacci]

showm (Just j) = show j
showm Nothing = ""

colWidths8 width tab =
   let
      ncols = length tab
      cellLengthCols = map2 length tab
      counts2 = map (snd
                  . maximum
                  . map tflip
                  . M.toList
                  . foldr (\key -> M.insertWith (+) key key) M.empty) cellLengthCols

      colWidths = replicate ncols 1 -- do putStr $ showGrid 420 $ transpose $ map2 show celllsrows

   in counts2
{-
colWidths9 width tab =
   let
      ncols = length tab
      celllsrows = map2 length $ transposez "" tab
      colWidths = replicate ncols 1 -- do putStr $ showGrid 420 $ transpose $ map2 show celllsrows
      in
      (colWidths9A width celllsrows, colWidths, id)

-- repeatedly widen the column that has the most cells that are keeping their row from being lower
colWidths9A :: Int -> [[Int]] -> [Int] -> [Int]
colWidths9A width celllsrows colWidths =
   if sum colWidths >= width then colWidths else adjustElem colWidths 1 $ snd $ maximum $ zip (map sum $ transpose $ mapNeedsWidening colWidths celllsrows) [0 ..]

colWidths9B width tab celllrows colWidths = do
   let t = transpose $ map (needsWidening colWidths) celllrows
   let ts = map2 show t
   -- putStr $ showGrid 420 ts
   let t2 = map sum t
   print t2
   let t3 = zip t2 [0 ..]
   print t3
   let t4 = maximum t3
   print t4
   let colws2 = adjustElem colWidths 1 $ snd t4
   print colws2
   if sum colws2 < width && colws2 /= colWidths
      then colWidths5B width tab celllrows colws2
      else return colWidths
-}

-- tank of liquid
--colWidths10 width tab = 
{-
pointratios = map (x / y, x, y) points

sum x filter (r > xw / yw) + sum y filter (r < xw / yw)

to move from xv / yv to xw / yw

(sum x $ filter (r > xv / yv)) / xv + (sum y $ filter (r < xv / yv)) / yv

a = (sum x $ filter (r > xw / (1 - xw)))
b = (sum y $ filter (r < xw / (1 - xw)))
a / xw + b / yw
a / xw + b / (1 - xw)
a / (r / (r + 1)) + b / (1 / (r + 1))
a * (r + 1) / r + b * (r + 1)
a / r * (r + 1) + b * (r + 1)
(r + 1) * (a / r + b)

r = xw / (1 - xw)
(1 - xw) * r = xw
r - r * xw = xw
r = r * xw + xw
r = (r + 1) * xw
xw = r / (r + 1)

colWidths6 width tab = let
      ncols = length tab
      celllsrows = map2 length $ transpose tab
      colWidths = colWidths2 width tab

      in --do putStr $ showGrid 420 $ transpose $ map2 show celllsrows
            colWidths6A width celllsrows colWidths colWidths

-- repeatedly widen the column that has the most cells that are keeping their row from being lower
-- and narrow the column that has the least
colWidths6A :: Int -> [[Int]] -> [Int] -> [Int] -> [Int]
colWidths6A width celllrows colWidths colws1 = let
      colsums = zip (map sum $ transpose $ map (needsWidening colWidths) celllrows) [0..]
      mincol  = snd $ minimum colsums
      maxcol  = snd $ maximum colsums
      colws2  = adjustElem colws1   1  maxcol
      colws3  = adjustElem colws2 (-1) mincol

      in if colws3 == colWidths || colws3 == colws1
         then colWidths
         else colWidths6A width celllrows colws1 colws3
-}
showTerm1 (String1 s) = (0, 0, length s, s)
showTerm1 (Int1 i) = (length $ show i, 0, 0, show i)
showTerm1 (Double1 d) =
   let
      s = showFFloat (Just 3) d ""
      l = length s
      p = elemIndex '.' s

   in case p of
         Just j -> (j, l - j, 0, s)
         Nothing -> (l, 0, 0, s)

showTerm b a l (String1 s) = (0, 0, length s, padr l s)
showTerm b a l (Int1 i) = let s = show i in (length s, 0, 0, padl b s)
showTerm b a l (Double1 d) =
   let
      s = showFFloat (Just 3) d ""
      l = length s
      p = fromMaybe l $ elemIndex '.' s

   in (p, l - p, 0, replicate (b - p) ' ' ++ s ++ replicate (a - (l - p)) ' ')

showCol col =
   let
      (b, a, l, c) = unzip4 $ map (showTerm b2 a1 l2) col
      b1 = maximum b
      a1 = maximum a
      l1 = maximum l
      l2 = max (b1 + a1) l1
      b2 = max b1 (l2 - a1)

   in c

-- showGridF f width tab = showGrid1 (f (width - length tab) tab) tab
showGrid = showGridF colWidthsIntegral showGrid1 width

showGridWrap = showGridF colWidthsIntegral showGridWrap1 width

showGridW = showGridF colWidthsIntegral showGrid1

showGridF f g width1 tab =
   let
      cellLengthCols = map2 length tab
      colWidths1 = colWidthsF cellLengthCols
      width2 = width1 - length tab
      colWidths =
         if sum colWidths1 < width2
            then forceLess width2 colWidths1
            else forceLess width2 $ f width cellLengthCols

   in g colWidths cellLengthCols $ padRWith "" tab

showTerms (cw, rh, b, a, l, _) d =
   case fromDynamic d :: Maybe String of
      Just s -> (0, 0, length s, padr l s)
      Nothing ->
         case fromDynamic d :: Maybe Int of
            Just i -> let
               s = show i
               in (length s, 0, length s, padl b s)
            Nothing ->
               case fromDynamic d :: Maybe Double of
                  Just d -> let
                     s = showFFloat (Just 3) d ""
                     l1 = length s
                     p = fromMaybe l1 $ elemIndex '.' s

                     in (p, l1 - p, length s, replicate (b - p) ' ' ++ showFFloat (Just $ 3 + a + p - l1) d "")
                  Nothing -> let
                     s = show d
                     in (0, 0, length s, padr l s)

showColD1 xs dyncol = let
   (b, a, l, s) = unzip4 $ zipWith showTerms xs dyncol
   b1 = maximum b
   a1 = maximum a
   l1 = maximum l

   in map (b1, a1, l1,) s

showRowD1 row = let
   rh = maximum $ map (\(cw, (b1, a1, l1, s)) -> ceiling (fromIntegral l1 / fromIntegral cw)) row
   in map (\(cw, (b1, a1, l1, s)) -> let
         l2 = cw * rh
         --b2 = if b1 + a1 > cw then ceiling (fromIntegral l2 / fromIntegral cw) * cw else b1
         --a2 = l2 - b2
         a2 = if b1 + a1 > cw then l2 // cw * cw else a1
         b2 = l2 - a2
         in (cw, rh, b2, a2, l2, s)) row

showGridD :: HasCallStack => (Int -> [[Int]] -> [Int]) -> Int -> [[Dynamic]] -> String
showGridD f width1 tab1 =
   let
      colsz           = map2 (showTerms (0, 0, 0, 0, 0, toDyn (0::Int))) tab1
      cellLengthColsz = map2 (\(b1, a1, l1, _) -> max (b1 + a1) l1) colsz
      colWidths00     = colWidthsF cellLengthColsz
      ncols           = maxColumns width1 colWidths00
      colWidths0      = take ncols colWidths00
      cols            = take ncols colsz
      tab             = take ncols tab1
      cellLengthCols  = take ncols cellLengthColsz
      width2          = width1 - ncols
      colWidths       = if sum colWidths0 < width2
                           then colWidths0
                           else forceLess width2 $ f width cellLengthCols

      cols1  = zipWith showColD1 (map2 (\(b, a, l, s) -> (0, 0, b, a, l, s)) cols) tab
      cols2  = zipWith zip (map repeat colWidths) cols1
      rows3  = map showRowD1 $ transpose cols2
      cols4  = transpose rows3
      cols5  = zipWith showColD1 cols4 tab
      cols6  = zipWith zip (map repeat colWidths) cols5
      rows7  = map showRowD1 $ transpose cols6
      cols8  = transpose rows7
      cols9  = zipWith showColD1 cols8 tab
      cols10 = zipWith zip (map repeat colWidths) cols9
      rows11 = map showRowD1 $ transpose cols10
      rows12 = unsafePerformIO $ do
         putStrLn ""
         print $ map head cols2
         putStrLn ""
         print $ head rows3
         putStrLn ""
         return $ map2 (\(cw, rh, _, _, _, s) -> map (take cw . padr cw) $ take rh $ padRWith1 "" rh $ groupN cw s) rows11

   in if | null tab1 -> error "grid is entirely empty"
         | any null tab1 -> "grid contains null columns"
         | otherwise -> unlines $ concat $ map2 (intercalate "|") $ map transpose rows12

showGridD1 f g width1 tab =
   let
      cellLengthCols = map2 (\(b1, a1, l1, _) -> max (b1 + a1) l1) $ map2 (showTerms (0, 0, 0, 0, 0, toDyn (0::Int))) tab
      colWidths1     = colWidthsF cellLengthCols
      width2         = width1 - length tab
      colWidths      = if sum colWidths1 < width2
                           then colWidths1
                           else f width cellLengthCols

      cols  = zipWith showColD1 cols2 tab
      cols1 = zipWith zip (map repeat colWidths) cols
      rows  = map showRowD1 $ transpose cols1
      cols2 = transpose rows

      in g colWidths cellLengthCols $ map2 (\(_, _, _, s) -> s) cols

      --rows2 = map (maximum $ map (\(cw, (b, a, l)) -> showRowD1) rows
                  --in map (\(cw, cell) -> map (padr cw) $ padRWith1 "" li cell) row

showGrid1 colWidths cellLengthCols tab =
   let
      rows = transpose $ zipWith (\cw c -> map (\cell -> (cw, groupN cw cell)) c) colWidths tab
      rows2 =
         map
            ( \row ->
                  let li = maximum $ map (length . snd) row
                  in map (\(cw, cell) -> if cell == ["£"] then replicate li (replicate cw '-') else map (padr cw) $ padRWith1 "" li cell) row
            )
            rows

   in unlines $ concat $ map2 (intercalate "|") $ map transpose rows2

showGridWrap1 colWidths cellLengthCols tab =
   let
      rows = transpose $ zipWith (\cw c -> map (\cell -> (cw, wrapText cw cell)) c) colWidths tab
      rows2 =
         map
            ( \row ->
                  let li = maximum $ map (length . snd) row
                  in map (\(cw, cell) -> map (padr cw) $ padRWith1 "" li cell) row
            )
            rows

   in unlines $ concat $ map2 (intercalate "|") $ map transpose rows2

doColour cc = "\27["++show (40+cc)++"m"

combineColours :: Int -> Int -> Int
combineColours a b = if a == 0 then b else a

smudge f xs = head xs : concat (zipWith (\a b -> [a, f a b]) xs (tail xs)) ++ [last xs, last xs]

smudge2 f = smudge (zipWith f) . map (smudge f)

showGridColour colWidths cellLengthCols tab = let
   cellLengthRows = transpose cellLengthCols
   cellColourCols = colourCols colWidths cellLengthRows

   in showGridColour1 colWidths cellColourCols cellLengthCols tab

showGridColour1 colWidths cellColours cellLengthCols tab = let
   rows = transpose $ zipWith3 (\cw cc c -> zipWith (\cellc celltext -> (cw, cellc, groupN cw celltext)) cc c) colWidths cellColours tab
   rows2 = map (\row -> let
            lc = maximum $ map (\(cw, cc, cell) -> length cell) row
            in map (\(cw, cc, cell) -> let
               li = replicate cw '-'
               in (cw, cc, lc, if cell == ["£"] then replicate lc li else map (padr cw) $ padRWith1 "" lc cell)) row) rows
   rows3 = smudge2 combineColours $ map2 (\(_, cc, _, _) -> cc) rows2
   rows4 = concatMap (\row -> let
                              a = concatMap (\(cw, cc, lc, cell) -> [["+"]           , [replicate cw '-']]) row ++ [head a]
                              b = concatMap (\(cw, cc, lc, cell) -> [replicate lc "|", cell              ]) row ++ [head b]
                              in [a, b]) rows2 ++ [head rows4]

   in unlines $ map (++ doColour 0) $ concat $ map2 concat $ map transpose $ zipWith (zipWith (zipWith (\cc cl -> doColour cc ++ cl))) (map2 repeat rows3) rows4
--   in unlines $ intercalate [line1] $ map2 (intercalate "|") $ map transpose rows2

putGridW w = putStr . showGridW w

putGrid grid = putStr $ showGrid grid

getWidth1 = do
   mwindow <- Term.size
   return $ case mwindow of
      Just window -> Term.width window
      Nothing     -> width1

takeWhileUnique :: (Ord a) => [a] -> [a]
takeWhileUnique = takeWhileUnique1 S.empty

-- putGrid2 = putStr . showGridF colWidths5 420
takeWhileUnique1 :: (Ord a) => S.Set a -> [a] -> [a]
takeWhileUnique1 set (x : xs) = if S.member x set then [x] else x : takeWhileUnique1 (S.insert x set) xs

takeWhileUniqueM x = do
   (ok, set) <- get
   if not ok || S.member x set
      then do
         put (False, set)
         return (False, x)

      else do
         put (True, S.insert x set)
         return (True, x)

gridDriver width (f1, s, f2) = forceLess width $ last $ takeWhileUnique $ map f2 $ iterate f1 s

gridTester width (f1, s, f2) = takeWhileUnique $ map f2 $ iterate f1 s

gridTester2 width (f1, s, f2) tab = let
   cellLengthCols = map2 length tab
   in mapM_ (\cws -> putStrLn $ showGridColour cws cellLengthCols tab) $ takeWhileUnique $ map f2 $ iterate f1 s

pages xs = pages1 xs 0

pages1 xs n = do
   putStrLn $ xs !! n
   c <- getChar
   pages1 xs $ case c of
      '+' -> n + 1
      '-' -> n - 1

iterateM mf x = do
   mf_x <- mf x
   loop <- iterateM mf mf_x
   return (x : loop)

whileM f mf x = do
   let (str, mfx) = mf x
   (ok, fx) <- f x
   loop <- if ok then
      whileM f mf mfx
   else
      return []
   return $ (str, fx) : loop

gridTesterM width (f1, s, f2) tab = do
   let cellLengthCols = map2 length tab
   blah <- evalStateT (whileM takeWhileUniqueM f1 s) (True, S.empty)
   let (strs, cws) = unzip blah
   let cws1 = map f2 cws
   pages $ zipWith (\str cws -> str ++ showGridColour cws cellLengthCols tab) strs cws1

roundList width colWidthsD = let
   total       =  sum colWidthsD
   mult        =  fromIntegral width / total
   colWidths2  =  map (mult *) colWidthsD
   colWidthsI  =  map floor colWidths2
   colWidthsZ  =  sort $ zipWith (\w n -> (snd $ properFraction w, w, n)) colWidths2 [0..]
   slack       =  width - sum colWidthsI
   (colWidthsB, colWidthsA)
               =  splitAt (length colWidths - slack) colWidthsZ
   colWidthsC  =  map (\(f, w, n) -> (n, floor   w)) colWidthsB ++
                  map (\(f, w, n) -> (n, ceiling w)) colWidthsA

   in map snd $ sort colWidthsC

forceLess width colWidths = let
   colWidthsD  =  map fromIntegral colWidths

   in if sum colWidths > width then roundList width colWidthsD else colWidths


tryGridF f width tab = gridTester $ f (width - length tab) tab
{-
putGridT tab = do
   let f = colWidths5
   let cellLengthCols = map2 length tab
   let (colws1, colWidths) = gridTester $ f (width - length tab) cellLengthCols
   mapM_ (pp width) colws1
   pp width colWidths
-}
randGrid y x seed = runStateGen_ (mkStdGen seed) (replicateM y . replicateM x . uniformRM (1, 100))

randGridS y x seed = map2 (`replicate` 'x') $ randGrid y x seed

readGrid str = map (map trim . transpose . map snd)
   $ filter (fst . head)
   $ groupBy (on (==) fst)
   $ mapfxx (null . dropWhile (== ' '))
   $ transposez ' ' $ lines str


groupCols [] = []
groupCols e = let
   (left, _):filled = dropWhile snd e
   (right, _):empty = dropWhile (not . snd) filled

   in (left, right - 1):groupCols empty
{-
I think I found a general law:
No column A with less total text than B can end up wider than B
I can't think of a counterexample that would make sense

Is that true for rows as well?

Also, colWidths7 finds the ideal ratio for any two columns
It calculates the ratio of corresponding cells in each column
And then finds the ratio that has equal amounts of text in the limiting cells above and below that ratio
With three columns you want equal amounts of text in all the limiting cells of each column
If you put the columns together in order of total text, would that be ideal?

The question remains: can there be multiple minima?

I think I found an analogy:
Gas makes no sense, text is more like incompressible liquid

Liquid in a compartmentalised tank where the walls can move left or right or up or down
according to whether theyre vertical or horizontal (without friction)
Somehow the vertical can move through the horizontal and vice versa

No gravity, no air pressure

the width has a fixed maximum ie. the thing is between four planes of infinite strength
that do not move

and the other two planes (horizontal) have some non-zero force pushing them together and
are able to move

And then... equalise the forces?

pressure = vertical force / horizontal size
pressure = horizontal force / vertical size

vertical force * vertical size = horizontal force * horizontal size
but only if that cell is holding up the row above (a limiting cell)/has more height than others in the row
if the row above is resting on a cell, there is both a horizontal force and a vertical force from that cell

I think the analogy breaks down when dealing with single characters
Probably becomes like a Diophantine equation
--------------------------------------------------------------------------------------------
Similarities with/differences to finding a Nash equilibrium

Everyone's working together in this

Columns will sacrifice width if it helps the grid
Rows want to be lower, I suppose sacrifice for them is getting higher

--------------------------------------------------------------------------------------------
finding the best column widths seems to be exponential in the number of columns

when you have two columns eg.

a b
4 4
2 4

width a b height
8     4 4 2
7     3 4 3
6     2 4 3
5     1 4 6
5     2 3 4

height = sum zipWith (\y -> let row = a !! y in maximum $ zipWith (\x row -> row !! x) [0..xsize]) [0..ysize]

height = sum (y = 0..ysize) (maximum (x = 0..xsize) (a[x][y]))

the column widths best for each of two sets of two columns is not related to the column widths best for all four columns together in any simple way

width = sum (x = 0..xsize) (maximum (y = 0..ysize) (a[x][y]))

columns and rows are interchangeable
it's the amount of wasted space that we need to minimise

for each column you have a two dimensional array of column width by row number containing the height of each cell

possible simplifications:
considering similar columns together can simplify it
want to make a column A that contains more text in every row than another column B wider than column B
a b c d e f
4 1 4 1 4 1
1 2 1 2 1 2

width a b c d e f height
18    4 2 4 2 4 2 2
15    4 1 4 1 4 1 3

with a two column table you have the height as sum of cells where left column is highest plus sum of cells where right column is highest

            sum of length of highest left cells   sum of length of highest right cells
            ----------------------------------- + ------------------------------------
                        width of left column                 width of right column

left cells become highest when left length   right length
                              ----------- > ------------
                              left width    right width

lengths
12 3
12 6

widths 12 3
heights
1 1
1 2
total = 3

widths 10 5
heights
1.2 0.6
1.2 1.2
total = 2.4



with 3 columns

      sum of length of highest A cells   sum of length of highest B cells   sum of length of highest C cells
      -------------------------------- + -------------------------------- + --------------------------------
            width of column A                  width of column B                    width of column C

as you increase the width of A


if you do a cumulative histogram of height of each column
then narrowing a column expands its histogram to the right
the total height is the integral between the rightmost histogram and the y-axis

once you've set the best ratio between two columns
does adding a third change anything?

if you plot the lengths of cells from the same row on a scatter chart (x=left size, y = right)
the column widths lie on the line x+y = k

then draw a line through the origin and the column widths point (xw, yw)
cells below the line have a larger x component so the height for that row is x/xw
cells above the line have a larger y component so the height for that row is y/yw

then draw a line through the origin and the column widths point with reversed co-ordinates (yw, xw) or even (1/xw, 1/yw)
cells below the line have a larger y component so the height for that row is y/yw

NOOOOOO

draw a line x=y

choose xw and yw and multiply the points by this

as you go from high xw to high yw the points cross the line

sort the points in order of ratio

sum x / xw $ filter x / xw > y / yw + sum y / yw $ filter

for each column, work out the number of cells with each area or more
when adding two columns
its pretty easy to add these up column by column

-}

wrapText width xs = let
   ws = words xs
   ls = map length ws

   in wrapText1 width ws ls

wrapText1 :: Int -> [String] -> [Int] -> [String]
wrapText1 width [] _  = []
wrapText1 width ws ls = let
   ys = zipWith (+) (0 : [0..]) $ scanl (+) 0 ls
   zs = takeWhile (<= width) ys
   in if length zs <= 1
         then let
            w = take width $ head ws
            w1 = drop width $ head ws

            in w : wrapText1 width (w1:tail ws) (tail ls)

         else let
            (ws1, ws2) = splitAt (length zs - 1) ws
            (ls1, ls2) = splitAt (length zs - 1) ls

            in justify1 width ws1 ls1 : wrapText1 width ws2 ls2
         --else unwords (take (length zs) ws) : wrapText1 width ws ls

justify width xs = let ws = words xs in justify1 width ws (map length ws)

justify1 width ws ls = let
   n = length ws - 1
   sp = width - sum ls
   d = fromIntegral sp / fromIntegral n
   sps = map round [0, d..fromIntegral sp]
   sp3 = map (\n -> replicate n ' ') $ zipWith (-) (tail sps) sps

   in if n > 0
         then concat $ (head ws :) $ zipWith (++) sp3 (tail ws)
         else concat ws ++ replicate sp ' '
