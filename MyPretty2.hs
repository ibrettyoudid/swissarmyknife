{-# LANGUAGE TupleSections #-}
-- Copyright 2025 Brett Curtis
{-# LANGUAGE UndecidableInstances #-}

module MyPretty2 (
   parseT,
   reformat,
   format,
   format1,
   pp,
   findWidth,
   putGrid,
   putGridF,
   showGridF,
   tryGridF,
   firstRpt,
   colWidthsF,
   colWidths1,
   colWidths5,
   showGrid,
   showGrid1,
   showGridW,
   showCol,
   putGridW,
   putGridT,
   width,
   GridH(..), 
   GridV(..),
   Term (Int1, Integer1, Data, Double1, String1, Date, DateTime, Bool1, List, NDiffTime, ByteStr),
)
where

import Data.Functor
import Data.List
import Favs
import Numeric
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language

import Data.ByteString qualified as B
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock

import Control.Monad
import System.Random
import System.Random.Stateful

import Data.Map qualified as M
import Data.Set qualified as S
import Text.ParserCombinators.Parsec.Token qualified as T

width = 413

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

instance Show a => Show (GridV a) where
   show (GridV es) = showGrid $ map2 show es

instance Show a => Show (GridH a) where
   show g = showGrid $ map2 show es where GridV es = v g

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
      in
      if length res <= width
         then res
         else openind ++ indent1 ind (intercalate (sep ++ "\n") fterms) ++ close

formatList1 width open sep close terms =
   let
      ind = 3
      fterms = map (format1 (width - ind)) terms
      res = open ++ intercalate sep fterms ++ close
      in
      if length res <= width
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

colWidthsF cellwcols = map maximum cellwcols

colWidthsV cellwcols =
   let
      colws = map sum cellwcols
      mult = width // sum colws
      in
      (colws, map (mult *) colws)

-- make the column widths proportional to the total length of all cells in them
-- but no wider than the widest

-- shrink them until no need
colWidths1 width tab =
   let
      cellwcols = map2 length tab
      colwsF = map maximum cellwcols
      colwsV = map sum cellwcols
      in
      -- mult      = fromIntegral width / fromIntegral (sum colwsV)
      -- colwsV1   = map (ceiling . (mult*) . fromIntegral) colwsV

      (colWidths1A width, (0, sum colwsV, [], colwsF, colwsV), colWidths1B)

colWidths1A width (wf, wv, _, colwsF, colwsV) =
   let
      mult = fromIntegral (width - wf) / fromIntegral wv
      colwsV1 = map (ceiling . (mult *) . fromIntegral) colwsV
      colwsMin = zipWith min (map (,1) colwsF) (map (,2) colwsV1)
      wf1 = sum [w | (w, x) <- colwsMin, x == 1]
      wv1 = sum [w | (w, x) <- colwsMin, x == 2]
      -- [(1, wf1), (2, wv1)] = combine (+) 0 $ map tflip colws5
      colwsV2 = map fst colwsMin
      in
      (wf1, wv1, colwsMin, colwsF, if wv1 == 0 then colwsF else colwsV2)

colWidths1B (wf, wv, _, colwsF, colwsV) = map fst $ zipWith min (map (,1) colwsF) (map (,2) colwsV)

-- a refinement of colwidths1. divide cell widths by total width of row first
rowHeights a b = zipWith (flip (//)) a b

rowHeight colws = maximum . rowHeights colws

gridHeight celllrows colws = sum $ map (rowHeight colws) celllrows

extend colws by col = zipWith (+) colws $ replicate col 0 ++ [by] ++ repeat 0

tryCW width cellwrows colws by
   | sum colws + by >= width = colws
   | otherwise = let 
      r = mapfxx (gridHeight cellwrows) $ map (extend colws by) [0 .. length colws - 1]
      ma = maximum r
      mi = minimum r
      in if fst ma == fst mi
            then tryCW width cellwrows colws (by + 1)
            else tryCW width cellwrows (snd mi) 1

colWidths3 width tab =
   let
      ncols = length tab
      cellwrows = map2 length $ transpose tab
      colws = tryCW width cellwrows (replicate ncols 1) 1
      in
      colws

-- minimum increased widths to reduce height of row
minToReduce cellwsrow celllsrow =
   let
      maxh = rowHeight cellwsrow celllsrow
      maxhm1 = max 1 (maxh - 1)
      in
      zipWith (\cew cel -> max cew $ ceiling $ cel // maxhm1) cellwsrow celllsrow

-- in (zipped, maxh, maxhm1, sum blah, blah)

-- putGrid w = putStr . showGrid w

colWidths4 width celllrows =
   let
      ncols = length $ head celllrows
      -- celllsrows = map2 length $ transpose tab
      colws = replicate ncols 1
      in
      colWidths4A width celllrows colws

-- colWidths4A :: Int -> [[a]] -> [[Int]] -> [Int] -> [Int]
colWidths4A width celllrows colws =
   let
      th = gridHeight celllrows colws
      tw = sum colws
      colws2 =
         snd
            $ maximum
            $ mapfxx
               ( \colws ->
                     fromIntegral (th - gridHeight celllrows colws)
                        / fromIntegral (sum colws - tw)
               )
            $ nubSet
            $ map (minToReduce colws) celllrows
      in
      if sum colws2 < width && colws2 /= colws
         then colWidths4A width celllrows colws2
         else colws

-- which cells in the row are keeping it from being lower
-- don't worry about rows where the height is 0 or 1 because they cant be lower
needsWidening cellwsrow celllsrow =
   let
      rowhs = rowHeights cellwsrow celllsrow
      maxh = maximum rowhs
      in
      map (if maxh > 1 then (\x -> if x == maxh then 1 else 0) else const 0) rowhs

-- in (zipped, maxh, maxhm1, sum blah, blah)

colWidths5 width tab =
   let
      ncols = length tab
      celllsrows = map2 length $ transposez "" tab
      colws = replicate ncols 1 -- do putStr $ showGrid 420 $ transpose $ map2 show celllsrows
      in
      (colWidths5A width celllsrows, colws, id)

-- repeatedly widen the column that has the most cells that are keeping their row from being lower
colWidths5A :: Int -> [[Int]] -> [Int] -> [Int]
colWidths5A width celllsrows colws =
   if sum colws >= width then colws else extend colws 1 $ snd $ maximum $ zip (map sum $ transpose $ map (needsWidening colws) celllsrows) [0 ..]

colWidths5B width tab celllrows colws = do
   let t = transpose $ map (needsWidening colws) celllrows
   let ts = map2 show t
   -- putStr $ showGrid 420 ts
   let t2 = map sum t
   print t2
   let t3 = zip t2 [0 ..]
   print t3
   let t4 = maximum t3
   print t4
   let colws2 = extend colws 1 $ snd t4
   print colws2
   if sum colws2 < width && colws2 /= colws
      then colWidths5B width tab celllrows colws2
      else return colws

colWidths5Z width tab colws =
   let
      celllsrows = map2 length $ transpose tab -- do putStr $ showGrid 420 $ transpose $ map2 show celllsrows
      in
      colWidths5B width tab celllsrows colws

colWidths6 width celllcols =
   let
      cellhcols = map (\celllcol -> mapxfx (\colw -> map (// colw) celllcol) [1 .. width]) celllcols
      colwscombs = crossList cellhcols
      tabhcomb = mapMaybe ((\(cw, rh) -> ifJust (sum cw == width) (sum rh, cw)) . (\colwscomb -> (map fst colwscomb, map maximum $ transpose $ map snd colwscomb))) colwscombs
      in
      tabhcomb

colWidths6A width = sort . colWidths6 width

colWidths62 width = putStr . unlines . map ((`replicate` '#') . min 470 . ceiling . (400 /) . fst) . colWidths6 width

colWidths7 twidth celllcols =
   let
      celllrows = transpose celllcols
      rowratios = sort $ map (\[x, y] -> [x / y, x, y]) celllrows
      [r, xc, yc] = transpose rowratios
      a = scanr (+) 0 xc
      b = scanl (+) 0 yc
      -- rowratios = M.fromList $ zip (map (\[x, y] -> x // y) celllrows) $ zip xacc yacc
      rp1 = map (+ 1) r -- r + 1
      rp1or = zipWith (/) rp1 r -- (r + 1) / r = 1 / xw
      aoxw = zipWith (*) rp1or a -- a / xw
      bo1mxw = zipWith (*) rp1 b -- b / (1 - xw)
      total = zipWith (+) aoxw bo1mxw -- a / xw + b / (1 - xw)
      xw = zipWith (/) r rp1
      yw = map (1 -) xw
      check = map (gridHeight celllrows) $ zipWith (\xw yw -> [xw, yw]) xw yw
      in
      [r, rp1or, xc, a, aoxw, rp1, yc, b, bo1mxw, total, xw, yw, check]

z cols = zipWith (:) ["r", "1 / xw", "x", "a", "a / xw", "1 / (1 - xw)", "y", "b", "b / (1 - xw)", "total", "xw", "yw", "check"] $ map2 show cols

outersperse x l = x : intersperse x l ++ [x]

xyz [r, rp1or, xacc, rp1, yacc] =
   let
      xacci = intersperse Nothing $ map Just xacc
      yacci = intersperse Nothing $ map Just yacc
      ri = outersperse Nothing $ map Just r
      rp1i = outersperse Nothing $ map Just rp1
      rp1ori = outersperse Nothing $ map Just rp1or
      in
      putGrid $ map2 showm [ri, rp1ori, xacci, rp1i, yacci]

showm (Just j) = show j
showm Nothing = ""

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
      colws = colWidths2 width tab

      in --do putStr $ showGrid 420 $ transpose $ map2 show celllsrows
            colWidths6A width celllsrows colws colws

-- repeatedly widen the column that has the most cells that are keeping their row from being lower
-- and narrow the column that has the least
colWidths6A :: Int -> [[Int]] -> [Int] -> [Int] -> [Int]
colWidths6A width celllrows colws colws1 = let
      colsums = zip (map sum $ transpose $ map (needsWidening colws) celllrows) [0..]
      mincol  = snd $ minimum colsums
      maxcol  = snd $ maximum colsums
      colws2  = extend colws1   1  maxcol
      colws3  = extend colws2 (-1) mincol

      in if colws3 == colws || colws3 == colws1
         then colws
         else colWidths6A width celllrows colws1 colws3
-}
showTerm1 (String1 s) = (0, 0, length s, s)
showTerm1 (Int1 i) = (length $ show i, 0, 0, show i)
showTerm1 (Double1 d) =
   let
      s = showFFloat (Just 3) d ""
      l = length s
      p = elemIndex '.' s
      in
      case p of
         Just j -> (j, l - j, 0, s)
         Nothing -> (l, 0, 0, s)

showTerm b a l (String1 s) = (0, 0, length s, padr l s)
showTerm b a l (Int1 i) = let s = show i in (length s, 0, 0, padl b s)
showTerm b a l (Double1 d) =
   let
      s = showFFloat (Just 3) d ""
      l = length s
      p = fromMaybe l $ elemIndex '.' s
      in
      (p, l - p, 0, replicate (b - p) ' ' ++ s ++ replicate (a - (l - p)) ' ')

showCol col =
   let
      (b, a, l, c) = unzip4 $ map (showTerm b2 a1 l2) col
      b1 = maximum b
      a1 = maximum a
      l1 = maximum l
      l2 = max (b1 + a1) l1
      b2 = max b1 (l2 - a1)
      in
      c

-- showGridF f width tab = showGrid1 (f (width - length tab) tab) tab
showGrid = showGridW width

showGridW = showGridF colWidths5

showGridF f width1 tab =
   let
      cellLengthsByCol = map2 length tab
      colWidths1 = colWidthsF cellLengthsByCol
      width = width1 - length tab
      colWidths =
         if sum colWidths1 < width
            then colWidths1
            else blah $ f width tab
      in
      showGrid1 colWidths $ padRWith "" tab

showGrid1 colws tab =
   let
      rows = transpose $ zipWith (\cw c -> map (\cell -> (cw, groupN cw cell)) c) colws tab
      rows2 =
         map
            ( \row ->
                  let m = maximum $ map (length . snd) row
                  in map (\(lc, cell) -> map (padr lc) $ padLWith1 "" m cell) row
            )
            rows
      in
      -- in newrows
      -- in length $ concat $ map2 (intercalate "|") $ map transpose rows2

      unlines $ concat $ map2 (intercalate "|") $ map transpose rows2

putGridW w = putStr . showGridW w

putGrid = putStr . showGrid

-- putGrid2 = putStr . showGridF colWidths5 420
putGridF t = putStr $ showGrid1 (colWidthsF $ map2 length t) $ padRWith "" t

firstRpt1 :: (Ord a) => S.Set a -> [a] -> [a]
firstRpt1 set (x : xs) = if S.member x set then [x] else x : firstRpt1 (S.insert x set) xs

firstRpt :: (Ord a) => [a] -> [a]
firstRpt = firstRpt1 S.empty

blah (f1, s, f2) = f2 $ last $ firstRpt $ iterate f1 s

bleh (f1, s, f2) = (r, f2 $ last r) where r = firstRpt $ iterate f1 s

tryGridF f width tab = bleh $ f (width - length tab) tab

putGridT tab = do
   let f = colWidths5
   let (colws1, colws) = bleh $ f (width - length tab) tab
   mapM_ (pp width) colws1
   pp width colws

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

mmm = 0
