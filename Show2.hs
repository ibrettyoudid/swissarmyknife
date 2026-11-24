module Show2 where

import Favs hiding (indent1, indent2)

import Prelude hiding (Left, Right)

import Data.List

import qualified Data.Map as M

class Show2 a where
   show2 :: a -> Doc Char String

data Doc char str = DStr                 str 
                  | DGroup     [Doc char str] 
                  | DSeq       [Doc char str] 
                  | DLines              [str] 
                  | DTab      [[Doc char str]] [Bool] [Bool]
                  | DRow       [Doc char str]
                  | DCol       [Doc char str]
                  | Stretch             [str]
                  | HStretch            [str]
                  | VStretch             str
                  | CharStr         char
                  | Align               [str] Align Align
                  | Space
                  deriving (Eq, Ord, Show)

data Align = Start | Centre | End | Repeat deriving (Eq, Ord, Show)

data Doc2 str = Doc2 {docWidth :: Int, docHeight :: Int, docText :: [str]} deriving (Eq, Ord)

format1 ind (DStr string) = string
format1 ind (DGroup group) = concatMap (\item -> "\n" ++ replicate ind ' ' ++ format1 (ind + 3) item) group
format1 ind (DSeq docs) = concatMap (format1 ind) docs

sizes (DTab tab _ _) = let
   (cws, rhs) = tabSizes tab

   in (sum cws, sum rhs)
sizes  Space        = (0, 0)
sizes (Align l z v) = (maximum (map length l), length l)

tabSizes tab = let 
   sizes1 = map2 sizes tab
   colWidths  = map maximum $ transpose $ map2 fst sizes1
   rowHeights = map maximum $             map2 snd sizes1

   in (colWidths, rowHeights)

format2 w h (DTab tab css rss) = let
   (cws, rhs) = tabSizes tab
   cws1 = sums $ zip css cws ++ [(False, 0), (True, 0)]
   rhs1 = sums $ zip rss rhs ++ [(False, 0), (True, 0)]
   wpm  = 1000 * w `div` (cws1 M.! True)
   hpm  = 1000 * h `div` (rhs1 M.! True)
   cws2 = zipWith (\s w -> if s then w * wpm `div` 1000 else w) css cws
   rhs2 = zipWith (\s h -> if s then h * hpm `div` 1000 else h) rss rhs

   in format5 $ zipWith (format3 cws2) rhs2 tab
format2 w h  doc      = format4 w h doc

format3 cws rh row = zipWith (format4 rh) cws row

format4 w h (DTab  lines c r) = format2 w h (DTab lines c r)
format4 w h  Space            = replicate h $ replicate w ' '
format4 w h (Align lines z v) = align v h (replicate w ' ') $ map (align z w ' ') lines

align Start  s c t = take s $ t ++ cycle [c]
align End    s c t = take s $ replicate (max 0 $ s - length t) c ++ t
align Centre s c t = take s $ replicate l1 c ++ t ++ replicate l2 c 
   where 
      l = max 0 $ s - length t
      l1 = div l 2
      l2 = l - l1
align Repeat s c t = take s $ cycle t

format5 lines = map concat $ concatMap transpose lines

--line (x0, y0) (x1, y1) lines = 
hline x0 x1 y lines  | x0 < x1 = modifyIndex y (\l -> take x0 l ++ replicate (x1-x0+1) '-' ++ drop (x1+1) l) lines
                     | otherwise = hline x1 x0 y lines
vline x y0 y1 lines  | y0 < y1 = take y0 lines ++ map (replaceIndex x '|') (take (y1-y0+1) $ drop y0 lines) ++ drop (y1+1) lines
                     | otherwise = vline x y1 y0 lines
dline x0 y0 x1 y1 lines | y0 > y1 = dline x1 y1 x0 y0 lines
                        | x0 > x1 = eline x0 y0 x1 y1 lines
                        | otherwise = take y0 lines ++ zipWith (\x -> replaceIndex x '\\') [x0..] (take (y1-y0+1) $ drop y0 lines) ++ drop (y1+1) lines
eline x0 y0 x1 y1 lines = take y0 lines ++ zipWith (\x -> replaceIndex x '/') [x0, x0-1 ..] (take (y1-y0+1) $ drop y0 lines) ++ drop (y1+1) lines

data Side = Left | Top | Right | Bottom deriving (Eq, Ord, Show)

cline x0 y0 x1 y1 diag side lines = let
   (x2, y2, x3, y3) = if y1 < y0 then (x1, y1, x0, y0) else (x0, y0, x1, y1)

   in if diag
         then if x3 < x2 then eline x2 y2 x3 y3 lines else dline x2 y2 x3 y3 lines
      else
         let
            sides Top    = hline x2 x3 y2
            sides Bottom = hline x2 x3 y3
            sides Left   = vline x2 y2 y3
            sides Right  = vline x3 y2 y3
            
            otherCorners = if x2 < x3 
               then [[Top, Right], [Bottom, Left ]]
               else [[Top, Left ], [Bottom, Right]]
         
            blah = fromMaybe (error "weird") $ find (elem side) otherCorners
            
         in foldr ($) lines $ map sides blah

mergeDocs (DSeq   s) = mergeSeq $ mergeStrs1 $ map mergeDocs s
mergeDocs (DGroup a) = DGroup $ map mergeDocs a
mergeDocs (DStr   s) = DStr s

mergeStrs1 [] = []
mergeStrs1 (DStr a : DStr b : cs) = mergeStrs1 (DStr (a ++ b) : cs)
mergeStrs1 (a : as) = a : mergeStrs1 as

mergeSeq1 [] = []
mergeSeq1 (DSeq a : DSeq b : cs) = mergeSeq1 (DSeq (a ++ b) : cs)
mergeSeq1 (a : as) = a : mergeSeq1 as

mergeSeq s = case mergeSeq1 $ mergeStrs1 s of
   [a] -> a
   b   -> DSeq b

wrap w d = d

t s = Doc2 (length s) 1 [s]
ts ss = Doc2 (maximum $ map length ss) (length ss) ss

toString (Doc2 _ _ ss) = unlines $ padcoll0 ss

minWidthDoc = minimumBy (compare `on` docWidth)
minHeightDoc ds = minimumBy (compare `on` docHeight) ds

minWidth ds = minimum $ map docWidth ds
minHeight ds = minimum $ map docHeight ds

maxWidth ds = maximum $ map docWidth ds
maxHeight ds = maximum $ map docHeight ds

sumHeight ds = sum $ map docHeight ds
sumWidth ds = sum $ map docWidth ds

fit :: Int -> [Doc2 a] -> Doc2 a
fit w opts = let f = filter ((<= w) . docWidth) opts in if null f then minWidthDoc opts else head f

indent1 n (Doc2 w h t) = Doc2 (w + n) h $ indent2 n t
indent2 n = map (replicate n ' ' ++)

-- sequential append
sapp (Doc2 aw ah at) (Doc2 bw bh (bth : btt)) =
   let
   ati = init at
   atl = last at
   in
   ts $ ati ++ ((atl ++ bth) : indent2 (length atl) btt)

(<\>) = sapp

scat ds = Prelude.foldl1 sapp ds

vcat ds =
   Doc2
   (maximum $ map docWidth ds)
   (sum $ map docHeight ds)
   (concat $ map docText ds)

vapp a b = vcat [a, b]

iapp a b = vapp a $ indent1 3 b

