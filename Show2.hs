module Show2 where

class Show2 a where
    show2 :: a -> Doc Char String

data Doc char str = DStr                 str 
                  | DGroup     [Doc char str] 
                  | DSeq       [Doc char str] 
                  | DLines              [str] 
                  | DTab      [[Doc char str]] 
                  | DRow       [Doc char str] -- should be made up of DStrs and VStretches
                  | DCol       [Doc char str] -- should be made up of DStrs and HStretches
                  | HStretch            [str]
                  | VStretch             str
                  | CharStr         char
                  deriving (Eq, Ord, Show)

data Doc2 str = Doc2 {docWidth :: Int, docHeight :: Int, docText :: [str]} deriving (Eq, Ord)

format1 ind (DStr string) = string
format1 ind (DGroup group) = concatMap (\item -> "\n" ++ replicate ind ' ' ++ format1 (ind + 3) item) group
format1 ind (DSeq docs) = concatMap (format1 ind) docs

sizes (DTab tab) = let
   (cws, rhs) = tabSizes tab

   in (sum cws, sum rhs)

tabSizes tab = let 
   sizes1 = map2 sizes tab
   colWidths  = map maximum $ transpose $ map2 fst sizes1
   rowHeights = map maximum $             map2 snd sizes1

   in (colWidths, rowHeights)

format2 (DTab tab) = let
   (colWidths, rowHeights) = tabSizes tab

   in zipWith (\rh row -> format3 cws rh row) rowHeights tab

format3 cw rh col = zipWith (format4 rh) cw col

format4 w h (DLines   lines1) = map (padRWith1 ' ' w) $ padRWith1 "" h lines1
format4 w h (HStretch lines1) = map (take w . cycle) lines1
format4 w h (VStretch line1 ) = replicate h line1

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

