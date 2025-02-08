-- Copyright 2025 Brett Curtis
{-# LANGUAGE NoMonomorphismRestriction #-}

module Favs (
  module Favs,
  fromMaybe,
  catMaybes,
  mapMaybe,
  listToMaybe,
  sortOn,
  fromJust,
  isJust,
)
where

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import Numeric hiding (readInt)
import System.IO
import System.Random hiding (split)

import Data.ByteString qualified as B
import Data.Map qualified as M
import Data.Set qualified as S
import System.IO.Unsafe

-- foldl f   z [a,b,c] = f (f (f z a) b) c
-- foldl (+) 0 [a,b,c] = ((0+a)+b)+c)

-- foldr f   z [a,b,c] = f a (f b (f c z))
-- foldr (+) 0 [a,b,c] = a+(b+(c+0))

-- foldl f   z [] = z
-- foldl f   z (x:xs) = f x $ foldl f z xs

-- foldr (+) z $ map g [a,b,c] = g a+(g b+(g c+z))
-- foldr ((+) . g) z [a,b,c]   = g a+(g b+(g c+z))
infixr 9 $=
f $= b = \x -> f x == b
f $< b = \x -> b `isPrefixOf` f x

replaceIndex index new list =
  let
    (before, _ : after) = splitAt index list
   in
    before ++ new : after

deleteIndex index list = let (before, _ : after) = splitAt index list in before ++ after

insertIndex index new list =
  let
    (before, after) = splitAt index list
   in
    before ++ new : after

insertWith2 combine zero (k, v) map = M.alter f k map
 where
  f Nothing = Just $ combine v zero
  f (Just v2) = Just $ combine v v2

mapFromList combine zero list = foldr (insertWith2 combine zero) M.empty list
foldIntoMap = mapFromList

combine f zero list = M.toList $ mapFromList f zero list
multimapOn f = mapFromList (:) [] . mapfxx f

for lst fun = map fun lst

-- ppFor lst fun = mapM_ (\d -> putStr (show d ++ "  " ++ show (fun d) ++ "\n")) lst
-- ppFor lst fun = putStr $ concatMap (\d -> show d ++ "  " ++ show (fun d) ++ "\n") lst
ppFor :: (Show a, Show b) => [a] -> (a -> b) -> IO ()
ppFor = flip ppMap

ppMap :: (Show a, Show b) => (a -> b) -> [a] -> IO ()
ppMap fun lst = ppTableS $ map (\e -> [show e, show $ fun e]) lst

ppListT :: (Show a, Show b) => [(a, b)] -> IO ()
ppListT lst = ppTableS $ map (\(a, b) -> [show a, show b]) lst
ppListT4 lst = ppTableS $ map (\(a, b, c, d) -> [show a, show b, show c, show d]) lst

ppList lst = putStr $ intercalate1 "\n" $ map show lst

-- readInt = read
readInt1 = foldl (\a b -> a * 10 + ord b - ord '0') 0

readInt = readInt1 . takeWhile isDigit . filter (`notElem` ", ")

readNum :: (Read a, Num a) => String -> a
readNum = (\r -> if null r then 0 else read r) . takeWhile (`elem` ".+-eE0123456789") . filter (`notElem` ", ")

readFileB f = do
  handle <- openBinaryFile f ReadMode
  hGetContents handle

readFileU f = unsafePerformIO $ readFileB f

(.:) f g x y = f (g x y)

on f g x y = f (g x) (g y)

-- repeatN :: Integer -> a -> [a]
repeatN 0 x = []
repeatN n x = x : repeatN (n - 1) x

iterateN n f x = genericTake n $ iterate f x

substr f n = take n . drop f

all2 p l = and $ zipWith p l (tail l)

allEqual = all2 (==)
allDiff = all2 (/=)

-- same as fromJust
unjust (Just j) = j

-- same as iterate!
-- iterateL f x = let y = f x in y : iterateL f y

fixN _ x 0 = x
fixN f x n = fixN f (f x) (n - 1)

trim = trimtrailing . dropWhile (== ' ')

trimtrailing [] = []
trimtrailing " " = []
trimtrailing (x : xs) = let xst = trimtrailing xs in if x == ' ' && null xst then [] else x : xst

split sep = splitWith (stripPrefix sep)

splitSeps seps = splitWith (justPrefixs seps)

split1 sep = split1With (stripPrefix sep)

split1M sep = split1WithM (stripPrefix sep)

splitWith pred = unfoldr (\s -> ifJust (not $ null s) $ split1With pred s)

split1With pred str = firstJustElse (str, []) (zipWith (\a b -> (a,) <$> pred b) (inits str) (tails str))

split1WithM pred str = case catMaybes $ zipWith (\a b -> (a,) <$> pred b) (inits str) (tails str) of
  [] -> Nothing
  (x : _) -> Just x

-- disallow zero length entries
splitNZ :: (Eq a) => [a] -> [a] -> [[a]]
splitNZ sep str = filter (not . null) $ split sep str

{-
split :: (Eq a) => [a] -> [a] -> [[a]]
split sep str = let l                   = length sep
                    split2 accum []     = [accum]
                    split2 accum (x:xs) = let (tak, drp) = splitAt l (x:xs)

                                          in if tak == sep
                                                   then accum : split2 [] drp
                                                   else split2 (accum ++ [x]) xs

                in split2 [] str
-}

-- note that pred has type [a] -> Bool
-- splitWhen (\s -> take 2 s == "--") "hello--there--my--friends"   =   ["hello
split1When :: ([a] -> Bool) -> [a] -> ([a], [a])
split1When _ [] = ([], [])
split1When pred str =
  if pred str
    then ([], str)
    else let (before, after) = split1When pred (tail str) in (head str : before, after)

-- pred returns Just rest on successful split
-- split once
split1WithB _ [] = ([], [])
split1WithB pred str = case pred str of
  Just rest -> ([], rest)
  Nothing ->
    let
      (before, after) = split1WithB pred (tail str)
     in
      (head str : before, after)

split1WithA1 pred str = case findIndex pred str of
  Just n -> splitAt n str
  Nothing -> (str, [])

-- split all
-- splitWith pred str = unfoldr (\s -> let (a,b) = split1With pred s in if a /= [] || b /= [] then Just (a,b) else Nothing) str
left (Left l) = l
left (Right r) = error (show r)
right (Left l) = error (show l)
right (Right r) = r

-- this is in Data.Maybe
{-
fromJust (Just j) = j
dfault _ (Just j) = j
dfault d Nothing  = d

-- this is in Data.Maybe
catMaybes []           = []
catMaybes (Just  x:xs) = x:catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs

-- this is in Data.Maybe
mapMaybe f l = catMaybes $ map f l

listToMaybe [   ] = Nothing
listToMaybe (x:_) = Just x

fromMaybe d Nothing = d
fromMaybe d (Just x) = x
-}
-- isPrefixOf pre str = take (length pre) str == pre
justPrefixs pres str = firstJust1 $ map (`stripPrefix` str) pres

justIf x bool = if bool then Just x else Nothing
ifJust bool x = if bool then Just x else Nothing
ifPred pred x = ifJust (pred x) x

filterMaybe pred x = if pred x then Just x else Nothing

-- predJust pred res = do guard $ pred res; return res

-- return the first Just, without the Just, error if there is none
-- firstJust :: [Maybe a] -> a
-- firstJust (Just x :_ ) = x
-- firstJust (Nothing:xs) = firstJust xs

firstJust = head . catMaybes

-- return the first Just, as a Just, if there is one, otherwise Nothing
-- probably better to use catMaybes
-- firstJust1 = foldl mplus Nothing
firstJust1 = listToMaybe . catMaybes

-- firstJustElse (Just x :_ ) _     = x
-- firstJustElse (Nothing:xs) else1 = firstJustElse xs else1
-- firstJustElse []           else1 = else1

-- firstJustElse xs else1 = fromMaybe else1 $ listToMaybe $ catMaybes xs
firstJustElse :: a -> [Maybe a] -> a
firstJustElse else1 = fromMaybe else1 . firstJust1

mapFJ f = firstJust . map f

forFJ = flip mapFJ

mapFJE :: b -> (a -> Maybe b) -> [a] -> b
mapFJE else1 f = firstJustElse else1 . map f

forFJE else1 xs f = mapFJE else1 f xs

unfoldr1 f x =
  let (a, b) = f x
   in a : unfoldr1 f b

tapp (f, g) (x, y) = (f x, g y)
tmap f (x, y) = (f x, f y)
tzip f (x, y) = f x y
tflip (x, y) = (y, x)

tmapM f (x, y) = do x1 <- f x; y1 <- f y; return (x1, y1)
tappM (f, g) (x, y) = do x1 <- f x; y1 <- g y; return (x1, y1)

-- groupN n lst = if length lst >= n then take n lst : groupN n (drop n lst) else []
-- groupN n = unfoldr ((\x -> if snd x == [] then Nothing else x) . splitAt n)
-- groupN n = unfoldr (predJust (\x -> snd x /= []) . splitAt n)
-- groupN n = unfoldr (predJust ((/= []) . snd) . splitAt n)
-- groupN n = unfoldr (predJust (\(x,y) -> y /= []) . splitAt n)
spat ([], []) = Nothing
spat (a, b) = Just (a, b)

groupN n = unfoldr (spat . splitAt n)

replace from to lst =
  let l = length from
      rep [] = []
      rep (x : xs) =
        let (tak, drp) = splitAt l (x : xs)
         in if tak == from
              then to ++ rep drp
              else x : rep xs
   in rep lst

-- indent all but first line
indent1 n str = indent2 (replicate n ' ') str

indent2 i [] = []
indent2 i ['\n'] = ['\n']
indent2 i ('\n' : xs) = '\n' : i ++ indent2 i xs
indent2 i (x : xs) = x : indent2 i xs

-- indent all
indent n str = indent1 n (replicate n ' ' ++ str)

map2 = map . map
map3 = map . map . map
map4 = map . map . map . map

f1 x = map (x :)
cross xs ys = map (\y -> map (\x -> (x, y)) xs) ys
cross3 xs ys zs = map (\z -> map (\y -> map (\x -> (x, y, z)) xs) ys) zs
cross4 ws xs ys zs = map (\z -> map (\y -> map (\x -> map (\w -> (w, x, y, z)) ws) xs) ys) zs
cross5 vs ws xs ys zs = map (\z -> map (\y -> map (\x -> map (\w -> map (\v -> (v, w, x, y, z)) vs) ws) xs) ys) zs
cross6 us vs ws xs ys zs = map (\z -> map (\y -> map (\x -> map (\w -> map (\v -> map (\u -> (u, v, w, x, y, z)) us) vs) ws) xs) ys) zs
cross7 ts us vs ws xs ys zs = map (\z -> map (\y -> map (\x -> map (\w -> map (\v -> map (\u -> map (\t -> (t, u, v, w, x, y, z)) ts) us) vs) ws) xs) ys) zs

crossListA xs ys = concatMap (\x -> map (x :) ys) xs

crossList :: [[a]] -> [[a]]
crossList = foldr crossListA [[]]

f2 xs ys = map (\x -> map (x :) ys) xs

f3 xs ys = f2 xs $ f2 ys []

-- crossWith f xs ys = map (\y -> map (\x -> f x y) xs) ys

-- crossWithL :: (a -> a -> a) -> [a] -> [a] -> [[a]]
crossWithL f xs ys = (0 : xs) : for ys (\y -> y : for xs (`f` y))

crossWith f xs ys = map (\y -> map (\x -> f x y) xs) ys
crossWith3 f xs ys zs = map (\z -> map (\y -> map (\x -> f x y z) xs) ys) zs
crossWith4 f ws xs ys zs = map (\z -> map (\y -> map (\x -> map (\w -> f w x y z) ws) xs) ys) zs
crossWith5 f vs ws xs ys zs = map (\z -> map (\y -> map (\x -> map (\w -> map (\v -> f v w x y z) vs) ws) xs) ys) zs
crossWith6 f us vs ws xs ys zs = map (\z -> map (\y -> map (\x -> map (\w -> map (\v -> map (\u -> f u v w x y z) us) vs) ws) xs) ys) zs
crossWith7 f ts us vs ws xs ys zs = map (\z -> map (\y -> map (\x -> map (\w -> map (\v -> map (\u -> map (\t -> f t u v w x y z) ts) us) vs) ws) xs) ys) zs

crossWithR f xs ys = map (\x -> map (f x) ys) xs

ppCross f xs ys =
  let shownxs = map show xs
      shownys = map show ys
      tab1 = crossWith (show .: f) xs ys
      tab2 = ("" : shownxs) : zipWith (:) shownys tab1
      tab3 = map (\(a : b) -> a : "|" : b) tab2
      (h : t) = map concat $ padTable tab3
      tab5 = h : replicate (length h) '-' : t
   in putStr $ intercalate1 "\n" tab5

{-
ppCross f xs ys = let shownxs = map show xs
                      shownys = map show ys
                  in
                      putStr
                      $ intercalate1 "\n"
                      $ (\(h:t) -> h : replicate (length h) '-': t)
                      $ map concat
                      $ padTable
                      $ map (\(a:b) -> a:"|":b)
                      $ ("" : shownxs) : (zipWith (:) shownys $ crossWith ((show .) . f) xs ys)
-}
padl = padLWith1 ' '
padr = padRWith1 ' '
padq True = padl
padq False = padr
pad0 = padLWith1 '0'
isNum str = let l = length $ filter isDigit str in l * 2 >= length str
pad n str = padq (isNum str) n str

padLWith1 z n list = replicate (n - length list) z ++ list

padRWith1 z n list = list ++ replicate (n - length list) z

padLWith z lists =
  let
    m = maximum $ map length lists
   in
    map (padLWith1 z m) lists

padRWith z lists =
  let
    m = maximum $ map length lists
   in
    map (padRWith1 z m) lists

transpose1 = transposez []

transposez z = transpose . padRWith z

padshowcol col = padcol $ map show col

padcol :: [String] -> [String]
padcol shown = map (pad $ (+ 1) $ maximum $ map length shown) shown
padcoll shown = map (('|' :) . padl (maximum $ map length shown)) shown
padcol0 shown = map (pad $ maximum $ map length shown) shown
padcoll0 shown = map (padl $ maximum $ map length shown) shown

putTable1 :: [[String]] -> IO ()
putTable1 = mapM_ (putStrLn . concat) . transpose . map padcol

mapxfx f xs = map (withxfx f) xs
mapfxx f xs = map (withfxx f) xs
withxfx f x = (x, f x)
withfxx f x = (f x, x)

orPred p q x = p x || q x
infixr 2 `orPred`
andPred p q x = p x && q x
infixr 3 `andPred`

filterMap f = catMaybes . map f

-- sortOn  f xs = map snd $ sort $ mapfxx f xs
rsort xs = sortBy (flip compare) xs
rsortOn f xs = map snd $ rsort $ mapfxx f xs

groupOn f = combine (:) [] . mapfxx f

minOn f xs = M.findMin $ M.fromListWith const $ mapfxx f xs

-- maxOn takes a function and a list and maps the function over the list
-- then returns a pair (maxresult, maxelement)
maxOn :: (Ord k) => (a -> k) -> [a] -> (k, a)
maxOn f xs = M.findMax $ M.fromListWith const $ mapfxx f xs

range xs = (minimum xs, maximum xs)
rangeOn f xs =
  let
    map1 = M.fromListWith const $ mapfxx f xs
    min1 = M.findMin map1
    max1 = M.findMax map1
   in
    (min1, max1)

table1a cols =
  let lengths = map2 length cols
      maxlens = map maximum lengths
      padded = zipWith (\maxl col -> map (pad (maxl + 1)) col) maxlens cols
      rows2 = transpose padded
      result = intercalate1 "\n" $ map concat rows2
   in putStr result

infixl 0 >-
a >- b = b $ a

infixl 9 >.
a >. b = b . a

table1b rows =
  rows
    >- transpose
    >- map padcol
    >- transpose
    >- map concat
    >- intercalate1 "\n"
    >- putStr

-- equalise the widths in a list of lists of strings
padTable = transpose . map padcoll . transpose

{-
ppTable1 = putStr . intercalate1 "\n" . map concat . padTable
table    = putTable . padTable
-}
ppTableS = putStr . intercalate1 "\n" . map concat . padTable

ppTable :: (Show a) => [[a]] -> IO ()
ppTable = putStr . intercalate1 "\n" . map concat . padTable . map2 show

table1c rows =
  transpose $
    map
      ( \col ->
          let res1 =
                map
                  ( \el ->
                      let sh = show el
                          le = length sh
                          padded = pad (maxl + 1) sh
                       in (le, padded)
                  )
                  col
              maxl = maximum $ map fst res1
           in map snd res1
      )
      (transpose rows)

intersperse1 _ [] = []
intersperse1 sep (x : xs) = x : sep : intersperse1 sep xs

intercalate1 sep xs = concat (intersperse1 sep xs)

-- not sure what this does
-- rsorted (x:y:ys) = if x > y then rsorted (y:ys) else (x,y):rsorted (y:ys)

-- merge  = merge a sorted list of sorted lists
-- rmerge = merge a reverse sorted list of reverse sorted lists

headComp f (l1 : ls1) (l2 : ls2) = f l1 l2

mergeBy1 f [] = []
mergeBy1 f ((x : xs) : ys) =
  x
    : if xs == []
      then mergeBy1 f ys
      else mergeBy1 f (insertBy f xs ys)

mergeBy f lst = mergeBy1 f $ filter (/= []) lst

merge :: (Ord a) => [[a]] -> [a]
merge = mergeBy (headComp compare)

-- merge = mergeS

mergeS = merge2 . S.fromList . map HCList . filter (/= [])

merge1 set =
  if S.null set
    then []
    else
      let
        (m : ms) = S.findMin set
        set1 = S.deleteMin set
       in
        m : merge1 (if ms == [] then set1 else S.insert ms set1)

merge2 set =
  if S.null set
    then []
    else
      let
        HCList (m : ms) = S.findMin set
        set1 = S.deleteMin set
       in
        m : merge2 (if ms == [] then set1 else S.insert (HCList ms) set1)

data HCList a = HCList [a] deriving (Eq)

instance (Ord a) => Ord (HCList a) where
  compare (HCList a) (HCList b) = compare (head a) (head b)

rmerge :: (Ord a) => [[a]] -> [a]
rmerge = mergeBy (headComp (flip compare))

randChar :: IO Char
randChar = randomRIO (chr 33, chr 126)

-- make up a random password of n characters
password n = sequence $ replicate n randChar

passworda n = sequence $ replicate n $ randomRIO ('a', 'z')

noise = do
  c <- randChar
  putChar c
  noise

gears from to n = take (round n) $ iterate (* ((to / from) ** (1 / (n - 1)))) from

decodeurl ('%' : a : b : rest) = chr (decodehex a * 16 + decodehex b) : decodeurl rest
decodeurl (a : rest) = a : decodeurl rest
decodeurl [] = []

decodehex a
  | isDigit a = ord a - ord '0'
  | a >= 'a' && a <= 'f' = ord a - ord 'a' + 10
  | a >= 'A' && a <= 'F' = ord a - ord 'A' + 10
  | otherwise = -1

{-
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f zero xs0 = lgo zero xs0
             where
                lgo accum []     = z
                lgo accum (x:xs) = lgo (f z x) xs

foldl f   z [a,b,c] = f (f (f z a) b) c
                    =
foldl (+) z [a,b,c] = z + a + b + c

foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldr f z xs = go xs
               where
                 go []     = z
                 go (x:xs) = f x (go xs)

foldr f   z [a,b,c] = f a (f b (f c z))

foldr (+) z [a,b,c] = a + (b + (c + z))
-}

intersectionByS comp [] _ = []
intersectionByS comp _ [] = []
intersectionByS comp (x : xs) (y : ys) = case comp x y of
  EQ -> x : intersectionByS comp xs ys
  LT -> intersectionByS comp xs (y : ys)
  GT -> intersectionByS comp (x : xs) ys

intersectionS = intersectionByS compare

-- nub optimised for a sorted list
nubSorted [] = []
nubSorted [x] = [x]
nubSorted (x : y : ys) = if x == y then nubSorted (y : ys) else x : nubSorted (y : ys)

-- nub using a set
nubSet = S.toList . S.fromList

-- nub (kind of) using a map
-- Ord k => [(k, a)] -> [(k, [a])]
nubMulti = M.toList . multiMapFromList

{-
counts :: (Ord a) => [a] -> [(Int, a)]
counts = rsort . map tflip . counts1

counts1 :: Ord a => [a] -> [(a, Int)]
counts1 = combine (+) 0 . map (, 1)

counts1 = mapFromList (+) 0 . map (, 1)
-}
countUnique l = S.size $ S.fromList l

counts = rsort . counts0
counts0 = map tflip . M.toList . counts1
counts1 = foldr (\key -> M.insertWith (+) key 1) M.empty
sums = foldr (\(key, val) -> M.insertWith (+) key val) M.empty
sumss = foldr (\(key, vals) -> M.insertWith (zipWith (+)) key vals) M.empty
mode = snd . maximum . counts0
mean xs = sum xs / fromIntegral (length xs)
imean xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- keys that are in both (inner join)
-- Map k a -> Map k b -> Map k (a, b)
joinMaps l r = M.intersectionWith (,) l r

-- keys that are in left (left join)
-- Map k a -> Map k b -> Map k (a, Maybe b)
joinMapsLeft1 l r = M.mapWithKey (\k lv -> (lv, M.lookup k r)) l
joinMapsLeft l r = M.union (M.map (\(a, b) -> (a, Just b)) $ joinMaps l r) (M.map (,Nothing) $ l M.\\ r)

-- keys not in right
-- Map k a -> Map k b -> Map k a
joinMapsNotRight l r = l M.\\ r

-- joinMapsNotRight l r = M.mapMaybeWithKey (\k lv -> if isJust $ M.lookup k r then Nothing else Just lv) l

-- Map k a -> Map k b -> Map k (Maybe a, Maybe b)
-- keys in either
joinMapsOuter l r =
  let
    inner = M.map (\(a, b) -> (Just a, Just b)) $ joinMaps l r
    left1 = M.map (\a -> (Just a, Nothing)) $ l M.\\ r
    right1 = M.map (\b -> (Nothing, Just b)) $ r M.\\ l
   in
    left1 `M.union` inner `M.union` right1

joinMapsOuter2 l r =
  let
    l1 = M.map (\(lv, rv) -> (Just lv, rv)) $ joinMapsLeft l r
    r1 = M.map (\rv -> (Nothing, Just rv)) $ joinMapsNotRight r l
   in
    M.union l1 r1

-- f is used to combine records instead of ( , )
-- (l -> r -> x) -> M.Map k l -> M.Map k r -> M.Map k x
joinMapsWith f l r = M.mapMaybeWithKey (\k l1 -> do r1 <- M.lookup k r; return (f l1 r1)) l

-- multimap functions, on type Map k [r]
lofmb (Just r) = r
lofmb Nothing = []

multiMapFromList l = M.fromListWith (++) $ map (\(k, v) -> (k, [v])) l

joinMMapsLeft l r = M.map (\(l, r) -> ([l], lofmb r)) $ joinMapsLeft l r

joinMMapsOuter l r = M.map (\(l, r) -> (lofmb l, lofmb r)) $ joinMapsOuter l r

mofl = M.fromList . map (\(x : xs) -> (x, xs))

-- join lists by first element of tuple (key)
lofm = map (\(x, xs) -> x : xs) . M.toList

lofmbn n (Just r) = r
lofmbn n Nothing = n

joinLists2 z l r =
  let
    ae = replicate (length $ M.findMin l) z
    be = replicate (length $ M.findMin r) z
    inner = M.map (\(a, b) -> a ++ b) $ joinMaps l r
    left1 = M.map (\a -> a ++ be) $ l M.\\ r
    right1 = M.map (\b -> ae ++ b) $ r M.\\ l
   in
    left1 `M.union` inner `M.union` right1

joinLists z ls = lofm $ foldr1 (joinLists2 z) $ map mofl ls

join3 (fl, fi, fr) l r = (M.map fl (l M.\\ r), M.intersectionWith fi l r, M.map fr (r M.\\ l))

join3a fi l r = (l M.\\ r, M.intersectionWith fi l r, r M.\\ l)

join3b z (lt, l) (rt, r) =
  let
    ae = replicate (length lt) z
    be = replicate (length rt) z
   in
    join3 ((++ be), (++), (ae ++)) l r

joinTMapsFuzzy z (lt, l) (rt, r) =
  let
    lz = replicate (length lt) z
    rz = replicate (length rt) z
   in
    (lt ++ rt, joinMapsFuzzy ((++ rz), (++), (lz ++)) l r)

joinMapsFuzzy (fl, fi, fr) l r =
  let
    (l1, i, r1) = (l M.\\ r, M.intersectionWith fi l r, r M.\\ l)
    levs = sort $ concat $ crossWith (\(lk, lv) (rk, rv) -> (levenshtein lk rk, lk, rk, lv ++ rv)) (M.toList l1) (M.toList r1)
    (l2, i2, r2) = foldr joinMapsFuzzyAux (l1, i, r1) levs
   in
    (M.map fl l2) `M.union` i2 `M.union` (M.map fr r2)

joinMapsFuzzyAux (lev, lk, rk, a) (lks, res, rks) =
  if M.member lk lks && M.member rk rks
    then (M.delete lk lks, M.insert lk a res, M.delete rk rks)
    else (lks, res, rks)

trimBrackets1 l r [] = []
trimBrackets1 l r a = case elemIndex l a of
  Just c ->
    let (b, d) = splitAt c a
     in case elemIndex r d of
          Just f -> let (e, g) = splitAt (f + 1) d in b ++ trimBrackets1 l r g
          Nothing -> a
  Nothing -> a

trimBrackets = trim . trimBrackets1 '[' ']' . trimBrackets1 '(' ')'

isNumeric xs =
  let
    c = sum $ map length xs
    d = sum $ map (length . filter isDigit) xs
   in
    d * 100 > c * 75

tmofl ((_ : x) : xs) =
  let
    (as, bs) = unzip $ map (\(d : ds) -> (d, ds)) xs
    cs = map trimBrackets as
   in
    if isNumeric cs then tmofl (x : bs) else (x, mofl $ zipWith (:) cs bs)
tmofl _ = ([], M.empty)

tmofl1 t ((_ : x) : xs) =
  let
    (as, bs) = unzip $ map (\(d : ds) -> (d, ds)) xs
    cs = map trimBrackets as
   in
    if isNumeric cs then tmofl1 t (x : bs) else (map (t,) x, mofl $ zipWith (:) cs bs)
tmofl1 t _ = ([], M.empty)

loftm z (x, xs) = ((z : x) : lofm xs)

loftm1 z (x, xs) = ((fst z : map fst x) : (snd z : map snd x) : lofm xs)

joinTLists z ls = loftm z $ foldr1 (joinTMapsFuzzy z) $ map tmofl ls

foldm f z xs = foldm0 f z $ groupN 2 xs

foldm0 f z xss =
  let
    ys = map (foldm3 f) xss
   in
    if length ys <= 1
      then head ys
      else foldm0 f z $ groupN 2 ys

foldm1 f xs = foldm2 f $ groupN 2 xs

foldm2 f xss =
  let
    ys = map (foldm3 f) xss
   in
    if length ys <= 1
      then head ys
      else foldm2 f $ groupN 2 ys

foldm3 f [] = error "[] in foldm3"
foldm3 f [a] = a
foldm3 f [a, b] = f a b

scanm1 f xs = scanm2 f $ groupN 2 xs

scanm2 :: (a -> a -> a) -> [[a]] -> [a]
scanm2 f xss =
  let
    ys = map (foldm3 f) xss
   in
    if length ys <= 1
      then ys
      else ys ++ scanm2 f (groupN 2 ys)

joinTLists1 z ls = (concat titles :) $ joinLists z ls1
 where
  titles = map (\(x : xs) -> x) ls
  ls1 = map (\(x : xs) -> xs) ls

g987 (k, r) [] = [(k, [r])]
g987 (k, r) ((ks, rs) : xs) = if k == ks then (k, r : rs) : xs else (k, [r]) : (ks, rs) : xs

groupT = foldr g987 []

-- groupT ((a,b):cs) = foldr (\(k,r) ((ks,rs):xs) -> if k == ks then (k,r:rs):xs else (k,[r]):(ks,rs):xs) [(a,[b])] cs

{-
joinLists1 l0 r0 =
   let
      l = group $ sort l0
      r = group $ sort r0

   in
-}
joinLists1 l r =
  map
    ( \(k, l1) -> case lookup k r of
        Just r1 -> (k, l1, r1)
        Nothing -> (k, l1, [])
    )
    l

swapMap :: (Ord k, Ord a) => M.Map a k -> M.Map k a
swapMap = M.fromList . map swap . M.toList

swap (a, b) = (b, a)

commonSubsequences a b =
  let
    c = crossWith commonSubsequencesA [0 .. length a] [0 .. length b]
    commonSubsequencesA an bn
      | an == 0 || bn == 0 = ([], [], 0, 1, True)
      | otherwise =
          let
            (as, at, al, aw, ap) = c !! (an - 1) !! bn
            (bs, bt, bl, bw, bp) = c !! an !! (bn - 1)
            (cs, ct, cl, cw, cp) = c !! (an - 1) !! (bn - 1)

            ac = a !! (an - 1)
            bc = b !! (bn - 1)
           in
            if ac == bc
              then (ac : cs, ct, cl + 1, cw, True)
              else
                if al > bl || al == bl && aw < bw
                  then
                    if null as
                      then ([], at, al, aw, False)
                      else ([], as : at, al, aw + 1, False)
                  else
                    if null bs
                      then ([], bt, bl, bw, False)
                      else ([], bs : bt, bl, bw + 1, False)
    (r, rs, _, _, _) = c !! length a !! length b
   in
    reverse $ map reverse $ if null r then rs else r : rs

levenshtein a b =
  let
    c = crossWith commonSubsequencesA [0 .. length a] [0 .. length b]
    commonSubsequencesA an bn
      | an == 0 || bn == 0 = 0
      | otherwise =
          let
            al = c !! (an - 1) !! bn
            bl = c !! an !! (bn - 1)
            cl = c !! (an - 1) !! (bn - 1)

            ac = a !! (an - 1)
            bc = b !! (bn - 1)
           in
            if ac == bc
              then cl
              else
                if al < bl
                  then al + 1
                  else bl + 1
   in
    c !! length a !! length b

dsqrt n = floor $ sqrt $ fromIntegral n
fsqrt = floor . sqrt . fromIntegral
csqrt n = ceiling $ sqrt $ fromIntegral n

csqrt1 n = until (\x -> x * x >= n) (+ 1) $ csqrt n

{- searches a list for a sublist -}
subIndex sub lst = findIndex (isPrefixOf sub) $ tails lst
subIndices sub lst = findIndices (isPrefixOf sub) $ tails lst

after sub lst = do s <- subIndex sub lst; return $ drop s lst

before sub lst = do s <- subIndex sub lst; return $ take s lst

sbreak sub lst = aux 0 lst
 where
  aux _ [] = Nothing
  aux done todo = if isPrefixOf sub todo then Just (take done lst, drop (length sub) todo) else aux (done + 1) (tail todo)

sbetween a b lst = do lsta <- after a lst; before b lsta
sbetweens a b lst = case sbreak a lst of
  Nothing -> []
  Just (_, l1) -> case sbreak b l1 of
    Nothing -> []
    Just (l2, l3) -> l2 : sbetweens a b l3

readHx = fst . head . readHex

{-
enumFTSC from to step count = if from <= to && count > 0 then from:enumFTSC (from+step) to step (count-1) else []

enumFTS from to step        = if from <= to then from:enumFTS (from+step) to step                  else []
enumFTC from to      count  =                         enumFTS from        to ((to - from) / count)
enumFSC from    step count  = if count > 0  then from:enumFSC (from+step)    step (count-1)        else []

enumFT  from to             = if from <= to then from:enumFT (from+1)    to                        else []
enumFS  from    step        =                    from:enumFS (from+step)     step
enumFC  from         count  = if count > 0  then from:enumFC (from+1)             (count-1)        else []

enumTSC      to step count  = enumFSC (to-step*count) step count

enumTC       to      count  = enumFC  (to-count) count

enumFtc  from to      count = if count > 0 && not (to from) then from:enumFtc (from+1) to (count-1)
enumFtsc from to step count = if count > 0 && not (to from) then from:enumFtsc (step from) to (count-1)
enumFts  from to step       = if not (to from) then from:enumFts (step from) to
-}
{-
to = takeWhile
step = iterate
count = take
-}

enumFtsc from to step count = take count $ takeWhile to $ iterate step from

enumFts from to step = takeWhile to $ iterate step from
enumFtc from to count = enumFts from (< to) (+ (div (to - from) count))
enumFsc from step count = take count $ iterate step from

-- enumFt   from to            =                             iterate (+1) from
enumFs from step = iterate step from
enumFc from count = take count $ iterate (+ 1) from

-- showFF5 :: Double -> String
showFF5 n = showFF 5 n

-- showFF17 :: Double -> String
showFF17 n = showFF 17 n

-- showFF :: Int -> Double -> String
showFF d n = pad (d + 3) $ showFFloat (Just d) n ""

-- secantMethod::(Double -> Double) -> Double -> Double -> Double -> Int -> Double
secantMethod f n lx rx targety = search lx (f lx) rx (f rx) n
 where
  search lx ly rx ry n =
    let
      x = lx + (targety - ly) / (ry - ly) * (rx - lx)
      y = f x
     in
      if n <= 0
        then x
        else
          if y < targety
            then search x y rx ry (n - 1)
            else search lx ly x y (n - 1)

-- secantMethodD::(Double -> Double) -> Double -> Double -> Double -> Int -> IO Double
secantMethodD f n lx rx targety = do
  putStrLn ("targety=" ++ showFF17 targety)
  search lx (f lx) rx (f rx) n
 where
  search lx ly rx ry n = do
    let x = lx + (targety - ly) / (ry - ly) * (rx - lx)
    let y = f x
    putStrLn ("lx=" ++ showFF5 lx ++ " ly=" ++ showFF5 ly ++ " rx=" ++ showFF5 rx ++ " ry=" ++ showFF5 ry ++ " x=" ++ showFF5 x ++ " y=" ++ showFF17 y)
    if y == targety || n <= 0
      then return x
      else
        if y < targety
          then search x y rx ry (n - 1)
          else search lx ly x y (n - 1)

falsePos f epsilon findy = falsePos1 f epsilon findy $ getBrackets f findy

falsePos1 f epsilon findy = loop
 where
  loop ((lx, ly), (rx, ry)) =
    if abs y < epsilon
      then x
      else case signum ly * signum y of
        1 -> loop ((x, y), (rx, ry))
        0 -> x
        -1 -> loop ((lx, ly), (x, y))
   where
    x = (lx * ry - rx * ly) / (ry - ly)
    y = f x - findy

itp1 f epsilon findy lx rx = itp2 0.1 2 1 f epsilon findy ((lx, f lx), (rx, f rx))

itp2 k1 k2 n0 f epsilon findy ((lx, ly), (rx, ry)) = loop 0 ((lx, ly), (rx, ry))
 where
  nh = ceiling $ logBase 2 ((rx - lx) / (2 * epsilon))
  nmax = nh + n0
  loop j ((lx, ly), (rx, ry)) =
    let
      r = epsilon * 2 ^ (nmax - j) - (rx - lx) / 2
      xh = (lx + rx) / 2
      xf = (ry * lx - ly * rx) / (ry - ly)
      sigma = signum (xh - xf)
      delta = k1 * abs (rx - lx) ^ k2
      xt = if delta <= abs (xh - xf) then xf + sigma * delta else xh
      xitp = if abs (xt - xh) <= r then xt else xh - sigma * r
      yitp = f xitp - findy
     in
      if rx - lx > 2 * epsilon
        then case signum yitp of
          1 -> loop (j + 1) ((lx, ly), (xitp, yitp))
          0 -> xitp
          -1 -> loop (j + 1) ((xitp, yitp), (rx, ry))
        else xh

itp1d k1 k2 n0 f epsilon findy lx rx = itp2d k1 k2 n0 f epsilon findy ((lx, f lx), (rx, f rx))

itp2d k1 k2 n0 f epsilon findy ((lx, ly), (rx, ry)) = loop 0 ((lx, ly), (rx, ry))
 where
  nh = ceiling $ logBase 2 ((rx - lx) / (2 * epsilon))
  nmax = nh + n0
  loop j ((lx, ly), (rx, ry)) =
    let
      r = epsilon * 2 ^ (nmax - j) - (rx - lx) / 2
      xh = (lx + rx) / 2
      xf = (ry * lx - ly * rx) / (ry - ly)
      sigma = signum (xh - xf)
      delta = k1 * abs (rx - lx) ^ k2
      xt = if delta <= abs (xh - xf) then xf + sigma * delta else xh
      xitp = if abs (xt - xh) <= r then xt else xh - sigma * r
      yitp = f xitp - findy
     in
      if rx - lx > 2 * epsilon
        then
          (xitp, lx, rx, ly, ry, j, nh, nmax, r, xf, sigma, delta, xt, xitp, yitp)
            : case signum yitp of
              --        then xitp : case signum yitp of
              1 -> loop (j + 1) ((lx, ly), (xitp, yitp))
              0 -> []
              -1 -> loop (j + 1) ((xitp, yitp), (rx, ry))
        else [(xh, lx, rx, ly, ry, j, nh, nmax, r, xf, sigma, delta, xt, xitp, yitp)]

getBrackets f = getBrackets1 f 0 (f 0) 0.9 2 1 2

getBrackets1 f c fc smult zmult s z findy = loop s z
 where
  loop s z =
    let
      fxs = mapxfx (sub f findy) [c - z, c - z + s .. c + z]
     in
      case find (\(_, a) -> fc * a < 0) fxs of
        Just b -> maybeSwap ((c, fc), b)
        Nothing -> loop (s * smult) (z * zmult)

getBracketsD f s c z findy = do
  putStrLn $ "getBracketsD s=" ++ showFF 8 s ++ " c=" ++ showFF 8 c ++ " z=" ++ showFF 8 z
  let fxs = mapxfx (sub f findy) [c - z, c - z + s .. c + z]
  mapM_ print fxs
  case find (\((_, a) : (_, b) : _) -> a * b < 0) $ init $ init $ tails fxs of
    Just (l : r : _) -> return (l, r)
    Nothing -> getBracketsD f (s / 2) c (z * 2) findy

sub f y x = f x - y

maybeSwap ((lx, ly), (rx, ry)) = if lx < ly then ((lx, ly), (rx, ry)) else ((rx, ry), (lx, ly))

sofbs = map (chr . fromIntegral) . B.unpack
bsofs = B.pack . map (fromIntegral . ord)
