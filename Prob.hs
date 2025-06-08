-- Copyright 2025 Brett Curtis
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LexicalNegation #-}

module Prob where

import Data.List
import Data.Ratio
import Favs
import MyPretty2
import Numeric

ftake n l = take (round n) l
fdrop n l = drop (round n) l
flength l = fromIntegral $ length l
freplicate n = replicate (round n)
fsplitAt n = splitAt (round n)

divider = replicate (width + 18) '=' ++ " "
spaces = replicate width ' '

dup x = zipWith (+) x (tail x)

pascal = [1] : map (\row -> zipWith (+) (0 : row) (row ++ [0])) pascal

--product [k + 1 .. n] `div` product [1 .. n - k]
comb n k | k < 0 = 0
         | k > n - k = product [k + 1 .. n] `div` product [1 .. n - k]
         | otherwise = comb n (n-k)
perm n k = product [n - k + 1 .. n]
combp n k = "comb " ++ show n ++ " " ++ show k ++ " = product " ++ show [n - k + 1 .. n] ++ "/product " ++ show [1 .. k] ++ "=" ++ show (product [n - k + 1 .. n]) ++ "/" ++ show (product [1 .. k]) ++ "=" ++ show (comb n k)
permp n k = "perm " ++ show n ++ " " ++ show k ++ " = product " ++ show [n - k + 1 .. n] ++ "=" ++ show (perm n k)
fac n = product [1 .. n]

-- binomial distribution, probability mass function, for discrete random variable, result is probability
binomial :: Integer -> Rational -> Integer -> Rational
binomial n p k = toRational (comb n k) * p ^ k * (1 - p) ^ (n - k)

-- binomial n p k = pascal n k * p**k * (1-p)**(n-k)

-- normal distribution, probability density function, for continuous variable
-- result needs to be integrated to get probability
normalstd x = exp (-0.5 * x ^ 2) / sqrt (2 * pi)
normalpdf 0 1 x = normalstd x
normalpdf mu sig x = exp (-0.5 * (x - mu) ^ 2 / sig ^ 2) / sqrt (2 * pi * sig ^ 2)

normal mu var n x y = integratei n (normalpdf mu var) y - integratei n (normalpdf mu var) x

-- normal approximation to binomial
-- note this is a PDF
binnormpdf n p = normalpdf (n * p) (n * p * (1 - p))

-- this is the PMF
binnormpmf n p k = integrate (binnormpdf n p) $ stepNFT 100 (k - 0.5, k + 0.5)

-- handy functions
ps25 = [0, 0.04 .. 1]
ps50 = [0, 0.02 .. 1]
ps100 = [0, 0.01 .. 1]
probs n k ps = map (\p -> (p, binomial n p k)) ps

{-

showProbs   n k = graph (binomial n k)
showProbs2 n p = graph (\k -> binomial n k p)
-}
-- graph a list of numbers
graph1 nums =
   let
      minnum = minimum nums
      maxnum = maximum nums
      range = maxnum - minnum
    in
      map (\num -> replicate (round ((num - minnum) / range * (fromIntegral width - 10))) '*') nums

graphD nums =
   let
      minnum = minimum nums
      maxnum = maximum nums
      range = maxnum - minnum
    in
      (minnum, maxnum, range)

graph nums = putLines $ graph1 nums

nums1 xs = map (\x -> showFFloat (Just 5) x "" ++ " ") xs

nums xs = putLines $ nums1 xs

graphN1 xs = zipWith (++) (nums1 xs) (graph1 xs)

graphN xs = putLines $ graphN1 xs

{-
graphF f n a b = let
    xs = enumNFT n a b
    in putLines $ zipWith (++) (map showFF5 xs) $ graph1 xs

graphG f xs = let
    fxs = accumulate $ zipSteps f xs
    g    = map showFF5 xs
    h    = zipWith (++) (map showFF5 fxs) $ graph1 fxs
    in putLines $ alternate g h
-}
-- graph a list of lists of numbers
graphList fxss =
   let
      minfx = minimum $ concat fxss
      maxfx = maximum $ concat fxss
      range = maxfx - minfx
      scale = fromIntegral width / range
      fxsst = transpose fxss
      fxss1 = map (map (\x -> (x - minfx) * scale)) fxsst
      overwrite l (n, c) = ftake n l ++ [c] ++ fdrop (n + 1) l
    in
      putLines $ map (\fxs -> foldl overwrite spaces (zip fxs "123456789")) fxss1

oddf f x = if x < 0 then -f (-x) else f x

mapTake n l = map (ftake n) l

graphListN n fxss = graphList $ mapTake n fxss

putLines l = putStr $ unlines l

-- put1 :: RealFloat a => [a] -> IO ()
-- put1 = mapM_ (putStrLn . showFF5)

-- put2 :: RealFloat a => [(a, a)] -> IO ()
-- put2 = mapM_ (\(a, b) -> putStrLn (showFF5 a ++ showFF5 b))

binsearch1debug f target l r n =
   let
      x = (l + r) / 2
    in
      if n <= 0
         then [x]
         else
            if f x < target
               then x : binsearch1debug f target x r (n - 1)
               else x : binsearch1debug f target l x (n - 1)

binsearch1 n f l r target =
   let
      x = (l + r) / 2
    in
      if n <= 0
         then x
         else
            if f x < target
               then binsearch1 (n - 1) f x r target
               else binsearch1 (n - 1) f l x target

binsearch n f l r target =
   -- if f is decreasing, swap l and r
   if f l < f r
      then binsearch1 n f l r target
      else binsearch1 n f r l target

isMonotonic n f l r = zipNFT n (\x y -> f x < f y) l r

{-
conf n k probTarget = binsearch (binomial n k) probTarget 0 1 1000

-- confidence limits (use 0.05 for 95%)
conf2 n k probTarget =
    (binsearch (binomial n k) probTarget 0.0 (k/n) 1000, binsearch (binomial n k) probTarget (k/n) 1.0 1000)
-}

enumStep from to step = [from, from + step .. to]
enumNFT n from to = [from, from + step .. to] where step = (to - from) / n

zipTail xs = zip xs (tail xs)

stepNFT n (from, to) = zipTail $ enumNFT n from to

mapSteps f xs = map (uncurry f) $ zipTail xs

zipNFT n f a b = mapSteps f $ enumNFT n a b

simpson f (a, b) = (b - a) / 6 * (f a + 4 * f ((a + b) / 2) + f b)

-- integrate f between a and b with n steps
integrate f = sum . map (simpson f)

-- for integrating f between a and +infinity if f is even
integratei n f a =
   if
      | a > 0 -> 0.5 + integrate f (stepNFT n (0, a))
      | a < 0 -> 0.5 - integrate f (stepNFT n (a, 0))
      | True -> 0

percofiq sd iq = integratei 10 normalstd ((iq - 100) / sd)

iqofperc sd perc = falsePos1 (percofiq sd) 0.00000001 perc $ getBrackets1 (percofiq sd) 100 (percofiq sd 100) 0.5 2 5 90 perc

differentiate f x = (f (x + 0.001) - f x) * 1000

accumulate xs = scanl (+) 0 xs

-- give a running total of the integration
-- integratea :: (Fractional a) => (a -> a) -> [a] -> [a]
integratea f = accumulate . map (simpson f)

alternate [] _ = []
alternate _ [] = []
alternate (x : xs) (y : ys) = x : y : alternate xs ys

-- debug version
conf2b1 clev dat =
   let
      count = flength dat
      total = last dat
      fx1target = total * (1 - clev) / 2
      fx2target = min (total * clev + fx1) total
      (fx1, x1) = findNearest fx1target dat
      (fx2, x2) = findNearest fx2target dat
    in
      do
         putStrLn ("fx1target = " ++ show fx1target)
         putStrLn ("fx1          = " ++ show fx1)
         putStrLn ("fx2target = " ++ show fx2target)
         putStrLn ("total       = " ++ show total)
         putStrLn ("x1            = " ++ show x1)
         putStrLn ("x2            = " ++ show x2)
         mapM_ print dat
         return (x1, x2)

-- (fx1target, fx2target, total, integrated)
-- (x1 / count, x2 / count)

conf2b n k clev =
   let
      count = 1000
    in
      do
         (x1, x2) <- conf2b1 clev (integratea (\p -> binomial n p k) $ stepNFT count (0, 1))
         return (x1 / count, x2 / count)

-- do confidence interval on a bunch of accumulated numbers
conf2a1 clev dat =
   let
      count = flength dat
      total = last dat
      fx1target = total * (1 - clev) / 2
      fx2target = min (total * clev + fx1) total
      (fx1, x1) = findNearest fx1target dat
      (fx2, x2) = findNearest fx2target dat
    in
      (x1, x2)

-- do it on what we want
conf2a n k clev =
   let
      steps = 10000
      (x1, x2) = conf2a1 clev (integratea (\p -> binomial n p k) $ stepNFT steps (0, 1))
    in
      (x1 / steps, x2 / steps)

-- testing
-- conf2a1graph n k steps = putLines $ graphLabels (enumN 0 1 steps) $ integratea (\p -> binomial n p k) 0 1 steps

-- binary search integrated, TWO TAILS!
conf2 n k clev =
   let
      steps = 100
      depth = 100

      total = integrate (\p -> binomial n p k) $ stepNFT (steps * 10) (0, 1)

      fx1target = total * (1 - clev) / 2

      x1 = binsearch depth (\x -> integrate (\p -> binomial n p k) $ stepNFT steps (0, x)) 0 1 fx1target

      fx2target = total * clev + fx1target

      x2 = binsearch depth (\x -> integrate (\p -> binomial n p k) $ stepNFT steps (0, x)) 0 1 fx2target
    in
      (x1, x2)

-- binary search integrated, ONE TAIL
conf1 n k clev =
   let
      steps = 100
      depth = 100

      total = integrate (\p -> binomial n p k) $ stepNFT (steps * 10) (0, 1)

      fx1target = total * clev

      x1 = binsearch depth (\x -> integrate (\p -> binomial n p k) $ stepNFT steps (0, x)) 0 1 fx1target
    in
      x1

findNearest f ys = find1 0 ys
 where
   find1 n [] = (f, n)
   find1 n [y] = (y, n)
   find1 n (y : y1 : ys) =
      if y1 >= f
         then if abs (f - y) < abs (f - y1) then (y, n) else (y1, n + 1)
         else find1 (n + 1) (y1 : ys)

findNearestElem f ys = fst $ findNearest f ys
findNearestIndex f ys = snd $ findNearest f ys

{-
increase simpson steps as the search depth increases because we need the accuracy

may as well use simpson interpolation instead of binary search
   but that's tricky
-}

-- n way search
{-
nwaysearch intfunc target = let
    steps = 100
    dat    = intfunc l r steps
    x       = findNearestIndex target dat

    in x
-}

normalise xs = map (/ maximum xs) xs
normalisec xs = map (/ last xs) xs

-- graph for function conf2
{-
conf2graph n k clev = let
    (x1, x2) = conf2 n k clev
    steps      = 100
    x1t         = (x1 * steps)
    x2t         = (x2 * steps)
    g            = graphLabels (enumN 0 1 100) $ normalisec $ integratea (\p -> binomial n p k) 0 1 steps
    (g1, g2_) = fsplitAt x1t g
    (g2, g3)   = fsplitAt (x2t - x1t) g2_

    in putStr (concat g1 ++
                     divider ++ showFF5 x1 ++ "\n" ++
                     concat g2 ++
                     divider ++ showFF5 x2 ++ "\n" ++
                     concat g3 ++ "\n")
-}

{- if a goat is tethered on the edge of a circular field of radius 100 yards, what must the length
of the tether be to enable the goat to graze exactly half the area of the field -}

goatArea :: Double -> Double
goatArea l =
   let
      r = 100
      costheta = (2 * r ^ 2 - l ^ 2) / (2 * r ^ 2)
      theta = acos costheta
      cosphi = l ^ 2 / (2 * r * l)
      phi = acos cosphi
    in
      r ^ 2 * theta - r ^ 2 * sin theta * cos theta + l ^ 2 * phi - l ^ 2 * sin phi * cos phi

go = secantMethod goatArea (100 :: Int) (50 :: Double) (150 :: Double) (pi * 5000)

goatArea2 l =
   let
      r = 100
      step = 1
    in
      sum $
         for
            [-100, (-100) + step .. 100]
            ( \x ->
                  sum $
                     for
                        [-100, (-100) + step .. 100]
                        ( \y ->
                              if goatArea3 l r x y then step ^ 2 else 0
                        )
            )

goatArea3 l r x y =
   let
      r1 = sqrt (x ^ 2 + y ^ 2)
      l1 = sqrt ((x + r) ^ 2 + y ^ 2)
    in
      r1 < r && l1 < l



hypercatalan m = fac (e m - 1) `div` (fac (v m - 1) * product (map fac m))

v m = 2 + sum (zipWith (*) [1..] m)

e m = 1 + sum (zipWith (*) [2..] m)

facpower x n = x ^ n % fac n

iterateM f x n = foldl (>>=) (return x) $ replicate n f

polysol1M c n x = iterateM ps2 x n
   where
      ps2a = polysol2M (length c)
      ps2 x = ps2a (shift x c)

polysol2M cl = \c -> do
   let terms = map ($ c) ps3
   mapM_ (\x -> putStrLn $ showFFloat (Just 20) x "") terms
   putStrLn ""
   putStrLn $ showFFloat (Just 20) (sum terms) ""
   putStrLn ""
   return $ sum terms
   where
      ps3 = map polysol5 $ corner cl cl

polysol1 c = iterate ps2 0
   where
      ps2 x = polysol2 (length c) (shift x c)

polysol2 cl = \c -> sum $ map ($ c) ps3
   where
      ps3 = map polysol5 $ corner cl cl

polysol3 m c = (1 % c !! 1) * facpower (c !! 0) (v m - 1) / facpower (c !! 1) (e m - 1) * product (zipWith facpower (drop 2 c) m)

polysol4 m c = hypercatalan m * (c !! 0 ^ (v m - 1) % c !! 1 ^ e m) * (product (zipWith (^) (drop 2 c) m) % 1)

polysol5 m = \c -> h * (c !! 0 ^^ vm1 / c !! 1 ^^ em) * product (zipWith (^) (drop 2 c) m)
    where
         h = fromIntegral $ hypercatalan m
         vm1 = v m - 1
         em = e m

corner  0 _ = [[]]
corner  1 n = map singleton [0, 1..n]
corner ml n = concatMap (\q -> map (q :) $ corner (ml-1) (n-q)) [0..n]

shift s cs = map sum $ transpose $ zipWith (\c -> reverse . zipWith (*) (iterate (*s) 1) . map ((*c) . fromIntegral)) cs pascal

poly x c = sum $ zipWith (*) c $ iterate (*x) 1

{-
x^2+x+1

x=y+5
(y+5)^2+(y+5)+1
(y^2+10*y+25)+(y+5)+1

5(y+10)^5+4(y+10)^4+3(y+10)^3+2(y+10)^2+1(y+10)+0
(y^5+20*y^4+150*y^3+


(y+10)^3
(y^3+300y^2
-}