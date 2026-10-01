-- Copyright 2025 Brett Curtis
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Prob where

import Favs
import MyPretty2
import Numeric
import Poly

import Data.List
import Data.Ratio
import qualified Data.Map as M 

import Control.Monad.State

import System.Random
import System.IO.Unsafe
-- I use the term probability mass function to mean the integral of the p d f

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
         | k > n - k = product [k + 1 .. n] `div` product [2 .. n - k]
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

normal = integral $ taylor normalstd $ concatMap (\x -> [1.1^x, -(1.1^x)]) [-2..4]


-- normal approximation to binomial
-- note this is a PDF
binnormpdf n p = normalpdf (n * p) (n * p * (1 - p))

-- this is the PMF
binnormpmf n p k = integrate (binnormpdf n p) $ stepNFT 100 (k - 0.5, k + 0.5)

ps25  = [0, 0.04 .. 1]
ps50  = [0, 0.02 .. 1]
ps100 = [0, 0.01 .. 1]
probs n k ps = map (\p -> (p, binomial n p k)) ps

norm2 m1 s1 p1 m2 s2 x = p1*(normalpdf m1 s1 x) + (1-p1)*normalpdf m2 s2 x
{-
747
showProbs   n k = graph (binomial n k)
showProbs2 n p = graph (\k -> binomial n k p)
-}
-- graph a list of numbers
graph1 nums = let
   minnum = minimum nums
   maxnum = maximum nums
   range = maxnum - minnum
   
   in map (\num -> replicate (round ((num - minnum) / range * (fromIntegral width - 10))) '*') nums

graphD nums = let
   minnum = minimum nums
   maxnum = maximum nums
   range = maxnum - minnum
   
   in (minnum, maxnum, range)

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
graphList fxss = let
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

enumStep from to step = [from, from + step .. to]
enumNFT n from to = [from, from + step .. to] where step = (to - from) / n

zipTail xs = zip xs (tail xs)

stepNFT n (from, to) = zipTail $ enumNFT n from to

mapSteps f xs = map (uncurry f) $ zipTail xs

zipNFT n f a b = mapSteps f $ enumNFT n a b

simpson f (a, b) = (b - a) / 6 * (f a + 4 * f ((a + b) / 2) + f b)

-- integrate f over the given areas using simpson's rule
integrate f = sum . map (simpson f)
-- to integrate f between a and b with n steps, do
-- integrate f $ stepNFT n (a, b)
integrateNFT n f (a, b) = integrate f $ stepNFT n (a, b)

-- for integrating f between a and +infinity if f is even
integratei n f a =
   if
      | a > 0 -> 0.5 + integrate f (stepNFT n (0, a))
      | a < 0 -> 0.5 - integrate f (stepNFT n (a, 0))
      | True -> 0

testp x = norm2 100 15 0.5 85 15 x

testp2 y = inout $ \x -> return $ integrateNFT 10 testp (0, x) - y

sample y = getBracketsM1 0.0001 $ testp2 y
-- handy functions

percofiq sd iq = integratei 10 normalstd ((iq - 100) / sd)

iqofperc sd perc = let f = findy (percofiq sd) perc in falsePos1 0.00000001 f $ getBrackets1 0.5 2 5 90 f 100 (f 100)

differentiate d f x = (f (x + d) - f x) / d

accumulate xs = scanl (+) 0 xs

-- give a running total of the integration
-- integratea :: (Fractional a) => (a -> a) -> [a] -> [a]
integratea f = accumulate . map (simpson f)

alternate [] _ = []
alternate _ [] = []
alternate (x : xs) (y : ys) = x : y : alternate xs ys

-- debug version
conf2b1 clev dat = let
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

conf2b n k clev = let
   count = 1000
   in
   do
      (x1, x2) <- conf2b1 clev (integratea (\p -> binomial n p k) $ stepNFT count (0, 1))
      return (x1 / count, x2 / count)

-- do confidence interval on a bunch of accumulated numbers
conf2a1 clev dat = let
   count = flength dat
   total = last dat
   fx1target = total * (1 - clev) / 2
   fx2target = min (total * clev + fx1) total
   (fx1, x1) = findNearest fx1target dat
   (fx2, x2) = findNearest fx2target dat
   in
   (x1, x2)

-- do it on what we want
conf2a n k clev = let
   steps = 10000
   (x1, x2) = conf2a1 clev (integratea (\p -> binomial n p k) $ stepNFT steps (0, 1))
   in
   (x1 / steps, x2 / steps)

-- testing
-- conf2a1graph n k steps = putLines $ graphLabels (enumN 0 1 steps) $ integratea (\p -> binomial n p k) 0 1 steps

-- binafb search integrated, TWO TAILS!
conf2 n k clev = let
   steps = 100
   depth = 100

   total = integrate (\p -> binomial n p k) $ stepNFT (steps * 10) (0, 1)

   fx1target = total * (1 - clev) / 2

   x1 = binsearch depth (\x -> integrate (\p -> binomial n p k) $ stepNFT steps (0, x)) 0 1 fx1target

   fx2target = total * clev + fx1target

   x2 = binsearch depth (\x -> integrate (\p -> binomial n p k) $ stepNFT steps (0, x)) 0 1 fx2target
   in
   (x1, x2)

-- binafb search integrated, ONE TAIL
conf1 n k clev = let
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

may as well use simpson interpolation instead of binafb search
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
of the tether be to enable the goat to graze exactfa half the area of the field -}

goatArea :: Double -> Double
goatArea l = let
   r = 100
   costheta = (2 * r ^ 2 - l ^ 2) / (2 * r ^ 2)
   theta = acos costheta
   cosphi = l ^ 2 / (2 * r * l)
   phi = acos cosphi
   in
   r ^ 2 * theta - r ^ 2 * sin theta * cos theta + l ^ 2 * phi - l ^ 2 * sin phi * cos phi

go = secantMethod 0.0001 (findy goatArea (pi * 5000))

goatArea2 l = let
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

goatArea3 l r x y = let
   r1 = sqrt (x ^ 2 + y ^ 2)
   l1 = sqrt ((x + r) ^ 2 + y ^ 2)
   in
   r1 < r && l1 < l

main = putStrLn "hello world"

binsearch1debug f target l r n = let
   x = (l + r) / 2
   in
   if n <= 0
      then [x]
      else
      if f x < target
      then x : binsearch1debug f target x r (n - 1)
      else x : binsearch1debug f target l x (n - 1)

binsearch1 n f l r target = let
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

secantMethod epsilon f = secantMethod1 epsilon f $ getBracketsR f 
   
secantMethod1 epsilon f ((x0, fx0), (x1, fx1)) =
         let
            x2 = (x0 * fx1 - x1 * fx0) / (fx1 - fx0)
            fx2 = f x2
         in if abs fx2 < epsilon
               then x2
               else secantMethod1 epsilon f ((x1, fx1), (x2, fx2))

secantMethodM epsilon mf = secantMethodM1 epsilon mf =<< getBracketsMR mf
   
secantMethodM1 epsilon mf ((x0, fx0), (x1, fx1)) = do
   let x2 = (x0 * fx1 - x1 * fx0) / (fx1 - fx0)
   fx2 <- mf x2
   if abs fx2 < epsilon
      then return x2
      else secantMethodM1 epsilon mf ((x1, fx1), (x2, fx2))

inout f x = do
   fx <- f x
   putStrLn $ "x="++show x++" f(x)="++show fx
   return fx

-- the following equation solvers (falsePos and itp) are used like:

-- itp epsilon f
-- falsePos epsilon f
-- eg, itp 0.1 (\x -> x^2 - 5)

-- epsilon = how close to 0 we want to f(x) to be
-- f = the equation/function to solve

-- they both use getBracketsR to initialfa find values of opposite sign (bracketing)

-- they both search for the x such that f(x) = 0

-- to find other values of f(x), alter f(x) using findy f y, 
-- which creates a new function g(x) = f(x) - y that will be zero where f(x) = y

-- eg. falsePos 0.1 $ findy (^2) 5

falsePos epsilon f = falsePos1 epsilon f $ getBracketsR f

-- falsePos1 and itp1 allow you to specify the initial brackets yourself

falsePos1 epsilon f = loop
   where
      loop ((a, fa), (b, fb)) = let
         c = (a * fb - b * fa) / (fb - fa)
         fc = f c
         in if abs fc < epsilon
            then c
            else case signum fa * signum fc of
               1 -> loop ((c, fc), (b, fb))
               0 -> c
               -1 -> loop ((a, fa), (c, fc))

itp epsilon f = itp1 epsilon f $ getBracketsR f

itp1 = itp2 0.1 2 1 -- some numbers specific to the itp algorithm

itp2 k1 k2 n0 epsilon f ((a, fa), (b, fb)) = loop 0 ((a, fa), (b, fb))
   where
      nh = ceiling $ logBase 2 ((b - a) / (2 * epsilon))
      nmax = nh + n0
      loop j ((a, fa), (b, fb)) =
         let
            r = epsilon * 2 ^ (nmax - j) - (b - a) / 2
            xh = (a + b) / 2
            xf = (fb * a - fa * b) / (fb - fa)
            sigma = signum (xh - xf)
            delta = k1 * abs (b - a) ^ k2
            xt = if delta <= abs (xh - xf) then xf + sigma * delta else xh
            xitp = if abs (xt - xh) <= r then xt else xh - sigma * r
            yitp = f xitp
         in
            if b - a > 2 * epsilon
               then case signum yitp of
                  1 -> loop (j + 1) ((a, fa), (xitp, yitp))
                  0 -> xitp
                  -1 -> loop (j + 1) ((xitp, yitp), (b, fb))
               else xh

itp1d k1 k2 n0 epsilon f a b = itp2d k1 k2 n0 epsilon f ((a, f a), (b, f b))

itp2d k1 k2 n0 epsilon f ((a, fa), (b, fb)) = loop 0 ((a, fa), (b, fb))
   where
      nh = ceiling $ logBase 2 ((b - a) / (2 * epsilon))
      nmax = nh + n0
      loop j ((a, fa), (b, fb)) =
         let
            r = epsilon * 2 ^ (nmax - j) - (b - a) / 2
            xh = (a + b) / 2
            xf = (fb * a - fa * b) / (fb - fa)
            sigma = signum (xh - xf)
            delta = k1 * abs (b - a) ^ k2
            xt = if delta <= abs (xh - xf) then xf + sigma * delta else xh
            xitp = if abs (xt - xh) <= r then xt else xh - sigma * r
            yitp = f xitp
         in
            if b - a > 2 * epsilon
               then
                  (xitp, a, b, fa, fb, j, nh, nmax, r, xf, sigma, delta, xt, xitp, yitp)
                     : case signum yitp of
                        --        then xitp : case signum yitp of
                        1 -> loop (j + 1) ((a, fa), (xitp, yitp))
                        0 -> []
                        -1 -> loop (j + 1) ((xitp, yitp), (b, fb))
               else [(xh, a, b, fa, fb, j, nh, nmax, r, xf, sigma, delta, xt, xitp, yitp)]

until2 :: Num a => (a -> Bool) -> (g -> (a, g)) -> g -> [(a, g)]
until2 pred f x = dropWhile (not . pred . fst) $ iterate (f . snd) (0, x)

-- given a function f :: Num -> Num, should find values of either sign, ((x0, f x0), (x1, f x1)) with f x0 * f x1 < 0
getBracketsR :: (Ord b, Floating a, Random a, Num b) => (a -> b) -> ((a, b), (a, b))
getBracketsR f = let 
   i  = map wild (randoms $ unsafePerformIO initStdGen)
   x0 = head i
   fx0 = f x0
   x1 = head $ filter (\x -> f x * fx0 < 0) (tail i)
   fx1 = f x1
   in ((x0, fx0), (x1, fx1))

getBracketsMR :: (Ord b, Floating a, Random a, MonadFail m, Num b) => (a -> m b) -> m ((a, b), (a, b))
getBracketsMR f = do
   let
      (x0:xs) = map wild (randoms $ unsafePerformIO initStdGen)
   fx0 <- f x0
   Just x1 <- findM (\x1 -> do fx1 <- f x1; return $ fx1 * fx0 < 0) xs
   fx1 <- f x1
   return ((x0, fx0), (x1, fx1))

getBracketsMR2 :: (Ord b, Floating a, Random a, MonadFail m, Num b) => (a -> m b) -> m ((a, b), (a, b))
getBracketsMR2 f = do
   let
      (x0:xs) = map wild (randoms $ unsafePerformIO initStdGen)
   fx0 <- f x0
   Just x1 <- findM (\x1 -> do fx1 <- f x1; return $ fx1 * fx0 < 0) xs
   fx1 <- f x1
   return ((x0, fx0), (x1, fx1))

findM p []     = return Nothing
findM p (x:xs) = do
   b <- p x
   if b
      then return $ Just x
      else findM p xs


-- i don't think we want it uniform, we want it more spread out towards the larger numbers
wild x = 2 ** (64 ** x)

getBracketsUser f x0 x1 = ((x0, f x0), (x1, f x1))

getBrackets f = getBrackets1 6 11 1 2 f 0 (f 0) 

getBrackets1 smult zmult s z f c fc = loop s z
   where
      loop s z =
         let
            fxs = mapxfx f [c - z, c - z + s .. c + z]
         in
            case find (\(_, a) -> fc * a < 0) fxs of
               Just b -> maybeSwap ((c, fc), b)
               Nothing -> loop (s * smult) (z * zmult)

getBracketsD f = let
   fc = f 0
   
   in if fc < 0 
         then getBracketsD1 1.1 4.7 1 2 (negatef f) 0
         else getBracketsD1 1.1 4.7 1 2 f 0

negatef f x = negate $ f x

negatefM f x = negate <$> f x

getBracketsD1 smult rmult s r f c = loop s r
   where
      loop s r = do
         let
            xs = [c - r, c - r + s .. c + r]
            fxs = mapxfx f xs
         mapM_ (\(x, fx) -> putStrLn $ "x="++show x++" f(x)="++show fx) fxs
         case find (\(_, a) -> a < 0) fxs of
            Just b -> return b
            Nothing -> loop (s * smult) (r * rmult)

getBracketsM1 epsilon f = do
   gen <- initStdGen
   let x = pi
   fx <- f x
   
   if fx < 0 
      then getBracketsM3 epsilon (negatefM f)
      else getBracketsM3 epsilon f

getBracketsM2 epsilon f = do
   let x = pi
   fx <- f x
   gen <- initStdGen
   loop (M.singleton x fx, M.singleton (0, fx) x, gen)
      where 
         loop (xs, fxs, gen) = do
            let
               ((n, fx), x) = M.findMin fxs
               (xmin, fxmin) = case M.lookupLT x xs of
                  Just (a, b) -> (a, b)
                  Nothing     -> (x / 2, x / 2)
               (xmax, fxmax) = case M.lookupGT x xs of
                  Just (a, b) -> (a, b)
                  Nothing     -> (x * 2, x * 2)
               (x1, gen1) = randomR (xmin, xmax) gen
            fx1 <- f x1
            if fx1 < epsilon 
               then
                  return (x1, fx1)
               else
                  loop (M.insert x1 fx1 xs, M.insert (0, fx1) x1 $ M.insert (n+1, fx) x $ M.delete (n, fx) fxs, gen1)

listFromMultimap m = concatMap (\(k, vs) -> map (k,) vs) $ M.toList m

getBracketsM3 epsilon f = do
   let x = 0
   fx <- f x
   loop (M.singleton fx (x, 1, 2, [0], cycle1), fractal 20)
      where 
         loop (fxs, fr:frs) = do
            let
               (fx, (x, xm, xmm, fr2:fr2s, fr3:fr3s)) = M.elemAt fr fxs
               x1 = x + xm * fr2
               (xm1, fr2a, fr3a) = if null fr2s
                  then (xm * xmm, fr3, fr3s)
                  else (xm      , fr2s, fr3:fr3s)

            if fr < M.size fxs
               then do
                  fx1 <- f x1
                  if fx1 < epsilon 
                     then
                        return (x1, fx1)
                     else
                        loop (M.insert fx (x, xm1, xmm, fr2a, fr3a) $ M.insert fx1 (x1, xm * xmm, if xmm > 0.5 then xmm * 0.9 else xmm, fractal2 1, fractal3) fxs, frs)
               else
                  loop (fxs, fractal 20)

{-
       *   
   *       *
 *   *   *   *
* * * * * * * *
-}
--fractal = concatMap (\x -> [0..round x]) $ iterate (*1.1) 1

fractal 0 = [0]
fractal n = fractal (n-1) ++ n : fractal n

fractal2 0 = [0]
fractal2 n = map (\x -> x*2-1) (fractal2 $ n - 1) ++ map (\x -> x*2+1) (fractal2 $ n - 1)

fractal3 = map fractal2 [0..]

fractal4 0 = [0]
fractal4 n = map (\x -> x*2-1) (fractal4 $ n - 1) ++ map (\x -> x*2+1) (fractal4 $ n - 1)

cycle1 = repeat [-1,1]

getBracketsM4 epsilon f = do
   let x = 0
   fx <- f x
   loop (M.singleton fx (x, 1, 2, [0], cycle1)) (M.singleton 0 $ map (\x -> (1.2^x, x)) [0..99]) 0.0
      where 
         loop fxs queue now = do
            let
               todo = listFromMultimap $ M.takeWhileAntitone (<= now) queue
               later = M.dropWhileAntitone (<= now) queue
               queue1 = M.unionWith (++) later $ multimap $ map (\(when, (step, which)) -> (when + step, (step, which))) todo
               try fxs [                ] = loop fxs queue1 (now+1)
               try fxs ((_, (_, fr)):frs) = do
                  let
                     (fx, (x, xm, xmm, fr2:fr2s, fr3:fr3s)) = M.elemAt fr fxs
                     x1 = x + xm * fr2
                     (xm1, fr2a, fr3a) = if null fr2s
                        then (xm * xmm, fr3, fr3s)
                        else (xm      , fr2s, fr3:fr3s)
                  print fr
                  if fr < M.size fxs
                     then do
                        fx1 <- f x1
                        if fx1 < epsilon 
                           then
                              return (x1, fx1)
                           else
                              try (M.insert fx (x, xm1, xmm, fr2a, fr3a) $ M.insert fx1 (x1, xm * xmm, if xmm > 0.5 then xmm * 0.4 else xmm, fractal2 1, fractal3) fxs) frs
                     else
                        loop fxs queue1 (now+1)                      

            mapM_ print todo
            try fxs todo

-- [0, -4, 4, -6, 2, -2, 6, -7, 1, -3, 5, -5, 3, -1, 7]
-- 1 3/4 9/16 27/64
-- 4 3 2.25 1.69 1.23
-- 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3
-- 1 2 1 2 1 2 3 4 1 2 3 4 5 6 7 8 
-- 1 2 3 4 1 2 3 1 2 1
findy f y x = f x - y

findyM f y x = do fx <- f x; return $ fx - y

maybeSwap ((a, fa), (b, fb)) = if a < fa then ((a, fa), (b, fb)) else ((b, fb), (a, fa))

