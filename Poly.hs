{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Parenthesize unary negation" #-}
module Poly where

import Favs

import Data.List
import Data.Ratio
import Numeric
import Debug.Trace


newtype Poly a = Poly [a]

instance (Eq a, Num a) => Eq (Poly a) where
   a == b = polyempty (a - b)

instance (Eq a, Num a) => Num (Poly a) where
   Poly as * Poly bs = sum $ zipWith (\a z -> Poly $ replicate z 0 ++ map (a*) bs) as [0..]
   Poly as + Poly bs = Poly $ snarf $ zerozip (+) as bs 
   Poly as - Poly bs = Poly $ snarf $ zerozip (-) as bs 
   fromInteger a = Poly [fromInteger a]
   abs (Poly as) = Poly $ map abs as
   signum (Poly as) = Poly $ map signum as
   
instance (Show a) => Show (Poly a) where
   show (Poly as) = intercalate "+" $ zipWith (\a x -> show a ++ x) as ("":"x":map (\x -> "x^" ++ show x) [2..])

fac n = product [2 .. n]

hypercatalan m = fac (e m - 1) `div` (fac (v m - 1) * product (map fac m))

v m = 2 + sum (zipWith (*) [1..] m)

e m = 1 + sum (zipWith (*) [2..] m)

facpower x n = x ^ n % fac n

iterateM f n x = foldl (>>=) (return x) $ replicate n f

plength (Poly cs) = length cs

solve1M p n x = iterateM ps2 n x
   where
      ps2a = solve2M (plength p)
      ps2 x = (x+) <$> ps2a (shift x p)

solve2M cl = \p -> do
   let terms = map ($ negate1 p) ps3
   mapM_ (\x -> putStrLn $ showFFloat (Just 20) x "") terms
   putStrLn ""
   putStrLn $ showFFloat (Just 20) (sum terms) ""
   putStrLn ""
   return $ sum terms
      where
         ps3 = map solve3 $ corner (cl-2) corn

solve1 p = iterate ps2 0
   where
      ps2 x = solve2 (plength p) (shift x p) + x

solve2 cl = \p -> sum $ map ($ negate1 p) ps3
   where
      ps3 = map solve3 $ corner (cl-2) corn

corn = 4

solve3 m = \c -> fromRational blah * (c !! 0)^(vm - 1) / (c !! 1)^em * product (zipWith (^) (drop 2 c) m)
   where
      vm = v m
      em = e m
      blah = product [vm..em - 1] % product (map fac m)

solve4 m c = hypercatalan m * (c !! 0 ^ (v m - 1) % c !! 1 ^ e m) * (product (zipWith (^) (drop 2 c) m) % 1)

solve5 m = \c -> h * (c !! 0 ^ vm1 / c !! 1 ^ em) * product (zipWith (^) (drop 2 c) m)
   where
      h = fromIntegral $ hypercatalan m
      vm1 = v m - 1
      em = e m

negate1 (Poly cs) = head cs : negate (cs !! 1) : drop 2 cs

corner  0 _ = [[]]
corner  1 n = map singleton [0..n]
corner ml n = concatMap (\q -> map (q :) $ corner (ml-1) (n-q)) [0..n]

polydiv (/) n@(Poly ns) d@(Poly ds) = let 
   ln = length ns
   ld = length ds
   q = Poly $ replicate (ln - ld) 0 ++ [last ns / last ds]
   r = n - q * d
   (q1, r1) = polydiv (/) r d
   in if ln >= ld then (q + q1, r1) else (Poly [], n)

snarf as = reverse $ dropWhile (==0) $ reverse as

zerozip f a b = let
   [a1, b1] = padRWith 0 [a, b]
   in zipWith f a1 b1

polyempty (Poly as) = null as

shift x (Poly p) = Poly (map num p) @ Poly [x, 1]

poly c x = sum $ zipWith (*) c $ iterate (*x) 1

(@) (Poly as) = poly as

root r = Poly [-r, 1]

num n = Poly [n]

withRoots rs = product $ map root rs

aberth p = (!! 20) $ iterate (aberth1 p (polyderiv p)) [1..fromIntegral $ plength p - 1]

aberth1 p d zs = let
   ws = map (\k -> let
      zk = zs !! k
      f = p @ zk / d @ zk
      in f / (1 - f * sum (map (\zj -> 1 / (zk - zj)) $ take k zs ++ drop (k + 1) zs))) [0..length zs - 1]
   in zipWith (-) zs ws

solve (Poly [c, b, a]) = let
   d = sqrt (b^2 - 4*a*c)
   in [(-b-d)/(2*a), (d-b)/(2*a)]

solve (Poly [d, c, b, a]) = let
   d0 = b^2-3*a*c
   d1 = 2*b^3-9*a*b*c+27*a^2*d
   cc = ((d1+sqrt(d1^2-4*d0^3)) / 2)**(1/3)
   in [-(b + cc + d0/cc) / (3*a)]

solve p@(Poly [ee, dd, cc, bb, aa]) = let
   Poly [c, b, a, z0, z1] = shift (-bb / (4 * aa)) (Poly [ee/aa, dd/aa, cc/aa, bb/aa, 1])
   (y:_) = trace (show [z0, z1]) $ solve $ Poly [a*c - b^2/4, -2*c, -a, 2]
   sy2a = sqrt (2*y - a)
   in map (subtract $ bb / (4 * aa)) [
         ( sy2a + sqrt (-2*y - a - 2*b / sy2a)) / 2,
         ( sy2a - sqrt (-2*y - a - 2*b / sy2a)) / 2,
         (-sy2a + sqrt (-2*y - a + 2*b / sy2a)) / 2,
         (-sy2a - sqrt (-2*y - a + 2*b / sy2a)) / 2]

check solve poly = let roots = solve poly in trace (show (mapfxx (poly @) roots) ++ show poly) roots

polyderiv (Poly as) = Poly $ zipWith (*) [1..] $ tail as
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

data Complex a = Complex a a

instance (Num a, Floating a) => Num (Complex a) where
  Complex a b + Complex c d = Complex (a + c) (b + d)
  Complex a b - Complex c d = Complex (a - c) (b - d)
  Complex a b * Complex c d = Complex (a * c - b * d) (a * d + b * c)
  fromInteger a = Complex (fromInteger a) 0
  signum (Complex a b) = Complex (signum a) (signum b)
  abs (Complex a b) = Complex (sqrt (a^2 + b^2)) 0
  
instance Floating a => Fractional (Complex a) where
  Complex a b / Complex c d = let e = c*c - d*d in Complex (a / e) (b / e)
  fromRational a = Complex (fromRational a) 0 

instance RealFloat a => Floating (Complex a) where
   log c = let
      (rho, theta) = polar c
      in Complex (log rho) theta
   z@(Complex a b) ** w@(Complex c d) = let
      (rho, theta) = polar z
      in rect (rho ** c * exp (- d * theta), d * log rho + c * theta)

polar (Complex a b) = (sqrt (a*a + b*b), atan2 b a) 

rect (rho, theta) = Complex (rho * cos theta) (rho * sin theta)