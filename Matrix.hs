{-# LANGUAGE FlexibleContexts #-}
-- Copyright 2025 Brett Curtis
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}

module Matrix where

import Favs
import {-# SOURCE #-} MyPretty2 hiding (height)
import Debug 

import Data.List
import Data.Map qualified as M
import Prelude hiding ((<*>))
import Control.Applicative ((<|>))
import System.Random
import GHC.IO (unsafePerformIO)

-- import Prelude qualified

{- matrices are [[a11, a21, a31], [a12, a22, a32]] = | a11 a12 |
-                                                    | a21 a22 |
-                                                    | a31 a32 |
   this is a 3 x 2 matrix in mathematical terminology
   the sublists are column vectors

   a column vector is [[x, y, z]]
-}
a |*| b = map2 (a *) b
infixr 7 |*|

a <*> b = transpose $ map (\arow -> map (sum . zipWith (*) arow) b) $ transpose a
infixr 7 <*>

a <.> b = sum $ map sum $ zipWith2d (*) a b
infixl 7 <.>

a <+> b = zipWith2d (+) a b
infixl 6 <+>

a <-> b = zipWith2d (-) a b
infixl 6 <->

a <++> b = a ++ b
infixr 6 <++>

a <**> b = zipWith (++) a b
infixr 5 <**>

a <!> r = map (!! r) a -- extract row

a <!!> c = a !! c -- extract column

zipWith2d f xs ys = zipWith (zipWith f) xs ys

zipWith3d f xs ys = zipWith (zipWith (zipWith f)) xs ys

xofv [[x, y]] = x
yofv [[x, y]] = y
row r m = m <!> r
col c m = m !! c
rows m = length $ head m
cols m = length m
entry n v = head v !! n
modulus m = sqrt $ sum $ map (^ 2) $ concat m
argument [[x, y]] = atan2 y x
unit m = recip (modulus m) |*| m
minor j k m = deleteIndex j $ map (deleteIndex k) m
height m = length $ head m

gethl m = (height m, length m)
getrc m = (rows m, cols m)
identity (y, x) = map (\i -> take y $ replicate i 0 ++ 1 : replicate (y - i - 1) 0) [0 .. x - 1]
one y i = [replaceIndex i 1 $ replicate y 0]
zero (y, x) = replicate x $ replicate y 0
lpqnorm p q = (** (1 / q)) . sum . map ((** (q / p)) . sum . map (^ p))

rotation theta =
   [ [cos theta, sin theta]
   , [-(sin theta), cos theta]
   ]

rotZ theta = [[cos theta, sin theta, 0], [-(sin theta), cos theta, 0], [0, 0, 1]]
rotY theta = [[cos theta, 0, sin theta], [0, 1, 0], [-(sin theta), 0, cos theta]]
rotX theta = [[1, 0, 0], [0, cos theta, sin theta], [0, -(sin theta), cos theta]]

[[a, b]] >< [[c, d]] = a * c - b * d

-- broyden :: Num a => ([[a]] -> [[a]]) -> a -> [[a]] -> [[a]] -> [[a]] -> [[a]]
broyden f epsilon findy x0 x1 = loop (identity (k, k)) (f x0) (x1 <-> x0) x1
   where
      k = length x0
      jn1 = identity (k, k)
      alpha = 0.1
      -- x0 = replicate k $ replicate 1 0
      x1 = [1] : tail x0
      loop jn1 fn1 dx1 x =
         let
            fn = f x
            df = fn <-> fn1
            jn = jn1 <+> (1 / dx1 <.> dx1) |*| (df <-> jn1 <*> dx1) <*> transpose dx1
            dx = -(alpha |*| invMat jn1 <*> fn)
         in
            loop jn fn dx (x <+> dx)

checkRectangular m = let
   ls = map length m

   in minimum ls == maximum ls

newton f dotf x0 = let
   fx0 = mytrace "f(x0)=" $ f $ mytrace "x0=" x0
   jac = mytrace "jac=" $ dotf x0
   ijac = invMat jac
   x1 = x0 <-> ijac <*> fx0
   
   in x1

newtonRegress f dotf x0 = let
   fx0 = mytrace "f(x0)=" $ f $ mytrace "x0=" x0
   jac = mytrace "jac=" $ dotf x0
   
   in x0 <-> mvRegress jac fx0

quasiNewton f x0 = let
   fx0 = f x0
   jac = quasiJacobi f fx0 x0
   ijac = invMat jac
   x1 = x0 <-> ijac <*> fx0
   
   in x1

quasiNewtonR f x0 = let
   fx0 = f x0
   jac = quasiJacobi f fx0 x0
   (q, r) = householder jac
   (m, n) = gethl r
   dx = gaussElim $ take n $ r <++> transpose q <*> fx0
   x1 = x0 <-> dx
   
   in x1

quasiNewtonD f x0 = do
   let fx0 = f x0
   let jac = quasiJacobi f fx0 x0
   putGrid $ transpose $ map2 show jac
   let ijac = invMat jac
   putGrid $ transpose $ map2 show ijac
   putStrLn ""
   let x1 = x0 <-> 0.01 |*| ijac <*> f x0
   return x1

quasiNewton1 f x0 = let
   fx0 = f x0
   jac = quasiJacobi f fx0 x0
   (y, x) = gethl jac
   dx = map (drop x) $ gaussElim (jac <++> f x0)
   
   in x0 <+> dx

iterateD solve f x0 = do
   print $ x0 ++ f x0
   let x1 = solve f x0
   iterateD solve f x1

iterateDD solve f x0 = do
   print $ x0 ++ f x0
   x1 <- solve f x0
   iterateDD solve f x1

epsilon = 1 + 2 ** -10

epsilon1 = 2 ** -10

quasiJacobi f fx0 x0 = let
   k = length x0
   jac = foldr1 (<++>) $ map (diffAux f fx0 x0) [0 .. k - 1]
   
   in jac

diffAux f fx0 x0 i = let
   a0 = entry i x0
   a1 = if abs a0 > 1e-10 then a0 * epsilon else epsilon1
   x1 = replaceIndex i [a1] x0
   fx1 = f x1
   df = recip (a1 - a0) |*| (fx1 <-> fx0)
   
   in df

differentialEntry i f x0 = let
   fx0 = f x0
   a0 = entry i x0
   a1 = if abs a0 > 1e-10 then a0 * epsilon else epsilon1
   x1 = replaceIndex i [a1] x0
   fx1 = f x1
   df = recip (a1 - a0) |*| (fx1 <-> fx0)
   
   in df

solveSimul m b = drop x $ gaussElim $ m <++> b
   where
      (y, x) = gethl m

randomMat x y g = let
   (m, g1) = uniformListR (x*y) (-10, 10) g 
   in (groupN y m, g1)

testMat x y n = do
   g <- initStdGen
   let mats = take n $ unfoldr (Just . randomMat x y) g :: [[[Double]]] -- :: forall a. (Floating a, UniformRange a) => [[[a]]]
   let invs = map invMatMaybe mats
   return $ sum $ zipWith (\m i -> case i of
      Just  j -> if sum (map sum $ m <*> j <-> identity (y, x)) < 1e-5
         then 1
         else error $ "Matrix inversion gave wrong answer: \nmat="++show m++" \ninv="++show j++" \nmat*inv="++show (m <*> j)
      Nothing -> 0) mats invs


invMat m = fromMaybe (error $ "Matrix could not be inverted: "++show m) $ invMatMaybe m

invMatMaybe [[a]] = if a /= 0 then Just [[recip a]] else Nothing

invMatMaybe [[a, c], [b, d]] = if det /= 0 then Just $ recip det |*| [[d, -c], [-b, a]] else Nothing
   where
      det = a * d - b * c

invMatMaybe [[a, d, g], [b, e, h], [c, f, i]] = if det /= 0 then Just $ recip det |*| [[a1, b1, c1], [c*h-b*i, a*i-c*g, b*g-a*h], [b*f-c*e, c*d-a*f, a*e-b*d]] else Nothing
   where
      det = a * a1 + b * b1 + c * c1
      a1 = e*i-f*h
      b1 = f*g-d*i
      c1 = d*h-e*g

invMatMaybe m = 
   if checkRectangular m 
      then invMatBlocks m <|> Just (invMat1 m)
      else error $ "Matrix not rectangular: "++show m

invMat1 m = map (drop x) $ gaussZero y $ m <**> identity (y, x)
   where
      (y, x) = gethl m

invMatD m = drop x <$> gaussElimDD (m <++> identity (y, x))
   where
      (y, x) = gethl m

invMatBlocks m = let
   (y, x) = gethl m
   in if 
         | x /= y ->
            error $ "Matrix not square! "++show m
#if 0
         | x == 4 ->
            invMatBlocks1 1 1 m 
               <|> invMatBlocks2 3 3 m 
               <|> invMatBlocks1 2 2 m 
               <|> invMatBlocks2 2 2 m 
#endif               
         | even x ->
            invMatBlocks1 (div x 2) (div y 2) m 
               <|> invMatBlocks2 (div x 2) (div y 2) m 
         | odd x ->
            invMatBlocks1 (div x 2) (div y 2) m 
               <|> invMatBlocks2 (div x 2 + 1) (div y 2 + 1) m

invMatBlocks3 x y m = let 
   [[a, c], [b, d]] = mytrace0 "[[a, c], [b, d]]=" $ splitBlocks (mytrace0 "x=" x) (mytrace0 "y=" y) m

   in 
      (do
         ai    <- invMatMaybe a
         schur <- invMatMaybe (d <-> c <*> ai <*> b)
         return $ joinBlocks [[ai <+> ai <*> b <*> schur <*> c <*> ai, -1 |*| schur <*> c <*> ai], [-1 |*| ai <*> b <*> schur, schur]])
         <|>
         (do
            di    <- invMatMaybe d
            schur <- invMatMaybe (a <-> b <*> di <*> c)
            return $ joinBlocks [[schur, -1 |*| di <*> c <*> schur], [-1 |*| schur <*> b <*> di, di <+> di <*> c <*> schur <*> b <*> di]])

invMatBlocks1 x y m = let 
   [[a, c], [b, d]] = mytrace0 "[[a, c], [b, d]]=" $ splitBlocks (mytrace0 "x=" x) (mytrace0 "y=" y) m

   in 
      (do
         ai    <- invMatMaybe a
         schur <- invMatMaybe (d <-> c <*> ai <*> b)
         return $ joinBlocks [[ai <+> ai <*> b <*> schur <*> c <*> ai, -1 |*| schur <*> c <*> ai], [-1 |*| ai <*> b <*> schur, schur]])

invMatBlocks2 x y m = let 
   [[a, c], [b, d]] = mytrace0 "[[a, c], [b, d]]=" $ splitBlocks (mytrace0 "x=" x) (mytrace0 "y=" y) m

   in 
      (do
         di    <- invMatMaybe d
         schur <- invMatMaybe (a <-> b <*> di <*> c)
         return $ joinBlocks [[schur, -1 |*| di <*> c <*> schur], [-1 |*| schur <*> b <*> di, di <+> di <*> c <*> schur <*> b <*> di]])

splitBlocks x y m = let
   (ac, bd) = splitAt x m
   (a, c) = unzip $ map (splitAt y) ac
   (b, d) = unzip $ map (splitAt y) bd

   in [[a, c], [b, d]]

joinBlocks ms = foldr1 (<++>) $ map (foldr1 (<**>)) ms

leftInv m = invMat (transpose m <*> m) <*> transpose m

rightInv m = transpose m <*> invMat (m <*> transpose m)

gaussElim m = gaussZero (height m) m

gaussElimD m = return []

gaussElimDD m = return []

gaussRows yi xs = let
   (z, nz) = mytrace0 "(z, nz)=" $ partition (== 0) xs
   m1      = maximum nz
   m2      = minimum nz
   m       = if m1 > -m2 then m1 else m2
   Just xi = elemIndex m xs
   t       = sqrt (sum (map (^2) nz) - m^2)

   in ifJust (length nz > 1 || length nz == 1 && m /= 1) $ (length nz, m / t, xi, yi, m, xs)

gaussCols size yis xi ys = let
   ys1     = take size ys
   zip1    = zip ys1 [0..]
   (z, nz) = mytrace0 "(z, nz)=" $ partition ((== 0) . fst) $ map (zip1 !!) yis
   (_, yi) = maximum $ map (\(a, b) -> (abs a, b)) nz
   m       = ys1 !! yi
   t       = sqrt (sum (map (^2) ys1) - m^2)

   in ifJust (length nz > 0) (length z, m / t, xi, yi, m, ys)

gaussZero size m = let
   --outerds = catMaybes $ zipWith (countZeroes size) [0..] $ mytrace "m=" m
   innerds = catMaybes $ zipWith gaussRows [0..] $ take size $ transpose $ mytrace "m=" m
   (innernzmax, _, _, _, _, _) = mytrace "(nz, mot, xi, yi, m, xs)=" $ maximum innerds
   (_, _, _, innerpis, _, _) = unzip6 $ filter (\(nz, _, _, _, _, _) -> nz == innernzmax) innerds
   outerds = catMaybes $ zipWith (gaussCols size innerpis) [0..] $ m
   (outerzmax, mot, outerpi, inneri, mx, outerp) = mytrace "(z, mot, xi, yi, m, xs)=" $ maximum outerds
   outerp2 = mytrace "outerp2=" $ replaceIndex inneri 1 $ map (/ mx) outerp
   outers2 = zipWith (gaussAux2 inneri outerp2 outerpi) [0..] m
   
   in if null innerds
         then rsort m
         else gaussZero size outers2 

gaussAux m rowi coli = let
   c1 = map (^ 2) $ m <!> coli
   x = c1 !! rowi
   o = sum $ deleteIndex rowi c1
   
   in x / o

gaussAux2 rowi colp colpi coli col = let
   mult = col !! rowi -- / colp !! rowi
   
   in if mult /= 0 then mytrace ("mult="++show mult++"\ncolpi="++show colpi++"\ncoli="++show coli++"\ncol="++show col++"\nnewcol=") $ replaceIndex rowi 0 $ zipWith (gaussAux3 mult) col colp else col

gaussAux3 mult e p = e - p * mult

leastSquares x y = let
   (q, r) = qrDecompose x
   (_, n) = gethl r
   beta = solveSimul r $ map (take n) (transpose q <*> y)
   
   in beta

--mvRegress x y = invMat (transpose x <*> x) <*> transpose x <*> y
mvRegress x y = leftInv x <*> y

qrDecompose a = householder a

householder a = let
   (m, n) = gethl a
   q0 = identity (m, m)
   
   in householder1 m n q0 a

householder1 m0 n0 q0 a = let
   (m, n) = gethl a
   j   = 0 -- coli
   k   = 0 -- rowpi
   x   = map (singleton . (!! j)) a
   [xk] = x !! k
   alpha = signum xk * modulus x
   ae  = replaceIndex k [alpha] $ replicate m [0]
   u   = replaceIndex k [xk - alpha] x
   v   = unit u
   qk' = identity (m, m) <-> 2 |*| v <*> transpose v
   qk  = identity (m0 - m, m0 - m) <++> zero (m0 - m, m) <**> zero (m, m0 - m) <++> qk'
   q   = qk <*> q0
   r   = qk' <*> a
   r'  = tail $ map tail r
   (q2, r2) = householder1 m0 n0 q r'
   res = ae <++> (tail (col 0 r) : r2)
   
   in if n <= 1
         then (q0, a)
         else (q2, res)

showMat name vals = do
   putStrLn $ name ++ "="
   putGrid $ transpose $ map2 show vals

householderD a = let
   (m, n) = gethl a
   q0 = identity (m, m)
   
   in householderD1 m n q0 a

householderD1 m0 n0 q0 a = let
   {-
      colis = gaussLeadTerms a
      (coli, _) = minimum colis
      rowis = filter (\rowi -> (== Just coli) $ findIndex (/= 0) $ a !! rowi) [0 .. length a - 1]
      rowpi = snd $ maxOn (gaussAux a coli) rowis
      j = coli
      k = rowpi
      -}
   (m, n) = gethl a
   in
   if n >= 2
      then do
      showMat "a" a
      let j = 0 -- coli
      let k = 0 -- rowpi
      let x = map (singleton . (!! j)) a
      let [xk] = x !! k
      let alpha = signum xk * modulus x
      let ae = replaceIndex k [alpha] $ replicate m [0]
      let u = replaceIndex k [xk - alpha] x
      let v = unit u
      let qk' = identity (m, m) <-> 2 |*| v <*> transpose v
      showMat "qk'" qk'
      let qk = identity (m0 - m, m0 - m) <++> zero (m0 - m, m) <**> zero (m, m0 - m) <++> qk'
      showMat "qk" qk
      let q = qk <*> q0
      showMat "q" q
      let r = qk' <*> a
      showMat "r" r
      let r' = tail $ map tail r
      showMat "r'" r'
      (q2, r2) <- householderD1 m0 n0 q r'
      showMat "q2" q2
      showMat "r2" r2
      showMat "ae" ae
      let res = ae <++> (tail (col 0 r) : r2)
      showMat "res" res
      return (q2, res)
      else
      return (q0, a)

gaussLeadTerms m = filter ((> 1) . snd) $ counts1 $ mapMaybe (findIndex (/= 0)) $ transpose m

householderA (q0, a) = let
   q1 = householderA1 a
   q = q1 <*> q0
   r = q1 <*> a
   
   in (q, r)

householderA1 a = let
   colis = gaussLeadTerms a
   (coli, _) = minimum colis
   rowis = filter (\rowi -> (== Just coli) $ findIndex (/= 0) $ a !! rowi) [0 .. length a - 1]
   rowpi = snd $ maxOn (gaussAux a coli) rowis
   (m, n) = gethl a
   j = coli
   k = rowpi
   x = map (singleton . (!! j)) a
   alpha = modulus x
   [xk] = x !! k
   u = replaceIndex k [xk - alpha] x
   v = unit u
   qk = identity (m, m) <-> 2 |*| v <*> transpose v
   
   in qk

householderB a = let
   (m, n) = gethl a
   q0 = identity (m, m)
   
   in householderB1 m (q0, a)

householderB1 m0 (q0, a) = let
   {-
      colis = gaussLeadTerms a
      (coli, _) = minimum colis
      rowis = filter (\rowi -> (== Just coli) $ findIndex (/= 0) $ a !! rowi) [0 .. length a - 1]
      rowpi = snd $ maxOn (gaussAux a coli) rowis
      j = coli
      k = rowpi
      -}
   (m, n) = gethl a
   j = 0 -- coli
   k = 0 -- rowpi
   x = [a <!> j]
   xk = entry k x
   alpha = signum xk * modulus x
   ae = [replaceIndex k alpha $ replicate m 0]
   u = [replaceIndex k (xk - alpha) $ head x]
   v = unit u
   qk' = identity (m, m) <-> 2 |*| v <*> transpose v
   qk = identity (m0 - m, m0 - m) <++> zero (m0 - m, m) ++ zero (m, m0 - m) <++> qk'
   q = qk <*> q0
   r = qk' <*> a
   r' = tail $ map tail r
   (q2, r2) = householderB1 m0 (q, r')
   
   in (q2, if n <= 1 then a else ae <++> (tail (col 0 r) : r2))

{-
cramer [[a, b], [c, d]] = let
   rdet = 1 / (a*d - b*c)
   in (1 / (a*d - b*c)) * [[ d, -b],
                     [-c,  a]]
-}
{-
cramer [[a, b, c], [d, e, f], [g, h, i]] = let
   ai = c*i - f*h
   bi = -(d*i - f*g)
   ci = d*h - e*g
   di =

blockInv m = let
   x     = length (head m)
   y     = length m
   xs    = div x 2
   ys    = div y 2
   (ac, bd) = unzip $ map (splitAt xs) m
   (a, c)   = splitAt ys ac
   (b, d)   = splitAt ys bd
   ra    = recip a
   coa   = c * ra
   schur = recip (d - coa * b)
   ai    = ra + ra * b * schur * coa
   bi    = - ra * b * schur
   ci    = - schur * coa
   di    = schur
   in zipWith (++) ai bi ++ zipWith (++) ci di
-}
