{-# LANGUAGE FlexibleContexts #-}
-- Copyright 2025 Brett Curtis
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Matrix where

import Favs
import MyPretty2 hiding (height)

import Data.List
import Data.Map qualified as M
import Prelude hiding ((<*>))

-- import Prelude qualified

{- matrices are [[a11, a21, a31], [a12, a22, a32]] = | a11 a12 |
 -                                                   | a21 a22 |
 -                                                   | a31 a32 |
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

a <!> r = map (!! r) a

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

checkRectangular m = (== 1) $ length $ group $ map length m
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

invMat m = drop x $ gaussElim $ m <++> identity (y, x)
 where
  (y, x) = gethl m

invMatD m = drop x <$> gaussElimDD (m <++> identity (y, x))
 where
  (y, x) = gethl m

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

quasiNewton f x0 =
  let
    fx0 = f x0
    jac = quasiJacobi f fx0 x0
    ijac = invMat jac
    x1 = x0 <-> 0.01 |*| ijac <*> fx0
   in
    x1

quasiNewtonR f x0 =
  let
    fx0 = f x0
    jac = quasiJacobi f fx0 x0
    (q, r) = householder jac
    (m, n) = gethl r
    dx = gaussElim $ take n $ r <++> transpose q <*> fx0
    x1 = x0 <-> dx
   in
    x1

quasiNewtonD f x0 = do
  let fx0 = f x0
  let jac = quasiJacobi f fx0 x0
  putGrid $ transpose $ map2 show jac
  let ijac = invMat jac
  putGrid $ transpose $ map2 show ijac
  putStrLn ""
  let x1 = x0 <-> 0.01 |*| ijac <*> f x0
  return x1

quasiNewton1 f x0 =
  let
    fx0 = f x0
    jac = quasiJacobi f fx0 x0
    (y, x) = gethl jac
    dx = map (drop x) $ gaussElim (jac <++> f x0)
   in
    x0 <+> dx

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

quasiJacobi f fx0 x0 =
  let
    k = length x0
    jac = foldr1 (<++>) $ map (diffAux f fx0 x0) [0 .. k - 1]
   in
    jac

diffAux f fx0 x0 i =
  let
    a0 = entry i x0
    a1 = if abs a0 > 1e-10 then a0 * epsilon else epsilon1
    x1 = replaceIndex i [a1] x0
    fx1 = f x1
    df = recip (a1 - a0) |*| (fx1 <-> fx0)
   in
    df

solveSimul m b = drop x $ gaussElim $ m <++> b
 where
  (y, x) = gethl m

gaussElim m = gaussDoTrailingTerms $ gaussDoLeadTerms m

gaussDoLeadTerms m = until (null . gaussLeadTerms) gaussZeroLeadTerms m

gaussDoTrailingTerms m = until (null . gaussTrailingTerms) gaussZeroTrailingTerms m

untilD f g x = if f x then return x else let gx = g x in print gx >> untilD f g gx

untilDD f g x = if f x then return x else do gx <- g x; print gx; untilDD f g gx

gaussElimD m = untilD (null . gaussLeadTerms) gaussZeroLeadTerms m >>= untilD (null . gaussTrailingTerms) gaussZeroTrailingTerms

gaussElimDD m = untilDD (null . gaussLeadTerms) gaussZeroLeadTermsD m >>= untilDD (null . gaussTrailingTerms) gaussZeroTrailingTermsD

gaussLeadTerms m = filter ((> 1) . snd) $ M.toList $ counts1 $ mapMaybe (findIndex (/= 0)) $ transpose m

gaussZeroLeadTerms m =
  let
    colis = gaussLeadTerms m
    (coli, _) = minimum colis
    rowis = filter (\rowi -> (== Just coli) $ findIndex (/= 0) $ transpose m !! rowi) [0 .. height m - 1]
    rowpi = snd $ maxOn (gaussAux m coli) rowis
    rowp = m <!> rowpi
    rowp2 = map (/ rowp !! coli) rowp
    rows = map (m <!>) $ filter (/= rowpi) rowis
    rows2 = map (gaussAux2 coli rowp2) rows
   in
    transpose (map (m <!>) ([0 .. height m - 1] \\ rowis) ++ rowp2 : rows2)

gaussZeroLeadTermsD m =
  let
    colis = gaussLeadTerms m
    (coli, _) = minimum colis
    rowis = filter (\rowi -> (== Just coli) $ findIndex (/= 0) $ transpose m !! rowi) [0 .. height m - 1]
    rowpi = snd $ maxOn (gaussAux m coli) rowis
    rowp = m <!> rowpi
    rowp2 = map (/ rowp !! coli) rowp
    rows = map (transpose m !!) $ filter (/= rowpi) rowis
    rows2 = map (gaussAux2 coli rowp2) rows
   in
    do
      putGrid [["m", "colis", "coli", "rowis", "rowpi", "rowp", "rowp2", "rows", "rows2"], [show $ transpose m, show colis, show coli, show rowis, show rowpi, show rowp, show rowp2, show rows, show rows2]]
      return $ transpose (map (transpose m !!) ([0 .. height m - 1] \\ rowis) ++ rowp2 : rows2)

gaussTrailingTerms m = filter (\i -> (> 1) $ length $ filter (\row -> row !! i /= 0) $ transpose m) $ mapMaybe (findIndex (/= 0)) $ transpose m

gaussZeroTrailingTerms m =
  let
    colis = gaussTrailingTerms m
    coli = maximum colis
    rowis = filter (\rowi -> m !! coli !! rowi /= 0) [0 .. height m - 1]
    rowpi = head $ filter (\rowi -> (== Just coli) $ findIndex (/= 0) $ m <!> rowi) rowis
    rowp = transpose m !! rowpi
    rowp2 = map (/ rowp !! coli) rowp
    rows = map (transpose m !!) $ filter (/= rowpi) rowis
    rows2 = map (gaussAux2 coli rowp2) rows
   in
    transpose (rows2 ++ rowp2 : map (transpose m !!) ([0 .. height m - 1] \\ rowis))

gaussZeroTrailingTermsD m =
  let
    colis = gaussTrailingTerms m
    coli = maximum colis
    rowis = filter (\rowi -> m !! coli !! rowi /= 0) [0 .. height m - 1]
    rowpi = head $ filter (\rowi -> (== Just coli) $ findIndex (/= 0) $ m <!> rowi) rowis
    rowp = transpose m !! rowpi
    rowp2 = map (/ rowp !! coli) rowp
    rows = map (transpose m !!) $ filter (/= rowpi) rowis
    rows2 = map (gaussAux2 coli rowp2) rows
   in
    do
      putGrid [["m", "colis", "coli", "rowis", "rowpi", "rowp", "rowp2", "rows", "rows2"], [show $ transpose m, show colis, show coli, show rowis, show rowpi, show rowp, show rowp2, show rows, show rows2]]
      return $ transpose (rows2 ++ rowp2 : map (transpose m !!) ([0 .. height m - 1] \\ rowis))

gaussAux m coli rowi =
  let
    r1 = map (^ 2) $ m <!> rowi
    x = r1 !! coli
    o = sum $ deleteIndex coli r1
   in
    x / o

gaussAux2 coli rowp row =
  let
    mult = row !! coli / rowp !! coli
   in
    zipWith (gaussAux3 mult) row rowp

gaussAux2d coli rowp row =
  let
    mult = row !! coli / rowp !! coli
   in
    (mult, zipWith (gaussAux3 mult) row rowp)

gaussAux3 mult r p = r - p * mult

leastSquares x y = let
  (q, r) = qrDecompose x
  (_, n) = gethl r
  beta = solveSimul r $ map (take n) (transpose q <*> y)
  in beta

mvRegress x y = invMat (transpose x <*> x) <*> transpose x <*> y

qrDecompose a = householder a

householder a =
  let
    (m, n) = gethl a
    q0 = identity (m, m)
   in
    householder1 m n q0 a

householder1 m0 n0 q0 a = let
  (m, n) = gethl a
  j = 0 -- coli
  k = 0 -- rowpi
  x = map (singleton . (!! j)) a
  [xk] = x !! k
  alpha = signum xk * modulus x
  ae = replaceIndex k [alpha] $ replicate m [0]
  u  = replaceIndex k [xk - alpha] x
  v  = unit u
  qk' = identity (m, m) <-> 2 |*| v <*> transpose v
  qk  = identity (m0 - m, m0 - m) <++> zero (m0 - m, m) <**> zero (m, m0 - m) <++> qk'
  q = qk <*> q0
  r = qk' <*> a
  r' = tail $ map tail r
  (q2, r2) = householder1 m0 n0 q r'
  res = ae <++> (tail (col 0 r) : r2)
  in if n <= 1
        then (q0, a)
        else (q2, res)

showMat name vals = do
  putStrLn $ name ++ "="
  putGrid $ transpose $ map2 show vals

householderD a =
  let
    (m, n) = gethl a
    q0 = identity (m, m)
   in
    householderD1 m n q0 a

householderD1 m0 n0 q0 a =
  let
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

householderA (q0, a) =
  let
    q1 = householderA1 a
    q = q1 <*> q0
    r = q1 <*> a
   in
    (q, r)

householderA1 a =
  let
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
   in
    qk

householderB a =
  let
    (m, n) = gethl a
    q0 = identity (m, m)
   in
    householderB1 m (q0, a)

householderB1 m0 (q0, a) =
  let
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
   in
    (q2, if n <= 1 then a else ae <++> (tail (col 0 r) : r2))

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
