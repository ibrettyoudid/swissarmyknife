{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Move filter" #-}
{-# HLINT ignore "Parenthesize unary negation" #-}

module MPoly where

import Favs
import Poly
import Matrix hiding (zero)

import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Ratio
import Debug.Trace

type Mono coeff = ([Int], coeff)

data MPoly coeff = MPoly { vars :: [String], terms :: [Mono coeff] }

msolveNewton [] _ _ = []
msolveNewton mpolys@(mp1:_) inits n = let
   neqs  = length mpolys
   nvars = length $ vars mp1
   jac   = crossWith diffmp [0..nvars-1] mpolys
   [res] = (!! n) $ iterate ((if neqs == nvars
                                 then newton
                                 else newtonRegress) (\[x] -> [map (mpoly x) mpolys]) (\[x] -> map2 (mpoly x) jac)) [inits]

   in res


replaceIndices vp list = let
   indicesSorted = sort vp

   in foldr (uncurry replaceIndex) list indicesSorted

mpoly vals mp = sum $ map (\(ps, c) -> (c *) $ product $ zipWith (^) vals ps) (terms mp)

mono :: coeff -> [(Int, Int)] -> Int -> ([Int], coeff)
mono c vp nv = (replaceIndices vp $ replicate nv 0, c)

monos :: [String] -> [(coeff, [(Int, Int)])] -> MPoly coeff
monos vars terms = MPoly vars $ map (\(coeff, vp) -> mono coeff vp nvars) terms
   where
      nvars = length vars

mnum n = MPoly [] [([], n)]

mvar v = MPoly [v] [([1], 1)]

mvarn vars n = MPoly vars [(replaceIndex n 0 $ replicate (length vars) 0, 1)]

transm vsto vsfrom (ps, c) = let
   vp = zipWith (\power v -> (fromJust (error "var not found") $ S.lookupIndex v vsto, power)) ps vsfrom
   in mono c vp $ S.size vsto

transp vsto (MPoly vsfrom ms) = MPoly (S.toList vsto) $ map (transm vsto vsfrom) ms

collate vsto ms = map (transp vsto) ms

collate1 ms = let
   vsto = S.unions $ map (S.fromList . vars) ms
   in collate vsto ms

collate2 f a b = let
   vsto = S.union (S.fromList $ vars a) (S.fromList $ vars b)
   in if vars a == vars b then f a b else f (transp vsto a) (transp vsto b)

collate2A f a b = let
   [a1, b1] = if vars a == vars b then [a, b] else collate1 [a, b]
   in f a1 b1

instance (Show coeff, Num coeff, Ord coeff, Real coeff) => Show (MPoly coeff) where
   show mp = showmp mp

instance Eq coeff => Eq (MPoly coeff) where
   MPoly z a == MPoly zz b = a == b

instance Ord coeff => Ord (MPoly coeff) where
   compare (MPoly _ a) (MPoly _ b) = compare a b

showmp (MPoly vars []) = "0"
showmp (MPoly vars (t:terms)) = showm0 show vars t ++ concatMap (showm show vars) terms

showmp1 (MPoly vars []) = "0"
showmp1 (MPoly vars (t:terms)) = showm0 shown vars t ++ concatMap (showm shown vars) terms

showm0 shown vars (pis,  0) = "0"
showm0 shown vars (pis,  1) =                                            showOr (showv vars pis) "1"
showm0 shown vars (pis, -1) = "-" ++                                     showOr (showv vars pis) "1"
showm0 shown vars (pis, ci) | ci > 0    = shown (realToFrac ci) ++ ch ++ showv vars pis
                            | otherwise = shown (realToFrac ci) ++ ch ++ showv vars pis
   where
      ch = "*"

showm shown vars (pis,  0) = "" 
showm shown vars (pis,  1) = "+"                                      ++ showOr (showv vars pis) "1"
showm shown vars (pis, -1) = "-"                                      ++ showOr (showv vars pis) "1"
showm shown vars (pis, ci) | ci > 0    = "+" ++ shown (realToFrac ci) ++ showv vars pis
                           | otherwise =        shown (realToFrac ci) ++ showv vars pis

showOr s n = if null s then n else s

showv vars pis = intercalate "*" $ filter (not . null) $ zipWith showz vars pis

showz v pi | pi == 0 = ""
           | pi == 1 = v
           | pi >  1 = v ++ "^" ++ show pi

shown x = let
   (m, n) = decodeFloat x
   in show m ++ 
      if | n == 0 -> ""
         | n >  0 -> "*" ++ show (2^n)
         | n <  0 -> "/" ++ show (2^negate n)

shown1 x = show x

test1 = solvepolys [
            MPoly ["p","x","r","w","a","b","c","d"] [([3,1,0,0,0,0,0,0], 8), ([0,0,3,1,0,0,0,0],  8), ([0,0,0,0,1,0,0,0], 1)],
            MPoly ["p","x","r","w","a","b","c","d"] [([2,1,0,0,0,0,0,0], 4), ([0,0,2,1,0,0,0,0],  4), ([0,0,0,0,0,1,0,0], 1)],
            MPoly ["p","x","r","w","a","b","c","d"] [([1,1,0,0,0,0,0,0], 2), ([0,0,1,1,0,0,0,0],  2), ([0,0,0,0,0,0,1,0], 1)],
            MPoly ["p","x","r","w","a","b","c","d"] [([0,1,0,0,0,0,0,0], 1), ([0,0,0,1,0,0,0,0],  1), ([0,0,0,0,0,0,0,1], 1::Ratio Integer)]
        ]

test2 = let
   [p, x, r, w, a, b, c, d] = map (mvar . singleton) "pxrwabcd"

   in [
      mnum 8 * p^3*x + mnum 8 * r^3*w - a,
      mnum 4 * p^2*x + mnum 4 * r^2*w - b,
      mnum 2 * p  *x + mnum 2 * r  *w - c,
                   x +              w - d]

iii a b = a ^ b

instance (Eq a, Num a) => Num (MPoly a) where
   (*) = collate2 mulpp
   (+) = collate2 addpp
   (-) = collate2 subpp
   fromInteger a = mnum (fromInteger a)

lcmm (pi, ci) (pj, cj) = (zipWith max pi pj, 1)

divm (pi, ci) (pj, cj) = (zipWith (-) pi pj, ci/cj)

mulm (pi, ci) (pj, cj) = (zipWith (+) pi pj, ci*cj)

negm (pi, ci) = (pi, negate ci)

mulpm (MPoly z xs) m = MPoly z $ map (mulm m) xs

mulpp :: (Eq coeff, Num coeff) => MPoly coeff -> MPoly coeff -> MPoly coeff
mulpp p (MPoly z xs) = foldr addpp (MPoly z []) $ map (mulpm p) xs

addpp (MPoly z hi) (MPoly _ hj) = MPoly z $ rinse $ M.toList $ M.unionWith (+) (M.fromList hi) (M.fromList hj)

subpp (MPoly z hi) (MPoly _ hj) = MPoly z $ rinse $ M.toList $ M.unionWith (-) (M.fromList hi) (M.fromList hj)

subst (MPoly vars pcij) (MPoly w n) = let 
   [([], ci), (pj, cj)] = sortOn (length . fst) pcij

   mon = (pj, cj/ci)
   vs  = tail $ findIndices (/= 0) pj -- list variables in the first argument, dropping the head

   in MPoly w $ map (\n1@(pn, cn) -> if any ((/= 0) . (pn !!)) vs then divm n1 mon else n1) n

diffm wrt (pi, ci) = let
   p = pi !! wrt
   in (replaceIndex wrt (p-1) pi, ci * fromIntegral p)

diffmp wrt (MPoly v hi) = MPoly v $ rinse $ map (diffm wrt) hi
{-}
redpp fi fj = let
   mj = negm $ divm gi gj
   in sortp order $ addpp fi (mulpm fj mj)
-}

--substitutePinP v (MPoly vs ms) (MPoly vs1 ns) = 

--differentiatePV (MPoly v ms) vn = MPoly 

sortp order (MPoly z p) = MPoly z $ sortBy order p

rinse xs = filter ((/= 0) . snd) xs

nonzero (MPoly _ []) = False
nonzero _ = True

zero = MPoly [] []

leading order (MPoly _ p) = minimumBy order p

mvd order fi fj = let
   gi  = leading order fi
   gj  = leading order fj
   aij = lcmm gi gj
   mi  = divm aij gi
   mj  = divm aij gj
   hi  = sortp order $ mulpm fi mi
   hj  = sortp order $ mulpm fj mj
   sij = sortp order $ subpp hi hj
   in sij

mvdD order fi fj = do
   --putStrLn $ replicate 80 '-'
   let gi = leading order fi
   let gj = leading order fj
   let aij = lcmm gi gj
   let mi = divm aij gi
   let mj = negm $ divm aij gj
   let hi = sortp order $ mulpm fi mi
   let hj = sortp order $ mulpm fj mj
   let sij = sortp order $ addpp hi hj
   --putStrLn ""
   --putStrLn $ showm (vars fi) "*" mi ++ "( " ++ show fi ++ " ) " ++ showm (vars fj) "*" mj ++ "( " ++ show fj ++ ")"
   --putStrLn $ "= " ++ show sij
   return sij

reduce order polys fi = if fi == zero then return zero else reducelead order polys fi >>= reduce0 order polys

reducelead order polys fi = if fi == zero then return zero else reducelead1 order polys fi polys

reducelead1 _ _ fi [] = return fi
reducelead1 order polys fi (fj:fjs) = do
   let gi = leading order fi
   let gj = leading order fj
   let mj = negm $ divm gi gj
   let sij = sortp order $ addpp fi (mulpm fj mj)
   if fi /= fj && all (>= 0) (fst mj) && logBase (realToFrac $ snd mj) 10 > -300
      then do
         --putStrLn "reducelead"
         --putStrLn $ showm (vars fi) "*" mj ++ "( " ++ show fj ++ " )"
         --putStrLn $ "= " ++ show sij
         reducelead order polys sij
      else reducelead1 order polys fi fjs

reduce0 order polys fi = reduce1 order polys fi polys

reduce1 order polys fi [] = return fi
reduce1 order polys fi fjs = reduce2 order polys fi fjs

reduce2 order polys fi [] = return fi
reduce2 order polys fi fjs = reduce3 order polys fi fjs $ terms fi

reduce3 order polys fi (fj:fjs) [] = reduce2 order polys fi fjs
reduce3 order polys fi (fj:fjs) (gi:gis) = do
   let gj = leading order fj
   let mj = divm gi gj
   let sij = sortp order $ subpp fi (mulpm fj mj)
   if fi /= fj && all (>= 0) (fst mj) && logBase (realToFrac $ snd mj) 10 > -300
      then do
         --putStrLn "reduce"
         --putStrLn $ "- " ++ showm (vars fj) "*" mj ++ show fj
         --putStrLn $ "= " ++ show sij
         reduce order polys sij
      else reduce3 order polys fi (fj:fjs) gis

solvepolys polys = {- buchberger compare $ reorder compare $ -} buchberger2D grevlex polys

buchberger order polys = buchberger1 order polys polys

buchberger1 order old current = let
   new = M.fromList $ mapfxx (leading order) $ concat $ crossWith (mvd order) (map snd $ M.toList old) (map snd $ M.toList current)
   all = M.union old new
   newnew = new M.\\ old
   in if all == old then old else buchberger1 order all newnew

buchbergerD order polys = buchberger1D order polys polys

buchberger1D order old current = do
   let oldl = map snd $ M.toList old
   putStrLn $ replicate 80 '='
   mapM_ print oldl
   getLine
   bl <- sequence $ concat $ crossWith (\x y -> mvdD order x y >>= reduce order oldl) oldl (map snd $ M.toList current)
   let new = M.fromList $ mapfxx (leading order) $ filter (/= zero) bl
   let all = M.union new old
   let newnew = new M.\\ old
   if all == old then return old else buchberger1D order all newnew

buchberger2D order polysl = do
   let polys = M.fromList $ map (\(x, y) -> (fst x, y)) $ mapfxx (leading order) polysl
   putStrLn $ replicate 80 '='
   mapM_ print polysl
   getLine
   let t1 = filter (uncurry (>)) $ concat $ crossWith (,) polysl polysl
   t2 <- firstM (buchberger3D order polys) $ map (\(x, y) -> mvdD order x y >>= reduce order polysl) t1
   case t2 of
      Nothing             -> return polysl
      Just (key, newpoly) -> buchberger2D order $ map snd $ M.toList $ M.insert key newpoly polys

buchberger3D order polys newpoly =
   if newpoly == zero
         then Nothing
         else let key = fst $ leading order newpoly
                  in case M.lookup key polys of
                           Just  j -> ifJust (j /= newpoly) (key, newpoly)
                           Nothing -> Just (key, newpoly)

triang f xs = map (\(x:xs) -> map (f x) xs) (init $ tails xs)


firstM f [] = return Nothing
firstM f (m:ms) = do
   r <- m
   case f r of
      j@(Just _) -> return j
      Nothing    -> firstM f ms

{-
buchberger2 order polys = buchberger1 order polys polys

buchberger3 order old current = let
   new = S.fromList $ concat $ crossWith (mvd order) (S.toList old) (S.toList current)
   all = S.union new old
   newnew = new S.\\ old
   in if all == old then old else buchberger1 order all newnew
-}
grevlex (a,_) (b,_) = case compare (sum b) (sum a) of
                        LT -> LT
                        GT -> GT
                        EQ -> compare b a

reorder order polys = M.fromList $ mapfxx (leading order) $ map snd $ M.toList polys

{-
-4p^3x -4r^3w  = 3(t4+t2)+(t6+t0) 
4p^2x +4r^2w  = t5+t1+2*t3       
2p  x +2r  w  = t4+t2            
      x +    w  = t3               
-}
stagger f xs = map f $ tails xs