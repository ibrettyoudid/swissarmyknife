{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Move filter" #-}
{-# HLINT ignore "Parenthesize unary negation" #-}
module MPoly where

import Favs
import Poly

import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Ratio
import Debug.Trace

-- frequency of a sine wave (in a list) in radians/sample
freq (x0:x1:x2:_) = acos ((x2+x0)/(2*x1))

-- frequency, amplitude and phase (at first sample) of a sine wave
paramsOfWave (x0:x1:x2:_) = let
      p     = (x2+x0)/(2*x1)
      q     = sqrt (1 - p^2)
      freq  = acos p
      y0    = (x1-p*x0)/q
      amp   = sqrt (x0^2+y0^2)
      phase = atan2 x0 y0
      in (freq, amp, phase)

waveOfParams :: (Floating b, Enum b) => (b, b, b) -> [b]
waveOfParams (freq, amp, phase) = map ((*amp) . sin) [phase, phase+freq ..]

wop = waveOfParams
pow = paramsOfWave

wop2 a b = zipWith (+) (wop a) (wop b)
pow2 xs = paramsOfWave2 xs
pow2a = paramsOfWave2a

wopn ps = map sum $ transpose $ map wop ps

type Mono coeff = ([Int], coeff)

data MPoly coeff = MPoly { vars :: [String], terms :: [Mono coeff] }

instance (Show coeff, Num coeff, Ord coeff, Real coeff) => Show (MPoly coeff) where
   show p@(MPoly z ms) = if p == zero then "0" else concatMap (showm z "") ms

instance Eq coeff => Eq (MPoly coeff) where
   MPoly z a == MPoly zz b = a == b

instance Ord coeff => Ord (MPoly coeff) where
   compare (MPoly _ a) (MPoly _ b) = compare a b

showm z ch (pi,  0) = "+0" ++ ch
showm z ch (pi,  1) = "+"                                       ++ concat (zipWith showz pi z) ++ if any (> 0) pi then ch else ""
showm z ch (pi, -1) = "-"                                       ++ concat (zipWith showz pi z) ++ if any (> 0) pi then ch else ""
showm z ch (pi, ci) | ci > 0 = "+" ++ showFF 10 (realToFrac ci) ++ concat (zipWith showz pi z) ++ ch
                              | otherwise =     showFF 10 (realToFrac ci) ++ concat (zipWith showz pi z) ++ ch

showz pi v | pi == 0 = ""
           | pi == 1 = v
           | pi >  1 = v ++ "^" ++ show pi

test1 = solvepolys [
            MPoly ["p","x","r","w","a","b","c","d"] [([3,1,0,0,0,0,0,0],-4), ([0,0,3,1,0,0,0,0], -4), ([0,0,0,0,1,0,0,0], 1)],
            MPoly ["p","x","r","w","a","b","c","d"] [([2,1,0,0,0,0,0,0], 4), ([0,0,2,1,0,0,0,0],  4), ([0,0,0,0,0,1,0,0], 1)],
            MPoly ["p","x","r","w","a","b","c","d"] [([1,1,0,0,0,0,0,0], 2), ([0,0,1,1,0,0,0,0],  2), ([0,0,0,0,0,0,1,0], 1)],
            MPoly ["p","x","r","w","a","b","c","d"] [([0,1,0,0,0,0,0,0], 1), ([0,0,0,1,0,0,0,0],  1), ([0,0,0,0,0,0,0,1], 1::Ratio Integer)]
        ]

lcmm (pi, ci) (pj, cj) = (zipWith max pi pj, 1)

divm (pi, ci) (pj, cj) = (zipWith (-) pi pj, ci/cj)

mulm (pi, ci) (pj, cj) = (zipWith (+) pi pj, ci*cj)

negm (pi, ci) = (pi, negate ci)

mulpm (MPoly z xs) m = MPoly z $ map (mulm m) xs

mulpp p (MPoly z xs) = foldr addpp (MPoly z []) $ map (mulpm p) xs

addpp (MPoly z hi) (MPoly _ hj) = MPoly z $ rinse $ M.toList $ M.unionWith (+) (M.fromList hi) (M.fromList hj)

subpp (MPoly z hi) (MPoly _ hj) = MPoly z $ rinse $ M.toList $ M.unionWith (-) (M.fromList hi) (M.fromList hj)

sortp order (MPoly z p) = MPoly z $ sortBy order p

rinse xs = filter ((/= 0) . snd) xs

nonzero (MPoly _ []) = False
nonzero _ = True

zero = MPoly [] []

leading order (MPoly _ p) = minimumBy order p

mvd order fi fj = let
   gi = leading order fi
   gj = leading order fj
   aij = lcmm gi gj
   mi = divm aij gi
   mj = divm aij gj
   hi = sortp order $ mulpm fi mi
   hj = sortp order $ mulpm fj mj
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

paramsOfWave2 xs = let
   (t0:t1:t2:t3:t4:t5:t6:t7:t8:_) = map realToFrac xs
   in solvepolys [
                        MPoly ["p","x","r","w"] [([4,1,0,0], 16), ([0,0,4,1], 16), ([0,0,0,0], -(t8+4*t6+6*t4+4*t2+t0))],
                        MPoly ["p","x","r","w"] [([3,1,0,0], -4), ([0,0,3,1], -4), ([0,0,0,0], -(t7+3*t5+3*t3+t1))],
                        MPoly ["p","x","r","w"] [([2,1,0,0],  4), ([0,0,2,1],  4), ([0,0,0,0], -(t6+2*t4+t2))],
                        MPoly ["p","x","r","w"] [([1,1,0,0],  2), ([0,0,1,1],  2), ([0,0,0,0], -(t5+t3))],
                        MPoly ["p","x","r","w"] [([0,1,0,0],  1), ([0,0,0,1],  1), ([0,0,0,0], -t4::Ratio Integer)]
      ]

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
paramsOfWave2a (t0:t1:t2:t3:t4:t5:t6:t7:t8:_) = let
   a = t8+4*t6+6*t4+4*t2+t0
   b = t7+3*t5+3*t3+t1
   c = t6+2*t4+t2
   d = t5+t3
   e = t4
   {-
   a =  16p^4x+16r^4w
   b =  -4p^3x -4r^3w
   c =   4p^2x +4r^2w
   d =   2p  x +2r  w
   e =       x +    w

   t5+t3 = 2px4 + 2rw4
   t5-t3 = 2qy3 + 2sz3
   -}
   
   (r:_) = check aberth $ Poly [4*(a*d^2-c^3), 8*(c^2*d - a*d*e), 16*(a*e^2 - c^2), 32*(c*d - c*d*e), 64*(c*e^2 - d^2)]
   s = sqrt (1 - r*r)
   p = (c - 2*d*r)/(2*d - 4*e*r)
   q = sqrt (1 - p*p)
   x = (d/2 - e*r)/(p - r)
   w = e - x
   freq1 = acos p
   freq2 = acos r
   in (freq1, freq2, s)

pow2b (x0:x1:x2:x3:x4:x5:_) = [pow [x0,x1,x2], pow [x1,x2,x3], pow [x2,x3,x4], pow [x3,x4,x5], pow [x0,x2,x4], pow [x1,x3,x5]]

stagger f xs = map f $ tails xs




{-
t6 = (p^2-q^2)x4+2pqy4 + (r^2-s^2)w4+2rsz4
t5 = px4+qy4 + rw3+sz3
t3 = px4-qy4 + rw3-sz3
t5+t3 = 2px4 + 2rw4
t5-t3 = 2qy4 + 2sz4

t2 = (p^2-q^2)x4-2pqy4 + (r^2-s^2)w4-2rsz4

t6+t2 = 2(p^2-q^2)x4 + 2(r^2-s^2)w4
t6-t2 = 4pqy4  + 4rsz4

t7 = (p^3-3pq^2)x3 + (3p^2q-q^3)y3 + (r^3-3rs^2)w3 + (3r^2s-s^3)z3
t1 = (p^3-3pq^2)x3 - (3p^2q-q^3)y3 + (r^3-3rs^2)w3 - (3r^2s-s^3)z3

t7+t1 = 2(p^3-3pq^2)x4 + 2(r^3-3rs^2)w4
t7-t1 = 2(3p^2q-q^3)y4 + 2(3r^2s-s^3)z4

t8+t0 = 2(p^4-6p^2q^2+q^4)x4 + 2(r^4-6r^2s^2+s^4)w4
t7+t1 = 2(p^3-3pq^2)x4       + 2(r^3-3rs^2)w4 
t6+t2 = 2(p^2-q^2)  x4       + 2(r^2-s^2)  w4
t5+t3 = 2p          x4       + 2r          w4
t4    =             x4       +             w4

t8+t0           = 2(p^4-6p^2(1-p^2)+(1-p^2)^2)x4 + 2(r^4-6r^2s^2+s^4)w4
t8+t0           = 2(p^4-6p^2+6p^4+p^4-2p^2+1)x4  + 2(r^4-6r^2s^2+s^4)w4
t8+t0           = 2(8p^4-8p^2+1)x4               + 2(8r^4-8r^2+1)w4
t8+t0+8*(t5+t3) = 2(8p^4+1)x4                    + 2(8r^4+1)w4
t8+t0+8*(t5+t3)-2t4 = 16p^4x4                    + 16r^4w4
t7+t1+3*(t5+t3)

t7+t1 = 2(p^3-3p(1-p^2))x4 + 2(r^3-3r(1-r^2)w4
t7+t1 = 2(-3p-2p^3))x4 + 2(-3r-2r^3)w4
t7+t1 = (-4p^3 - 6p)x4 + (-4r^3 - 6r)w4 

[ p  q][p^3-3pq^2 3p^2q-q^3] = [p^4-6p^2q^2+q^4 4p^3q-4pq^3    ]
[-q  p][q^3-3p^2q p^3-3pq^2]   [4pq^3-4p^3q     p^4-6p^2q^2+q^4]

q^2 = 1-p^2

p^2-q^2+p^2+q^2 = p^2-q^2+1 = 2p^2
p^2-q^2 = 2p^2 - 1
t5+t1 = 2(2p^2-1)  x3 + 2(2r^2-1)  w3

t6+t0 = (-4p^3-6p)x3 + (-4r^3 - 6r)w3 
t5+t1 = (4p^2-2)x3 + (4r^2-2)w3
t4+t2 = 2p          x3 + 2r          w3
t3    = x3 + w3

x1=px0+sqrt(1-p^2)y0
x2=(2p^2-1)x0+2psqrt(1-p^2)y0
x2+x0=2p^2x0+2psqrt(1-p^2)y0

x1=px0+sqrt(1-p^2)y0
x1^2=p^2x0^2+2pqx0y0+q^2y0^2

x1-px0=sqrt(1-p^2)y0
(x1-px0)^2=(1-p^2)^2y0^2
x1^2+p^2x0^2-px0x1=(1+p^4-p^2)y0^2
x1^2+p^2x0^2-px0x1=y0^2+p^4y0^2-p^2y0^2

[ p  q][xn]=[xn+1]
[-q  p][yn]=[yn+1]
x1=px0+qy0
y1=py0-qx0
p=cos f
q=sin f
p^2+q^2=1
q^2=1-p^2
q=sqrt(1-p^2)
[ r  s][wn]=[wn+1]
[-s  r][zn]=[zn+1]

tn=xn+wn
t0=x0+w0
t1=px0+qy0+rw0+sz0

x2+x0=2px1
w2+w0=2rw1
t2+t0=x2+w2+x0+w0=2px1+2rw1

x3=px2+qy2
   =p((2p^2-1)x0+2pqy0)+q((2p^2-1)y0-2pqx0)
   =p(2ppx0-x0+2pqy0)+q(2ppy0-y0-2pqx0)
   =2pppx0-px0+2ppqy0+2ppqy0-qy0-2pqqx0
   =2pppx0-px0+2ppqy0+2ppqy0-qy0-2p(1-pp)x0
   =2pppx0-px0+2ppqy0+2ppqy0-qy0-2px0+2pppx0
   =4pppx0-3px0+4ppqy0-qy0
   =4p^3x0-3px0+4p^2qy0-qy0
   =4pppx0-3px0+4pp(x1-px0)-(x1-px0)
   =4pppx0-3px0+4ppx1-4pppx0-x1-px0
   =-4px0+4ppx1
y3=py2-qx2  
y3=p((2p^2-1)y0-2pqx0)-q((2p^2-1)x0+2pqy0)
   =p(2ppy0-y0-2pqx0)-q(2ppx0-x0+2pqy0)
   =2pppy0-py0-2ppqx0-2ppqx0+qx0-2pqqy0
   =2pppy0-py0-4ppqx0+qx0-2pqqy0
   =2pppy0-py0-4ppqx0+qx0-2p(1-pp)y0
   =2pppy0-py0-4ppqx0+qx0-2py0+2pppy0
   =4pppy0-3py0-4ppqx0+qx0
   =4p^3y0-3py0-4p^2qx0+qx0
   
x4=px3+qy3
   =p(4pppx0-3px0+4ppqy0-qy0)+q(4pppy0-3py0-4ppqx0+qx0)
   =4ppppx0-3ppx0+4pppqy0-pqy0+4pppqy0-3pqy0-4ppqqx0+qqx0
   =4ppppx0-3ppx0+8pppqy0-4pqy0-4ppqqx0+qqx0
   =4ppppx0-3ppx0+8pppqy0-4pqy0-(1-pp)4ppx0+(1-pp)x0
   =4ppppx0-3ppx0+8pppqy0-4pqy0-4ppx0+4ppppx0+x0-ppx0
   =8ppppx0-8ppx0+8pppqy0-4pqy0+x0
   =8ppppx0-8ppx0+x0+8pppqy0-4pqy0
   =8ppppx0-8ppx0+x0+8ppp(x1-px0)-4p(x1-px0)
   =8ppppx0-8ppx0+x0+8pppx1-8ppppx0-4px1+4ppx0
   =-4ppx0+x0+8pppx1-4px1

y4=py3-qx3
   =p(4p^3y0-3py0-4p^2qx0+qx0)-q(4p^3x0-3px0+4p^2qy0-qy0)
   =4p^4y0-3p^2y0-4p^3qx0+pqx0-4p^3qx0+3pqx0-4p^2q^2y0+q^2y0
   =4p^4y0-3p^2y0-8p^3qx0+4pqx0-4p^2q^2y0+q^2y0
   =4p^4y0-3p^2y0-8p^3qx0+4pqx0-4p^2(1-p^2)y0+(1-p^2)y0
   =4p^4y0-3p^2y0-8p^3qx0+4pqx0-4p^2y0+4p^4y0+y0-p^2y0
   =8p^4y0-8p^2y0-8p^3qx0+4pqx0+y0
   =8p^4y0-8p^2y0+y0-8p^3qx0+4pqx0
   =8ppppy0-8ppy0+y0-8pppqx0+4pqx0
   
   
x5=px4+qy4
   =p(8ppppx0-8ppx0+x0+8pppqy0-4pqy0)+q(8ppppy0-8ppy0+y0-8pppqx0+4pqx0)
   =8pppppx0-8pppx0+px0+8ppppqy0-4ppqy0+8ppppqy0-8ppqy0+qy0-8pppqqx0+4pqqx0
   =8pppppx0-8pppx0+px0+16ppppqy0-12ppqy0+qy0-8pppqqx0+4pqqx0
   =8pppppx0-8pppx0+px0+16ppppqy0-12ppqy0+qy0-8ppp(1-pp)x0+4p(1-pp)x0
   =8pppppx0-8pppx0+px0+16ppppqy0-12ppqy0+qy0-8pppx0+8pppppx0+4px0-4pppx0
   =16pppppx0-20pppx0+5px0+16ppppqy0-12ppqy0+qy0
y5=py4-qx4
   =16pppppy0-20pppy0+5py0-16ppppqx0+12ppqx0-qx0

t0=x0+w0
t1=x1+w1=px0+qy0+rw0+sz0
t2=2p^2x0-x0+2pqy0+2r^2w0-w0+2rsz0


t2+t0=x2+x0+w2+w0
      =2px1+2rw1
      =2px1+2r(t1-x1)
      =2px1+2rt1-2rx1
t2=x2+w2
   =-x0+2px1-w0+2rw1
   =-x0+2px1-(t0-x0)+2r(t1-x1)
   =-x0+2px1-t0+x0+2rt1-2rx1
   =2px1-t0+2rt1-2rx1
t3=x3+w3
   =-4px0+4ppx1-4rw0+4rrw1
   =-4px0+4ppx1-4r(t0-x0)+4rr(t1-x1)
   =-4px0+4ppx1-4rt0+4rx0+4rrt1-4rrx1

-}

peaks xs = aux xs 0 where
      aux [] _ = []
      aux [x0] _ = []
      aux [x0,x1] _ = []
      aux (x0:x1:x2:xs) n =
         (if x1 > x0 && x1 >= x2 then (n:) else id) $ aux (x1:x2:xs) (n+1)

pdiff xs = aux $ peaks xs
      where
         aux [] = []
         aux [x0] = []
         aux (x0:x1:xs) = (x1-x0) : aux (x1:xs)

