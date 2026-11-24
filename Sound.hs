{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Move filter" #-}
{-# HLINT ignore "Parenthesize unary negation" #-}

module Sound where

import Favs
import Poly
import MSolve
import MPoly
import Matrix hiding (zero)
import MyPretty2
import Prob
import Debug

import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Ratio
 
import Debug.Trace

freq1 (c0:c1:c2:_) = acos ((c2+c0)/(2*c1))

wavex f = map cos [0,f..]

test w = map (\n -> freq $ drop n w) [0..]

freq4 w = secantMethod 0.0001 (freq1 . wavex)

--wave2 f g = zipWith (+) (map cos [0,f..]) (map cos [0,g..])

freq2 = do
      let f = 0.01
      let g = 0.032
      let ff = f*f
      let gg = g*g
      let a = map sin $ drop 1 [0,f..]
      let b = map sin $ drop 1 [0,g..]
      let c = zipWith (+) a b
      let (a0:a1:a2:a3:a4:_) = a
      let (b0:b1:b2:b3:b4:_) = b
      let (c0:c1:c2:c3:c4:_) = c
      let da = map (ff *) a
      let db = map (gg *) b
      let d  = zipWith (+) da db
      let (d0:d1:d2:d3:d4:_) = d
      {-
      let d1 = (c2-c1)-(c1-c0)
      let d2 = (c3-c2)-(c2-c1)
      let d3 = (c4-c3)-(c3-c2)
      -}
      putStrLn ("ff="++showFF 10 ff++" (gga1-ggc1+d1)/a1="++showFF 10 ((gg*a1-gg*c1+d1)/a1))
      putStrLn ("ff="++showFF 10 ff++" (gga2-ggc2+d2)/a2="++showFF 10 ((gg*a2-gg*c2+d2)/a2))
      putStrLn ("gg="++showFF 10 gg++" (d2a1-d1a2)/(c2a1-c1a2)="++showFF 10 ((d2*a1-d1*a2)/(c2*a1-c1*a2)))

im = invMat $ crossWith (^) [1..8] [0..8]
-- frequency of a sine wave (in a list) in radians/sample
freq (x0:x1:x2:_) = acos ((x2+x0)/(2*x1))

-- frequency, amplitude and phase (at first sample) of a sine wave
params (x0:x1:x2:_) = let
      p     = (x2+x0)/(2*x1)
      q     = sqrt (1 - p^2)
      freq  = acos p
      y0    = (x1-p*x0)/q
      amp   = sqrt (x0^2+y0^2)
      phase = atan2 x0 y0
      in (freq, amp, phase)

wave :: (Floating b, Enum b) => (b, b, b) -> [b]
wave (freq, amp, phase) = map ((*amp) . cos) [phase, phase+freq ..]

wave2 a b = zipWith (+) (wave a) (wave b)

params3 xs = let
   (f0, a0, p0) = params xs
   (f1, a1, p1) = params $ drop 3 xs
   (f2, a2, p2) = params $ drop 6 xs
   in (params [f0, f1, f2], params [a0, a1, a2], params [p0, p1, p2])

waven ps = map sum $ transpose $ map wave ps

test1 = solvepolys [
                        MPoly ["p","x","r","w","a","b","c","d"] [([3,1,0,0,0,0,0,0], 8), ([0,0,3,1,0,0,0,0], 8), ([0,0,0,0,1,0,0,0], 1)],
                        MPoly ["p","x","r","w","a","b","c","d"] [([2,1,0,0,0,0,0,0], 4), ([0,0,2,1,0,0,0,0], 4), ([0,0,0,0,0,1,0,0], 1)],
                        MPoly ["p","x","r","w","a","b","c","d"] [([1,1,0,0,0,0,0,0], 2), ([0,0,1,1,0,0,0,0], 2), ([0,0,0,0,0,0,1,0], 1)],
                        MPoly ["p","x","r","w","a","b","c","d"] [([0,1,0,0,0,0,0,0], 1), ([0,0,0,1,0,0,0,0], 1), ([0,0,0,0,0,0,0,1], 1::Ratio Integer)]
      ]
{-
a1+b1 = cos(c-b) + cos(c+ b) = 2 cos c cos b
b1-a1 = cos(c-b) - cos(c+ b) = 2 sin c sin b
b2-x  = cos(c  ) - cos(c+2b) = 
x = cos c

a+d=z0
ab^2+de^2=-z2
ab^4+de^4=z4
ab^2+(z0-a)e^2=-z2
ab^4+(z0-a)e^4=z4
ab^6+(z0-a)e^6=-z6
-}
params2 wave0@(freq0, amp0, phase0) wave1@(freq1, amp1, phase1) = let
   (b4:b3:b2:b1:a:a1:a2:a3:a4:_) = wave2 wave0 wave1
   b = a1+b1
   c = a2+2*a+b2
   d = a3+3*a1+3*b1+b3

   m = c*b - d*a
   n = b*b - c*a
   sq = sqrt (a*a*d*d + 4*a*c*c*c + 4*b*b*b*d - 3*b*b*c*c - 6*a*b*c*d)
   ps = filter (\x -> x >= -1 && x <= 1) 
               [(m - sq) / (4*n), 
                (m + sq) / (4*n)]
   
   rs = map (\p -> m/(2*n) - p) ps
   fs = map acos (ps ++ rs)

   in fs

waveparams2 wave0@(freq0, amp0, phase0) wave1@(freq1, amp1, phase1) = let
   (b4:b3:b2:b1:a:a1:a2:a3:a4:_) = wave2 wave0 wave1
   b = a1+b1
   c = a2+2*a+b2
   d = a3+3*a1+3*b1+b3

   m = c*b - d*a
   n = b*b - c*a
   sq = sqrt (a*a*d*d + 4*a*c*c*c + 4*b*b*b*d - 3*b*b*c*c - 6*a*b*c*d)
   ps = filter (\x -> x >= -1 && x <= 1) 
               [(m - sq) / (4*n), 
                (m + sq) / (4*n)]
   
   rs = map (\p -> m/(2*n) - p) ps
   fs = map acos (ps ++ rs)
   --in map acos (ps++rs)
   p = cos freq0
   q = sin freq0
   r = cos freq1
   s = sin freq1
   x = amp0 * cos (phase0 + 3*freq0)
   w = amp1 * cos (phase1 + 3*freq1)

   az =        x +      w
   bz =  2*p  *x +2*r  *w
   cz =  4*p^2*x +4*r^2*w
   dz =  8*p^3*x +8*r^3*w
   in putGrid $ transpose [
      ["a", show a],
      ["b", show b],
      ["c", show c],
      ["d", show d],
      ["ps", show ps],
      ["fs", show fs],
      ["f0", show freq0],
      ["f1", show freq1],
      ["p", show p],
      ["q", show q],
      ["r", show r],
      ["w", show w],
      ["az", show az],
      ["bz", show bz],
      ["cz", show cz],
      ["dz", show dz]]

waveparams3 wave0@(freq0, amp0, phase0) wave1@(freq1, amp1, phase1) = let
   (b5:b4:b3:b2:b1:a:a1:a2:a3:a4:a5:_) = wave2 wave0 wave1
   b = a1+b1
   c = a2+2*x+b2
   d = a3+3*a1+3*b1+b3
   e = a4+4*a2+6*a+4*b2+b4
   f = a5+5*a3+10*a1+10*b1+5*b3+b5
     
   m = c*b - d*a
   n = b*b - c*a
   sq = sqrt (a*a*d*d + 4*a*c*c*c + 4*b*b*b*d - 3*b*b*c*c - 6*a*b*c*d)
   ps = filter (\x -> x >= -1 && x <= 1) 
               [(m - sq) / (4*n), 
                (m + sq) / (4*n)]
   
   rs = map (\p -> m/(2*n) - p) ps
   fs = map acos (ps ++ rs)
   --in map acos (ps++rs)
   p = cos freq0
   q = sin freq0
   r = cos freq1
   s = sin freq1
   x = amp0 * cos (phase0 + 3*freq0)
   w = amp1 * cos (phase1 + 3*freq1)

   az =  8*p^3*x +8*r^3*w
   bz =  4*p^2*x +4*r^2*w
   cz =  2*p  *x +2*r  *w
   dz =        x +      w
   in putGrid $ transpose [
      ["a", show a],
      ["b", show b],
      ["c", show c],
      ["d", show d],
      ["ps", show ps],
      ["fs", show fs],
      ["f0", show freq0],
      ["f1", show freq1],
      ["p", show p],
      ["q", show q],
      ["r", show r],
      ["w", show w],
      ["az", show az],
      ["bz", show bz],
      ["cz", show cz],
      ["dz", show dz]]

alternateElems [] = []
alternateElems [a] = [a]
alternateElems (a:b:cs) = a:alternateElems cs

alternateNegate [] = []
alternateNegate [a] = [a]
alternateNegate (a:b:cs) = a:negate b:alternateNegate cs

--waveparamsn :: forall a. (Num a, Enum a, Show a, Real a, Floating a) => [(a, a, a)] -> IO ()
waveparamsn :: [(Double, Double, Double)] -> Int -> IO ([(Double, Double, Double)], [(Double, Double, Double)])
waveparamsn waves num = let
   xts     = trace "hello" $ waven waves
   --num     = length waves
   neq     = 4*num
   nvars   = 4*num
   hsamps  = 2*num
   ts1     = map (\d -> take (d*2-1) $ drop (hsamps-d) xts) [1..neq]
   ts2     = map alternateElems ts1
   ts3     = zipWith (zipWith (*)) pascal ts2
   ts4     = map sum ts3
   ts2a    = map alternateNegate ts2
   ts3a    = zipWith (zipWith (*)) pascal ts2a
   ts4a    = map sum ts3a
   vars    = map (("p"++) . show) [0..num-1] ++ map (("q"++) . show) [0..num-1] ++ map (("x"++) . show) [0..num-1] ++ map (("y"++) . show) [0..num-1]
   zeroes  = replicate nvars 0
   mpolyspx = zipWith (\p t -> monos vars $ map (\v -> (2^p, [(v    , p), (v+num*2, 1)])) [0..num-1] ++ [(-t, [])]) [0..num*2-1] ts4 
   mpolysqy = zipWith (\p t -> monos vars $ map (\v -> (2^p, [(v    , p), (v+num*2, 1)])) [0..num-1] ++ [(-t, [])]) [0..num*2-1] ts4a
      --map     (\v   -> monos vars [(1, [(v, 2)]), (1, [(v+num, 2)]), (-1, [])]) [0..num-1]
   --psxs    = case msolve mpolys of
      -- [] -> []
      -- (h:_) -> map head h
   psxs    = msolveNewton mpolyspx (map (cos . (0.4^)) [1..num] ++ take num xts) 100
   qsys    = msolveNewton mpolyspx (map (sin . (0.4^)) [1..num] ++ take num xts) 100
   [ps1, xs1] = groupN num psxs
   [qs1, ys1] = groupN num qsys
   xs      = xs1
   ys      = ys1
   ps      = ps1
   qs      = qs1
   q2s     = map (2*) qs
   freqs   = zipWith atan2 qs ps
   amps    = map sqrt $ zipWith (+) (map (^2) xs) (map (^2) ys)
   phases  = zipWith atan2 xs ys
   tscheck = map (mpoly psxs) mpolyspx
   tscheckq= map (mpoly qsys) mpolysqy
   pscheck = map (\(freq, amp, phase) -> cos freq) waves
   qscheck = map (\(freq, amp, phase) -> sin freq) waves
   xscheck = map (\(freq, amp, phase) -> amp * cos (phase + (fromIntegral hsamps-1)*freq)) waves
   yscheck = map (\(freq, amp, phase) -> amp * sin (phase + (fromIntegral hsamps-1)*freq)) waves
   freqsc  = zipWith atan2 qscheck pscheck
   ampsc   = map sqrt $ zipWith (+) (map (^2) xscheck) (map (^2) yscheck)
   phasesc = zipWith atan2 xscheck yscheck

   in do
         putGrid $ transpose [
               ["ts1"      , show ts1],
               ["ts2"      , show ts2],
               ["ts3"      , show ts3],
               ["ts4"      , show ts4],
               ["ts2a"     , show ts2a],
               ["ts3a"     , show ts3a],
               ["ts4a"     , show ts4a],
               ["mpolyspx" , show mpolyspx],
               ["psxs"     , show psxs],
               ["mpolysqy" , show mpolysqy],
               ["qsys"     , show qsys],

               ["xs"       , show xs],
               ["ys"       , show ys],
               ["ps"       , show ps],
               ["qs"       , show qs],
               ["q2s"      , show q2s],
               ["freqs"    , show freqs],
               ["amps"     , show amps],
               ["phases"   , show phases],
               ["tscheck"  , show tscheck],
               ["tscheckq" , show tscheckq],
               ["pscheck"  , show pscheck],
               ["qscheck"  , show qscheck],
               ["xscheck"  , show xscheck],
               ["yscheck"  , show yscheck],
               ["freqsc"   , show freqsc],
               ["ampsc"    , show ampsc],
               ["phasesc"  , show phasesc]


            ]
         
         return (zip3 freqs amps phases, zip3 freqsc ampsc phasesc)

waveparamsn1 :: [(Double, Double, Double)] -> Int -> ([(Double, Double, Double)], [(Double, Double, Double)])
waveparamsn1 waves num = let
   xts     = trace "hello" $ waven waves
   --num     = length waves
   neq     = 4*num
   nvars   = 4*num
   hsamps  = 2*num
   ts1     = mytrace "ts1=" $ map (\d -> take (d*2-1) $ drop (hsamps-d) xts) [1..neq]
   ts2     = mytrace "ts2=" $ map alternateElems ts1
   ts3     = mytrace "ts3=" $ zipWith (zipWith (*)) pascal ts2
   ts4     = mytrace "ts4=" $ map sum ts3
   ts2a    = mytrace "ts2a=" $ map alternateNegate ts2
   ts3a    = mytrace "ts3a=" $ zipWith (zipWith (*)) pascal ts2a
   ts4a    = mytrace "ts4a=" $ map sum ts3a
   vars    = mytrace "vars=" $ map (("p"++) . show) [0..num-1] ++ map (("q"++) . show) [0..num-1] ++ map (("x"++) . show) [0..num-1] ++ map (("y"++) . show) [0..num-1]
   zeroes  = mytrace "zeroes=" $ replicate nvars 0
   mpolys  = mytrace "mpolys=" $ 
      zipWith (\p t -> monos vars $ map (\v -> (2^p, [(v    , p), (v+num*2, 1)])) [0..num-1] ++ [(-t, [])]) [0..num*2-1] ts4 ++ 
      zipWith (\p t -> monos vars $ map (\v -> (2^p, [(v+num, p), (v+num*3, 1)])) [0..num-1] ++ [(-t, [])]) [0..num*2-1] ts4a ++ 
      map     (\v   -> monos vars [(1, [(v, 2)]), (1, [(v+num, 2)]), (-1, [])]) [0..num-1]
   --psxs    = case msolve mpolys of
      -- [] -> []
      -- (h:_) -> map head h
   psxs    = mytrace "psxs   =" $ msolveNewton mpolys (map (sin . (0.5^)) [1..num] ++ map (\n -> cos (0.5^n)) [1..num] ++ take (num *2) xts) 100
   [ps1, qs1, xs1, ys1] = groupN num psxs
   xs      = mytrace "xs     =" $ xs1
   ys      = mytrace "ys     =" $ ys1
   ps      = mytrace "ps     =" $ ps1
   qs      = mytrace "qs     =" $ qs1
   q2s     = mytrace "q2s    =" $ map (2*) qs
   freqs   = mytrace "freqs  =" $ zipWith atan2 qs ps
   amps    = mytrace "amps   =" $ map sqrt $ zipWith (+) (map (^2) xs) (map (^2) ys)
   phases  = mytrace "phases =" $ zipWith atan2 xs ys
   tscheck = mytrace "tscheck=" $ map (mpoly psxs) mpolys
   pscheck = mytrace "pscheck=" $ map (\(freq, amp, phase) -> cos freq) waves
   qscheck = mytrace "qscheck=" $ map (\(freq, amp, phase) -> sin freq) waves
   xscheck = mytrace "xscheck=" $ map (\(freq, amp, phase) -> amp * cos (phase + (fromIntegral hsamps-1)*freq)) waves
   yscheck = mytrace "yscheck=" $ map (\(freq, amp, phase) -> amp * sin (phase + (fromIntegral hsamps-1)*freq)) waves
   freqsc  = mytrace "freqs  =" $ zipWith atan2 qscheck pscheck
   ampsc   = mytrace "amps   =" $ map sqrt $ zipWith (+) (map (^2) xscheck) (map (^2) yscheck)
   phasesc = mytrace "phases =" $ zipWith atan2 xscheck yscheck

   in (zip3 freqs amps phases, zip3 freqsc ampsc phasesc)

waveparamsn2 waves num = let
   xts     = trace "hello" $ waven waves
   --num     = length waves
   neq     = 2*num
   nvars   = 2*num
   hsamps  = 2*num
   ts1     = mytrace "ts1=" $ map (\d -> take (d*2-1) $ drop (hsamps-d) xts) [1..neq]
   ts2     = mytrace "ts2=" $ map alternateElems ts1
   ts3     = mytrace "ts3=" $ zipWith (zipWith (*)) pascal ts2
   ts4     = mytrace "ts4=" $ map sum ts3
   ts2a    = mytrace "ts2a=" $ map alternateNegate ts2
   ts3a    = mytrace "ts3a=" $ zipWith (zipWith (*)) pascal ts2a
   ts4a    = mytrace "ts4a=" $ map sum ts3a
   vars    = mytrace "vars=" $ map (("p"++) . show) [0..num-1] ++ map (("x"++) . show) [0..num-1]
   zeroes  = mytrace "zeroes=" $ replicate nvars 0
   mpolys  = mytrace "mpolys=" $ zipWith (\p t -> MPoly vars $ (++ [(zeroes, -t)]) $ 
      map (\v -> (replaceIndices [(v, p), (v+num, 1)] zeroes, 2^p)) [0..num-1]) [0..neq-1] ts4
   --psxs    = case msolve mpolys of
      -- [] -> []
      -- (h:_) -> map head h
   psxs    = mytrace "psxs   =" $ msolveNewton mpolys (map (0.9^) [1..num] ++ take num xts) 100
   (ps1,xs1) = splitAt num psxs
   xs      = mytrace "xs     =" $ xs1
   ps      = mytrace "ps     =" $ ps1
   qs      = mytrace "qs     =" $ map (\p -> sqrt (1 - p^2)) ps
   q2s     = mytrace "q2s    =" $ map (2*) qs
   mat     = mytrace "mat    =" $ crossWith (^) q2s [1..length q2s]
   inv     = mytrace "inv    =" $ invMat mat
   [ys]    = mytrace "ys     =" $ inv Matrix.<*> [tail ts4a]
   freqs   = mytrace "freqs  =" $ zipWith atan2 qs ps
   amps    = mytrace "amps   =" $ map sqrt $ zipWith (+) (map (^2) xs) (map (^2) ys)
   phases  = mytrace "phases =" $ zipWith atan2 xs ys
   tscheck = mytrace "tscheck=" $ map (mpoly psxs) mpolys
   pscheck = mytrace "pscheck=" $ map (\(freq, amp, phase) -> cos freq) waves
   qscheck = mytrace "qscheck=" $ map (\(freq, amp, phase) -> sin freq) waves
   xscheck = mytrace "xscheck=" $ map (\(freq, amp, phase) -> amp * cos (phase + (fromIntegral hsamps-1)*freq)) waves
   yscheck = mytrace "yscheck=" $ map (\(freq, amp, phase) -> amp * sin (phase + (fromIntegral hsamps-1)*freq)) waves
   freqsc  = mytrace "freqs  =" $ zipWith atan2 qscheck pscheck
   ampsc   = mytrace "amps   =" $ map sqrt $ zipWith (+) (map (^2) xscheck) (map (^2) yscheck)
   phasesc = mytrace "phases =" $ zipWith atan2 xscheck yscheck

   in (zip3 freqs amps phases, zip3 freqsc ampsc phasesc)

params2e xs = let
   -- xs = a cos(bx+c) + d cos(ex+f)
   vs = zipWith (-) (tail xs) xs -- vs = - ab sin(bx+c) - de sin(ex+f)
   as = zipWith (-) (tail vs) vs -- as = - ab^2 cos(bx+c) - de^2*cos(ex+f)
   ks = zipWith (-) (tail as) as
   js = zipWith (-) (tail ks) ks -- js = ab^4 cos(bx+c) + de^4 cos(ex+f)

   in 0

params2b xs = let
   (t0:t1:t2:t3:t4:t5:t6:_) = map realToFrac xs
   a = (t6+3*t4+3*t2+t0)
   b = (t5+2*t3+t1)
   c = (t4+t2)
   d = t3
   -- s = sqrt(4 * a*c^3 - 32 * b^3*d - 12 * b^2*c^2 + 12 * a*b*c*d + a^2*d^2)
   -- p1 = (       s - 2 * b*c - a*d)/(4 * c^2 + 8 * b*d)
   -- p2 = (negate s - 2 * b*c - a*d)/(4 * c^2 + 8 * b*d)
   e = 2*b*c + 4*a*d
   f = c*c - b*d
   s = sqrt (e^2 - 16 * (b^2 + 2*a*c) * f)
   ps = filter (\x -> x >= -1 && x <= 1) 
               [(e - s) / (8*f), 
                (e + s) / (8*f)]
   --r = acos ((negate s - 2*b*c - 4*a*d) / (8*b*d-8*c^2))
   rs = map (\p -> e/(2*f) - p) ps
   in map acos (ps++rs)

params2c xs = let
   (t0:t1:t2:t3:t4:t5:t6:t7:t8:_) = map realToFrac xs
   in solvepolys [
                        MPoly ["p","x","r","w"] [([4,1,0,0], 16), ([0,0,4,1], 16), ([0,0,0,0], -(t8+4*t6+6*t4+4*t2+t0))],
                        MPoly ["p","x","r","w"] [([3,1,0,0],  8), ([0,0,3,1],  8), ([0,0,0,0], -(t7+3*t5+3*t3+t1))],
                        MPoly ["p","x","r","w"] [([2,1,0,0],  4), ([0,0,2,1],  4), ([0,0,0,0], -(t6+2*t4+t2))],
                        MPoly ["p","x","r","w"] [([1,1,0,0],  2), ([0,0,1,1],  2), ([0,0,0,0], -(t5+t3))],
                        MPoly ["p","x","r","w"] [([0,1,0,0],  1), ([0,0,0,1],  1), ([0,0,0,0], -t4::Ratio Integer)]
      ]

{-
-4p^3x -4r^3w  = 3(t4+t2)+(t6+t0) 
4p^2x +4r^2w  = t5+t1+2*t3       
2p  x +2r  w  = t4+t2            
      x +    w  = t3               
-}
params2a (t0:t1:t2:t3:t4:t5:t6:t7:t8:_) = let
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


params2f (x0:x1:x2:x3:x4:x5:_) = [params [x0,x1,x2], params [x1,x2,x3], params [x2,x3,x4], params [x3,x4,x5], params [x0,x2,x4], params [x1,x3,x5]]

stagger f xs = map f $ tails xs

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

{-
cos (bx + c) = 1 - (bx+c)^2 + (bx+c)^4 - (bx+c)^6
= 1 - c^2 + c^4 - 2bcx + 4bxc^3 - b^2x^2 + 6(bx)^2c^2 + 4(bx)^3c + b^4x^4
= 1 - c^2 + c^4 + (4c^3 - 2c)(bx) + (6c^2 - 1)(bx)^2 + (4c)(bx)^3 + (bx)^4
= 1 - c^2 + c^4 - c^6 + c^8 - c^10
+ (-2c + 4c^3 - 6c^5 + 8c^7 - 10c^9)bx 
+ (-1 + 6c^2 - 15c^4 + 28c^6)(bx)^2
+ (4c - 20c^3 + 56c^5 - 120c^7 + 225c^9)(bx)^3
+ (1 - 15c^2 + 70c^4 - 210c^6 + 495c^8)(bx)^4
+ (-6c + 56c^3 - 252c^5 + 792c^7)(bx)^5
+ (-1 + 28c^2 - 210c^4 + 924c^6)(bx)^6
= sum (k=0..)(n=0,2,4..) product (n k) * (-1)^n * (bx)^k * c^(n-k)

          1
         1  1
        1  2  1 
       1  3  3   1
      1  4  6   4   1
     1  5 10  10   5   1
    1  6 15  20  15   6   1
   1  7 21  35  35  21   7   1
  1  8 28  56  70  56  28   8   1
 1  9 36  84 126 126  84  36   9   1
1 10 45 120 210 252 210 120  45  10  1
               462 462 330 165  55 11  1
                  924 792 495 225 66 12  1
(bx+c)^2 = c^2 + 2bc  x +   (bx)^2
(bx+c)^4 = c^4 + 4bc^3x +  6(bx)^2c^2 +  4(bx)^3c   +   (bx)^4
(bx+c)^6 = c^6 + 6bc^5x + 15(bx)^2c^4 + 20(bxc)^3   + 15(bx)^4c^2 + 6(bx)^5c + (bx)^6 
(bx+c)^8 = c^8 + 8bc^7x + 28(bx)^2c^6 + 56(bx)^3c^5 + 70(bxc)^4 + 56(bx)^5c^3 + 28(bx)^6c^2 + 8(bx)^7c + (bx)^8


d cos (ex + f) = d - d(ex+f)^2 + d(ex+f)^4 - d(ex+f)^6

m+n+o+p+q+r+s+t = z0
m+2n+4o+8p+16q+32r+64s+128t= 

   1   1   1   1    1     1      1       1
   1   2   4   8   16    32     64     128
   1   3   9  27   81   243    729    2187
   1   4  16  64  256  1024   4096   16384
   1   5  25 125  625  3125  15625   78125
   1   6  36 216 1296  7776  46656  279936
   1   7  49 343 2401 16807 117649  823543
   1   8  64 512 4096 32768 262144 2097152


dy^2/d^2x = -b^2x
dy^4/d^4y = b^4x

sum (n=0,2,4..)(k=0..n) product (n-k+1..n)(2..k) * (-1)^n * (bx)^k * c^(n-k)


Although this is probably easy for a Maths professor I worked this all out myself

how to work out the frequency, amplitude and phase of a sine wave using just 3 equally spaced (in time) samples as long as the middle one is not zero

treat the wave as if it's one element of a 2-vector which is being rotated

[ p  q][ p  q] = [p^2-q^2 2pq    ]
[-q  p][-q  p]   [-2pq    p^2-q^2]

[ p  q][p^2-q^2 2pq    ] = [p^3-pq^2-2pq^2  2p^2q+p^2q-q^3] = [p^3-3pq^2 3p^2q-q^3]
[-q  p][-2pq    p^2-q^2]   [-p^2q+q^3-2p^2q p^3-pq^2-2pq^2]   [q^3-3p^2q p^3-3pq^2]

[ p  q][p^3-3pq^2 3p^2q-q^3] = [p^4-6p^2q^2+q^4 4p^3q-4pq^3    ]
[-q  p][q^3-3p^2q p^3-3pq^2]   [4pq^3-4p^3q     p^4-6p^2q^2+q^4]

[ p  q][p^4-6p^2q^2+q^4 4p^3q-4pq^3    ] = [p^5-6p^3q^2+pq^4+4pq^4
[-q  p][4pq^3-4p^3q     p^4-6p^2q^2+q^4]

tn=wn+xn+yn+zn
t0=w0+x0+y0+z0

[p^2-q^2 2pq    ][x0] = [x2] 
[-2pq    p^2-q^2][y0]   [y2]

(p^2-q^2)x2-2pqy2 = x0

(p^2-q^2)x2+2pqy2 = x4

p^2+q^2 = 1
r^2+s^2 = 1

t5 = px4+qy4 + rw4+sz4
t3 = px4-qy4 + rw4-sz4

t5+t3 = 2px4 + 2rw4
t5-t3 = 2qy3 + 2sz3

t6 = (p^2-q^2)x4+2pqy4 + (r^2-s^2)w4+2rsz4
t2 = (p^2-q^2)x4-2pqy4 + (r^2-s^2)w4-2rsz4

t6+t2 = 2(p^2-q^2)x4 + 2(r^2-s^2)w4
t6-t2 = 4pqy4  + 4rsz4

t7 = (p^3-3pq^2)x4 + (3p^2q-q^3)y4 + (r^3-3rs^2)w4 + (3r^2s-s^3)z4
t1 = (p^3-3pq^2)x4 - (3p^2q-q^3)y4 + (r^3-3rs^2)w4 - (3r^2s-s^3)z4

t7+t1 = 2(p^3-3pq^2)x4 + 2(r^3-3rs^2)w4
t7-t1 = 2(3p^2q-q^3)y4 + 2(3r^2s-s^3)z4
t7-t1 = 2(3(1-q^2)q-q^3)y4 + 2(3(1-s^2)s-s^3)z4
t7-t1 = 2(3q-3q^3-q^3)y4   + 2(3s-3s^3  -s^3)z4
t7-t1 = 2(3q-4q^3)y4       + 2(3s-4s^3)z4
t7-t1 =  (6q-8q^3)y4       +  (6s-8s^3)z4
t1-t7 =  (8q^3-6q)y4       +  (8s^3-6s)z4
t1-3t3+3t5-t7 = 8q^3y4 + 8s^3z4

t7+t1 = 2(p^3-3pq^2)x4 + 2(r^3-3rs^2)w4 
t6+t2 = 2(p^2-q^2)  x4 + 2(r^2-s^2)  w4
t5+t3 = 2p          x4 + 2r          w4

t7+t1 = 2(p^3-3p(1-p^2))x4 + 2(r^3-3r(1-r^2)w4
t7+t1 = 2(p^3-3p+3p^3)x4 + 2(r^3-3r+3r^3)w4
t7+t1 = 2(4p^3 - 3p)x4 + 2(4r^3 - 3r)w4
t7+t1 = (8p^3 - 6p)x4 + (8r^3 - 6r)w4
t7+t1+3(t5+t3) = 8p^3x4 +8r^3w4

t8 = (p^4-6p^2q^2+q^4)x4 + (4p^3q-4pq^3)y4
t0 = (p^4-6p^2q^2+q^4)x4 - (4p^3q-4pq^3)y4
t8+t0     = (2p^4-12p^2q^2+2q^4)x4
4*(t6+t2) = (8p^2-8q^2)x4
6*t4      = 6x4
f         = 2p^4-12p^2(1-p^2)+2(1-p^2)^2 + 8p^2-8(1-p^2) + 6
               = 2p^4-12p^2+12p^4+2+2p^4-4p^2 + 8p^2-8+8p^2 + 6
               = 2p^4+12p^4+2p^4-12p^2-4p^2+8p^2+8p^2 + 6+2-8
               = 16p^4x

e = t0+4*t2+6*t4+4*t6+t8
d = t1+3*t3+3*t5+t7
c = t2+2*t4+t6
b = t3+t5
a = t4

for one wave
t0    = x
a1+b1 = 2px

freq = acos (a1+b1)/(2*t0)

for two waves
       t0        =     x +    w
      a1+b1       = 2p  x +2r  w
    a2+2t0+b2     = 4p^2x +4r^2w
  a3+3a1+3b1+b3   = 8p^3x +8r^3w
      b1-a1       = 2q  y +2s  z
    b2-2t0+a2     = 4q^2y +4s^2z
  b3-3b1+3a1-b3   = 8q^3y +8s^3z

for three waves
          t0            =      x+     w+     v
        b1+a1           =  2p  x+ 2r  w+ 2u  v
      b2+2t0+a2         =  4p^2x+ 4r^2w+ 4u^2v
    b3+3b1+3a1+a3       =  8p^3x+ 8r^3w+ 8u^3v
  b4+4b2+6t0+4a2+a4     = 16p^4x+16r^4w+16u^4v
b5+5b3+10b1+10a1+5a3+a5 = 32p^5x+32r^5w+32u^5v
        b1-a1           =  2q  y+ 2s  z+ 2u  v
      b2-2t0+a2         =  4q^2y+ 4s^2z+ 4u^2v
    b3-3b1+3a1-a3       =  8q^3y+ 8s^3z+ 8u^3v
  b4-4b2+6t0-4a2+a4     = 16q^4y+16s^4z+16u^4v
b5-5b3+10b1-10a1+5a3-a5 = 32q^5y+32s^5z+32u^5v

-}