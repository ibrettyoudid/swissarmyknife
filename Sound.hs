{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Move filter" #-}
module Sound where

import Favs
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Ratio
import Numeric

freq1 (c0:c1:c2:_) = let
   d1 = c2+c0-2*c1 -- (c2-c1)-(c1-c0)
   in sqrt (-d1/c1)

wave f = map sin [0,f..]

test w = map (\n -> freq $ drop n w) [0..]

freq4 w = secantMethod (freq1 . wave) (freq1 w) (freq1 w / 2) (freq1 w * 2) 10

{-  
a0+b0=c0
fa0+gb0=d0
b0=c0-a0
f^2a0+g^2(c0-a0)=d0
f^2a0+g^2c0-g^2a0=d0
f^2a0=g^2a0-g^2c0+d0
f^2=(g^2a0-g^2c0+d0)/a0
f^2=(g^2a1-g^2c1+d1)/a1
(g^2a0-g^2c0+d0)/a0 = (g^2a1-gc1+d1)/a1
(g^2a0-g^2c0+d0)*a1 = (g^2a1-gc1+d1)*a0
g^2a0a1-g^2c0a1+d0a1 = g^2a1a0-g^2c1a0+d1a0
d0a1-g^2c0a1 = d1a0-g^2c1a0
g^2c1a0-g^2c0a1 = d1a0-d0a1
g^2 = (d1a0-d0a1)/(c1a0-c0a1)

(cjai-ciaj)/(djai-diaj) = (cnam-cman)/(dnam-dman)
(cjai-ciaj)(dnam-dman) = (cnam-cman)(djai-diaj)
cjaidnam-cjaidman-ciajdnam+ciajdman = cnamdjai-cnamdiaj-cmandjai+cmandiaj
cjaidnam-cjaidman-ciajdnam+ciajdman-cnamdjai+cnamdiaj+cmandjai-cmandiaj = 0
(cjdn-cndj)aiam+(-cjdm+cmdj)aian+(-cidn+cndi)ajam+(cidm-cmdi)ajan = 0
(cjdn-cndj)aiam+(cmdj-cjdm)aian+(cndi-cidn)ajam+(cidm-cmdi)ajan = 0
if n=i
(cjdi-cidj)aiam+(cmdj-cjdm)aiai+(cidi-cidi)ajam+(cidm-cmdi)ajai = 0
*cidi-cidi cancels, divide by ai, change m to k*
(cjdi-cidj)ak+(ckdj-cjdk)ai+(cidk-ckdi)aj = 0
*if j=k
(cjdi-cidj)aj+(cjdj-cjdj)ai+(cidj-cjdi)aj = 0
(cjdi-cidj)aj+(cidj-cjdi)aj = 0
0=0
-}

wave2 f g = zipWith (+) (map sin [0,f..]) (map sin [0,g..])

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

{-
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

t5 = (p^2-q^2)x3+2pqy3 + (r^2-s^2)w3+2rsz3
t4 = px3+qy3 + rw3+sz3
t2 = px3-qy3 + rw3-sz3
t4+t2 = 2px3 + 2rw3
t4-t2 = 2qy3 + 2sz3

t1 = (p^2-q^2)x3-2pqy3 + (r^2-s^2)w3-2rsz3

t5+t1 = 2(p^2-q^2)x3 + 2(r^2-s^2)w3
t5-t1 = 4pqy3  + 4rsz3

t6 = (p^3-3pq^2)x3 + (3p^2q-q^3)y3 + (r^3-3rs^2)w3 + (3r^2s-s^3)z3
t0 = (p^3-3pq^2)x3 - (3p^2q-q^3)y3 + (r^3-3rs^2)w3 - (3r^2s-s^3)z3

t6+t0 = 2(p^3-3pq^2)x3 + 2(r^3-3rs^2)w3
t6-t0 = 2(3p^2q-q^3)y3 + 2(3r^2s-s^3)z3

t6+t0 = 2(p^3-3pq^2)x3 + 2(r^3-3rs^2)w3 
t5+t1 = 2(p^2-q^2)  x3 + 2(r^2-s^2)  w3
t4+t2 = 2p          x3 + 2r          w3

t6+t0 = 2(p^3-3p(1-p^2))x3 + 2(r^3-3r(1-r^2)w3 
t6+t0 = 2(-3p-2p^3))x3 + 2(-3r-2r^3)w3 
t6+t0 = (-4p^3 - 6p)x3 + (-4r^3 - 6r)w3 

q^2 = 1-p^2

p^2-q^2+p^2+q^2 = p^2-q^2+1 = 2p^2
p^2-q^2 = 2p^2 - 1
t5+t1 = 2(2p^2-1)  x3 + 2(2r^2-1)  w3

t6+t0 = (-4p^3-6p)x3 + (-4r^3 - 6r)w3 
t5+t1 = (4p^2-2)x3 + (4r^2-2)w3
t4+t2 = 2p          x3 + 2r          w3
t3    = x3 + w3
-4p^3x3 -4r^3w3 = 3(t4+t2)+(t6+t0) 
 4p^2x3 +4r^2w3 = t5+t1+2*t3       
 2p  x3 +2r  w3 = t4+t2            
     x3 +    w3 = t3               
                                        
-4p^3x -4r^3w  = 3(t4+t2)+(t6+t0) 
 4p^2x +4r^2w  = t5+t1+2*t3       
 2p  x +2r  w  = t4+t2            
     x +    w  = t3               

a = 3*(t4+t2)+(t6+t0)
b = t5+t1+2*t3
c = t4+t2
d = t3

sq = 4*a^2*d^2+12*a*b*c*d-8*a*c^3+4*b^3*d-3*b^2*c^2
   = 64*(p^6x^2+r^6w^2+2p^3r^3xw)*(x^2+w^2+2xw)+12*(-16p^5x^2-16r^5w^2

A =  16p^4x+16r^4w
a =  -4p^3x -4r^3w
b =   4p^2x +4r^2w
c =   2p  x +2r  w
d =       x +    w
w = d - x


b+c+d = (4p^2+2p+1)x+(4r^2+2r+1)(d-x)
      = 4(p+0.5+0.25)x
      = ((2p+1/2)^2)x - ((2r+1/2)^2)x + ((2r+1/2)^2+3/4)d
      = ((2p+1/2)^2)x - ((2r+1/2)^2)x = b+c+d - ((2r+1/2)^2+3/4)d
b+c+d+((2r = 

      = ((2p+1/2)^2) + ((2r+1/2)^2)(d/x)+3d/4x
A =  16p^4x+16r^4d-16r^4x
a =  -4p^3x -4r^3d+4r^3x
b =   4p^2x +4r^2d-4r^2x
c =   2p  x +2r  d-2rx

a = 4(r^3-p^3)x - 4r^3d
b = 4(p^2-r^2)x + 4r^2d
c = 2(p-r)x     + 2rd

a + 4r^3d = 4(r^3-p^3)x
b - 4r^2d = 4(p^2-r^2)x
c - 2rd   = 2(p-r)x    

(a/4 + r^3d)/(r^3-p^3) = x
(b/4 - r^2d)/(p^2-r^2) = x
(c/2 - rd  )/(p-r)     = x    

(p^2-r^2)(a/4 + r^3d) = (r^3-p^3)(b/4 - r^2d)
(p-r)    (a/4 + r^3d) = (r^3-p^3)(c/2 - rd)
(p^2-r^2)(a/4 + r^3d)/(b/4 - r^2d) = (r^3-p^3)
(p-r)    (a/4 + r^3d)/(c/2 - rd)   = (r^3-p^3)
ap^2/4 - ar^2/4 + p^2r^3d - r^5d = br^3/4 - bp^3/4 - r^5d + p^3r^2d
ap^2/4 - ar^2/4 + p^2r^3d = br^3/4 - bp^3/4 + p^3r^2d
ap^2/4 - ar^2/4 + p^2r^3d - br^3/4 + bp^3/4 - p^3r^2d
p^2r^3d - p^3r^2d + ap^2/4 + bp^3/4 - ar^2/4 - br^3/4
p^2r^3d - p^3r^2d + ap^2/4 - ar^2/4 + bp^3/4 - br^3/4
p^2r^2(r-p)d = (a+br)r^2/4 - (a+bp)p^2/4

(p-r)(b/4 - r^2d) = (p^2-r^2)(c/2 - rd)
p(b/4 - r^2d)-r(b/4 - r^2d) = p^2(c/2 - rd)-r^2(c/2 - rd)
r^2(c/2 - rd)+r(b/4 - r^2d) = -p^2(c/2 - rd)+p(b/4 - r^2d)
r^2(c/2 - rd)+r(b/4 - r^2d) = p^2(rd - c/2)+p(b/4 - r^2d)
r^2(c/2 - rd)+rb/4 - r^3d = p^2(rd - c/2)+p(b/4 - r^2d) = p(prd - pc/2 + b/4 - r^2d)
r(-2r^2d + rc/2 + b/4) = (rd - c/2)*(p+(b/4 - r^2d))^2

r^2(c/2 - rd)+r(b/4 - r^2d) = p((b/4 - r^2d)-p(c/2 - rd))
cr^2/2 - r^3d+br/4 - r^3d = bp/4 - cp^2/2 + drp(p-r)
- 2dr^3 + cr^2/2 - br/4 = drp(p-r) - cp^2/2 + bp/4 
- 2dr^3 + cr^2/2 - br/4 = drp^2 - dr^2p - cp^2/2 + bp/4 
rd(r^2 - cr/4d + b/8d) = dr^2p/2 - drp^2/2 + cp^2/4 - bp/8
rd(r - c/8d)^2 - c^2r^2/64d + br/4 = (2c-dr)p^2/4 + (4dr^2-b)p/8
rd(r - c/8d)^2 - c^2r^2/64d + br/4 = (2c-dr)/4(p^2 + (4dr^2-b)p/(4c-2dr))
rd(r - c/8d)^2 - c^2r^2/64d + br/4 = (2c-dr)/4(p + (4dr^2-b)/(8c-4dr))^2 - (4dr^2-b)^2/(32c-16dr)
rd(r - c/8d)^2 - c^2r^2/64d + br/4 + (dr-2c)/4(p + (4dr^2-b)/(8c-4dr))^2 = - (4dr^2-b)^2/(32c-16dr)
rd(r - c/8d)^2 + (dr-2c)/4(p + (4dr^2-b)/(8c-4dr))^2 = c^2r^2/64d + br/4 - (4dr^2-b)^2/(32c-16dr)
sqrt(rd)(r - c/8d) + sqrt(dr-2c)/2*(p + (4dr^2-b)/(8c-4dr)) = sqrt(c^2r^2/64d + br/4 - (4dr^2-b)^2/(32c-16dr))

dr^3 - cr^2/4 + br/8              = (2c+dr)/4(p + (4dr^2-b)/(8c+4dr))^2 - (4dr^2-b)^2/(32c+16dr)
2*sqrt(dr^3 - cr^2/4 + br/8 - (4dr^2-b)^2/(32c+16dr))/sqrt(2c+dr) = (p + (4dr^2-b)/(8c+4dr))
2*sqrt(dr^3 - cr^2/4 + br/8 - (4dr^2-b)^2/(32c+16dr))/sqrt(2c+dr) - (4dr^2-b)/(8c+4dr) = p 

(b/4 - r^2d)/(c/2 - rd) = (p^2-r^2)/(p-r)





2px = c - 2rw
p  = (c - 2rw)/2x
x  = (c - 2rw)/2p
p^2 = (4r^2w^2 + c^2 +2rwc)/4x^2
p^2 = (b-4r^2w)/4x
(4r^2w^2 + c^2 +2rwc) = (b-4r^2w)x
4p^2x = b - 4r^2w
x  =-(a/4 - r^3w)/p^3
x  = (b/4 - r^2w)/p^2
x  = (c/2 - rw)/p
(b/4 - r^2w)/p^2 = (c/2 - rw)/p
(b/4 - r^2w)/(c/2 - rw) = p
-(a/4 - r^3w)/(c/2 - rw) = p^2
(b/4 - r^2w)^2 = -(a/4 - r^3w)(c/2 - rw)
b/16 + r^4w^2 - br^2w/2 = -ac/8 - r^4w^2 + cr^3w/2 - arw/4
2r^4w^2 - cr^3w/2 - br^2w/2 - arw/4 + b/16  + ac/8 = 0
r^4w^2  - cr^3w/4 - br^2w/8 - arw/8 + ac/16 + b/32 = 0


x  = (b - 4r^2(d-x))/4p^2
b/4p^2-x= 4r^2(d-x)/4p^2
b-4p^2x/(d-x) = 4r^2
b/4r^2x-(d-x)/x = p^2/r^2
b/4r^2w =   p^2x/r^2w +1
b/4r^2w =   p^2(d-w)/r^2w +1
b/4r^2w =   p^2(d/w-1)/r^2 +1

x/(a-x) = 1/(a/x)-1
1/(1-x)= -1/(x-1)


 x + 4r^2(d-x)
 p  = (2rw - c)/2(b-4r^2w)
 b  = 4p^2x = 4*4r^2(x^2+d^2+2xd)

c/d = 2px+2r(d-x)/(x+w)
c/d = 2px+2r(d-x))(x-w)/(x^2-w^2)
c-d = (2p-1)x+(2r-1)w

(c-d)*2p-(b-c) = (4p^2-2p)x+2p(4r^2-2r)w 
b-c = (4p^2-2p)x+(4r^2-2r)w



-4*p*a   = 16p^4x+16p  r^3w
4*p^2*b  = 16p^4x+16p^2r^2w
8*p^3*c  = 16p^4x+16p^3rw
16*p^4*d = 16p^4x+16p^4w
b^2      = 16p^4x^2+16r^4w^2+16p^2r^2xw
d^2      = x^2+w^2+xw

b*p     =  4p^3x +4pr^2w
c*p^2*2 =  4p^3x +4p^2rw
d*p^3*4 =  4p^3x +4p^3 w

bp+a     = 4pr^2w-4r^3w  = e
bp-2cp^2 = -4p^2rw+4pr^2w = f
bp-4dp^3 = -4p^3w+4pr^2w  = g

e-f = -4r^3w+4p^2rw
e-g = 4(p^3-r^3)w
f-g = -4p^2rw+4p^3w

f*p = -4p^3rw+4p^2r^2w
e*p^2 = 4p^3r^2w-4p^2r^3w

e*p^2-f*p = 4p^2wr(r+1)(pw-rw)

a+c*p^2 = 



 2p^2x +2pr w  = (t4+t2) * p      
 2p  x +2r^2w  = (t4+t2) * r      
 2p^3x +2p^2rw = (t4+t2) * p^2     
 2pr^2x+2r^3w  = (t4+t2) * r^2


 8pxrw+4p^2x^2+4r^2w^2 = (t4+t2)^2
 -4p^6x^2
    px +   pw  = t * p 
    rx +   rw  = t * r
    pw +   rx  = t * (p + r) - (t4+t2)/2

     x3 +    w3 = t3               


w3 = t3-x3

3(t4+t2)+(t6+t0) = -4p^3x3 -4r^3(t3-x3)
t5+t1+2*t3       =  4p^2x3 +4r^2(t3-x3)
t4+t2            =  2p  x3 +2r  (t3-x3)

3(t4+t2)+(t6+t0) = (-4p^3 +4r^3)x3 -4r^3t3
t5+t1+2*t3       = ( 4p^2 -4r^2)x3 +4r^2t3
t4+t2            = ( 2p   -2r  )x3 +2r  t3


r^2 = 2(t4+t2)/w3
p = (t4+t2-rw3)/2x3

(t4+t2)pr = 2p^2rx3 + 2pr^2w3
(t4-t2)qs = 2q^2sy3 + 2qs^2z3

[x5] = [p^3x2-3pq^2x2]
[y5]   [


[ p  q][x0]=[x1]
[-q  p][y0]=[y1]
p=cos f
q=sin f
p^2+q^2=1
q^2=1-p^2
q=sqrt(1-p^2)

x1=px0+qy0
qy0=px0-x1
y0 =(px0-x1)/q
y1=py0-qx0

x2=px1+qy1
x2=p(px0+qy0)+q(py0-qx0)
x2=p^2x0+pqy0+qpy0-q^2x0
x2=(p^2-q^2)x0+2pqy0
x2=(2p^2-1)x0+2pqy0
qy0=x1-px0
x2=(2p^2-1)x0+2p(x1-px0)
x2=(2p^2-1)x0+2px1-2p^2x0
x2=2p^2x0-x0+2px1-2p^2x0
x2=-x0+2px1
x2+x0=2px1
(x2+x0)/2x1=p
-}
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

test1 = solvepolys [
                Poly "pxrwabcd" [([3,1,0,0,0,0,0,0],-4), ([0,0,3,1,0,0,0,0], -4), ([0,0,0,0,1,0,0,0], 1)],
                Poly "pxrwabcd" [([2,1,0,0,0,0,0,0], 4), ([0,0,2,1,0,0,0,0],  4), ([0,0,0,0,0,1,0,0], 1)],
                Poly "pxrwabcd" [([1,1,0,0,0,0,0,0], 2), ([0,0,1,1,0,0,0,0],  2), ([0,0,0,0,0,0,1,0], 1)],
                Poly "pxrwabcd" [([0,1,0,0,0,0,0,0], 1), ([0,0,0,1,0,0,0,0],  1), ([0,0,0,0,0,0,0,1], 1::Ratio Integer)]
    ]

paramsOfWave2 xs = let
  (t0:t1:t2:t3:t4:t5:t6:_) = map realToFrac xs
  in solvepolys [
                Poly "pxrw" [([4,1,0,0], 16), ([0,0,4,1], 16)],
                Poly "pxrw" [([3,1,0,0], -4), ([0,0,3,1], -4), ([0,0,0,0], -(t6+3*t4+3*t2+t0))],
                Poly "pxrw" [([2,1,0,0],  4), ([0,0,2,1],  4), ([0,0,0,0], -(t5+2*t3+t1))],
                Poly "pxrw" [([1,1,0,0],  2), ([0,0,1,1],  2), ([0,0,0,0], -(t4+t2))],
                Poly "pxrw" [([0,1,0,0],  1), ([0,0,0,1],  1), ([0,0,0,0], -t3::Ratio Integer)]
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

type Mono coeff = ([Int], coeff)

data Poly coeff = Poly {  vars :: String, terms :: [Mono coeff] }

instance (Show coeff, Num coeff, Ord coeff, Real coeff) => Show (Poly coeff) where
  show p@(Poly z ms) = if p == zero then "0" else concatMap (showm z "") ms

instance Eq coeff => Eq (Poly coeff) where
  Poly z a == Poly zz b = a == b

instance Ord coeff => Ord (Poly coeff) where
  compare (Poly _ a) (Poly _ b) = compare a b

showm z ch (pi,  0) = "+0" ++ ch
showm z ch (pi,  1) = "+"                     ++ concat (zipWith showz pi z) ++ if any (> 0) pi then ch else ""
showm z ch (pi, -1) = "-"                     ++ concat (zipWith showz pi z) ++ if any (> 0) pi then ch else ""
showm z ch (pi, ci) | ci > 0 = "+" ++ showFF 10 (realToFrac ci) ++ concat (zipWith showz pi z) ++ ch
                    | otherwise =     showFF 10 (realToFrac ci) ++ concat (zipWith showz pi z) ++ ch

showz pi c | pi == 0 = ""
           | pi == 1 = [c]
           | pi >  1 = [c] ++ "^" ++ show pi

lcmm (pi, ci) (pj, cj) = (zipWith max pi pj, 1)

divm (pi, ci) (pj, cj) = (zipWith (-) pi pj, ci/cj)

mulm (pi, ci) (pj, cj) = (zipWith (+) pi pj, ci*cj)

negm (pi, ci) = (pi, negate ci)

mulpm (Poly z xs) m = Poly z $ map (mulm m) xs

mulpp p (Poly z xs) = foldr addpp (Poly z []) $ map (mulpm p) xs

addpp (Poly z hi) (Poly _ hj) = Poly z $ rinse $ M.toList $ M.unionWith (+) (M.fromList hi) (M.fromList hj)

subpp (Poly z hi) (Poly _ hj) = Poly z $ rinse $ M.toList $ M.unionWith (-) (M.fromList hi) (M.fromList hj)

sortp order (Poly z p) = Poly z $ sortBy order p

rinse xs = filter ((/= 0) . snd) xs

nonzero (Poly z []) = False
nonzero _ = True

zero = Poly "" []

leading order (Poly _ p) = minimumBy order p

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

{-
-4p^3x -4r^3w  = 3(t4+t2)+(t6+t0) 
 4p^2x +4r^2w  = t5+t1+2*t3       
 2p  x +2r  w  = t4+t2            
     x +    w  = t3               
-}
paramsOfWave2a (t0:t1:t2:t3:t4:t5:t6:_) = let
  a = t6+3*t4+3*t2+t0
  b = t5+2*t3+t1
  c = t4+t2
  d = t3

  sq = 4*a^2*d^2+12*a*b*c*d-8*a*c^3+4*b^3*d-3*b^2*c^2
  sqrt1 = sqrt sq
  p = (2*a*d+b*c - sqrt1) / (4*(c^2-b*d))
  r = (2*a*d+b*c + sqrt1) / (4*(c^2-b*d))
  in (acos p, r)

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

