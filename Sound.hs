module Sound where

import Favs

freq1 (c0:c1:c2:_) = let
   d1 = c2+c0-2*c1 -- (c2-c1)-(c1-c0)
   in sqrt(-d1/c1)
   
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
   putStrLn ("ff="++showFF 10 (ff)++" (gga1-ggc1+d1)/a1="++showFF 10 ((gg*a1-gg*c1+d1)/a1))
   putStrLn ("ff="++showFF 10 (ff)++" (gga2-ggc2+d2)/a2="++showFF 10 ((gg*a2-gg*c2+d2)/a2))
   putStrLn ("gg="++showFF 10 (gg)++" (d2a1-d1a2)/(c2a1-c1a2)="++showFF 10 ((d2*a1-d1*a2)/(c2*a1-c1*a2)))
   
{-
Although this is probably easy for a Maths professor I worked this all out myself

how to work out the frequency, amplitude and phase of a sine wave using just 3 equally spaced non-zero values

treat the wave as if it's one element of a 2-vector which is being rotated

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
waveParams (x0:x1:x2:_) = let
   p     = (x2+x0)/(2*x1)
   q     = sqrt (1 - p^2)
   freq  = acos p
   y0    = (x1-p*x0)/q
   amp   = sqrt (x0^2+y0^2)
   phase = atan2 x0 y0
   in (freq, amp, phase)
{-
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