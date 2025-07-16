module Chess where

import Favs

{-
class Square s where
   x :: s -> Int
   y :: s -> Int
   n :: s -> s
   s :: s -> s
   w :: s -> s
   e :: s -> s
-}

data S = S { z::Int, p :: S, q :: S } | O

line con off = li
   where
      li    = map sf [0..7]
      sf  z = con z (sf1 (z-1)) (sf1 (z+1))
      sf1 z | z >= 0 && z <= 7 = li !! z
            | otherwise = off

{-
grid con off = gr
   where
      gr     = crossWith gf [0..7] [0..7]
      gf y x = 
-}
data Sq = 
   Sq  { x::Int, y::Int, n :: Sq,  s :: Sq , w :: Sq , e :: Sq , nw :: Sq , ne :: Sq , sw :: Sq , se :: Sq  , 
                         n1::[Sq], s1::[Sq], w1::[Sq], e1::[Sq], nw1::[Sq], ne1::[Sq], sw1::[Sq], se1::[Sq] } |
   Sq1 { x::Int, y::Int, n :: Sq,  s :: Sq , w :: Sq , e :: Sq , nw :: Sq , ne :: Sq , sw :: Sq , se :: Sq  } |
   Off

instance Eq Sq where
   a@Sq{} == b@Sq{} = x a == x b && y a == y b
   Sq{} == Off = False
   Off == Sq{} = False

startBoard = crossWith square [0..7] [0..7]

square y x = Sq1 x y (square1 (y-1) x) (square1 (y+1) x) (square1 y (x-1)) (square1 y (x+1)) (square1 (y-1) (x-1)) (square1 (y-1) (x+1)) (square1 (y+1) (x-1)) (square1 (y+1) (x+1))

square1 y x | y >= 0 && y <= 7 && x >= 0 && x <= 7 = startBoard !! y !! x
            | otherwise                            = Off

step 0 = n
step 1 = s
step 2 = w
step 3 = e
step 4 = nw
step 5 = ne
step 6 = sw
step 7 = se

startBoard1 = map2 copy startBoard

copy sq = Sq (x sq) (y sq) (n sq) (s sq) (w sq) (e sq) (nw sq) (ne sq) (sw sq) (se sq)

rook sq = Sq (x sq) (y sq) (n sq) (s sq) (w sq) (e sq) (nw sq) (ne sq) (sw sq) (se sq) (twit n sq) (twit s sq) (twit w sq) (twit e sq) (twit nw sq) (twit ne sq) (twit sw sq) (twit se sq)

twit f sq = takeWhile (/= Off) $ iterate f sq

