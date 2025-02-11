module Bimap where

import Favs

import qualified Data.Map as M
import qualified Data.List as L

data Bimap a b = Bimap (M.Map a b) (M.Map b a)

fromList xs = check $ Bimap (M.fromList xs) (M.fromList $ L.map tflip xs)

toList (Bimap a b) = a

fst (Bimap a b) = a
snd (Bimap a b) = b

aofb (Bimap a b) b1 = M.lookup b1 b
bofa (Bimap a b) a1 = M.lookup a1 a

insert a1 b1 (Bimap a b) = Bimap (M.insert a1 b1 a) (M.insert b1 a1 b)
check1 (Bimap a b) = M.toList a == L.sort (map tflip $ M.toList b)
check m = if check1 m then m else error "Bimap is not a bijection"
