{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NewTuple where

import Show1

import Data.Typeable
import Data.List

data a :- b = a :- b deriving (Eq, Show, Typeable)

infixr 1 :-

class ProperTuple a where

instance ProperTuple ()

instance ProperTuple b => ProperTuple (a :- b)

class ProperTuple a => ElemsProperTuple a

instance ElemsProperTuple ()

instance (ProperTuple a, ElemsProperTuple as) => ElemsProperTuple (a :- as)

instance (Ord a, Ord b) => Ord (a :- b) where
   compare (a1 :- b1) (a2 :- b2) = compare (a1, b1) (a2, b2)

instance (Show1 a, Show1 b) => Show1 (a :- b) where
   show1 (a :- b) = show1 a ++ " :- " ++ show1 b

class ShowNewTuple a where
   showNewT :: a -> [String]

instance ShowNewTuple () where
   showNewT () = []

instance (Show1 a, ProperTuple b, ShowNewTuple b) => ShowNewTuple (a :- b) where
   showNewT (a :- b) = show1 a : showNewT b

class (ElemsProperTuple a) => Concat a b where
   concat2 :: a -> b
--using AllowAmbiguousTypes

instance Concat () () where
   concat2 () = ()

instance (ProperTuple a, Append a bs cs, Concat as bs) => Concat (a :- as) cs where
   concat2 (a :- as) = appendT a (concat2 as)

concatT xs = foldrT (appendT :: Append a b c => a -> b -> c) () xs

class (ProperTuple a, ProperTuple b, ProperTuple c) => Append a b c | a b -> c, a c -> b where
   appendT :: a -> b -> c

instance ProperTuple b => Append () b b where
   appendT () b = b

instance Append as bs cs => Append (a :- as) bs (a :- cs) where
   appendT (a :- as) bs = a :- appendT as bs

class Map f a b where
   mapT :: f -> a -> b

class Apply f a b where
   applyC :: f -> a -> b

instance Map f () () where
   mapT f () = ()

instance (Apply f a b, Map f as bs) => Map f (a :- as) (b :- bs) where
   mapT f (a :- as) = applyC f a :- mapT f as

class TofN a b where
   tofn :: a -> b

class NofT a b where
   noft :: a -> b

class Apply2 f a b c | f a b -> c, f a c -> b where
   applyC2 :: f -> a -> b -> c

instance Apply2 (a -> b -> c) a b c where
   applyC2 f = f

class FoldR f z xs y where
   foldrT :: f -> z -> xs -> y

instance FoldR f z () z where
   foldrT f z () = z

-- requires Undecidable Instances
instance (Apply2 f a b c, FoldR f z xs b) => FoldR f z (a :- xs) c where
   foldrT f z (a :- xs) = applyC2 f a (foldrT f z xs)

data Flatten = Flatten

instance Apply2 Flatten [a] [a] [a] where
   applyC2 Flatten = (++)

instance Apply2 Flatten a [a] [a] where
   applyC2 Flatten = (:)

instance NofT (a, b) (a :- b) where noft (a, b) = a :- b
instance NofT (a, b, c) (a :- b :- c) where noft (a, b, c) = a :- b :- c
instance NofT (a, b, c, d) (a :- b :- c :- d) where noft (a, b, c, d) = a :- b :- c :- d
instance NofT (a, b, c, d, e) (a :- b :- c :- d :- e) where noft (a, b, c, d, e) = a :- b :- c :- d :- e
instance NofT (a, b, c, d, e, f) (a :- b :- c :- d :- e :- f) where noft (a, b, c, d, e, f) = a :- b :- c :- d :- e :- f
instance NofT (a, b, c, d, e, f, g) (a :- b :- c :- d :- e :- f :- g) where noft (a, b, c, d, e, f, g) = a :- b :- c :- d :- e :- f :- g
instance NofT (a, b, c, d, e, f, g, h) (a :- b :- c :- d :- e :- f :- g :- h) where noft (a, b, c, d, e, f, g, h) = a :- b :- c :- d :- e :- f :- g :- h
instance NofT (a, b, c, d, e, f, g, h, i) (a :- b :- c :- d :- e :- f :- g :- h :- i) where noft (a, b, c, d, e, f, g, h, i) = a :- b :- c :- d :- e :- f :- g :- h :- i
instance NofT (a, b, c, d, e, f, g, h, i, j) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j) where noft (a, b, c, d, e, f, g, h, i, j) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j
instance NofT (a, b, c, d, e, f, g, h, i, j, k) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k) where noft (a, b, c, d, e, f, g, h, i, j, k) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l) where noft (a, b, c, d, e, f, g, h, i, j, k, l) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w :- x) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w :- x
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w :- x :- y) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w :- x :- y
instance NofT (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w :- x :- y :- z) where noft (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w :- x :- y :- z

instance TofN (a :- b) (a, b) where tofn (a :- b) = (a, b)
instance TofN (a :- b :- c) (a, b, c) where tofn (a :- b :- c) = (a, b, c)
instance TofN (a :- b :- c :- d) (a, b, c, d) where tofn (a :- b :- c :- d) = (a, b, c, d)
instance TofN (a :- b :- c :- d :- e) (a, b, c, d, e) where tofn (a :- b :- c :- d :- e) = (a, b, c, d, e)
instance TofN (a :- b :- c :- d :- e :- f) (a, b, c, d, e, f) where tofn (a :- b :- c :- d :- e :- f) = (a, b, c, d, e, f)
instance TofN (a :- b :- c :- d :- e :- f :- g) (a, b, c, d, e, f, g) where tofn (a :- b :- c :- d :- e :- f :- g) = (a, b, c, d, e, f, g)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h) (a, b, c, d, e, f, g, h) where tofn (a :- b :- c :- d :- e :- f :- g :- h) = (a, b, c, d, e, f, g, h)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i) (a, b, c, d, e, f, g, h, i) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i) = (a, b, c, d, e, f, g, h, i)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j) (a, b, c, d, e, f, g, h, i, j) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j) = (a, b, c, d, e, f, g, h, i, j)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k) (a, b, c, d, e, f, g, h, i, j, k) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k) = (a, b, c, d, e, f, g, h, i, j, k)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l) (a, b, c, d, e, f, g, h, i, j, k, l) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l) = (a, b, c, d, e, f, g, h, i, j, k, l)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m) (a, b, c, d, e, f, g, h, i, j, k, l, m) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m) = (a, b, c, d, e, f, g, h, i, j, k, l, m)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n) (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w :- x) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w :- x) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w :- x :- y) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w :- x :- y) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)
instance TofN (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w :- x :- y :- z) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) where tofn (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- k :- l :- m :- n :- o :- p :- q :- r :- s :- t :- u :- v :- w :- x :- y :- z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)

makenoft v = "instance NofT ("++ intercalate ", " v++") ("++intercalate " :- " v++") where noft ("++intercalate ", " v++") = "++intercalate " :- " v
maketofn v = "instance TofN ("++ intercalate " :- " v++") ("++intercalate ", " v++") where tofn ("++intercalate " :- " v++") = ("++intercalate ", " v++")"

vars a b = map singleton [a..b]

go = map (maketofn . vars 'a') ['b'..'z']

test a = concat2 (((1::Int) :- (2::Int) :- "hello" :- ()) :- ('a' :- [1::Int,2,3] :- ()) :- ())
