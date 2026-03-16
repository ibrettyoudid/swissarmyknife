{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module NewTuple where

import Show1

import Data.Typeable
import Data.List

data a :- b = (:-) { fstT::a, sndT::b } deriving (Eq, Show, Typeable)

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

class NamedTuple n ns vs v | n ns vs -> v where
   lookup :: n -> ns -> vs -> v
   update :: n -> v -> ns -> vs -> vs

instance NamedTuple n ns vs v => NamedTuple n (m :- ns) (u :- vs) v where
   lookup n (m :- ns) (u :- vs) = NewTuple.lookup n ns vs
   update n v (m :- ns) (u :- vs) = u :- NewTuple.update n v ns vs

instance {-# OVERLAPPING #-} NamedTuple n (n :- ns) (v :- vs) v where
   lookup n (n1 :- ns) (v :- vs) = v
   update n v (n1 :- ns) (v1 :- vs) = v :- vs

class Delete n ns ns2 vs vs2 | n ns vs -> ns2 vs2 where
   delete :: n -> ns -> vs -> (ns2, vs2)

instance Delete n (n :- ns) ns (v :- vs) vs where
   delete n (n1 :- ns) (v1 :- vs) = (ns, vs)

instance Delete n ns ns2 vs vs2 => Delete n (n1 :- ns) (n1 :- ns2) (v1 :- vs) (v1 :- vs2) where
   delete n (n1 :- ns) (v1 :- vs) = let (ns2, vs2) = NewTuple.delete n ns vs in (n1 :- ns2, v1 :- vs2)

class DeleteIndex i ns ns2 vs vs2 | i vs -> vs2 where
   deleteIndex :: i -> ns -> vs -> (ns2, vs2)

instance DeleteIndex Z (n :- ns) ns (v :- vs) vs where
   deleteIndex Z (n :- ns) (v :- vs) = (ns, vs)

instance DeleteIndex i ns ns2 vs vs2 => DeleteIndex (S i) (n :- ns) (n :- ns2) (v :- vs) (v :- vs2) where
   deleteIndex (S i) (n :- ns) (v :- vs) = let (ns1, vs1) = NewTuple.deleteIndex i ns vs in (n :- ns1, v :- vs1)

class Difference is ns ns2 vs vs2 | is ns vs -> ns2 vs2 where
   difference :: is -> ns -> vs -> (ns2, vs2)

instance Difference () ns ns vs vs where
   difference () ns vs = (ns, vs)

instance (Delete i ns ns2 vs vs2, Difference is ns2 ns3 vs2 vs3) => Difference (i :- is) ns ns3 vs vs3 where
   difference (i :- is) ns vs = let (ns2, vs2) = NewTuple.delete i ns vs in difference is ns2 vs2
{-}
class LookupList ns1 ns2 vs1 vs2 vs3 | ns1 ns2 vs1 -> vs2 where
   lookupList :: ns1 -> ns2 -> vs1 -> (vs2, vs3)

instance LookupList () a b () b where
   lookupList () a b = ((), b)

instance (NamedTuple n ns2 vs1 v, LookupList ns1 ns2 vs1 vs2 vs3) => LookupList (n :- ns1) ns2 vs1 (v :- vs2) vs4 where
   lookupList (n :- ns1) ns2 vs1 = let (vs2, vs3) = lookupList ns1 ns2 vs1 in (NewTuple.lookup n ns2 vs1 :- vs2, vs3)
-}
class Delete1 n ninc vinc v ndel vdel | n ninc -> ndel, v vinc -> vdel, vinc vdel -> v, n ninc vinc -> v vdel where
   delete1 :: n -> ninc -> vinc -> (v, ndel, vdel)

instance {-# OVERLAPPING #-} Delete1 n (n :- ns) (v :- vs) v ns vs where
   delete1 n (n1 :- ns) (v :- vs) = (v, ns, vs)

instance Delete1 n ninc vinc v ndel vdel => Delete1 n (n1 :- ninc) (v1 :- vinc) v (n1 :- ndel) (v1 :- vdel) where
   delete1 n (n1 :- ninc) (v1 :- vinc) = let (v, ndel, vdel) = NewTuple.delete1 n ninc vinc in (v, n1 :- ndel, v1 :- vdel)

class Select nsel nall vall vsel nleft vleft | nsel nall vall -> vsel nleft vleft where
   selectT :: nsel -> nall -> vall -> (vsel, nleft, vleft)

instance {-# OVERLAPPING #-} Select () nleft vleft () nleft vleft where
   selectT () nleft vleft = ((), nleft, vleft)

instance (Delete1 n nall vall v ndel vdel, Select nsel ndel vdel vsel nleft vleft) => Select (n :- nsel) nall vall (v :- vsel) nleft vleft where
   selectT (n :- ns0) ns1 vs0 = let 
      (v, nsdel, vsdel) = delete1 n ns1 vs0 
      (vs8, ns8, vs9) = selectT ns0 nsdel vsdel

      in (v :- vs8, ns8, vs9)

--j k = selectT (A :- B :- ()) (A :- B :- C :- ()) (D :- C :- B :- ())

{-}
class Sort a b | a -> b where
   sortT :: a -> b

instance Sort () () where
   sortT () = ()

instance (Sort as bs, Insert a bs cs) => Sort (a :- as) cs where
   sortT (a :- as) = insertT a (sortT as :: bs)
-}
class Insert a b c | a b -> c where
   insertT :: a -> b -> c

instance Insert a () (a :- ()) where
   insertT a () = a :- ()

instance (Greater a b, Insert a bs cs) => Insert a (b :- bs) (b :- cs) where
   insertT a (b :- bs) = b :- insertT a bs 

instance Greater b a => Insert a (b :- bs) (a :- b :- bs) where
   insertT a (b :- bs) = a :- b :- bs

--qqq a = insertT i9 (S (S Z) :- ())

class Greater a b where

instance Greater (S a) a where

instance Greater a b => Greater (S a) b where

class GreaterEqual a b where

instance GreaterEqual a a where

instance GreaterEqual a b => GreaterEqual (S a) b where

class Number a b c | a b -> c, c -> a where
   numberT :: a -> b -> c

instance Number () b () where
   numberT () _ = ()

instance Number as (() :- b) cs => Number (a :- as) b ((a, b) :- cs) where
   numberT (a :- as) b = (a, b) :- numberT as (() :- b)

class MapSnd a b | a -> b where
   mapSndT :: a -> b

instance MapSnd () () where
   mapSndT () = ()

instance MapSnd as bs => MapSnd ((a, b) :- as) (b :- bs) where
   mapSndT ((a, b) :- as) = b :- mapSndT as

class FindIndex a b c where
   findIndexT :: a -> b -> c

instance FindIndex a (a :- as) () where
   findIndexT a (a1 :- as) = ()

instance FindIndex a bs c => FindIndex a (b :- bs) (() :- c) where
   findIndexT a (b :- bs) = () :- findIndexT a bs

--instance Insert 
{-
class DeleteIndex i vs vs2 | i vs -> vs2 where
   deleteIndex :: i -> vs -> vs2

instance DeleteIndex () (v :- vs) vs where
   deleteIndex () (v :- vs) = vs

instance DeleteIndex is vs vs2 => DeleteIndex (i :- is) (v1 :- vs) (v1 :- vs2) where
   deleteIndex (i :- is) (v :- vs) = v :- NewTuple.deleteIndex is vs
-}
data A = A deriving (Eq, Ord, Show)
data B = B deriving (Eq, Ord, Show)
data C = C deriving (Eq, Ord, Show)
data D = D deriving (Eq, Ord, Show)

nt = (A :- "hello") :- (B :- "there") :- (C :- "jim") :- ()

class ShowNewTuple a where
   showNewT :: a -> [String]

instance ShowNewTuple () where
   showNewT () = []

instance (Show1 a, ProperTuple b, ShowNewTuple b) => ShowNewTuple (a :- b) where
   showNewT (a :- b) = show1 a : showNewT b

class ReadNewTuple a where
   readNewT :: [String] -> a

instance ReadNewTuple () where
   readNewT [] = ()

instance (Read1 a, ProperTuple b, ReadNewTuple b) => ReadNewTuple (a :- b) where
   readNewT (a : b) = read1 a :- readNewT b

class (ElemsProperTuple a) => Concat a b where
   concat2 :: a -> b
--using AllowAmbiguousTypes

instance Concat () () where
   concat2 () = ()

instance (ProperTuple a, Append a bs cs, Concat as bs) => Concat (a :- as) cs where
   concat2 (a :- as) = appendT a (concat2 as)

concatT xs = foldrT (appendT :: Append a b c => a -> b -> c) () xs

class Append a b c | a b -> c, a c -> b where
   appendT :: a -> b -> c

instance Append () b b where
   appendT () b = b

instance Append as bs cs => Append (a :- as) bs (a :- cs) where
   appendT (a :- as) bs = a :- appendT as bs

headT (a :- b) = a
tailT (a :- b) = b

class Length a b where
   lengthT :: a -> b

instance Length () Z where
   lengthT () = Z

instance Length b bl => Length (a :- b) (S bl) where
   lengthT (a :- b) = S $ lengthT b

class Index a b c | a b -> c where
   indexT :: a -> b -> c

instance Index Z (a :- b) a where
   indexT Z (a :- b) = a

instance Index b d e => Index (S b) (c :- d) e where
   indexT (S b) (c :- d) = indexT b d

class SplitAt a b c d | a b -> c d where
   splitAtT :: a -> b -> (c, d)

instance SplitAt Z b () b where
   splitAtT Z b = ((), b)

instance SplitAt as bs cs ds => SplitAt (S as) (b :- bs) (b :- cs) ds where
   splitAtT (S as) (b :- bs) = let (cs, ds) = splitAtT as bs in (b :- cs, ds)

dropT a b = snd $ splitAtT a b
takeT a b = fst $ splitAtT a b

data Z = Z

newtype S s = S s

type I0 = Z
type I1 = S Z
type I2 = S (S Z)
type I3 = S (S (S Z))
type I4 = S (S (S (S Z)))
type I5 = S (S (S (S (S Z))))
type I6 = S (S (S (S (S (S Z)))))
type I7 = S (S (S (S (S (S (S Z))))))
type I8 = S (S (S (S (S (S (S (S Z)))))))
type I9 = S (S (S (S (S (S (S (S (S Z))))))))
type I10 = S (S (S (S (S (S (S (S (S (S Z)))))))))

i0 = Z :: I0
i1 = S Z :: I1
i2 = S (S Z) :: I2
i3 = S (S (S Z)) :: I3
i4 = S (S (S (S Z))) :: I4
i5 = S (S (S (S (S Z)))) :: I5
i6 = S (S (S (S (S (S Z))))) :: I6
i7 = S (S (S (S (S (S (S Z)))))) :: I7
i8 = S (S (S (S (S (S (S (S Z))))))) :: I8
i9 = S (S (S (S (S (S (S (S (S Z)))))))) :: I9
i10 = S (S (S (S (S (S (S (S (S (S Z))))))))) :: I10

class Mult10 a b | a -> b, b -> a where
   mult10T :: a -> b

instance Mult10 Z Z where
   mult10T Z = Z

instance Mult10 s t => Mult10 (S s) (S (S (S (S (S (S (S (S (S (S t)))))))))) where
   mult10T (S s) = let t = mult10T s in S (S (S (S (S (S (S (S (S (S t)))))))))

class Power10 a b | a -> b where
   power10T :: a -> b

instance Power10 Z (S Z) where
   power10T Z = S Z

instance (Power10 s t, Mult10 t u) => Power10 (S s) u where
   power10T (S s) = mult10T $ power10T s

class Mult a b c | a b -> c where
   multT :: a -> b -> c

instance Mult a Z Z where
   multT a Z = Z

instance (Mult a b c, Plus a c d) => Mult a (S b) d where
   multT a (S b) = plusT a $ multT a b

class Plus a b c | a b -> c, b c -> a where
   plusT :: a -> b -> c

instance Plus a Z a where
   plusT a Z = a

instance Plus a b c => Plus a (S b) (S c) where
   plusT a (S b) = S $ plusT a b

class Digits a b | a -> b where
   digits :: a -> b

instance Digits () () where
   digits () = ()

instance (Power10 as bs, Mult a bs cs, Digits as ds, Plus ds cs es) => Digits (a :- as) es where
   digits (a :- as) = plusT (digits as) $ multT a $ power10T as

class Map f a b where
   mapT :: f -> a -> b

instance Map f () () where
   mapT f () = ()

class Apply f a b where
   applyC :: f -> a -> b

instance Apply (a -> b) a b where
   applyC f = f

instance (Apply f a b, Map f as bs) => Map f (a :- as) (b :- bs) where
   mapT f (a :- as) = applyC f a :- mapT f as

class Zip a b c | a b -> c, c -> a b where
   zipT :: a -> b -> c

instance Zip () () () where
   zipT () () = ()

instance (Zip as bs cs) => Zip (a :- as) (b :- bs) ((a, b) :- cs) where
   zipT (a :- as) (b :- bs) = (a, b) :- zipT as bs

class ZipWith f a b c where
   zipWithT :: f -> a -> b -> c

instance ZipWith f () () () where
   zipWithT f () () = ()

instance (Apply2 f a b c, ZipWith f as bs cs) => ZipWith f (a :- as) (b :- bs) (c :- cs) where
   zipWithT f (a :- as) (b :- bs) = applyC2 f a b :- zipWithT f as bs

class Zip3 f a b c d where
   zipWith3T :: f -> a -> b -> c -> d

instance Zip3 f () () () () where
   zipWith3T f () () () = ()

instance (Apply3 f a b c d, Zip3 f as bs cs ds) => Zip3 f (a :- as) (b :- bs) (c :- cs) (d :- ds) where
   zipWith3T f (a :- as) (b :- bs) (c :- cs) = applyC3 f a b c :- zipWith3T f as bs cs

class Apply2 f a b c | f a b -> c, f a c -> b where
   applyC2 :: f -> a -> b -> c

instance Apply2 (a -> b -> c) a b c where
   applyC2 f = f

class Apply3 f a b c d | f a b c -> d, f a b d -> c where
   applyC3 :: f -> a -> b -> c -> d

instance Apply3 (a -> b -> c -> d) a b c d where
   applyC3 f = f

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

class TofN a b where
   tofn :: a -> b

class NofT a b where
   noft :: a -> b

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
