-- Copyright 2025 Brett Curtis
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ShowTuple where

import Show1

class ShowTuple a where
   showT :: a -> [String]

instance (Show1 a, Show1 b) => ShowTuple (a, b) where showT (a, b) = [show1 a, show1 b]
instance (Show1 a, Show1 b, Show1 c) => ShowTuple (a, b, c) where showT (a, b, c) = [show1 a, show1 b, show1 c]
instance (Show1 a, Show1 b, Show1 c, Show1 d) => ShowTuple (a, b, c, d) where showT (a, b, c, d) = [show1 a, show1 b, show1 c, show1 d]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e) => ShowTuple (a, b, c, d, e) where showT (a, b, c, d, e) = [show1 a, show1 b, show1 c, show1 d, show1 e]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f) => ShowTuple (a, b, c, d, e, f) where showT (a, b, c, d, e, f) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g) => ShowTuple (a, b, c, d, e, f, g) where showT (a, b, c, d, e, f, g) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h) => ShowTuple (a, b, c, d, e, f, g, h) where showT (a, b, c, d, e, f, g, h) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h, Show1 i) => ShowTuple (a, b, c, d, e, f, g, h, i) where showT (a, b, c, d, e, f, g, h, i) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h, show1 i]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h, Show1 i, Show1 j) => ShowTuple (a, b, c, d, e, f, g, h, i, j) where showT (a, b, c, d, e, f, g, h, i, j) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h, show1 i, show1 j]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h, Show1 i, Show1 j, Show1 kk) => ShowTuple (a, b, c, d, e, f, g, h, i, j, kk) where showT (a, b, c, d, e, f, g, h, i, j, kk) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h, show1 i, show1 j, show1 kk]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h, Show1 i, Show1 j, Show1 kk, Show1 l) => ShowTuple (a, b, c, d, e, f, g, h, i, j, kk, l) where showT (a, b, c, d, e, f, g, h, i, j, kk, l) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h, show1 i, show1 j, show1 kk, show1 l]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h, Show1 i, Show1 j, Show1 kk, Show1 l, Show1 m) => ShowTuple (a, b, c, d, e, f, g, h, i, j, kk, l, m) where showT (a, b, c, d, e, f, g, h, i, j, kk, l, m) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h, show1 i, show1 j, show1 kk, show1 l, show1 m]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h, Show1 i, Show1 j, Show1 kk, Show1 l, Show1 m, Show1 n) => ShowTuple (a, b, c, d, e, f, g, h, i, j, kk, l, m, n) where showT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h, show1 i, show1 j, show1 kk, show1 l, show1 m, show1 n]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h, Show1 i, Show1 j, Show1 kk, Show1 l, Show1 m, Show1 n, Show1 o) => ShowTuple (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o) where showT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h, show1 i, show1 j, show1 kk, show1 l, show1 m, show1 n, show1 o]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h, Show1 i, Show1 j, Show1 kk, Show1 l, Show1 m, Show1 n, Show1 o, Show1 p) => ShowTuple (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p) where showT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h, show1 i, show1 j, show1 kk, show1 l, show1 m, show1 n, show1 o, show1 p]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h, Show1 i, Show1 j, Show1 kk, Show1 l, Show1 m, Show1 n, Show1 o, Show1 p, Show1 q) => ShowTuple (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q) where showT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h, show1 i, show1 j, show1 kk, show1 l, show1 m, show1 n, show1 o, show1 p, show1 q]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h, Show1 i, Show1 j, Show1 kk, Show1 l, Show1 m, Show1 n, Show1 o, Show1 p, Show1 q, Show1 r) => ShowTuple (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q, r) where showT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q, r) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h, show1 i, show1 j, show1 kk, show1 l, show1 m, show1 n, show1 o, show1 p, show1 q, show1 r]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h, Show1 i, Show1 j, Show1 kk, Show1 l, Show1 m, Show1 n, Show1 o, Show1 p, Show1 q, Show1 r, Show1 s) => ShowTuple (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q, r, s) where showT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q, r, s) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h, show1 i, show1 j, show1 kk, show1 l, show1 m, show1 n, show1 o, show1 p, show1 q, show1 r, show1 s]
instance (Show1 a, Show1 b, Show1 c, Show1 d, Show1 e, Show1 f, Show1 g, Show1 h, Show1 i, Show1 j, Show1 kk, Show1 l, Show1 m, Show1 n, Show1 o, Show1 p, Show1 q, Show1 r, Show1 s, Show1 t) => ShowTuple (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q, r, s, t) where showT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q, r, s, t) = [show1 a, show1 b, show1 c, show1 d, show1 e, show1 f, show1 g, show1 h, show1 i, show1 j, show1 kk, show1 l, show1 m, show1 n, show1 o, show1 p, show1 q, show1 r, show1 s, show1 t]

class MapTuple f a b where
   mapT :: f -> a -> b

class Apply f a a1 where
   apply :: f -> a -> a1

instance (Apply f a a1, Apply f b b1) => MapTuple f (a, b) (a1, b1) where mapT f (a, b) = (apply f a, apply f b)
instance (Apply fn a a1, Apply fn b b1, Apply fn c c1) => MapTuple fn (a, b, c) (a1, b1, c1) where mapT fn (a, b, c) = (apply fn a, apply fn b, apply fn c)
instance (Apply fn a a1, Apply fn b b1, Apply fn c c1, Apply fn d d1) => MapTuple fn (a, b, c, d) (a1, b1, c1, d1) where mapT fn (a, b, c, d) = (apply fn a, apply fn b, apply fn c, apply fn d)
instance (Apply fn a a1, Apply fn b b1, Apply fn c c1, Apply fn d d1, Apply fn e e1) => MapTuple fn (a, b, c, d, e) (a1, b1, c1, d1, e1) where mapT fn (a, b, c, d, e) = (apply fn a, apply fn b, apply fn c, apply fn d, apply fn e)
instance (Apply fn a a1, Apply fn b b1, Apply fn c c1, Apply fn d d1, Apply fn e e1, Apply fn f f1) => MapTuple fn (a, b, c, d, e, f) (a1, b1, c1, d1, e1, f1) where mapT fn (a, b, c, d, e, f) = (apply fn a, apply fn b, apply fn c, apply fn d, apply fn e, apply fn f)
instance (Apply fn a a1, Apply fn b b1, Apply fn c c1, Apply fn d d1, Apply fn e e1, Apply fn f f1, Apply fn g g1) => MapTuple fn (a, b, c, d, e, f, g) (a1, b1, c1, d1, e1, f1, g1) where mapT fn (a, b, c, d, e, f, g) = (apply fn a, apply fn b, apply fn c, apply fn d, apply fn e, apply fn f, apply fn g)
instance (Apply fn a a1, Apply fn b b1, Apply fn c c1, Apply fn d d1, Apply fn e e1, Apply fn f f1, Apply fn g g1, Apply fn h h1) => MapTuple fn (a, b, c, d, e, f, g, h) (a1, b1, c1, d1, e1, f1, g1, h1) where mapT fn (a, b, c, d, e, f, g, h) = (apply fn a, apply fn b, apply fn c, apply fn d, apply fn e, apply fn f, apply fn g, apply fn h)
instance (Apply fn a a1, Apply fn b b1, Apply fn c c1, Apply fn d d1, Apply fn e e1, Apply fn f f1, Apply fn g g1, Apply fn h h1, Apply fn i i1) => MapTuple fn (a, b, c, d, e, f, g, h, i) (a1, b1, c1, d1, e1, f1, g1, h1, i1) where mapT fn (a, b, c, d, e, f, g, h, i) = (apply fn a, apply fn b, apply fn c, apply fn d, apply fn e, apply fn f, apply fn g, apply fn h, apply fn i)
instance (Apply fn a a1, Apply fn b b1, Apply fn c c1, Apply fn d d1, Apply fn e e1, Apply fn f f1, Apply fn g g1, Apply fn h h1, Apply fn i i1, Apply fn j j1) => MapTuple fn (a, b, c, d, e, f, g, h, i, j) (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1) where mapT fn (a, b, c, d, e, f, g, h, i, j) = (apply fn a, apply fn b, apply fn c, apply fn d, apply fn e, apply fn f, apply fn g, apply fn h, apply fn i, apply fn j)

class ConsTuple a b c where
   consT :: a -> b -> c

instance ConsTuple a (b, c) (a, b, c) where
   consT a (b, c) = (a, b, c)

instance ConsTuple a (b, c, d) (a, b, c, d) where
   consT a (b, c, d) = (a, b, c, d)

instance ConsTuple a (b, c, d, e) (a, b, c, d, e) where
   consT a (b, c, d, e) = (a, b, c, d, e)

