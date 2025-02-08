-- Copyright 2025 Brett Curtis
{-# LANGUAGE FunctionalDependencies #-}

module ApplyTuple where

class ApplyTuple a b c | a -> b c, b c -> a where
   applyT :: a -> b -> c

instance ApplyTuple (x -> a, x -> b) x (a, b) where applyT (a, b) x = (a x, b x)
instance ApplyTuple (x -> a, x -> b, x -> c) x (a, b, c) where applyT (a, b, c) x = (a x, b x, c x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d) x (a, b, c, d) where applyT (a, b, c, d) x = (a x, b x, c x, d x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e) x (a, b, c, d, e) where applyT (a, b, c, d, e) x = (a x, b x, c x, d x, e x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f) x (a, b, c, d, e, f) where applyT (a, b, c, d, e, f) x = (a x, b x, c x, d x, e x, f x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g) x (a, b, c, d, e, f, g) where applyT (a, b, c, d, e, f, g) x = (a x, b x, c x, d x, e x, f x, g x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h) x (a, b, c, d, e, f, g, h) where applyT (a, b, c, d, e, f, g, h) x = (a x, b x, c x, d x, e x, f x, g x, h x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h, x -> i) x (a, b, c, d, e, f, g, h, i) where applyT (a, b, c, d, e, f, g, h, i) x = (a x, b x, c x, d x, e x, f x, g x, h x, i x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h, x -> i, x -> j) x (a, b, c, d, e, f, g, h, i, j) where applyT (a, b, c, d, e, f, g, h, i, j) x = (a x, b x, c x, d x, e x, f x, g x, h x, i x, j x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h, x -> i, x -> j, x -> kk) x (a, b, c, d, e, f, g, h, i, j, kk) where applyT (a, b, c, d, e, f, g, h, i, j, kk) x = (a x, b x, c x, d x, e x, f x, g x, h x, i x, j x, kk x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h, x -> i, x -> j, x -> kk, x -> l) x (a, b, c, d, e, f, g, h, i, j, kk, l) where applyT (a, b, c, d, e, f, g, h, i, j, kk, l) x = (a x, b x, c x, d x, e x, f x, g x, h x, i x, j x, kk x, l x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h, x -> i, x -> j, x -> kk, x -> l, x -> m) x (a, b, c, d, e, f, g, h, i, j, kk, l, m) where applyT (a, b, c, d, e, f, g, h, i, j, kk, l, m) x = (a x, b x, c x, d x, e x, f x, g x, h x, i x, j x, kk x, l x, m x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h, x -> i, x -> j, x -> kk, x -> l, x -> m, x -> n) x (a, b, c, d, e, f, g, h, i, j, kk, l, m, n) where applyT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n) x = (a x, b x, c x, d x, e x, f x, g x, h x, i x, j x, kk x, l x, m x, n x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h, x -> i, x -> j, x -> kk, x -> l, x -> m, x -> n, x -> o) x (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o) where applyT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o) x = (a x, b x, c x, d x, e x, f x, g x, h x, i x, j x, kk x, l x, m x, n x, o x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h, x -> i, x -> j, x -> kk, x -> l, x -> m, x -> n, x -> o, x -> p) x (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p) where applyT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p) x = (a x, b x, c x, d x, e x, f x, g x, h x, i x, j x, kk x, l x, m x, n x, o x, p x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h, x -> i, x -> j, x -> kk, x -> l, x -> m, x -> n, x -> o, x -> p, x -> q) x (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q) where applyT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q) x = (a x, b x, c x, d x, e x, f x, g x, h x, i x, j x, kk x, l x, m x, n x, o x, p x, q x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h, x -> i, x -> j, x -> kk, x -> l, x -> m, x -> n, x -> o, x -> p, x -> q, x -> r) x (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q, r) where applyT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q, r) x = (a x, b x, c x, d x, e x, f x, g x, h x, i x, j x, kk x, l x, m x, n x, o x, p x, q x, r x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h, x -> i, x -> j, x -> kk, x -> l, x -> m, x -> n, x -> o, x -> p, x -> q, x -> r, x -> s) x (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q, r, s) where applyT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q, r, s) x = (a x, b x, c x, d x, e x, f x, g x, h x, i x, j x, kk x, l x, m x, n x, o x, p x, q x, r x, s x)
instance ApplyTuple (x -> a, x -> b, x -> c, x -> d, x -> e, x -> f, x -> g, x -> h, x -> i, x -> j, x -> kk, x -> l, x -> m, x -> n, x -> o, x -> p, x -> q, x -> r, x -> s, x -> t) x (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q, r, s, t) where applyT (a, b, c, d, e, f, g, h, i, j, kk, l, m, n, o, p, q, r, s, t) x = (a x, b x, c x, d x, e x, f x, g x, h x, i x, j x, kk x, l x, m x, n x, o x, p x, q x, r x, s x, t x)
