{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

module Test where

import System.IO
import Control.Monad.Trans
import Control.Monad.State
import Control.Applicative
import ListT
import Data.Maybe

stateIO = do
   a <- get
   lift $ print a
   return a

listIO = do
   a <- cons 1 $ cons 2 $ cons 3 empty
   lift $ print a
   return a

stateList = do
   a <- lift [3,2,1]
   put a

listState = do
   a <- lift get
   cons a empty

listStateIO :: ListT (StateT String IO) Char
listStateIO = do
   lift $ lift $ print "hello"
   a <- lift token
   b <- lift token
   cons a empty
   return a

stateListIO = do
   a <- lift listIO
   b <- lift $ cons "hello" $ cons "there" $ cons "brett" empty
   put a
   return b

token = do
   (h:t) <- get
   put t
   return h

x = lift token

class Syntax d where
   seq1 :: d a -> d b -> d (a, b)
   alt1 :: d a -> d a -> d a

instance Monad m => Syntax m where
   seq1 a b = do
      ra <- a
      rb <- b
      return (ra, rb)

   alt1 a b = join $ cons a $ cons b empty

app :: (t -> Maybe b) -> Parser t -> Parser b
app f a = do
   ra <- a
   maybe empty return (f ra)

alt :: Parser a -> Parser a -> Parser a
alt a b = do
   ra <- a
   rb <- b
   cons ra $ cons rb empty
   --join $ cons a $ cons b empty

type Parser a = ListT (StateT String IO) a

sq :: Parser a -> Parser b -> Parser (a, b)
sq a b = do
   ra <- a
   rb <- b
   return (ra, rb)

test = evalStateT syn "hello"

syn :: (MonadFail m1, Monad m2, MonadTrans t, MonadState [b] m1) => t (ListT m2) (m1 (b, b))
syn = alt (sq token token) (sq token token)

parse = toList
{-
ioList = do
   a <- lift getLine
   cons a
-}
blah = print 1 <|> print 2 <|> print 3

kk = return 1 <|> return 2

jj = uncons

{-
>>> test
('h',((),'e'))
-}
