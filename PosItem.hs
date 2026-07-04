{-# LANGUAGE Safe #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Search for UndecidableInstances to see why this is needed
{-# LANGUAGE UndecidableInstances #-}
-- Needed because the CPSed versions of Writer and PosItem are secretly PosItem
-- wrappers, which don't force such constraints, even though they should legally
-- be there.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.PosItem.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- MonadPosItem class.
--
--      This module is inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.

-----------------------------------------------------------------------------

module PosItem (
    MonadPosItem(..),
    modify,
    modify',
    gets
  ) where

import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT) 
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import qualified Control.Monad.Trans.PosItem.Lazy as Lazy
import qualified Control.Monad.Trans.PosItem.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Select (SelectT)
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Trans.Class (lift)
import Data.Functor.Product (Product(..))

-- ---------------------------------------------------------------------------

-- | Minimal definition is either both of @get@ and @put@ or just @state@
class Monad m => MonadPosItem s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s
    get = state (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a
    {-# MINIMAL state | get, put #-}

-- | Monadic state transformer.
--
--      Maps an old state to a new state inside a state monad.
--      The old state is thrown away.
--
-- >      Main> :t modify ((+1) :: Int -> Int)
-- >      modify (...) :: (MonadPosItem Int a) => a ()
--
--    This says that @modify (+1)@ acts over any
--    Monad that is a member of the @MonadPosItem@ class,
--    with an @Int@ state.
modify :: MonadPosItem s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))

-- | A variant of 'modify' in which the computation is strict in the
-- new state.
--
-- @since 2.2
modify' :: MonadPosItem s m => (s -> s) -> m ()
modify' f = do
  s' <- get
  put $! f s'

-- | Gets specific component of the state, using a projection function
-- supplied.
gets :: MonadPosItem s m => (s -> a) -> m a
gets f = do
    s <- get
    return (f s)

instance Monad m => MonadPosItem s (Lazy.PosItemT s m) where
    get = Lazy.get
    put = Lazy.put
    state = Lazy.state

instance Monad m => MonadPosItem s (Strict.PosItemT s m) where
    get = Strict.get
    put = Strict.put
    state = Strict.state

-- | @since 2.3
instance (Monad m, Monoid w) => MonadPosItem s (CPSRWS.RWST r w s m) where
    get = CPSRWS.get
    put = CPSRWS.put
    state = CPSRWS.state

instance (Monad m, Monoid w) => MonadPosItem s (LazyRWS.RWST r w s m) where
    get = LazyRWS.get
    put = LazyRWS.put
    state = LazyRWS.state

instance (Monad m, Monoid w) => MonadPosItem s (StrictRWS.RWST r w s m) where
    get = StrictRWS.get
    put = StrictRWS.put
    state = StrictRWS.state

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

instance MonadPosItem s m => MonadPosItem s (ContT r m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 2.2
instance MonadPosItem s m => MonadPosItem s (ExceptT e m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadPosItem s m => MonadPosItem s (IdentityT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadPosItem s m => MonadPosItem s (MaybeT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadPosItem s m => MonadPosItem s (ReaderT r m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 2.3
instance (Monoid w, MonadPosItem s m) => MonadPosItem s (CPS.WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid w, MonadPosItem s m) => MonadPosItem s (Lazy.WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid w, MonadPosItem s m) => MonadPosItem s (Strict.WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 2.3
instance
  ( Monoid w
  , MonadPosItem s m
  ) => MonadPosItem s (AccumT w m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 2.3
instance MonadPosItem s m => MonadPosItem s (SelectT r m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 2.3.2
instance (MonadPosItem s m, MonadPosItem s n) => MonadPosItem s (Product m n) where
    get = Pair get get
    put s = Pair (put s) (put s)
    state sas = Pair (state sas) (state sas)