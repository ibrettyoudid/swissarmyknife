{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Biparser where

import RuleGADT
import Data.Map qualified as M

data Var n v = Var n v

newtype Pos = Pos { fromPos :: Int }
            deriving (Eq, Ord, Show, Num)

-- | The result of a parse.  This is parameterised over the type @i@
-- of string that was processed.
--
-- This type is an instance of 'Functor', where 'fmap' transforms the
-- value in a 'Done' result.
data IResult i f r
   = Fail [i] [f] String
    -- ^ The parse failed.  The @i@ parameter is the input that had
    -- not yet been consumed when the failure occurred.  The
    -- @[@'String'@]@ is a list of contexts in which the error
    -- occurred.  The 'String' is the message describing the error, if
    -- any.
   | Partial (i -> IResult i f r)
    -- ^ Supply this continuation with more input so that the parser
    -- can resume.  To indicate that no more input is available, pass
    -- an empty string to the continuation.
    --
    -- __Note__: if you get a 'Partial' result, do not call its
    -- continuation more than once.
   | Done [i] [f] r
    -- ^ The parse succeeded.  The @i@ parameter is the input that had
    -- not yet been consumed (if any) when the parse succeeded.

data FResult i f 
   = FDone [i] f
   | FFail [i] f String
{-
newtype Parser i f a = Parser {
      runParser :: forall r.
                   State i -> Pos -> More
                -> Failure i (State i) f r
                -> Success i (State i) f a r
                -> IResult i f r
    }

type family State i
type instance State ByteString = B.Buffer
type instance State Text = T.Buffer

type Failure i t f r = f -> t -> Pos -> More -> [String] -> String
                         -> IResult i f r
type Success i t f a r = f -> t -> Pos -> More -> a -> IResult i f r
-}

class Frame name value frame | frame -> value where
   myget1 :: name -> frame -> Maybe value
   myset1 :: name -> value -> frame -> frame

instance Ord a => Frame a b (M.Map a b) where
   myget1 = M.lookup
   myset1 = M.insert

parse :: (Eq i, Show i, Show n, Frame n v f) => [i] -> [f] -> Rule i n v f r -> IResult i f r
parse (ch:stream) fr (Token tok) = 
   if ch == tok
      then Done stream fr ch
      else Fail (ch:stream) fr $ "Expecting token "++show ch

parse i fs (Let f rule) = 
   case parse i (f:fs) rule of
      Done i1 (f:fs1) r -> Done i1 fs1 r
      fail@Fail { }     -> fail

parse i fs (Set name rule) = 
   case parse i fs rule of
      Done i1 fs r -> Done i1 (myset name r fs) r
      Fail i1 fs m -> Fail i1 fs m

parse i fs (Get name) = Done i fs $ myget name fs

format r fs (Token tok) = FDone [tok] fs

format r fs (Let f rule) = 
   case format r (f:fs) rule of
      FDone out (f:fs2) -> FDone out fs2
      fail@FFail { }    -> fail

format r fs (Get name) = FDone [] $ myset name r fs

format r fs (Set name rule) = format (myget name fs) fs rule


myset name value [] = error $ "failed to find variable "++show name
myset name value (f:frames) =
   case myget1 name f of
      Just  j -> myset1 name value f:frames
      Nothing -> f:myset name value frames

myget name []         = error $ "failed to get variable "++show name
myget name (f:frames) =
   case myget1 name f of
      Just  j -> j
      Nothing -> myget name frames
   
   