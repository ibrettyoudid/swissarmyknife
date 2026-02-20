{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE MultiWayIf #-}
{- HLINT ignore "Redundant bracket" -}


module Parser6 where

import Parser6Types
import Iso
import MHashDynamic3 hiding (Apply, Frame, Member, Let, Lambda, cname)
import NewTuple
import BString

import Prelude hiding ((++), length, null, tail, head, drop, take, concat, toLower, putStr, init)
import qualified Prelude

import Data.ByteString.Lazy qualified as LB

import Data.Word
import Data.Char hiding (toLower)
import Data.Map qualified as M
import Data.Maybe
import Control.Monad

import Debug.Trace
import Unsafe.Coerce

newtype Ig a = Ig a

data Lens a b
   = Lens (a -> b) (b -> a -> a)

data Rule s t f r where
   Many     ::  Rule s t f  a     -> Rule s t f [a]
   Alt      :: [Rule s t f  a ]   -> Rule s t f  a
   And      :: [Rule s t f  a ]   -> Rule s t f  a
   Not      ::  Rule s t f  a     -> Rule s t f  a
   Ignore   ::  Rule s t f  a     -> Rule s t f  a
   Eith     ::  Rule s t f  a     -> Rule s t f  b  -> Rule s t f (Either a b)
   (:+)     ::  Rule s t f  a     -> Rule s t f  b  -> Rule s t f (a :- b)
   (:/)     ::  Rule s t f  a     -> Rule s t f  b  -> Rule s t f  b
   (://)    ::  Rule s t f  a     -> Rule s t f  b  -> Rule s t f  a
   ManyTill ::  Rule s t f  a     -> Rule s t f  b  -> Rule s t f [a]
   Try      ::  Rule s t f  a     -> Rule s t f  a
   AnyTill  ::  Rule s t f  a                       -> Rule s t f s
   Option   ::  Rule s t f  a                       -> Rule s t f (Maybe a)
   AnyToken ::  Rule s t f  t  
   Rest     ::  Rule s t f  s  
   Return   ::              a     -> Rule s t f  a
   Default  ::              a     -> Rule s t f  a  -> Rule s t f  a
   String   ::       s            -> Rule s t f  s
   Count    ::             Int    -> Rule s t f  b  -> Rule s t f [b]
   Build    ::           f1       -> Rule s t f1 a  -> Rule s t f  f1
   Call     ::           f        -> Rule s t f  a  -> Rule s t f  a
   Name     :: String             -> Rule s t f  a  -> Rule s t f  a
   Apply    ::   Iso  a  b        -> Rule s t f  a  -> Rule s t f  b
   Seq      :: SeqTuple a b f s t =>           a    -> Rule s t f  b
   Redo     :: Frame      n  s  f =>           n    -> Rule s t f  v  -> Rule s t f  v
   Lambda   :: FrameTuple n  v  f =>           n    -> Rule s t f  v  -> Rule s t f  v
   Set      :: (Show n, Show v, Frame      n  v  f) =>           n    -> Rule s t f  v  -> Rule s t f  v
   SetM     :: FrameTuple n  v  f =>           n    -> Rule s t f  v  -> Rule s t f  v
   Get      :: Frame      n  v  f =>           n                      -> Rule s t f  v
   GetM     :: FrameTuple n  v  f =>           n                      -> Rule s t f  v
   Range    ::         t          ->        t       -> Rule s t f  t
   Tokens   ::             Int    ->                   Rule s t f  s
   Token    ::         t          -> Rule s t f  t
   OneWay   :: Frame      n  v  f =>           n    -> (v -> Rule s t f  a)  -> Rule s t f  a
   Bind     ::  Rule s t f a      -> (a -> Rule s t f b, b -> a) -> Rule s t f b
   Anything :: (BStringC tok str, Eq tok, Show tok, Ord tok) => (frame -> str -> IResult str tok frame res) -> (frame -> res -> FResult str tok frame) -> Rule str tok frame res
   Inner    ::  Rule s t f a      -> Rule s t b  f
   Member   :: Frame n v f        =>           n    -> Rule s t v  a  -> Rule s t f  v
   GetSub   :: Frame n v f        =>           n    -> Rule s t f  v  -> Rule s t f  v
   Taken    :: Eq s               => Rule s t f  a  -> Rule s t f  s

{-}
data Rule name value tok
   = Many     (Rule name value tok)
   | Seq      [Rule name value tok]
   | Alt      [Rule name value tok]
   | And      [Rule name value tok]
   | Not      (Rule name value tok)
   | Ignore   (Rule name value tok)
   | Then     (Rule name value tok) (Rule name value tok)
   | ManyTill (Rule name value tok) (Rule name value tok)
   | AnyTill  (Rule name value tok)
   | Apply    (Iso Dynamic Dynamic) (Rule name value tok)
   | Count    (Rule name value tok) (Rule name value tok)
   | Pure     Dynamic
   | Try      (Rule name value tok)
   | AnyToken
   | String   [tok]
   | Token    tok
   | Range    tok    tok
   | Get       name
   | GetM     [name]
   | Set       name  (Rule name value tok)
   | Let      [name] (Rule name value tok)
   | SetM     [name] (Rule name value tok)
   | Build           (Rule name value tok)
   | Redo      name  (Rule name value tok)
   | Name     String (Rule name value tok)
   | Rest
   deriving (Show)
-}
newtype Pos = Pos { fromPos :: Int }
            deriving (Eq, Ord, Show, Num)

-- | The result of a parse1.  This is parameterised over the type @i@
-- of string that was processed.
--
-- This type is an instance of 'Functor', where 'fmap' transforms the
-- value in a 'Done' result.
data IResult str tok frame res
   = Fail str frame String
   -- ^ The parse1 failed.  The @i@ parameter is the input that had
   -- not yet been consumed when the failure occurred.  The
   -- @[@'String'@]@ is a list of contexts in which the error
   -- occurred.  The 'String' is the message describing the error, if
   -- any.
--   | More (i -> IResult r f i)
   -- ^ Supply this continuation with more input so that the parse1r
   -- can resume.  To indicate that no more input is available, pass
   -- an empty string to the continuation.
   --
   -- __Note__: if you get a 'Partial' result, do not call its
   -- continuation more than once.
   | Done { rest::str, fr::frame, result::res }
   -- ^ The parse1 succeeded.  The @i@ parameter is the input that had
   -- not yet been consumed (if any) when the parse1 succeeded.
      deriving Show

data FResult str tok frame
   = FDone { fresult :: str, ffr :: frame }
   | FFail { fresult :: str, ffr :: frame, message :: String }
   deriving (Show)
{-
newtype parse1r i f a = parse1r {
      runparse1r :: forall r.
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

--data Tup a b where
--   TCons :: a -> b -> Tup a b

data TCons a b = TCons a b
data TNil = TNil

class Tuple t

instance Tuple TNil

instance Tuple b => Tuple (TCons a b)

type SSMap = M.Map String String

instance Ord a => Frame a b (M.Map a b) where
   myget1 k = fromJust . M.lookup k
   myset1 k v = M.insert k v

--parse1 :: (Eq tok, Show tok, Ord tok) => Rule res frame tok -> [frame] -> [tok] -> IResult res frame tok
class BString s => SeqTuple a b f s t where
   parseseq :: a -> f -> s -> IResult s t f b

instance BString s => SeqTuple () () f s t where
   parseseq () fs t = Done t fs ()

instance (Show s, BString s, BStringC t s, SeqTuple a b f s t, Eq t, Show t, Ord t, Show f) => SeqTuple (Rule s t f r :- a) (r :- b) f s t where
   parseseq (ah :- at) fs t =
      case parse1 ah fs t of
         Done tah fsah rah ->
            case parseseq at fsah tah :: IResult s t f b of
               Done tat fsat rat -> Done tat fsat (rah :- rat)

class FrameTuple names values frame where
   mygetm :: names -> frame -> values
   mysetm :: names -> values -> frame -> frame

instance FrameTuple () () frame where
   mygetm () frame = ()
   mysetm () () frame = frame

instance (FrameTuple names values frame, Frame name value frame) => FrameTuple (name :- names) (value :- values) frame where
   mygetm (name :- names) frame = let
      value = myget1 name frame
      values = mygetm names frame

      in (value :- values)
   mysetm (name :- names) (value :- values) frame = mysetm names values $ myset1 name value frame


data HostPort = HostPort String Int deriving (Eq, Ord, Show)

data Host = Host deriving (Eq, Ord, Show)
data Port = Port deriving (Eq, Ord, Show)

instance Frame Host String HostPort where
   myget1 Host (HostPort h p) = h
   myset1 Host hnew (HostPort h p) = HostPort hnew p

instance Frame Port Int HostPort where
   myget1 Port (HostPort h p) = p
   myset1 Port pnew (HostPort h p) = HostPort h pnew

parse rule toks = parse1 rule () toks

format rule syntax = format1 rule () syntax

--parse1 :: (Eq tok, Show tok, Ord tok) => Rule str tok frame res -> frame -> str -> IResult tok frame res
--parse1 :: Rule LB.ByteString Word8 frame res -> frame -> LB.ByteString -> IResult tok frame res
parse1 :: (Show str, BStringC tok str, Eq tok, Show tok, Ord tok) => Rule str tok frame res -> frame -> str -> IResult str tok frame res
parse1 AnyToken fs i =
   if null i
      then Fail i fs "Expecting any token but got EOF"
      else Done (tail i) fs $ head i

parse1 (Token tok) fs i
   | null i        = Fail i fs ("Expecting token "++show tok++" at EOF")
   | head i == tok = Done (tail i) fs $ head i
   | otherwise     = Fail i fs ("Expecting token "++show tok) 

parse1 r@(Range from to) fs i
   | null i = Fail i fs $ "EOF when expecting range "++show from++".."++show to
   | from <= head i && head i <= to = Done (tail i) fs $ head i
   | otherwise = Fail i fs $ "Expecting "++show from++".."++show to

parse1 (String str) fs i =
   case stripPrefix str i of
      Just  j -> Done j fs str
      Nothing -> Fail i fs $ "Expecting string "++show str

parse1 (a :+ b) fs i =
   case parse1 a fs i of
      Done i1 fs1 ar ->
         case parse1 b fs1 i1 of
            Done i2 fs2 br -> Done i2 fs2 (ar :- br)
            Fail i2 fs2 em -> Fail i2 fs2 em
      Fail i1 fs1 em -> Fail i1 fs1 em

parse1 (a :/ b) fs i =
   case parse1 a fs i of
      Done i1 fs1 ar ->
         case parse1 b fs1 i1 of
            Done i2 fs2 br -> Done i2 fs2 br
            Fail i2 fs2 em -> Fail i2 fs2 em
      Fail i1 fs1 em -> Fail i1 fs1 em

parse1 (a :// b) fs i =
   case parse1 a fs i of
      Done i1 fs1 ar ->
         case parse1 b fs1 i1 of
            Done i2 fs2 br -> Done i2 fs2 ar
            Fail i2 fs2 em -> Fail i2 fs2 em
      Fail i1 fs1 em -> Fail i1 fs1 em

parse1 (Eith a b) f t =
   case parse1 a f t of
      Done at af ar -> Done at af (Left ar)
      Fail at af ae ->
         case parse1 b f t of
            Done bt bf br -> Done bt bf (Right br)
            Fail bt bf be -> Fail bt bf be

parse1 (Option a) f t =
   case parse1 a f t of
      Done at af ar -> Done at af (Just ar)
      Fail at af ae -> Done t f Nothing

parse1 (Seq a) fs i = parseseq a fs i
{-
   case as of
      (a:as1) ->
         case parse1 a fs i c of
            Done ar1 fs1 i1 ->
               case parse1 (Seq as1) fs1 i1 c of
                  Done asr1 fs2 i2 ->
                     case a of
                        Ignore ig -> Done (toDyn asr1) fs2 i2
                        _         -> Done (toDyn (ar1:fromDyn1 asr1)) fs2 i2
                  fail@Fail   {}   -> fail
            fail@Fail {} -> fail
      [] -> Done (toDyn ([] :: [Dynamic])) fs i
-}
parse1 (Alt as) fs i =
   case as of
      (a:as1) ->
         case parse1 a fs i of
            Done i1 fs1 ar1 -> Done i1 fs1 ar1
            fail@Fail {} -> parse1 (Alt as1) fs i
      [] -> Fail i fs "no alternatives match"

parse1 m@(Many a) fs i =
   case parse1 a fs i of
      Done i1 fs1 ar ->
         case parse1 m fs1 i1 of
            Done i2 fs2 mr -> Done i2 fs2 (ar:mr)
            Fail i2 fs2 em -> Done i1 fs1 [ar]
      Fail i1 fs1 em -> Done i fs [] --trace ("reached last repeat: "++em) $ Done i fs []

parse1 m@(ManyTill a b) fs i =
   case parse1 b fs i of
      Done i1 fs1 br  -> Done i fs []
      Fail i1 fs1 em ->
         case parse1 a fs i of
            Done i2 fs2 ar ->
               case parse1 m fs2 i2 of
                  Done i3 fs3 mr -> Done i3 fs3 (ar:mr)
                  fail@Fail   {} -> fail
            Fail i2 fs2 em -> Done i fs []

parse1 m@(AnyTill b) fs i =
   case parse1 b fs i of
      Done i1 fs1 br -> Done i fs empty
      Fail i1 fs1 em | null i -> Done i fs empty
                     | otherwise ->
                           case parse1 m fs1 (tail i) of
                              Done i3 fs3 mr -> Done i3 fs3 (cons (head i) mr)
                              Fail i3 fs3 em -> Fail i3 fs3 em

parse1 (Apply iso a) f t =
   case parse1 a f t of
      Done t1 f1 ar ->
         case apply iso ar of
            Just  j -> Done t1 f1 j
            Nothing -> Fail t1 f1 "apply failed"
      Fail t1 f1 em -> Fail t1 f1 em

parse1 (Count n x) f t = parse1count x f t n

parse1 (Tokens n) f t
   | length t >= n = Done (drop n t) f (take n t)
   | otherwise     = Fail t f $ "EOF when trying to read "++show n++" tokens"

{-
parse1 (Let names rule) f t =
   case parse1 rule (M.fromList (map (, toDyn "") names):fs) t of
      Done i1 (f:fs1) r -> Done i1 fs1 r
      fail@Fail { }     -> fail
-}
parse1 (Set name rule) f t =
   case parse1 rule f t of
      Done t1 f1 r -> trace (show name ++ "=" ++ show r) $ 
         Done t1 (myset1 name r f1) r
      Fail t1 f1 m -> Fail t1 f1 m

parse1 (Member name rule) f t =
   case parse1 rule (myget1 name f) t of
      Done t1 f1 r -> Done t1 (myset1 name f1 f) f1
      Fail t1 f1 m -> Fail t1 f m

parse1 (Get name) f t = Done t f (myget1 name f)
{-
parse1 (GetSub name rule) f t = 
   case parse1 rule f t of
      Done t1 f1 r -> let x = myget1 name r in Done t1 (myget1 name r)
-}

parse1 (SetM names rule) f t =
   case parse1 rule f t of
      Done t1 f1 r -> Done t1 (mysetm names r f1) r
      {-
         case mysetm names r f1 of
            Just j  -> Done r j t1
      -}
      Fail t1 f1 em -> Fail t1 f1 em

parse1 (Lambda names rule) f t =
   case parse1 rule f t of
      Done t1 f1 r -> Done t1 (mysetm names r f1) r
      {-
         case mysetm names r f1 of
            Just j  -> Done r j t1
      -}
      Fail t1 f1 em -> Fail t1 f1 em
{-
parse1 (Call lambda args) f t =
   case parse1
-}
parse1 (GetM names) f t = Done t f (mygetm names f)
{-
   case mygetm names f of
      Just j  -> Done t f j
      Nothing -> Fail f failed" "GetM t
-}
parse1 (Build init rule) f t =
   case parse1 rule init t of
      Done t1 f1 r  -> Done t1 f f1
      Fail t1 f1 em -> Fail t1 f em

parse1 (Redo name rule) f t = 
   case parse1 rule f (myget1 name f) of
      Done t1 f1 r  -> Done t f1 r
      Fail t1 f1 em -> Fail t f1 em

parse1 Rest f t = Done empty f t

parse1 (Default d rule) f t = parse1 rule f t

parse1 (Return p) f t = Done t f p

parse1 (Bind a (b, c)) f t =
   case parse1 a f t of
      Done t1 f1 ar -> parse1 (b ar) f1 t1
      fail@Fail{}   -> Fail t f "first part of bind failed"

parse1 (Anything a b) f t = a f t

parse1 (OneWay n func) f t = parse1 (func $ myget1 n f) f t

parse1 (Taken rule) f t =
   case parse1 rule f t of
      Done t1 f1 r1 -> Done t1 f1 $ search t1 t
      Fail t1 f1 em -> Fail t1 f1 em

search :: (Eq s, BStringC c s) => s -> s -> s
search needle haystack
   | needle == haystack = empty
   | otherwise          = cons (head haystack) $ search needle (tail haystack) 
--parse1 (Ignore a) f t = parse1 a f t

--parse1 (Call fnew rule) fold ts =


parse1count :: (Show str, BString str, BStringC tok str, Num n, Eq n, Show tok, Ord tok) => Rule str tok frame res -> frame -> str -> n -> IResult str tok frame [res]
parse1count x f t 0 = Done t f []
parse1count x f t n =
   case parse1 x f t of
      Done t1 f1 xr ->
         case parse1count x f1 t1 (n-1) of
            Done t2 f2 xsr -> Done t2 f2 (xr:xsr)
            Fail t2 f2 em  -> Fail t2 f2 em --Done t2 f2 [xr]
      Fail t1 f1 em -> Fail t1 f1 "not enough repeats"


--formatcount (r:rs) fs x 0 = FDone [] fs
formatcount x fs []     = FDone empty fs
formatcount x fs (r:rs) =
   case formatcount x fs rs of
      FDone ts fs1 ->
         case format1 x fs1 r of
            FDone t fs2 -> FDone (t++ts) fs2
            fail@FFail{}-> fail
      fail@FFail{}-> fail

format1 :: (BString str, BStringC tok str, Eq tok, Show tok, Ord tok) => Rule str tok frame res -> frame -> res -> FResult str tok frame
--format1 :: Rule s t f u -> f -> u -> FResult s t u
--format1 :: Frame n v f => Rule r [f] t -> [f] -> r -> FResult t [f]
--format1 :: Rule s t f r -> f -> r -> FResult t f
format1 AnyToken fs r = FDone (cons r empty) fs

format1 (Token tok) fs r = FDone (cons tok empty) fs

format1 (String str) fs r = FDone str fs

format1 (a :+ b) fs r =
   case r of
      (ar :- br) ->
         case format1 a fs ar of
            FDone at fs1 ->
               case format1 b fs1 br of
                  FDone bt fs2 -> FDone (at++bt) fs2
                  fail@FFail{} -> fail
            fail@FFail{} -> fail

format1 (a :/ b) fs br =
   case format1 b fs br of
      FDone tb fsb ->
         case format1 a fs undefined of
            FDone ta fsa -> FDone (ta++tb) fsa
            fail@FFail{} -> fail
      fail@FFail{} -> fail

format1 (a :// b) fs ar =
   case format1 b fs undefined of
      FDone tb fsb ->
         case format1 a fs ar of
            FDone ta fsa -> FDone (ta++tb) fsa
            fail@FFail{} -> fail
      fail@FFail{} -> fail


   {-
         case format1 a fs ar of
            FDone at fs1 ->
               case format1 b fs1 undefined of
                  FDone bt fs2 -> FDone (at++bt) fs2
                  fail@FFail{} -> fail
            fail@FFail{} -> fail
-}
format1 (Eith a b) f r =
   case r of
      Left  ar -> format1 a f ar
      Right br -> format1 b f br

format1 (Option a) f r =
   case r of
      Just ar -> format1 a f ar
      Nothing -> FDone empty f

{-
format1 (Seq a) fs r = 
   case a of
      TCons a1 as1 ->
         case r of
            (r1:rs) ->
               case format1 (Seq as1) fs rs of
                  FDone t1 fs1 ->
                     case format1 a1 fs1 r1 of
                        FDone t2 fs2 -> FDone (t2++t1) fs2
                        fail@FFail {}-> fail
                  fail@FFail fail -> {}
            [] -> FFail "ran fs [] out of results to format1 in Seq"
      TNil ->
         case r of
            [] -> FDone [] fs
            (r1:rs) -> FFail "too fs [] many results to format1 in Seq"
-}
format1 (Alt as) fs r =
   case as of
      (a:as1) ->
         case format1 a fs r of
            FDone t1 fs1 -> FDone t1 fs1
            fail@FFail {} ->
               case format1 (Alt as1) fs r of
                  FDone t2 fs2  -> FDone t2 fs2
                  fail@FFail {} -> fail
      [] -> FFail empty fs "no alternatives match"

format1 m@(Many a) fs r =
   case r of
      (r1:rs) ->
         case format1 m fs rs of
            FDone ts fs1 ->
               case format1 a fs1 r1 of
                  FDone t1 fs2 -> FDone (t1++ts) fs2
                  fail@FFail{}-> fail
            fail@FFail{} -> fail
      [] -> FDone empty fs

format1 m@(ManyTill a b) fs r =
   case r of
      (r1:rs) ->
         case format1 m fs rs of
            FDone ts fs1 ->
               case format1 a fs1 r1 of
                  FDone t1 fs2 -> FDone (t1++ts) fs2
                  fail@FFail{}-> fail
            fail@FFail{} -> fail
      [] -> FDone empty fs

format1 m@(AnyTill b) fs r = FDone r fs

format1 (Apply iso a) fs r =
   case unapply iso r of
      Just  j -> format1 a fs j
      Nothing -> FFail empty fs "unapply iso failed"

format1 (Count n x) fs r = formatcount x fs r

format1 (Tokens n) fs r = FDone r fs
{-}
format1 (Let names rule) fs r =
   case format1 rule (M.fromList (map (, toDyn "") names):fs) r of
      FDone out (f:fs2) -> FDone out fs2
      fail@FFail { }    -> fail
-}
format1 (Get name) fs r = FDone empty (myset1 name r fs)

format1 (Set name rule) fs r = format1 rule fs $ myget1 name fs

format1 (Member name rule) f r =
   case format1 rule (myget1 name f) undefined of
      FDone t1 f1    -> FDone t1 f 
      FFail t1 f1 em -> FFail t1 f em


-- Name <-- Member <-- rule

format1 (GetM names) fs r = FDone empty $ mysetm names r fs

format1 (SetM names rule) f r = format1 rule f $ mygetm names f

format1 (Build init rule) f r =
   case format1 rule r undefined of
      FDone t1 f2    -> FDone t1 f
      FFail t2 f2 em -> FFail t2 f em

format1 Rest fs r = FDone r fs

format1 (Default d rule) f r = format1 rule f d

format1 (Return p) f r = FDone empty f

format1 (Bind a (b, c)) f r = let
   d = c r
   in case format1 (b d) f r of
         FDone t1 f1 ->
            case format1 a f1 d of
               FDone t2 f2 -> FDone (t2++t1) f2
               fail@FFail{}   -> fail
         fail@FFail{}   -> fail

format1 (OneWay n func) f r = format1 (func $ myget1 n f) f r

format1 (Anything a b) f r = b f r

format1 (Taken rule) f r = FDone r f
{-
format1 (Redo name rule) fs r =
   case format1 rule fs r of
      FDone t fs1   -> 
         case myset1 name t fs1 of
            FDone t f2
      fail@FFail {} -> fail
-}

visitPost :: (forall r f t. Rule s t f r -> Rule s t f r) -> Rule s t f r -> Rule s t f r
visitPost f x = f $ visit (visitPost f) x

visitPre :: (forall r f t. Rule s t f r -> Rule s t f r) -> Rule s t f r -> Rule s t f r
visitPre f x = visit (visitPre f) (f x)

visit :: (forall r f t. Rule s t f r -> Rule s t f r) -> Rule s t f r -> Rule s t f r
visit f (Apply iso  x  ) = Apply iso  $ visit f x
visit f (Alt        xs ) = Alt        $ map (visit f) xs
visit f (Many       x  ) = Many       $ visit f x
visit f (ManyTill   x y) = ManyTill    (visit f x) (visit f y)
visit f (Set  name  x  ) = Set name   $ visit f x
visit f (SetM names x  ) = SetM names $ visit f x
visit f (And        xs ) = And        $ map (visit f) xs
visit f (Not        x  ) = Not        $ visit f x
visit f (        x :+ y) = visit f x :+ visit f y
visit f (Count      x y) = Count        x (visit f y)
visit f (Name name  x  ) = Name name  $ visit f x
--visit f (Ignore     x  ) = Ignore     $ visit f x
visit f (Try        x  ) = Try        $ visit f x
visit f other            = other

doManyTill = visitPost doManyTill1

ll = AnyToken :+ AnyToken :+ AnyToken

--doManyTill1 (Many AnyToken :+ b) = AnyTill b :+ b
doManyTill1 (Many a :+ b) = ManyTill a b :+ b
doManyTill1 (AnyTill a :+ b) = AnyTill b :+ b
doManyTill1 (ManyTill a b :+ c) = ManyTill a c :+ c
doManyTill1 other = other

nonEmpty :: Rule s t f r -> Bool
nonEmpty (Alt      a  ) = all nonEmpty a
nonEmpty (      a :+ b) = nonEmpty a || nonEmpty b
nonEmpty (Many     a  ) = False
nonEmpty (ManyTill a b) = False
nonEmpty (AnyTill  a  ) = False
nonEmpty (Token    a  ) = True
nonEmpty (String   a  ) = True
nonEmpty  AnyToken      = False
nonEmpty (Count    a b) = False
nonEmpty (Apply    a b) = nonEmpty b
nonEmpty (Name     a b) = nonEmpty b

--nonEmptyPrefix :: Rule s t f r -> Rule s f t
--nonEmptyPrefix (a :+ b) = if nonEmpty a then a else a :+ nonEmptyPrefix b

strOfChars :: [Dynamic] -> String
strOfChars = map fromDyn1

charsOfStr :: String -> [Dynamic]
charsOfStr = map toDyn

istr = totald strOfChars charsOfStr
{-
difference (Range b c) (Token a) = Alt [Range b (pred a), Range (succ a) c]

diff1 (Token a) = Range a a
diff1 AnyToken = Range (chr 0) (chr 255)

diff2 (Range a b) (Range c d) = 
   if c <= b && a <= d
      then catMaybes [
         if | a < c -> Just $ Range a (pred c)
            | True  -> Nothing,
         if | b > d -> Just $ Range (succ d) b
            | True  -> Nothing]
      else [Range a b]

diff3 [Range a b] = Range a b
diff3 (a:b:cs) = Alt (a:b:cs)
-}
filename1 = AnyTill (Token '.') :+ Token '.' :+ Many AnyToken

hostport = Build (HostPort "" 0) $ (Host <-- AnyTill (Token ':')) :+ Token ':' :+ (Port <-- int)

rint s = read s :: Int

isoint :: Iso String Int
isoint = total read show

int = Apply isoint $ Many (Range '0' '9')

t = parse1 filename1 (M.empty::SSMap) "abc.mp3"

--t2 = format1 filename t

t3 = parse hostport "www.deathmetal.org:443"

t4 = parse1 int () "443"

fd f x = do d <- fromDynamic x; r <- f d; return $ toDyn r

isod (Iso f g) = Iso (fd f) (fd g)

totald :: (Typeable a, Typeable b) => (a -> b) -> (b -> a) -> Iso Dynamic Dynamic
totald f g = Iso (fd (Just . f)) (fd (Just . g))

infix 2 <=>
infixl 3 <|>

-- infixl 4 <+>
infixr 5 >$<
infixr 6 >*<
infixr 6 >*
infixr 6 *<

infixr 3 <--
infixl 9 <@>

a <-- b = Set a b

infixr 2 :+
infixr 2 :/
infixr 2 ://

(>>==) :: Rule s t f a -> (a -> Rule s t f b, b -> a) -> Rule s t f b
(>>==) = Bind

(-->) :: Frame n v f => n -> (v -> Rule s t f  a)  -> Rule s t f  a
(-->) = OneWay

(<@>) :: Frame n v f => n -> Rule s t v a -> Rule s t f v
(<@>) = Member

(>*<) :: Rule s t f a -> Rule s t f b -> Rule s t f (a :- b)
(>*<) = (:+)

(*<) :: Rule s t f a -> Rule s t f b -> Rule s t f b
(*<) = (:/)

(>*) :: Rule s t f a -> Rule s t f b -> Rule s t f a
(>*) = (://)

{-
SeqR a >*< SeqR b = SeqR (a ++ b)
SeqR a >*<      b = SeqR (a ++ [b])
a      >*< SeqR b = SeqR (a:b)
a      >*<      b = SeqR [a, b]
-}

Alt a <|> Alt b = Alt (a ++ b)
Alt a <|>     b = Alt (a ++ [b])
a     <|> Alt b = Alt (a:b)
a     <|>     b = Alt [a, b]

(>$<) :: Iso a b -> Rule s t f a -> Rule s t f b
(>$<) = Apply

(<=>):: String -> Rule s t f a -> Rule s t f a
(<=>) = Name

token :: (Eq t) => t -> Rule s t f t
token = Token

text xs = Seq $ map token xs

anytoken :: Rule s t f t
anytoken = AnyToken

pure :: a -> Rule s t f a
pure = Return

many :: Rule s t f a -> Rule s t f [a]
many = Many

many1 :: Rule s t f a -> Rule s t f [a]
many1 p = icons >$< p >*< many p

sepBy x sep = icons >$< x >*< many (sep *< x) <|> Return []

groupOf i = sepBy i (text ";") -- <|> aligned i


chainl1 arg op f = ifoldl f >$< (arg >*< (many (op >*< arg)))

chainr1 arg op f = f >$< arg >*< (op >*< chainr1 arg op f) <|> arg

instance Show tok => Show (Rule str tok frame res) where
   showsPrec d AnyToken r = "AnyToken"++r
   showsPrec d (Token t) r = "Token "++show t++r
   showsPrec d (Many m) r = "Many "++showsPrec 10 m r
   showsPrec d (ManyTill m t) r = "ManyTill "++showsPrec 10 m " "++showsPrec 10 t r
   showsPrec d (AnyTill t) r = "AnyTill "++showsPrec 10 t r
   showsPrec d (Alt as) r = "Alt"++concatMap (\a -> showsPrec 10 a "") as++r
   showsPrec d (a :+ b) r = showsPrec 1 a " :+ "++showsPrec 1 b r

data DataInfo  = DataInfo  { dtype :: String, dyntype :: String, conz :: [DataCon] } deriving (Eq, Ord, Show)
data DataCon   = DataCon   { cname :: String, fields :: [DataField] } deriving (Eq, Ord, Show)
data DataField = DataField { fname :: String, ftype :: String, fkey  :: String, fdynkey :: String } deriving (Eq, Ord, Show)

--data DataInfo  = DataInfo  { type1 :: String, conz   :: [DataCon] }
--data DataCon   = DataCon   { con   :: String, fields :: [DataField] }


data TypeK    = TypeK    deriving (Eq, Ord, Show, Read)     
data ConzK    = ConzK    deriving (Eq, Ord, Show, Read)     
data CNameK   = CNameK   deriving (Eq, Ord, Show, Read)      
data FieldsK  = FieldsK  deriving (Eq, Ord, Show, Read)       
data DynTypeK = DynTypeK deriving (Eq, Ord, Show, Read)        
data FNameK   = FNameK   deriving (Eq, Ord, Show, Read)      
data FKeyK    = FKeyK    deriving (Eq, Ord, Show, Read)     
data FDynKeyK = FDynKeyK deriving (Eq, Ord, Show, Read)   
data FTypeK   = FTypeK   deriving (Eq, Ord, Show, Read)      

instance Frame TypeK String DataInfo where
   myget1 TypeK = dtype
   myset1 TypeK value frame = frame { dtype = value }

instance Frame ConzK [DataCon] DataInfo where
   myget1 ConzK = conz
   myset1 ConzK value frame = frame { conz = value }

instance Frame CNameK String DataCon where
   myget1 CNameK = cname
   myset1 CNameK value frame = frame { cname = value }

instance Frame FieldsK [DataField] DataCon where
   myget1 FieldsK = fields
   myset1 FieldsK value frame = frame { fields = value }

instance Frame DynTypeK String DataInfo where
   myget1 DynTypeK = dyntype
   myset1 DynTypeK value frame = frame { dyntype = value }


instance Frame FNameK String DataField where
   myget1 FNameK = fname
   myset1 FNameK value frame = frame { fname = value }

instance Frame FKeyK String DataField where
   myget1 FKeyK = fkey
   myset1 FKeyK value frame = frame { fkey = value }

instance Frame FTypeK String DataField where
   myget1 FTypeK = ftype
   myset1 FTypeK value frame = frame { ftype = value }

instance Frame FDynKeyK String DataField where
   myget1 FDynKeyK = fdynkey
   myset1 FDynKeyK value frame = frame { fdynkey = value }


dataInfo = Build (DataInfo "" "" []) $ String "data" :/ s1 :/
   TypeK <-- ident :/ s1 :/ Token '=' :/ s1 :/
   ConzK <-- sepBy dataCon (s1 :/ Token '|' :/ s1) :/ crlf

dataCon = Build (DataCon {}) $
   CNameK  <-- ident :/ s1 :/
   FieldsK <-- dataFields

dataFields = dataFields1 -- <|> dataFields2

dataFields1 = (Token '{' :/ s1 :/ sepBy dataField (Token   ',' :/ s1) :// Token '}' :/ s1)

dataField  = (Build
                  (DataField "" "" "" "")
                  (FNameK <-- ident :+ s1 :/ String "::" :/ s1 :/ FTypeK <-- type1 :/ s1))

--dataField2 = (Build (DataField "" "" "" "") (FTypeK <-- type1))

--               <|>
--               ManyTill (Build (DataField "" "") (s :/ FTypeK  <-- ident :// s)) crlf

--dataP2 = Build (DataField "" "") $ String "instance Frame" :+ s :+ FNameK <-- ident :+ s :+ FTypeK <-- ident :+ s :+ String "where" :+ crlf :+
--   String "myget1" :+ s :+ FNameK <-- ident :+ s :+ String "=" :+ s :+ FNameK <-- ident :+ crlf :+
--   String "myset1" :+ s :+ FNameK <-- ident :+ s :+ String "value" :+ s :+ String "frame" :+ s :+ String "=" :+ s :+ String "frame" :+ s :+ String "{" :+ s :+ FNameK <-- ident :+ s :+ String "=" :+ s :+ String "value" :+ s :+ String "}" :+ crlf

dataInfoI = Build (DataInfo "" "" []) (TypeK --> \ty -> Many (Build (DataCon "" []) $ FieldsK <-- Many (dataInstance ty)))

dataInstance frame = Build (DataField "" "" "" "") $ String "instance Frame" :/ s1 :/ FKeyK <-- ident :/ s1 :/ FTypeK <-- ident :/ s1 :/ String frame :/ s1 :/ String "where" :/ crlf :/
   s3 :/ String "myget1" :/ s1 :/ FKeyK <-- ident :/ s1 :/ String "=" :/ s1 :/ FNameK <-- ident :/ crlf :/
   s3 :/ String "myset1" :/ s1 :/ FKeyK <-- ident :/ s1 :/ String "value frame = frame {" :/ s1 :/ FNameK <-- ident :/ s1 :/ String "= value }" :/ crlf

dataDyn = Build (DataInfo "" "" []) $ String "instance FrameD" :/ s1 :/ DynTypeK <-- ident :/ s1 :/ TypeK <-- ident :/ s1 :/ String "where" :/ crlf :/
   s3 :/ String "mygetD name frame       = case name of\n" :/ s6 :/ ConzK <-- Many dataConGet :/ crlf :/
   s3 :/ String "mysetD name value frame = case name of\n" :/ s6 :/ ConzK <-- Many dataConSet :/ crlf

dataConGet = Build (DataCon "" []) $ FieldsK <-- Many dataDynGet
dataConSet = Build (DataCon "" []) $ FieldsK <-- Many dataDynSet

dataDynGet = Build (DataField "" "" "" "") $ s6 :/ FDynKeyK <-- ident :/ s1 :/ String "-> toDyn $" :/ s1 :/ FNameK <-- ident :/ s1 :/ String "frame" :/ crlf
dataDynSet = Build (DataField "" "" "" "") $ s6 :/ FDynKeyK <-- ident :/ s1 :/ String "-> frame {" :/ s1 :/ FNameK <-- ident :/ s1 :/ String "= fromDyn1 value }" :/ crlf

isWS c = c == ' ' || c == '\n'

ws = Token ' ' <|> Token '\n'

spaced str = foldr ((:/) . (\(a :- b) -> String a :/ Default b (Many ws))) (Return ()) (result $ parse (Many (AnyTill ws :+ Many ws)) str)

uppiso = total uppk unuppk

makeKeys i = i { conz = map (\c -> c { fields = map (\f -> f { fdynkey = upp $ fname f, fkey = uppk $ fname f }) $ fields c }) $ conz i }

upp (c : cs) = toUpper c : cs

uppk cs = upp cs ++ "K"

unuppk (c:cs) = cons (toLower c) $ init cs

s1 = Default " " $ Many (Token ' ')
s2 = sp 2
s3 = sp 3
s4 = sp 4
s6 = sp 6

sp n = Default (replicate n ' ') $ Many (Token ' ')

a /+ b = a :+ s1 :/ b :// s1

crlf = Default "\n" $ Many (Token ' ' <|> Token '\n')

ident = Apply icons (alpha :+ Many alnum)

zz j = Taken (Return 8)

type1 :: Rule [Char] Char f [Char]
type1 = Taken ((Token '[' :/ s1 :/ type1 :// s1 :/ Token ']')) <|> Taken (sepBy ident (Token ' ' <|> Token '.'))

alpha = Apply (satisfy (\c -> isAsciiUpper c || isAsciiLower c || c == '_')) AnyToken

alnum = Apply (satisfy (\c -> isAsciiUpper c || isAsciiLower c || c == '_' || isDigit c)) AnyToken

low x = map toLower x

ilookup k = Iso
   (\xs -> do
      (_, v) <- find ((low k ==) . low . fst) xs
      let xs1 = filter ((low k /=) . low . fst) xs
      return (v, xs1))
   (\(v, xs) -> Just $ (k, v):filter ((low k /=) . low . fst) xs)

convdata xs = putStr $ (\x -> fresult (format dataInfoI x) ++ fresult (format dataDyn x)) $ makeKeys $ (\x -> x { dyntype = "Var" }) $ result $ parse dataInfo xs

a = 9 :: Int

b = 10 :: Int

toksParse n = Tokens n
replParse rule     n = Count  n rule

tokens :: BString s => (Int -> Rule s t f s, s -> Int)
tokens    = (toksParse, length)
repl rule = (replParse rule, length)



{-
a >< b = fwd a b

xxx x = Ignore a `fwd` b

class ThenIgnore a b c where
   fwd :: a -> b -> c

data Ignore a = Ignore a

instance ThenIgnore (Ignore a) b b where
   a `fwd` b = b

instance ThenIgnore a b (a, b) where
   a `fwd` b = (a, b)

class IgnoreAll a b where
   go :: a -> b

instance IgnoreAll b c => IgnoreAll (a, b) (Ignore a, c) where
   go (a, b) = (Ignore a, go b)
   -}

da = "data Tag = Tag { id3 :: String, verMajor :: Int, verMinor :: Int, unsync :: Bool, extHdr :: Bool, experi :: Bool, footer :: Bool, tagSize :: Int, dat :: ByteString }"
d1 = concat [
      "data Frame  = FText         { frameID :: FrameID, frameHeader :: FrameHeader, textEncoding :: Int, value :: T.Text }\n",
      "            | FUserText     { frameID :: FrameID, frameHeader :: FrameHeader, textEncoding :: Int, description :: T.Text, value :: T.Text }\n",
      "            | FUnsyncLyrics { frameID :: FrameID, frameHeader :: FrameHeader, textEncoding :: Int, description :: T.Text, value :: T.Text, language :: T.Text }",
      "            | FPicture      { frameID :: FrameID, frameHeader :: FrameHeader, textEncoding :: Int, mimeType :: T.Text, pictureType :: Int, description :: T.Text, picture :: IOString }",
      "            | FSyncLyrics   { frameID :: FrameID, frameHeader :: FrameHeader, textEncoding :: Int, value :: T.Text, language :: T.Text, timeFormat :: Int, contentType :: Int, syncedLyrics :: M.Map Int T.Text }"]


hjk = convdata d1


--test :: (SeqTuple () () Int ByteString Word8, SeqTuple (Rule ByteString Word8 Int ByteString :- ()) (ByteString :- ()) Int ByteString Word8) => Rule ByteString Word8 Int (ByteString :- ())
test = Seq ((String (convertString "hello") :: Rule ByteString Word8 Int ByteString) :- ()) :: Rule ByteString Word8 Int (ByteString :- ())
