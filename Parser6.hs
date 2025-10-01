{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE MultiWayIf #-}


module Parser6 where

import Parser6Types
import Iso
import MHashDynamic hiding (Apply, Frame, Let)
import Data.Word
import Data.Char
import Data.Map qualified as M
import Data.List
import Data.Maybe
import Control.Monad

import Debug.Trace
import Unsafe.Coerce

newtype Ig a = Ig a

data Lens a b
   = Lens (a -> b) (b -> a -> a)

data Rule r f t where
   Many     ::  Rule  r  f t  -> Rule [r] f t
   Seq      :: SeqTuple a b f t => a -> Rule b f t
   Alt      :: [Rule  r  f t] -> Rule  r  f t
   And      :: [Rule  r  f t] -> Rule  r  f t
   Not      ::  Rule  r  f t  -> Rule  r  f t
--   Ignore   ::  Rule  r  f t  -> Rule  r  f t
   Eith     ::  Rule  r  f t  -> Rule  s  f t -> Rule (Either r s) f t
   Option   ::  Rule  r  f t                  -> Rule (Maybe r) f t
   (:-)     ::  Rule  a  f t  -> Rule  b  f t -> Rule (a,b) f t
   (:/)     ::  Rule  a  f t  -> Rule  b  f t -> Rule  b    f t
   (://)    ::  Rule  a  f t  -> Rule  b  f t -> Rule  a    f t
   ManyTill ::  Rule  r  f t  -> Rule  b  f t -> Rule [r]   f t
   AnyTill  ::  Rule  r  f t                  -> Rule [t]   f t
   Apply    ::   Iso  a  b    -> Rule  a  f t -> Rule  b    f t
   Count    ::  Rule Int f t  -> Rule  b  f t -> Rule [b]   f t
   Pure     ::        r       -> Rule  r  f t
   Try      ::  Rule  r  f t  -> Rule  r  f t
   AnyToken ::  Rule  t  f t
   String   ::       [t]      -> Rule [t] f t
   Token    ::        t       -> Rule  t  f t
   Range    ::        t       ->       t      -> Rule  t  f t
   Get      :: Frame  n  r f  =>       n                      -> Rule r f t
   Set      :: Frame  n  v f  =>       n      -> Rule  v  f t -> Rule v f t
   GetM     :: FrameTuple n v f =>     n                      -> Rule v f t
   SetM     :: FrameTuple n v f =>     n      -> Rule  v  f t -> Rule v f t
   Build    ::             f1 -> Rule  r f1 t -> Rule  f1 f t
   Lambda   ::             f  -> Rule  r  f t -> Rule  r  f t
   Call     ::             f  -> Rule  r  f t -> Rule  r  f t
   Name     :: String         -> Rule  r  f t -> Rule  r  f t
   Redo     :: Frame  n [t] f =>       n      -> Rule  r  f t -> Rule r f t
   Rest     ::  Rule [t] f t

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
data IResult r f t
   = Fail String f [t]
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
   | Done r f [t]
    -- ^ The parse1 succeeded.  The @i@ parameter is the input that had
    -- not yet been consumed (if any) when the parse1 succeeded.
      deriving Show

data FResult t f
   = FDone [t] f
   | FFail [t] f String
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
class SeqTuple a b f t where
   parseseq :: a -> f -> [t] -> IResult b f t

instance SeqTuple () () f t where
   parseseq () fs t = Done () fs t

instance (SeqTuple a b f t, Eq t, Show t, Ord t, Show f) => SeqTuple (Rule r f t, a) (r, b) f t where
   parseseq (a, b) fs t =
      case parse1 a fs t of
         Done ra fsa ta ->
            case parseseq b fsa ta of
               Done rb fsb tb -> Done (ra, rb) fsb tb

class FrameTuple names values frame where
   mygetm :: names -> frame -> values
   mysetm :: names -> values -> frame -> frame

instance FrameTuple () () frame where
   mygetm () frame = ()
   mysetm () () frame = frame

instance (FrameTuple names values frame, Frame name value frame) => FrameTuple (name, names) (value, values) frame where
   mygetm (name, names) frame = let
      value = myget1 name frame
      values = mygetm names frame
      in (value, values)
   mysetm (name, names) (value, values) frame = mysetm names values $ myset1 name value frame


data HostPort = HostPort String Int deriving (Eq, Ord, Show)

data Host = Host
data Port = Port

instance Frame Host String HostPort where
   myget1 Host (HostPort h p) = h
   myset1 Host hnew (HostPort h p) = HostPort hnew p

instance Frame Port Int HostPort where
   myget1 Port (HostPort h p) = p
   myset1 Port pnew (HostPort h p) = HostPort h pnew

parse rule toks = parse1 rule () toks

format rule syntax = format1 rule () syntax

parse1 :: (Eq tok, Show tok, Ord tok) => Rule res frame tok -> frame -> [tok] -> IResult res frame tok
parse1 AnyToken fs []     = Fail "Expecting any token but got end of input" fs []
parse1 AnyToken fs (i:is) = Done i fs is

parse1 (Token tok) fs []     = Fail ("Expecting token "++show tok) fs []
parse1 (Token tok) fs (i:is) =
   if i == tok
      then Done i fs is
      else Fail ("Expecting token "++show tok) fs (i:is)

parse1 r@(Range from to) fs i =
   case i of
      []      -> Fail ("EOF when expecting range "++show from++".."++show to) fs i
      (i1:is) ->
         if from <= i1 && i1 <= to
            then Done i1 fs is
            else Fail ("expecting "++show from++".."++show to) fs i

parse1 (String str) fs i =
   case stripPrefix str i of
      Just  j -> Done str fs j
      Nothing -> Fail ("Expecting string "++show str) fs i

parse1 (a :- b) fs i =
   case parse1 a fs i of
      Done ar fs1 i1 ->
         case parse1 b fs1 i1 of
            Done br fs2 i2 -> Done (ar, br) fs2 i2
            Fail em fs2 i2 -> Fail em fs2 i2
      Fail em fs1 i1 -> Fail em fs1 i1

parse1 (a :/ b) fs i =
   case parse1 a fs i of
      Done ar fs1 i1 ->
         case parse1 b fs1 i1 of
            Done br fs2 i2 -> Done br fs2 i2
            Fail em fs2 i2 -> Fail em fs2 i2
      Fail em fs1 i1 -> Fail em fs1 i1

parse1 (a :// b) fs i =
   case parse1 a fs i of
      Done ar fs1 i1 ->
         case parse1 b fs1 i1 of
            Done br fs2 i2 -> Done ar fs2 i2
            Fail em fs2 i2 -> Fail em fs2 i2
      Fail em fs1 i1 -> Fail em fs1 i1

parse1 (Eith a b) f t = 
   case parse1 a f t of
      Done ar af at -> Done (Left ar) af at
      Fail ae af at ->
         case parse1 b f t of
            Done br bf bt -> Done (Right br) bf bt
            Fail be bf bt -> Fail be bf bt

parse1 (Option a) f t =
   case parse1 a f t of
      Done ar af at -> Done (Just ar) af at
      Fail ae af at -> Done Nothing f t

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
                  fail@Fail {}     -> fail
            fail@Fail {} -> fail
      [] -> Done (toDyn ([] :: [Dynamic])) fs i
-}
parse1 (Alt as) fs i =
   case as of
      (a:as1) ->
         case parse1 a fs i of
            Done ar1 fs1 i1 -> Done ar1 fs1 i1
            fail@Fail {} -> parse1 (Alt as1) fs i
      [] -> Fail "no alternatives match" fs i

parse1 m@(Many a) fs i =
   case parse1 a fs i of
      Done ar fs1 i1 ->
         case parse1 m fs1 i1 of
            Done mr fs2 i2 -> Done (ar:mr) fs2 i2
            Fail em fs2 i2 -> Fail em fs2 i2
      Fail em fs1 i1 -> Done [] fs1 i1

parse1 m@(ManyTill a b) fs i =
   case parse1 b fs i of
      Done br fs1 i1 -> Done [] fs i
      Fail em fs1 i1 ->
         case parse1 a fs i of
            Done ar fs2 i2 ->
               case parse1 m fs2 i2 of
                  Done mr fs3 i3 -> Done (ar:mr) fs3 i3
                  fail@Fail {}   -> fail
            Fail em fs2 i2 -> Done [] fs i

parse1 m@(AnyTill b) fs i =
   case parse1 b fs i of
      Done br fs1 i1 -> Done [] fs i
      Fail em fs1 i1 ->
         case i of
            (ar:i2) ->
               case parse1 m fs1 i2 of
                  Done mr fs3 i3 -> Done (ar:mr) fs3 i3
                  fail@Fail {}   -> fail
            [] -> Done [] fs i

parse1 (Apply iso a) f t =
   case parse1 a f t of
      Done ar f1 t1 ->
         case trace (unsafeCoerce ar) $ apply iso ar of
            Just  j -> Done j f1 t1
            Nothing -> Fail "apply failed" f1 t1
      Fail em f1 t1 -> Fail em f1 t1

parse1 (Count n x) f t =
   case parse1 n f t of
      Done nr f1 t1 -> parse1count x f t nr
      Fail em f1 t1 -> Fail em f1 t1
{-
parse1 (Let names rule) f t =
   case parse1 rule (M.fromList (map (, toDyn "") names):fs) t of
      Done r (f:fs1) i1 -> Done r fs1 i1
      fail@Fail { }     -> fail
-}
parse1 (Set name rule) f t =
   case parse1 rule f t of
      Done r f1 t1 -> Done r (myset1 name r f1) t1
         {-
         case myset1 name r f1 of
            Just j  -> Done r j t1
            Nothing -> Fail "Set failed" f1 t1
            -}
      Fail m f1 t1 -> Fail m f t1

parse1 (Get name) f t = Done (myget1 name f) f t
{-
   case myget1 name f of
      Just j  -> Done j f t
      Nothing -> Fail "Get failed" f t
-}
parse1 (SetM names rule) f t =
   case parse1 rule f t of
      Done r f1 t1 -> Done r (mysetm names r f1) t1
      {-
         case mysetm names r f1 of
            Just j  -> Done r j t1
      -}
      Fail em f1 t1 -> Fail em f1 t1

parse1 (GetM names) f t = Done (mygetm names f) f t
{-
   case mygetm names f of
      Just j  -> Done j f t
      Nothing -> Fail "GetM failed" f t
-}
parse1 (Build init rule) f t =
   case parse1 rule init t of
      Done r f1 t1 -> Done f1 f t1
      Fail em f1 t1 -> Fail em f t1

parse1 (Redo name rule) f t = parse1 rule f (myget1 name f)

parse1 Rest f t = Done t f []

--parse1 (Ignore a) f t = parse1 a f t

--parse1 (Call fnew rule) fold ts =


parse1count :: (Num n, Eq n, Show tok, Ord tok) => Rule res frame tok -> frame -> [tok] -> n -> IResult [res] frame tok
parse1count x f t 0 = Done [] f t
parse1count x f t n =
   case parse1 x f t of
      Done xr f1 t1 ->
         case parse1count x f1 t1 (n-1) of
            Done xsr f2 t2 -> Done (xr:xsr) f2 t2

--formatcount (r:rs) fs x 0 = FDone [] fs
formatcount x fs []     = FDone [] fs
formatcount x fs (r:rs) =
   case formatcount x fs rs of
      FDone ts fs1 ->
         case format1 x fs1 r of
            FDone t fs2 -> FDone (t++ts) fs2
            fail@FFail{}-> fail
      fail@FFail{}-> fail

format1 :: Rule r z t -> z -> r -> FResult t z
--format1 :: Frame n v f => Rule r [f] t -> [f] -> r -> FResult t [f]
--format1 :: Rule r f t -> f -> r -> FResult t f
format1 AnyToken fs r = FDone [r] fs

format1 (Token tok) fs r = FDone [tok] fs

format1 (String str) fs r = FDone str fs

format1 (a :- b) fs r =
   case r of
      (ar, br) ->
         case format1 a fs ar of
            FDone at fs1 ->
               case format1 b fs1 br of
                  FDone bt fs2 -> FDone (at++bt) fs2
                  fail@FFail{} -> fail
            fail@FFail{} -> fail

format1 (a :/ b) fs r =
   case r of
      br ->
         case format1 a fs undefined of
            FDone at fs1 ->
               case format1 b fs1 br of
                  FDone bt fs2 -> FDone (at++bt) fs2
                  fail@FFail{} -> fail
            fail@FFail{} -> fail

format1 (a :// b) fs r =
   case r of
      ar ->
         case format1 a fs ar of
            FDone at fs1 ->
               case format1 b fs1 undefined of
                  FDone bt fs2 -> FDone (at++bt) fs2
                  fail@FFail{} -> fail
            fail@FFail{} -> fail

format1 (Eith a b) f r =
   case r of
      Left  ar -> format1 a f ar
      Right br -> format1 b f br

format1 (Option a) f r =
   case r of
      Just ar -> format1 a f ar
      Nothing -> FDone [] f
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
                  fail@FFail {} -> fail
            [] -> FFail [] fs "ran out of results to format1 in Seq"
      TNil ->
         case r of
            [] -> FDone [] fs
            (r1:rs) -> FFail [] fs "too many results to format1 in Seq"
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
      [] -> FFail [] fs "no alternatives match"

format1 m@(Many a) fs r =
   case r of
      (r1:rs) ->
         case format1 m fs rs of
            FDone ts fs1 ->
               case format1 a fs1 r1 of
                  FDone t1 fs2 -> FDone (t1++ts) fs2
                  fail@FFail{}-> fail
            fail@FFail{} -> fail
      [] -> FDone [] fs

format1 m@(ManyTill a b) fs r =
   case r of
      (r1:rs) ->
         case format1 m fs rs of
            FDone ts fs1 ->
               case format1 a fs1 r1 of
                  FDone t1 fs2 -> FDone (t1++ts) fs2
                  fail@FFail{}-> fail
            fail@FFail{} -> fail
      [] -> FDone [] fs

format1 m@(AnyTill b) fs r = FDone r fs

format1 (Apply iso a) fs r =
   case unapply iso r of
      Just  j -> format1 a fs j
      Nothing -> FFail [] fs "unapply iso failed"

format1 (Count nr x) fs r = let
   rs = r
   n = length rs
   in case formatcount x fs rs of
         FDone ts fs1 ->
            case format1 nr fs1 n of
               FDone nt fs2 -> FDone (nt++ts) fs2

format1 Rest fs r = FDone r fs

{-}
format1 (Let names rule) fs r =
   case format1 rule (M.fromList (map (, toDyn "") names):fs) r of
      FDone out (f:fs2) -> FDone out fs2
      fail@FFail { }    -> fail
-}
format1 (Get name) fs r = FDone [] (myset1 name r fs)

format1 (Set name rule) fs r = format1 rule fs $ myget1 name fs

format1 (GetM names) fs r = FDone [] $ mysetm names r fs

format1 (SetM names rule) f r = format1 rule f $ mygetm names f

format1 (Build init rule) f r =
   case format1 rule r undefined of
      FDone t1 f2 -> FDone t1 f
      --FFail em t2 f2    -> FFail em t2 f2
{-
format1 (Redo name rule) fs r =
   case format1 rule fs r of
      FDone t fs1   -> 
         case myset1 name t fs1 of
            FDone t f2
      fail@FFail {} -> fail
-}

visitPost :: (forall r f t. Rule r f t -> Rule r f t) -> Rule r f t -> Rule r f t
visitPost f x = f $ visit (visitPost f) x

visitPre :: (forall r f t. Rule r f t -> Rule r f t) -> Rule r f t -> Rule r f t
visitPre f x = visit (visitPre f) (f x)

visit :: (forall r f t. Rule r f t -> Rule r f t) -> Rule r f t -> Rule r f t
visit f (Apply iso  x  ) = Apply iso  $ visit f x
visit f (Alt        xs ) = Alt        $ map (visit f) xs
visit f (Many       x  ) = Many       $ visit f x
visit f (ManyTill   x y) = ManyTill    (visit f x) (visit f y)
visit f (Set  name  x  ) = Set name   $ visit f x
visit f (SetM names x  ) = SetM names $ visit f x
visit f (And        xs ) = And        $ map (visit f) xs
visit f (Not        x  ) = Not        $ visit f x
visit f (        x :- y) = visit f x :- visit f y
visit f (Count      x y) = Count       (visit f x) (visit f y)
visit f (Name name  x  ) = Name name  $ visit f x
--visit f (Ignore     x  ) = Ignore     $ visit f x
visit f (Try        x  ) = Try        $ visit f x
visit f other            = other

doManyTill = visitPost doManyTill1

ll = AnyToken :- AnyToken :- AnyToken

doManyTill1 (Many AnyToken :- b) = AnyTill b :- b
doManyTill1 (Many a :- b) = ManyTill a b :- b
doManyTill1 (AnyTill a :- b) = AnyTill b :- b
doManyTill1 (ManyTill a b :- c) = ManyTill a c :- c
doManyTill1 other = other

nonEmpty :: Rule r f t -> Bool
nonEmpty (Alt      a  ) = all nonEmpty a
nonEmpty (      a :- b) = nonEmpty a || nonEmpty b
nonEmpty (Many     a  ) = False
nonEmpty (ManyTill a b) = False
nonEmpty (AnyTill  a  ) = False
nonEmpty (Token    a  ) = True
nonEmpty (String   a  ) = True
nonEmpty  AnyToken      = False
nonEmpty (Count    a b) = False
nonEmpty (Apply    a b) = nonEmpty b
nonEmpty (Name     a b) = nonEmpty b

--nonEmptyPrefix :: Rule r f t -> Rule s f t
--nonEmptyPrefix (a :- b) = if nonEmpty a then a else a :- nonEmptyPrefix b

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
filename = AnyTill (Token '.') :- Token '.' :- Many AnyToken

hostport = Build (HostPort "" 0) $ (Host <-- AnyTill (Token ':')) :- Token ':' :- (Port <-- int)

rint s = read s :: Int

isoint :: Iso String Int
isoint = total read show

int = Apply isoint $ Many (Range '0' '9')

t = parse1 filename (M.empty::SSMap) "abc.mp3"

t2 = format1 filename t

t3 = parse hostport "www.deathmetal.org:443"

t4 = parse1 int () "443"

fd f x = do d <- fromDynamic x; r <- f d; return $ toDyn r

isod (Iso f g) = Iso (fd f) (fd g)

totald :: (Typeable a, Typeable b) => (a -> b) -> (b -> a) -> Iso Dynamic Dynamic
totald f g = Iso (fd (Just . f)) (fd (Just . g))

infixr 3 <--
a <-- b = Set a b

infixr 2 :-
infixr 2 :/
infixr 2 ://

instance Show tok => Show (Rule res frame tok) where
   showsPrec d AnyToken r = "AnyToken"++r
   showsPrec d (Token t) r = "Token "++show t++r
   showsPrec d (Many m) r = "Many "++showsPrec 10 m r
   showsPrec d (ManyTill m t) r = "ManyTill "++showsPrec 10 m " "++showsPrec 10 t r
   showsPrec d (AnyTill t) r = "AnyTill "++showsPrec 10 t r
   showsPrec d (Alt as) r = "Alt"++concatMap (\a -> showsPrec 10 a "") as++r
   showsPrec d (a :- b) r = showsPrec 1 a " :- "++showsPrec 1 b r

data DataInfo = DataInfo { type1 :: String, con :: String, fields :: [DataField] }
data DataField = DataField { fname :: String, ftype :: String }

data TypeK = TypeK
data ConK = ConK
data FieldsK = FieldsK
data FNameK = FNameK
data FTypeK = FTypeK

instance Frame TypeK String DataInfo where
   myget1 TypeK = type1
   myset1 TypeK value frame = frame { type1 = value }

instance Frame ConK String DataInfo where
   myget1 ConK = con
   myset1 ConK value frame = frame { con = value }

instance Frame FieldsK [DataField] DataInfo where
   myget1 FieldsK = fields
   myset1 FieldsK value frame = frame { fields = value }

instance Frame FNameK String DataField where
   myget1 FNameK = fname
   myset1 FNameK value frame = frame { fname = value }

instance Frame FTypeK String DataField where
   myget1 FTypeK = ftype
   myset1 FTypeK value frame = frame { ftype = value }

dataP = Build (DataInfo "" "" []) $ String "data" :- sp :- 
   TypeK <-- ident :- sp :/ Token '=' :/ sp :/
   ConK  <-- ident :- sp :/ Token '{' :/ sp :/
   FieldsK <-- ManyTill (Build (DataField "" "") 
      (FNameK <-- ident :- sp :/ String "::" :/ sp :/
      FTypeK  <-- ident :- sp :/ Token   ',' :/ sp)) (Token '}') :// Token '}' :- sp

makeFields f = DataInfo (uppk $ type1 f) (uppk $ type1 f)

uppk (c:cs) = toUpper c : cs ++ "K"

sp = Many (Token ' ')

ident = Apply cons (alpha :- Many alnum)

alpha = Apply (satisfy (\c -> isAsciiUpper c || isAsciiLower c || c == '_')) AnyToken

alnum = Apply (satisfy (\c -> isAsciiUpper c || isAsciiLower c || c == '_' || isDigit c)) AnyToken

many1 p = Apply cons (p :- Many p)

low = map toLower

ilookup k = Iso
   (\xs -> do
      (_, v) <- find ((low k ==) . low . fst) xs
      let xs1 = filter ((low k /=) . low . fst) xs
      return (v, xs1))
   (\(v, xs) -> Just $ (k, v):filter ((low k /=) . low . fst) xs)


a = 9 :: Int

b = 10 :: Int

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