{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE MultiWayIf #-}

module Parser4 where

import Iso
import MHashDynamic2 hiding (Apply, Frame, Let)
import Data.Word
import Data.Char
import Data.Map qualified as M
import Data.List
import Data.Maybe

newtype Ig a = Ig a

data Lens a b
   = Lens (a -> b) (b -> a -> a)

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

newtype Pos = Pos { fromPos :: Int }
            deriving (Eq, Ord, Show, Num)

-- | The result of a parse1.  This is parameterised over the type @i@
-- of string that was processed.
--
-- This type is an instance of 'Functor', where 'fmap' transforms the
-- value in a 'Done' result.
data IResult r f i
   = Fail String [f] [i]
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
   | Done r [f] [i]
    -- ^ The parse1 succeeded.  The @i@ parameter is the input that had
    -- not yet been consumed (if any) when the parse1 succeeded.
      deriving Show

data FResult i f
   = FDone [i] f
   | FFail [i] f String
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

class Frame name value frame | frame -> value where
   myget1 :: name -> frame -> Maybe value
   myset1 :: name -> value -> frame -> frame

instance Ord a => Frame a b (M.Map a b) where
   myget1 = M.lookup
   myset1 = M.insert

parse rule str = parse1 rule [] str id

format rule r = format1 rule [] r

parse1 :: (Ord name, Show name, Typeable name) => Rule name Dynamic Char -> [M.Map name Dynamic] -> [Char] -> (IResult Dynamic (M.Map name Dynamic) Char -> IResult Dynamic (M.Map name Dynamic) Char) -> IResult Dynamic (M.Map name Dynamic) Char
parse1 AnyToken fs []     c = c $ Fail "Expecting any token but got end of input" fs []
parse1 AnyToken fs (i:is) c = c $ Done (toDyn i) fs is

parse1 (Token tok) fs []     c = c $ Fail ("Expecting token "++show tok) fs []
parse1 (Token tok) fs (i:is) c =
   if i == tok
      then Done (toDyn i) fs is
      else Fail ("Expecting token "++show tok) fs (i:is)

parse1 r@(Range from to) fs i c =
   case i of
      []      -> Fail ("EOF when expecting "++show r) fs i
      (i1:is) ->
         if from <= i1 && i1 <= to
            then Done (toDyn i1) fs is
            else Fail ("expecting "++show r) fs i

parse1 (String str) fs i c =
   case stripPrefix str i of
      Just  j -> Done (toDyn str) fs j
      Nothing -> Fail ("Expecting string "++str) fs i

parse1 (Then a b) fs i c =
   case parse1 a fs i c of
      Done ar fs1 i1 ->
         case parse1 b fs1 i1 c of
            Done br fs2 i2 -> Done (toDyn (ar, br)) fs2 i2
            fail@Fail {}   -> fail
      fail@Fail {} -> fail

parse1 (Seq as) fs i c =
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

parse1 (Alt as) fs i c =
   case as of
      (a:as1) ->
         case parse1 a fs i c of
            Done ar1 fs1 i1 -> Done ar1 fs1 i1
            fail@Fail {} -> parse1 (Alt as1) fs i c
      [] -> Fail i fs "no alternatives match"

parse1 m@(Many a) fs i c =
   case parse1 a fs i c of
      Done ar fs1 i1 ->
         case parse1 m fs1 i1 c of
            Done mr fs2 i2 -> Done (toDyn (ar:fromDyn1 mr)) fs2 i2
            fail@Fail {}   -> fail
      Fail em fs1 i1 -> Done (toDyn ([] :: [Dynamic])) fs i

parse1 m@(ManyTill a b) fs i c =
   case parse1 b fs i c of
      Done br fs1 i1 -> Done (toDyn ([] :: [Dynamic])) fs i
      Fail em fs1 i1 ->
         case parse1 a fs i c of
            Done ar fs2 i2 ->
               case parse1 m fs2 i2 c of
                  Done mr fs3 i3 -> Done (toDyn (ar:fromDyn1 mr)) fs3 i3
                  fail@Fail {}   -> fail
            Fail em fs2 i2 -> Done (toDyn ([] :: [Dynamic])) fs i

parse1 m@(AnyTill b) fs i c =
   case parse1 b fs i c of
      Done br fs1 i1 -> Done (toDyn ([] :: String)) fs i
      Fail em fs1 i1 ->
         case i of
            (ar:i2) ->
               case parse1 m fs1 i2 c of
                  Done mr fs3 i3 -> Done (toDyn (ar:fromDyn1 mr)) fs3 i3
                  fail@Fail {}   -> fail
            [] -> Done (toDyn ([] :: [Dynamic])) fs i

parse1 (Apply iso a) fs i c =
   case parse1 a fs i c of
      Done ar fs1 i1 ->
         case apply iso ar of
            Just  j -> Done j fs1 i1
            Nothing -> Fail "apply failed" fs i
      fail@Fail {} -> fail

parse1 (Count n x) fs i c =
   case parse1 n fs i c of
      Done nr fs1 i1 -> parse1count x fs i $ fromDyn1 nr
      fail@Fail {}   -> fail

parse1 (Let names rule) fs i c =
   case parse1 rule (M.fromList (map (, toDyn "") names):fs) i c of
      Done r (f:fs1) i1 -> Done r fs1 i1
      fail@Fail { }     -> fail

parse1 (Set name rule) fs i c =
   case parse1 rule fs i c of
      Done r fs i1 -> Done r (myset name r fs) i1
      Fail m fs i1 -> Fail m fs i1

parse1 (Get name) fs i c = Done (myget fs name) fs i

parse1 (SetM names rule) fs i c =
   case parse1 rule fs i c of
      Done r fs i1 -> Done r (Data.List.foldr (uncurry myset) fs (zip names $ fromDyn1 r)) i1
      Fail m fs i1 -> Fail m fs i1

parse1 (GetM names) fs i c = Done (toDyn $ map (myget fs) names) fs i

parse1 (Build rule) fs i c =
   case parse1 rule (M.empty:fs) i c of
      Done r (f:fs1) i1 -> Done (toDyn f) fs1 i1
      fail@Fail { }     -> fail

parse1 (Redo name rule) fs i c = parse1 rule fs (fromDyn1 $ myget fs name) c

parse1 Rest fs i c = Done (toDyn i) fs []

parse1 (Ignore a) fs i c = parse1 a fs i c

parse1 a b c d = error $ show (a, b, c)

parse1count :: (Show t, Ord t, Typeable t) => Rule t Dynamic Char -> [M.Map t Dynamic] -> [Char] -> Int -> IResult Dynamic (M.Map t Dynamic) Char
parse1count x fs i 0 = Done (toDyn ([] :: [Dynamic])) fs i
parse1count x fs i n =
   case parse1 x fs i id of
      Done xr fs1 i1 ->
         case parse1count x fs1 i1 (n-1) of
            Done xsr fs2 i2 -> Done (toDyn $ xr:fromDyn1 xsr) fs2 i2

--formatcount (r:rs) fs x 0 = FDone [] fs
formatcount x fs []     = FDone [] fs
formatcount x fs (r:rs) =
   case formatcount x fs rs of
      FDone ts fs1 ->
         case format1 x fs1 r of
            FDone t fs2 -> FDone (t++ts) fs2
            fail@FFail{}-> fail
      fail@FFail{}-> fail

format1 AnyToken fs r = FDone [fromDyn1 r] fs

format1 (Token tok) fs r = FDone [tok] fs

format1 (String str) fs r = FDone str fs

format1 (Then a b) fs r =
   case fromDynamic r of
      Just (ar, br) ->
         case format1 a fs ar of
            FDone at fs1 ->
               case format1 b fs1 br of
                  FDone bt fs2 -> FDone (at++bt) fs2
                  fail@FFail{} -> fail
            fail@FFail{} -> fail
      Nothing -> FFail [] fs "pattern match failed"

format1 (Seq a) fs r =
   case a of
      (a1:as1) ->
         case fromDyn1 r of
            (r1:rs) ->
               case format1 (Seq as1) fs (toDyn rs) of
                  FDone t1 fs1 ->
                     case format1 a1 fs1 r1 of
                        FDone t2 fs2 -> FDone (t2++t1) fs2
                        fail@FFail {}-> fail
                  fail@FFail {} -> fail
            [] -> FFail [] fs "ran out of results to format1 in Seq"
      [] ->
         case fromDyn1 r of
            ([] :: [Dynamic]) -> FDone [] fs
            (r1:rs) -> FFail [] fs "too many results to format1 in Seq"

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
{-
format1 r fs m@(Many a) =
   case format1 r fs a of
      Done i1 fs1 ar ->
         case parse1 i1 fs1 m of
            Done i2 fs2 mr -> Done i2 fs2 $ toDyn (ar:fromDyn1 mr)
            fail@Fail {}   -> fail
      Fail i1 fs1 em -> Done i1 fs1 $ toDyn ([] :: [Dynamic])

format1 i fs m@(ManyTill a b) =
   case parse1 i fs b of
      Done i1 fs1 br -> Done i1 fs1 $ toDyn ([] :: [Dynamic])
      Fail i1 fs1 em ->
         case parse1 i fs a of
            Done i2 fs2 ar ->
               case parse1 i2 fs2 m of
                  Done i3 fs3 mr -> Done i3 fs3 $ toDyn (ar:fromDyn1 mr)
                  fail@Fail {}   -> fail
-}

format1 m@(Many a) fs r =
   case fromDyn1 r :: [Dynamic] of
      (r1:rs) ->
         case format1 m fs (toDyn rs) of
            FDone ts fs1 ->
               case format1 a fs1 r1 of
                  FDone t1 fs2 -> FDone (t1++ts) fs2
                  fail@FFail{}-> fail
            fail@FFail{} -> fail
      [] -> FDone [] fs

format1 (Apply iso a) fs r =
   case unapply iso r of
      Just  j -> format1 a fs j
      Nothing -> FFail [] fs "unapply iso failed"

format1 (Count nr x) fs r = let
   rs = fromDyn1 r :: [Dynamic]
   n = length rs
   in case formatcount x fs rs of
         FDone ts fs1 ->
            case format1 nr fs1 (toDyn n) of
               FDone nt fs2 -> FDone (nt++ts) fs2

format1 (Let names rule) fs r =
   case format1 rule (M.fromList (map (, toDyn "") names):fs) r of
      FDone out (f:fs2) -> FDone out fs2
      fail@FFail { }    -> fail

format1 (Get name) fs r = FDone [] $ myset name r fs

format1 (Set name rule) fs r = format1 rule fs (myget fs name)

format1 (GetM names) fs r = FDone [] $ Data.List.foldr (uncurry myset) fs (zip names $ fromDyn1 r)

format1 (SetM names rule) fs r = format1 rule fs (toDyn $ map (myget fs) names)

format1 (Build rule) fs r =
   case format1 rule (fromDyn1 r:fs) r of
      FDone t1 (f:fs1) -> FDone t1 fs1
      fail@FFail {}    -> fail

format1 (Redo name rule) fs r =
   case format1 rule fs r of
      FDone t fs1   -> FDone t (myset name (toDyn t) fs)
      fail@FFail {} -> fail

format1 Rest fs r = FDone (fromDyn1 r) fs

myset2 name value [] = Nothing
myset2 name value (f:frames) =
   case myget1 name f of
      Just  j -> Just $ myset1 name value f:frames
      Nothing -> (f:) Prelude.<$> myset2 name value frames

myset name value (f:frames) = fromMaybe (myset1 name value f:frames) (myset2 name value (f:frames))

myget []         name = error $ "failed to get variable "++show name
myget (f:frames) name =
   case myget1 name f of
      Just  j -> j
      Nothing -> myget frames name

visitPost f x = f $ visit (visitPost f) x

visitPre f x = visit (visitPre f) (f x)

visit f (Apply iso  x  ) = Apply iso  $ visit f x
visit f (Seq        xs ) = Seq        $ map (visit f) xs
visit f (Alt        xs ) = Alt        $ map (visit f) xs
visit f (Many       x  ) = Many       $ visit f x
visit f (ManyTill   x y) = ManyTill    (visit f x) (visit f y)
visit f (Let  names x  ) = Let names  $ visit f x
visit f (Set  name  x  ) = Set name   $ visit f x
visit f (SetM names x  ) = SetM names $ visit f x
visit f (And        xs ) = And        $ map (visit f) xs
visit f (Not        x  ) = Not        $ visit f x
visit f (Then       x y) = Then        (visit f x) (visit f y)
visit f (Count      x y) = Count       (visit f x) (visit f y)
visit f (Name name  x  ) = Name name  $ visit f x
visit f (Ignore     x  ) = Ignore     $ visit f x
visit f (Try        x  ) = Try        $ visit f x
visit f other            = other

doManyTill = visitPost doManyTill1

doManyTill1 (Seq as) = Seq $ doManyTill2 as
doManyTill1 other = other

doManyTill2 (Many a:b:cs) = ManyTill a b:doManyTill2 (b:cs)
doManyTill2 (a:bs) = a:doManyTill2 bs
doManyTill2 [] = []

doString (String cs) = Seq $ map Token $ charsOfStr cs

strOfChars :: [Dynamic] -> String
strOfChars = map fromDyn1

charsOfStr :: String -> [Dynamic]
charsOfStr = map toDyn

istr = totald strOfChars charsOfStr

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


filename :: Rule String Dynamic Char
filename = doManyTill $ Seq [Many AnyToken, Token '.', Many AnyToken]

t = parse filename "abc.mp3"

fd :: (Typeable a, Typeable b) => (a -> Maybe b) -> Dynamic -> Maybe Dynamic
fd f x = fmap toDyn $ f $ fromDyn1 x

isod :: (Typeable a1, Typeable a2) => (a1 -> Maybe a2) -> (a2 -> Maybe a1) -> Iso Dynamic Dynamic
isod f g = Iso (fd f) (fd g)

totald :: (Typeable a, Typeable b) => (a -> b) -> (b -> a) -> Iso Dynamic Dynamic
totald f g = Iso (fd (Just . f)) (fd (Just . g))

a <-- b = Set a b