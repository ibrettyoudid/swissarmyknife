module Parser3Types where

import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Typeable
import Favs
import GHC.Stack
import Iso2 hiding (foldl, (!!))
import {-# SOURCE #-} HashDyn hiding (Apply, expr)
import NewTuple hiding (apply)

data Rule tok
   = Seq [Rule tok]
   | Then (Rule tok) (Rule tok)
   | Alt [Rule tok]
   | Name {name :: String, subrule :: Rule tok}
   | NameStub String
   | AnyToken
   | Token tok
   | Range tok tok
   | Many Int Int (Rule tok)
   | ManyTill Int Int (Rule tok) (Rule tok)
   | AnyTill Int Int (Rule tok)
   | Apply {iso :: Iso HashDyn HashDyn, subrule :: Rule tok}
   | Bind (Rule tok) (HashDyn -> Rule tok, HashDyn -> HashDyn) -- parsing Bind a (b,c), b turns the result of a into a new rule. printing, we have the result of b but not b itself. c turns the result of b back into b for printing
   | Return HashDyn
   | StrRes String HashDyn
   | Not (Rule tok)
   | And [Rule tok]
   | Set String (Rule tok)
   | Get String
   | Pos
   | Ignore (Rule tok)
   | Null

data IgnoreMe a = IgnoreMe a

data RuleR t r where
   BindR :: (Typeable a, Typeable b) => RuleR t a -> (a -> RuleR t b, b -> a) -> RuleR t b
   ApplyR :: (Typeable a, Typeable b) => Iso a b -> RuleR t a -> RuleR t b
   OptionR :: (Typeable a) => RuleR t a -> RuleR t (Maybe a)
   EithR :: (Typeable a, Typeable b) => RuleR t a -> RuleR t b -> RuleR t (Either a s)
   ThenR :: (Typeable a, Typeable b) => RuleR t a -> RuleR t b -> RuleR t (a :- b)
   (:/) :: (Typeable a, Typeable b) => RuleR t a -> RuleR t b -> RuleR t b
   (://) :: (Typeable a, Typeable b) => RuleR t a -> RuleR t b -> RuleR t a
   ManyTillR :: (Typeable a, Typeable b) => Int -> Int -> RuleR t a -> RuleR t b -> RuleR t [a]
   CountR :: (Typeable b) => RuleR t Int -> RuleR t b -> RuleR t [b]
   ManyR :: (Typeable a) => Int -> Int -> RuleR t a -> RuleR t [a]
   SeqR :: (Typeable a) => [RuleR t a] -> RuleR t [a]
   AltR :: (Typeable a) => [RuleR t a] -> RuleR t a
   AndR :: (Typeable a, Typeable b) => RuleR t a -> RuleR t b -> RuleR t (a :- b)
   NotR :: (Typeable a) => RuleR t a -> RuleR t a
   IgnoreR :: (Typeable a) => RuleR t a -> RuleR t (IgnoreMe a)
   AnyTillR :: (Typeable a) => Int -> Int -> RuleR t a -> RuleR t [t]
   TryR :: (Typeable a) => RuleR t a -> RuleR t a
   BuildR :: (Typeable a) => f -> RuleR t a -> RuleR t f
   LambdaR :: (Typeable a) => f -> RuleR t a -> RuleR t a
   CallR :: (Typeable a) => f -> RuleR t a -> RuleR t a
   NameR :: (Typeable a) => String -> RuleR t a -> RuleR t a
   NameStubR :: (Typeable a) => String -> RuleR t a
   PureR :: (Typeable a) => a -> RuleR t a
   TokenR :: (Typeable t, Eq t) => t -> RuleR t t
   RangeR :: (Typeable t) => t -> t -> RuleR t t
   AnyTokenR :: (Typeable t) => RuleR t t
   StringR :: (Typeable t) => [t] -> RuleR t [t]
   RestR :: (Typeable t) => RuleR t [t]
   PosR :: RuleR t (Int, Int)

-- GetR      :: Frame      n v f      =>       n              -> RuleR t v
-- SetR      :: Frame      n v f      =>       n              -> RuleR t v -> RuleR t v
-- GetMR     :: FrameTuple n v f      =>       n              -> RuleR t v
-- SetMR     :: FrameTuple n v f      =>       n              -> RuleR t v -> RuleR t v
-- RedoR     :: Frame  n [t] f        =>       n              -> RuleR t a -> RuleR t a

instance (Eq tok) => Eq (Rule tok) where
  --   Seqd  a b == Seqd c d = (a, b) == (c, d)
   Seq as == Seq bs = as == bs
   Alt as == Alt bs = as == bs
   Name a _ == Name b _ = a == b
   Token a == Token b = a == b
   Range a b == Range c d = (a, b) == (c, d)
   Not a == Not b = a == b
   Then a b == Then c d = (a, b) == (c, d)
   Many a b c == Many d e f = (a, b, c) == (d, e, f)
   ManyTill a b c d == ManyTill e f g h = (a, b, c, d) == (e, f, g, h)
   AnyTill a b c == AnyTill d e f = (a, b, c) == (d, e, f)
   Many a b c == Many d e f = (a, b, c) == (d, e, f)
   AnyToken == AnyToken = True
   Bind a b == Bind c d = a == c
   Apply a b == Apply c d = (a, b) == (c, d)
   _ == _ = False

instance (Ord tok) => Ord (Rule tok) where
  --   compare (Seqd a b) (Seqd c d) = compare (a, b) (c, d)
   compare (Seq a) (Seq b) = compare a b
   compare (Alt a) (Alt b) = compare a b
   compare (Name a _) (Name b _) = compare a b
   compare (Token a) (Token b) = compare a b
   compare (Range a b) (Range c d) = compare (a, b) (c, d)
   compare (Not a) (Not b) = compare a b
   compare AnyToken AnyToken = EQ
   compare (Bind a b) (Bind c d) = compare a c
   compare (Apply a b) (Apply c d) = compare (a, b) (c, d)
   compare (Many a b c) (Many d e f) = compare (c, a, b) (f, d, e)
   compare (ManyTill a b c d) (ManyTill e f g h) = compare (c, d, a, b) (g, h, e, f)
   compare (AnyTill a b c) (AnyTill d e f) = compare (c, a, b) (f, d, e)
   compare (Name {}) _ = LT
   compare (Seq {}) (Name {}) = GT
   compare (Seq {}) _ = LT
   compare (Alt {}) (Name {}) = GT
   compare (Alt {}) (Seq {}) = GT
   compare (Alt {}) _ = LT
   compare (Token {}) (Name {}) = GT
   compare (Token {}) (Seq {}) = GT
   compare (Token {}) (Alt {}) = GT
   compare (Token {}) _ = LT
   compare (Range {}) (Name {}) = GT
   compare (Range {}) (Seq {}) = GT
   compare (Range {}) (Alt {}) = GT
   compare (Range {}) (Token {}) = GT
   compare (Range {}) _ = LT
   compare (AnyToken {}) (Name {}) = GT
   compare (AnyToken {}) (Seq {}) = GT
   compare (AnyToken {}) (Alt {}) = GT
   compare (AnyToken {}) (Token {}) = GT
   compare (AnyToken {}) (Range {}) = GT
   compare (AnyToken {}) _ = LT
   compare (Apply {}) (Name {}) = GT
   compare (Apply {}) (Seq {}) = GT
   compare (Apply {}) (Alt {}) = GT
   compare (Apply {}) (Token {}) = GT
   compare (Apply {}) (Range {}) = GT
   compare (Apply {}) (AnyToken {}) = GT
   compare (Apply {}) _ = LT
   compare (Many {}) (Name {}) = GT
   compare (Many {}) (Seq {}) = GT
   compare (Many {}) (Alt {}) = GT
   compare (Many {}) (Token {}) = GT
   compare (Many {}) (Range {}) = GT
   compare (Many {}) (AnyToken {}) = GT
   compare (Many {}) (Apply {}) = GT
   compare (Many {}) _ = LT
   compare (Bind {}) (Name {}) = GT
   compare (Bind {}) (Seq {}) = GT
   compare (Bind {}) (Alt {}) = GT
   compare (Bind {}) (Token {}) = GT
   compare (Bind {}) (Range {}) = GT
   compare (Bind {}) (AnyToken {}) = GT
   compare (Bind {}) (Apply {}) = GT
   compare (Bind {}) (Many {}) = GT
   compare (Bind {}) _ = LT
   compare (Return {}) (Name {}) = GT
   compare (Return {}) (Seq {}) = GT
   compare (Return {}) (Alt {}) = GT
   compare (Return {}) (Token {}) = GT
   compare (Return {}) (Range {}) = GT
   compare (Return {}) (AnyToken {}) = GT
   compare (Return {}) (Apply {}) = GT
   compare (Return {}) (Many {}) = GT
   compare (Return {}) (Bind {}) = GT
   compare (Return {}) _ = LT
   compare (Not {}) _ = GT

--   compare (Range{}) _ = LT
{-
instance Show tok => Show (Rule tok) where
   --show = outershow Nothing
   show (Seq   as ) = unwords $ map show as
   show (Alt   as ) = unwords [intercalate " | " $ map show as]
   show (Name  a _) = unwords [a]
   show (Token a  ) = unwords [show a]
   show (Range a b) = unwords ["[" ++ show a ++ ".." ++ show b ++ "]"]
   show (Not     a) = "Not " ++ show a
   show (Apply a b) = "Apply " ++ show b
   show (Bind  a b) = "Bind " ++ show a
   show AnyToken    = "AnyToken"
   show (Many  a  ) = "Many " ++ show a
   show (Return  a) = "Return " ++ show a
-}

class PosItem s where
   from :: s t -> Int
   item :: s t -> Item t
   make :: Int -> Item t -> s t
   maket :: Int -> Int -> Item t -> s t

class (PosItem s) => PosItemTo s where
   to :: s t -> Int

instance PosItem PosItemF where
   from = fromf
   item = itemf
   make = PosItemF
   maket f t = PosItemF f

instance PosItem PosItemFT where
   from = fromt
   item = itemt
   make f = PosItemFT f f
   maket = PosItemFT

instance PosItemTo PosItemFT where
   to = tot

data PosItemF  tok = PosItemF  {fromf :: Int,             itemf :: Item tok} deriving (Eq, Ord)

data PosItemFT tok = PosItemFT {fromt :: Int, tot :: Int, itemt :: Item tok} deriving (Eq, Ord)

instance (Show z) => Show (PosItemFT z) where
   show (PosItemFT f t i) = show i ++ " " ++ unwords (map show [f, t])

instance (Show z) => Show (PosItemF z) where
   show (PosItemF f i) = show i ++ " " ++ show f

-- data Result = Running | Fail | Pass { asts :: [HashDyn] } deriving (Eq, Ord, Show)

data Item tok = Item {rule :: Rule tok, dot :: Dot} deriving (Eq)

instance (Ord tok) => Ord (Item tok) where
   compare a b = compare (rule a, dot a) (rule b, dot b)

data Dot
   = Pass { asts :: [HashDyn] }
   | Fail
   | Running
   | ISeq Int
   | IAlt Int -- Int is how many have finished
   | IName
   | IMany { masts :: [[HashDyn]] }
   | INot
   | IAnd
   | IGet
   | ISet
   | IApply
   deriving (Eq, Ord, Show)

pos2 :: (HasCallStack) => Dot -> Int
pos2 (ISeq n) = n
pos2 x = -100 --error $ show x

pos :: (HasCallStack) => Item tok -> Int
pos (Item r i) = pos2 i

pass (Pass _) = True
pass _ = False

finished (Pass _) = True
finished Fail = True
finished _ = False

passi = pass . dot

fini = finished . dot

type ItemSet tok = S.Set (Item tok)

puts a rest = a ++ rest

instance (Show tok) => Show (Rule tok) where
  -- show = outershow Nothing
  -- showsPrec p r =
   showsPrec :: (Show tok) => Int -> Rule tok -> ShowS
   showsPrec p r = case r of
      Seq      as -> showParen (p > 5) $ foldr (\x y -> showsPrec 5 x . showString "," . y) (showsPrec 5 $ last as) (init as)
      Alt      as -> showParen (p > 4) $ foldr (\x y -> showsPrec 4 x . showString "|" . y) (showsPrec 4 $ last as) (init as)
      AnyToken    -> showString "AnyToken"
      Token     a -> shows a
      Range   a b -> showString "[" . shows a . showString ".." . shows b . showString "]"
      Not       a -> showString "Not " . showsPrec 11 a
      Apply   a b -> showString "Apply " . shows a . showString " $ " . showsPrec 11 b
      Bind    a b -> showString "Bind " . showsPrec 11 a
      Return    a -> showString "Return " . shows a
      Many  a b c -> showString "Many " . shows a . showString ".." . shows b . showString " " . showsPrec 11 c
      ManyTill a b c d -> showString "ManyTill " . shows a . showString ".." . shows b . showString " " . showsPrec 11 c . showString " " . showsPrec 11 d
      AnyTill    a b c -> showString "AnyTill "  . shows a . showString ".." . shows b . showString " " . showsPrec 11 c
      Then    a b -> showParen (p > 6) $ showsPrec 6 a . showString " <*> " . showsPrec 6 b
      Pos         -> showString "Pos"
      Name    a b -> showString a


--foldl (showsPrec 5 a) [b, c, d, e]

--showsPrec 5 a . showString "," . showsPrec 5 b . showString "," . showsPrec 5 c

{-
showsPrec p (Seq   as ) = unwords $ map show as
showsPrec p (Alt   as ) = unwords [intercalate " | " $ map show as]
showsPrec p (Name  a _) = unwords [a]
showsPrec p (Token a  ) = unwords [show a]
showsPrec p (Range a b) = unwords ["[" ++ show a ++ ".." ++ show b ++ "]"]
showsPrec p (Not     a) = "Not " ++ show a
showsPrec p (Apply a b) = "Apply " ++ show b
showsPrec p (Bind  a b) = "Bind " ++ show a
showsPrec p AnyToken    = showString "AnyToken"
showsPrec p (Many  a  ) = ("Many " ++) show a
showsPrec p (Return  a) = "Return " ++ show a
-}


showSeq [] p = id
showSeq as p =
   let (ts, ns) = span isToken as
   in if length ts >= 2
         then maybeSep (show (map getToken ts)) "," . showSeq1 ns p
         else maybeSep (intercalate "," (map show ts)) "," . showSeq1 ns p

showSeq1 [] p = id
showSeq1 as p =
   let (ns, ts) = break isToken as
   in (intercalate "," (map show ns) ++) . showSeq ts p

maybeSep a x b = if null a || null b || isPrefixOf x b then a ++ b else a ++ x ++ b

isToken (Token _) = True
isToken _ = False

getToken (Token a) = a

show2 p prep = (show prep ++)

append = (++)

instance (Show tok) => Show (Item tok) where
   showsPrec p (Item rule i2) = append $ showItem rule i2 ++ " "

showPrec p i = showsPrec p i ""
-- showItem (Seq as) (ISeq n) = \p rest -> unwords $ insertIndex n "." $ map (\x -> showsPrec p x "") as
showItem (Seq as) (ISeq   n) = enclose True $ intercalate "," $ insertIndex n "o" $ map show as
showItem (Alt as) (IAlt   n) = enclose True $ (++ ' ' : show n) $ intercalate " | " $ map show as
showItem rule is = enclose True $ showPrec 1 rule ++ " " ++ show is

enclose flag str = if flag then "(" ++ str ++ ")" else str

enclose1 f outer inner = if outer >= inner then f 0 else f inner