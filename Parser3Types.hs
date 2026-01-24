module Parser3Types where

import {-# SOURCE #-} MHashDynamic3 hiding (Apply, expr)
import Iso2 hiding (foldl, (!!))
import NewTuple hiding (apply)
import Favs 

import Data.Map qualified as M
import Data.Set qualified as S
import Data.List

import Data.Typeable

import GHC.Stack

data Rule tok =
   Seq [Rule tok]
   | Then (Rule tok) (Rule tok)
   | Alt [Rule tok]
   | Name { name :: String, subrule ::Rule tok }
   | NameStub String
   | AnyToken
   | Token tok
   | Range tok tok
   | Many (Rule tok)
   | ManyTill (Rule tok) (Rule tok)
   | Apply { iso ::Iso Dynamic Dynamic, subrule::Rule tok }
   | Bind (Rule tok) (Dynamic -> Rule tok, Dynamic -> Dynamic) -- parsing, the first func turns the result of the nested rule into a new rule. printing, we have the result of the new rule but not the new rule itself. we need to use the nested rule and the result to recreate it
   | Return Dynamic
   | StrRes String Dynamic
   | Not (Rule tok)
   | And [Rule tok]
   | Set String (Rule tok)
   | Get String
   | Pos
   | Ignore (Rule tok)

data IgnoreMe a = IgnoreMe a

data RuleR t r where
   BindR     :: (Typeable a, Typeable b)                        =>  RuleR t a   -> (a -> RuleR t b, b -> a) -> RuleR t b
   ApplyR    :: (Typeable a, Typeable b) => Iso a b             ->  RuleR t a                 -> RuleR t b
   OptionR   :: (Typeable a, Typeable (Maybe a))                =>  RuleR t a                 -> RuleR t (Maybe a)
   EithR     :: (Typeable a, Typeable b, Typeable (Either a b)) =>  RuleR t a   -> RuleR t b  -> RuleR t (Either a s)
   ThenR     :: (Typeable a, Typeable b, Typeable (a :- b))     =>  RuleR t a   -> RuleR t b  -> RuleR t (a :- b)
   (:/)      :: (Typeable a, Typeable b)                        =>  RuleR t a   -> RuleR t b  -> RuleR t  b
   (://)     :: (Typeable a, Typeable b)                        =>  RuleR t a   -> RuleR t b  -> RuleR t  a
   ManyTillR :: (Typeable a, Typeable b)                        =>  RuleR t a   -> RuleR t b  -> RuleR t [a]
   CountR    :: Typeable b                                      =>  RuleR t Int -> RuleR t b  -> RuleR t [b]
   ManyR     :: Typeable a                                      =>  RuleR t a                 -> RuleR t [a]
   SeqR      :: Typeable a                                      => [RuleR t a]                -> RuleR t [a]
   AltR      :: Typeable a                                      => [RuleR t a]                -> RuleR t a
   AndR      :: (Typeable a, Typeable b)                        =>  RuleR t a   -> RuleR t b  -> RuleR t (a :- b)
   NotR      :: Typeable a                                      =>  RuleR t a                 -> RuleR t a
   IgnoreR   :: Typeable a                                      =>  RuleR t a                 -> RuleR t (IgnoreMe a)
   AnyTillR  :: Typeable a                                      =>  RuleR t a                 -> RuleR t [t]
   TryR      :: Typeable a                                      =>  RuleR t a                 -> RuleR t a
   BuildR    :: Typeable a                      =>  f           ->  RuleR t a                 -> RuleR t f
   LambdaR   :: Typeable a                      =>  f           ->  RuleR t a                 -> RuleR t a
   CallR     :: Typeable a                      =>  f           ->  RuleR t a                 -> RuleR t a
   NameR     :: Typeable a                      =>  String      ->  RuleR t a                 -> RuleR t a
   NameStubR :: Typeable a                      =>  String      ->  RuleR t a
   PureR     :: Typeable a                      =>  a           ->  RuleR t a
   TokenR    :: (Typeable t, Eq t)              =>  t           ->  RuleR t t
   RangeR    :: Typeable t                      =>  t ->  t     ->  RuleR t t
   AnyTokenR :: Typeable t                                      =>  RuleR t t
   StringR   :: Typeable t                      =>  [t]         ->  RuleR t [t]
   RestR     :: Typeable [t]                                    =>  RuleR t [t]
   PosR      ::                                                     RuleR t (Int, Int)
   --GetR      :: Frame      n v f      =>       n              -> RuleR t v
   --SetR      :: Frame      n v f      =>       n              -> RuleR t v -> RuleR t v
   --GetMR     :: FrameTuple n v f      =>       n              -> RuleR t v
   --SetMR     :: FrameTuple n v f      =>       n              -> RuleR t v -> RuleR t v 
   --RedoR     :: Frame  n [t] f        =>       n              -> RuleR t a -> RuleR t a

instance Eq tok => Eq (Rule tok) where
--   Seqd  a b == Seqd c d = (a, b) == (c, d)
   Seq   as  == Seq   bs  = as     == bs
   Alt   as  == Alt   bs  = as     == bs
   Name  a _ == Name  b _ = a      == b
   Token a   == Token b   = a      == b
   Range a b == Range c d = (a, b) == (c, d)
   Not   a   == Not   b   = a      == b
   Then  a b == Then  c d = (a, b) == (c, d)
   Many  a   == Many  b   = a      == b
   AnyToken  == AnyToken  = True
   Bind  a b == Bind  c d = a      == c
   Apply a b == Apply c d = (a, b) == (c, d)
   _ == _ = False

instance Ord tok => Ord (Rule tok) where
--   compare (Seqd a b) (Seqd c d) = compare (a, b) (c, d)
   compare (Seq       a) (Seq     b) = compare a b
   compare (Alt       a) (Alt     b) = compare a b
   compare (Name    a _) (Name  b _) = compare a b
   compare (Token   a  ) (Token b  ) = compare a b
   compare (Range   a b) (Range c d) = compare (a, b) (c, d)
   compare (Not       a) (Not     b) = compare a b
   compare  AnyToken      AnyToken   = EQ
   compare (Bind    a b) (Bind  c d) = compare a c
   compare (Apply   a b) (Apply c d) = compare (a, b) (c, d)
   compare (Name     {}) _             = LT
   compare (Seq      {}) (Name     {}) = GT
   compare (Seq      {}) _             = LT
   compare (Alt      {}) (Name     {}) = GT
   compare (Alt      {}) (Seq      {}) = GT
   compare (Alt      {}) _             = LT
   compare (Token    {}) (Name     {}) = GT
   compare (Token    {}) (Seq      {}) = GT
   compare (Token    {}) (Alt      {}) = GT
   compare (Token    {}) _             = LT
   compare (Range    {}) (Name     {}) = GT
   compare (Range    {}) (Seq      {}) = GT
   compare (Range    {}) (Alt      {}) = GT
   compare (Range    {}) (Token    {}) = GT
   compare (Range    {}) _             = LT
   compare (AnyToken {}) (Name     {}) = GT
   compare (AnyToken {}) (Seq      {}) = GT
   compare (AnyToken {}) (Alt      {}) = GT
   compare (AnyToken {}) (Token    {}) = GT
   compare (AnyToken {}) (Range    {}) = GT
   compare (AnyToken {}) _             = LT
   compare (Apply    {}) (Name     {}) = GT
   compare (Apply    {}) (Seq      {}) = GT
   compare (Apply    {}) (Alt      {}) = GT
   compare (Apply    {}) (Token    {}) = GT
   compare (Apply    {}) (Range    {}) = GT
   compare (Apply    {}) (AnyToken {}) = GT
   compare (Apply    {}) _             = LT
   compare (Many     {}) (Name     {}) = GT
   compare (Many     {}) (Seq      {}) = GT
   compare (Many     {}) (Alt      {}) = GT
   compare (Many     {}) (Token    {}) = GT
   compare (Many     {}) (Range    {}) = GT
   compare (Many     {}) (AnyToken {}) = GT
   compare (Many     {}) (Apply    {}) = GT
   compare (Many     {}) _             = LT
   compare (Bind     {}) (Name     {}) = GT
   compare (Bind     {}) (Seq      {}) = GT
   compare (Bind     {}) (Alt      {}) = GT
   compare (Bind     {}) (Token    {}) = GT
   compare (Bind     {}) (Range    {}) = GT
   compare (Bind     {}) (AnyToken {}) = GT
   compare (Bind     {}) (Apply    {}) = GT
   compare (Bind     {}) (Many     {}) = GT
   compare (Bind     {}) _             = LT
   compare (Return   {}) (Name     {}) = GT
   compare (Return   {}) (Seq      {}) = GT
   compare (Return   {}) (Alt      {}) = GT
   compare (Return   {}) (Token    {}) = GT
   compare (Return   {}) (Range    {}) = GT
   compare (Return   {}) (AnyToken {}) = GT
   compare (Return   {}) (Apply    {}) = GT
   compare (Return   {}) (Many     {}) = GT
   compare (Return   {}) (Bind     {}) = GT
   compare (Return   {}) _             = LT
   compare (Not      {}) _             = GT
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

class State s where
   from  :: s t -> Int
   item  :: s t -> Item t
   make  :: Int -> Item t -> s t
   maket :: Int -> Int -> Item t -> s t

class State s => StateTo s where
   to    :: s t -> Int

instance State StateF where
   from = fromf
   item = itemf
   make = StateF
   maket f t = StateF f

instance State StateFT where
   from = fromt
   item = itemt
   make f = StateFT f f
   maket = StateFT

instance StateTo StateFT where
   to   = tot


data StateF  tok = StateF  { fromf :: Int,             itemf :: Item tok } deriving (Eq, Ord)

data StateFT tok = StateFT { fromt :: Int, tot :: Int, itemt :: Item tok } deriving (Eq, Ord)

instance Show z => Show (StateFT z) where
   show (StateFT f t i) = show i ++ " " ++ unwords (map show [f, t])

instance Show z => Show (StateF z) where
   show (StateF f i) = show i ++ " " ++ show f

--data Result = Running | Fail | Pass { asts :: [Dynamic] } deriving (Eq, Ord, Show)

data Item tok  = Item { rule :: Rule tok, istate :: IState } deriving (Eq)

instance Ord tok => Ord (Item tok) where
   compare a b = compare (rule a, istate a) (rule b, istate b)

data IState    = Pass
               | Fail
               | Running 
               | ISeq  Int
               | IAlt  Int -- Int is how many have finished
               | IName
               | ITerm   --used for all terminal level ops
               | IMany [Dynamic]
               | INot
               | IAnd
               | IGet
               | ISet
               | IApply
               deriving (Eq, Ord, Show)

pos2 :: HasCallStack => IState -> Int
pos2 (ISeq n) = n
pos2 (IAlt n) = n
pos2 x = error $ show x

pos :: HasCallStack => Item tok -> Int
pos (Item r i) = pos2 i

pass Pass = True
pass _    = False

finished Pass = True
finished Fail = True
finished _    = False

passi = pass . istate

fini = finished . istate

type ItemSet tok = S.Set (Item tok)

puts a rest = a ++ rest

instance Show tok => Show (Rule tok) where
   --show = outershow Nothing
   --showsPrec p r = 
   showsPrec p r = showRule r p
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

showRule (Seq   as ) = enclose2 (showSeq as) 3
showRule (Alt   as ) = enclose1 (intercalate " | ") as 2
showRule (Name  a _) = \p -> (a++)
showRule (Token a  ) = \p -> (show a++)
showRule (Range a b) = \p -> (unwords ["[" ++ show a ++ ".." ++ show b ++ "]"]++)
showRule (Not     a) = enclose2 (("Not "   ++) .: showRule a) 7
showRule (Bind  a b) = enclose2 (("Bind "  ++) .: showRule a) 6
showRule (Apply a b) = enclose2 ((("Apply "++show a++" ") ++) .: showRule b) 5
showRule (Many  a  ) = enclose2 (("Many "  ++) .: showRule a) 4
showRule (Return  a) = enclose2 (("Return "++) .: show2    a) 1
showRule AnyToken    = \p -> showString "AnyToken"

showSeq [] p = id
showSeq as p = let
   (ts, ns) = span isToken as

   in if length ts >= 2
         then maybeSep (show (map getToken ts)       ) "," . showSeq1 ns p
         else maybeSep (intercalate "," (map show ts)) "," . showSeq1 ns p

showSeq1 [] p = id
showSeq1 as p = let
   (ns, ts) = break isToken as

   in (intercalate "," (map show ns) ++) . showSeq ts p

maybeSep a x b = if null a || null b || isPrefixOf x b then a++b else a++x++b

isToken (Token _) = True
isToken _         = False

getToken (Token a) = a

show2 p prep = (show prep ++)

instance Show tok => Show (Item tok) where
   showsPrec p (Item rule i2) = showItem rule i2 p . (' ':)

--showItem (Seq as) (ISeq n) = \p rest -> unwords $ insertIndex n "." $ map (\x -> showsPrec p x "") as
showItem (Seq as) (ISeq n) = enclose1 (intercalate "," . insertIndex n "o") as 3
showItem (Alt as) (IAlt n) = enclose1 ((++ ' ':show n) . intercalate " | ") as 2
showItem (Many a) (IMany as) = (("Many "++show a++"=") ++) .: enclose1 (intercalate ",") as 0

showItem rule Running = enclose2 (`showsPrec` rule) 1

enclose flag str = if flag then (("("++str++")")++) else (str ++)

--enclose1 f as n p = enclose (p > n) $ f $ map (\x -> showsPrec p x "") as
enclose1 f as n p = enclose (p >= n) $ f $ map (\x -> showsPrec p x "") as
--enclose1 f as = enclose2 (\p -> (f $ map (\x -> showsPrec p x "") as)++)


enclose2 :: (Ord t, Num t) => (t -> [Char] -> [Char]) -> t -> t -> [Char] -> [Char]
enclose2 f n p rest = if p >= n then '(':f 0 (')':rest) else f n rest
--enclose2 f n p rest = if p > n then '(':f 0 (')':rest) else f n rest
--enclose2 f n p rest = enclose (p > n) ('(':f 0 (')':rest)) (f n)

