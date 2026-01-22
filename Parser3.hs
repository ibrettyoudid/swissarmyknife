{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{- HLINT ignore "Use tuple-section" -}

module Parser3 where

import Favs hiding (indent1, indent2)
import qualified MyPretty2
import {-# SOURCE #-} MHashDynamic2 hiding (Apply, expr)
import NewTuple hiding (apply)
import qualified SetList as SL
import Iso2 hiding (foldl, (!!))
import qualified Iso2
--import Rule
--import Syntax3 hiding (foldl, foldr)
import Shell (ch)

import Control.Monad
import Control.Monad.State qualified as St
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.IntMap qualified as I
import Data.Typeable

import Debug.Trace

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
   | Bind (Rule tok) (Dynamic -> Rule tok, Dynamic -> Dynamic) -- parsing, the iso turns the result of the nested rule into a new rule. printing, we have the result of the new rule but not the new rule itself. we need to use the nested rule and the result to recreate it
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

infix 2 <=>
infixl 3 <|>

-- infixl 4 <+>
infixr 5 >$<
infixr 6 >*<
infixr 6 >*
infixr 6 *<

(>>==) :: (Typeable a, Typeable b) => RuleR t a -> (a -> RuleR t b, b -> a) -> RuleR t b
(>>==) = BindR

(>*<) :: (Typeable a, Typeable b) => RuleR t a -> RuleR t b -> RuleR t (a :- b)
(>*<) = ThenR

(*<) :: (Typeable a, Typeable b) => RuleR t a -> RuleR t b -> RuleR t b
(*<) = (:/)

(>*) :: (Typeable a, Typeable b) => RuleR t a -> RuleR t b -> RuleR t a
(>*) = (://)

{-
SeqR a >*< SeqR b = SeqR (a ++ b)
SeqR a >*<      b = SeqR (a ++ [b])
a      >*< SeqR b = SeqR (a:b)
a      >*<      b = SeqR [a, b]
-}

AltR a <|> AltR b = AltR (a ++ b)
AltR a <|>      b = AltR (a ++ [b])
a      <|> AltR b = AltR (a:b)
a      <|>      b = AltR [a, b]

(>$<) :: (Typeable a, Typeable b) => Iso a b -> RuleR t a -> RuleR t b
(>$<) = ApplyR

(<=>):: Typeable a => String -> RuleR t a -> RuleR t a
(<=>) = NameR

token :: (Typeable t, Eq t) => t -> RuleR t t
token = TokenR

text xs = SeqR $ map token xs

anytoken :: Typeable t => RuleR t t
anytoken = AnyTokenR

pure :: Typeable a => a -> RuleR t a
pure = PureR

many :: Typeable a => RuleR t a -> RuleR t [a]
many = ManyR

many1 :: Typeable a => RuleR t a -> RuleR t [a]
many1 p = icons >$< p >*< many p

sepBy x sep = Parser3.pure []
         <|> icons >$< x >*< many (sep *< x)

lc = PosR

groupOf i = sepBy i (text ";") -- <|> aligned i

aligned i = ialign >$< many1 (whiteSpace *< PosR >*< i)

ialign :: Iso [(Int, Int) :- c] [(Int, Int) :- c]
ialign = Iso "ialign" f g
   where
      f ((p :- i):is) = ifJust (all (\(p1 :- i1) -> snd p == snd p1) is) ((p :- i):is)
      g x = Just x


whiteSpace = token ' ' <|> token '\n'

chainl1 arg op f = Iso2.ifoldl f >$< (arg >*< (many (op >*< arg)))

chainr1 arg op f = f >$< arg >*< (op >*< chainr1 arg op f) <|> arg

{-
aligned (ParserIO i) = do
   ((_, l, c):_) <- ParserIO $ lift get
   many $ ParserIO $ do
   ((_::String, l1::Int, c1::Int):_) <- lift get
   guard $ l1 > l && c == c1
   i
-}
translate :: Typeable a => RuleR t a -> Rule t
translate (AltR as) = Alt $ map translate as
translate (SeqR as) = Seq $ map translate as
translate (ThenR a b) = Seq [translate a, translate b]
translate (a :/  b) = Apply ifst $ Seq [translate a, translate b]
translate (a :// b) = Apply isnd $ Seq [translate a, translate b]
translate (ApplyR a b) = Apply (isod a) $ translate b
translate (NameR a b) = Name a $ translate b
translate (NameStubR a) = NameStub a
translate (BindR a (b, c)) = Bind (translate a) (\a -> translate (b $ fromDyn1 a), \b -> toDyn $ c $ fromDyn1 b)
translate (TokenR a) = Token a
translate AnyTokenR = AnyToken
translate (RangeR a b) = Range a b
translate (ManyR a) = Many $ translate a
translate (PureR a) = Return $ toDyn a
translate PosR = Pos
--translate (OptionR a) = Option $ translate a
--translate (EithR a b) = Eith (translate a) (translate b)
--translate (CountR n a) = Count (translate n) (translate a)
translate (ManyTillR a b) = ManyTill (translate a) (translate b)
translate (AndR a b) = error "AND" --And (translate a) (translate b)
translate (NotR a) = Not (translate a)
translate (IgnoreR a) = Ignore (translate a)
--translate other = error $ show $ (unsafeCoerce other :: RuleR
translate (AnyTillR a) = error "ANYTILL"
translate (TryR a) = error "TRY"
translate (StringR a) = Return $ toDyn a
translate RestR = error "REST"
translate other = error "BAD"
{-
   EithR     :: (Typeable a, Typeable b, Typeable (Either a b)) =>  RuleR t a   -> RuleR t b  -> RuleR t (Either a s)
   CountR    :: Typeable b                                      =>  RuleR t Int -> RuleR t b  -> RuleR t [b]
   ManyTillR :: Typeable a                                      =>  RuleR t a   -> RuleR t b  -> RuleR t [a]
   AndR      :: Typeable a                                      => [RuleR t a]                -> RuleR t a
   NotR      :: Typeable a                                      =>  RuleR t a                 -> RuleR t a
   IgnoreR   :: Typeable a                                      =>  RuleR t a                 -> RuleR t (IgnoreMe a)
   AnyTillR  :: Typeable a                                      =>  RuleR t a                 -> RuleR t [t]
   TryR      :: Typeable a                                      =>  RuleR t a                 -> RuleR t a
   BuildR    :: Typeable a                      =>  f           ->  RuleR t a                 -> RuleR t f
   LambdaR   :: Typeable a                      =>  f           ->  RuleR t a                 -> RuleR t a
   CallR     :: Typeable a                      =>  f           ->  RuleR t a                 -> RuleR t a
   NameStubR :: Typeable a                      =>  String      ->  RuleR t a
   StringR   :: Typeable t                      =>  [t]         ->  RuleR t [t]
   RestR     :: Typeable [t]                                    =>  RuleR t [t]
-}
{-
translate :: Typeable a => RuleR t a -> Rule t
translate (AltR as) = Alt $ map translate as
translate (ThenR a b) = Seq [translate a, translate b]
translate (ApplyR a b) = Apply (isod a) $ translate b
translate (NameR a b) = Name a $ translate b
translate (BindR a (b, c)) = Bind (translate a) (\a -> translate (b $ fromDyn1 a), \b -> toDyn $ c $ fromDyn1 b)

translate1 :: Typeable a => M.Map String (IORef (RuleR t a)) -> RuleR t a -> IO (Rule t)
translate1 e (AltR   as)       = Alt $ mapM (translate e) as
translate1 e (ThenR  a b)      = do 
                        ra <- translate e a
                        rb <- translate e b
                        return $ Seq [ra, rb]
translate1 e (ApplyR a b)      = Apply (isod a) <$> translate e b
translate1 e (NameR  a b)      = Name a <$> translate e b
translate1 e (BindR  a (b, c)) = do
   ra <- translate e a
   
   return $ Bind (translate e a) (\a -> translate (b $ fromDyn1 a), \b -> toDyn $ c $ fromDyn1 b)
-}

doLookups env start = St.evalState (doLookups2 start) (M.map doLookups1 env)

doLookups1 r = (r, 0, r)

getEnv n = M.lookup n <$> St.get

setEnv n v = St.modify (M.insert n v)

doLookups2 (NameStub a) = do
   av <- getEnv a
   case av of
      Just (s, 0, u) -> do
         -- we have to use the T.A.R.D.I.S. for this
         e <- St.get
         let (r, e2) = St.runState (doLookups2 s) (M.insert a (s, 1, r) e)
         St.put e2
         return $ Name a r
      Just (s, 1, r) -> return r
doLookups2 (Apply iso  x  ) = Apply iso  <$> doLookups2 x
doLookups2 (Alt        xs ) = Alt        <$> mapM doLookups2 xs
doLookups2 (Seq        xs ) = Seq        <$> mapM doLookups2 xs
doLookups2 (Many       x  ) = Many       <$> doLookups2 x
--doLookups2 (SetR  name  x  ) = SetR name   $ doLookups2 f x
--doLookups2 (SetM names x  ) = SetM names $ doLookups2 f x
doLookups2 (And        xs ) = And        <$> mapM doLookups2 xs
doLookups2 (Not        x  ) = Not        <$> doLookups2 x
--doLookups2 (        x :+ y) = doLookups2 f x :+ doLookups2 f y
--doLookups2 (Count      x y) = Count        x (doLookups2 f y)
doLookups2 (Name name  x  ) = Name name  <$> doLookups2 x
--doLookups2 (Ignore     x  ) = Ignore     $ doLookups2 f x
--doLookups2 (Try        x  ) = Try        $ doLookups2 f x
doLookups2 (ManyTill   x y) = do
   xr <- doLookups2 x
   yr <- doLookups2 y
   return $ ManyTill xr yr
doLookups2 (Bind   a (b,c)) = do
   ar <- doLookups2 a
   return $ Bind ar (b, c)
doLookups2 other            = return other

--doLookups2 

{-
state
0     initial state, no expanded rules
1     we have started expanding this rule
2     we have finished expanding this rule, 3rd part is the result
3..   only triggered if we try to lookup a rule while currently expanding it
-}

parseT r t = parseED (translate r) t

parseEnv env r t = parseE (doLookups env r) t
{-   
parse (Token a) = a
parse (Ignore a) = 
format (Token a) = a
-}
--    EIso    :: Iso alpha beta -> Rule alpha -> Rule beta
fd f x = do d <- fromDynamic x; r <- f d; return $ toDyn r

isod (Iso n f g) = Iso (n++"d") (fd f) (fd g)

totald n f g = isod (total n f g)

ifst = Iso "ifst" f g
   where
      f d = do
         l <- fromDynamic d :: Maybe [Dynamic]
         return $ l !! 0
      g d = Just $ toDyn [d, undefined]

isnd = Iso "isnd" f g
   where
      f d = do
         l <- fromDynamic d :: Maybe [Dynamic]
         return $ l !! 1
      g d = Just $ toDyn [undefined, d]

--iconsd = isod icons

strOfChars :: [Dynamic] -> String
strOfChars = map fromDyn1

charsOfStr :: String -> [Dynamic]
charsOfStr = map toDyn

chars = totald "strOfChars" strOfChars charsOfStr

--repl1 r = do n <- St.get; return $ Seq $ replicate (fromDyn n 0 :: Int) r

replParse rule n = SeqR $ replicate n rule

replPrint rule res = length res

repl rule = (replParse rule, replPrint rule)

intiso :: Iso String Int
intiso = total "intiso" read show

intisod = isod intiso

int1 :: RuleR Char Int
int1 = ApplyR intiso $ SeqR [RangeR '0' '9']

intn n = ApplyR intiso $ SeqR $ replicate n $ RangeR '0' '9'


--test = int1 >>== repl (RangeR 'a' 'z')
--icons = isod (\(x, xs) -> Just (x:xs)) (\(x:xs) -> Just (x, xs))

{-
simulate this:
do
   n <- int
   rep n anychar

which is

int >>= (\n -> rep n anychar)

-}

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

--innershow d (Seqd  a b) = unwords $ ii d [innershow Nothing a, innershow Nothing b]
innershow :: Show tok => Maybe Int -> Rule tok -> [Char]
innershow d (Seq   as ) = unwords $ ii d $ map (innershow Nothing) as
innershow d (Alt   as ) = unwords $ ii d [intercalate " | " $ map (innershow Nothing) as]
innershow d (Name  a _) = unwords $ ii d [a]
innershow d (Token a  ) = unwords $ ii d [show a]
innershow d (Range a b) = unwords $ ii d ["[" ++ show a ++ ".." ++ show b ++ "]"]
innershow d (Not     a) = "Not " ++ innershow Nothing a

--outershow d r@(Seqd  a b) = "Seqd " ++ innershow d r
outershow d r@(Seq   as ) = "Seq " ++ innershow d r
outershow d r@(Alt   as ) = "Alt " ++ innershow d r
outershow d r@(Name  a b) = unwords $ ii d [a ++ " -> " ++ innershow Nothing b]
outershow d r@(Token a  ) = show a
outershow d r@(Range a b) = "[" ++ show a ++ ".." ++ show b ++ "]"
outershow d r@(Not     a) = innershow Nothing r

ii (Just d) = insertIndex d "*"
ii Nothing = id

data State tok = State { from::Int, to::Int, item::Item tok } deriving (Eq, Ord)

instance Show z => Show (State z) where
   show (State b c i) = show i ++ " " ++ unwords (map show [b, c])

data Result = Running | Fail | Pass { asts :: [Dynamic] } deriving (Eq, Ord, Show)

data Item tok  = Item   (Rule tok) Result Item2 deriving (Eq)

instance Ord tok => Ord (Item tok) where
   compare a b = compare (rule a, item2 a, result a) (rule b, item2 b, result b)

data Item2     = Item2
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

result (Item r re i) = re
rule   (Item r re i) = r
item2  (Item r re i) = i

pos2 :: HasCallStack => Item2 -> Int
pos2 (ISeq n) = n
pos2 (IAlt n) = n
pos2 x = error $ show x

pos :: HasCallStack => Item tok -> Int
pos (Item r s i) = pos2 i

pass (Pass _) = True
pass _        = False

passi = pass . result

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
   showsPrec p (Item rule res i2) = showItem rule i2 p . (' ':) . shows res

--showItem (Seq as) (ISeq n) = \p rest -> unwords $ insertIndex n "." $ map (\x -> showsPrec p x "") as
showItem (Seq as) (ISeq n) = enclose1 (intercalate "," . insertIndex n "o") as 3
showItem (Alt as) (IAlt n) = enclose1 ((++ ' ':show n) . intercalate " | ") as 2
showItem (Many a) (IMany as) = (("Many "++show a++"=") ++) .: enclose1 (intercalate ",") as 0

showItem rule Item2 = enclose2 (`showsPrec` rule) 1

enclose flag str = if flag then (("("++str++")")++) else (str ++)

--enclose1 f as n p = enclose (p > n) $ f $ map (\x -> showsPrec p x "") as
enclose1 f as n p = enclose (p >= n) $ f $ map (\x -> showsPrec p x "") as
--enclose1 f as = enclose2 (\p -> (f $ map (\x -> showsPrec p x "") as)++)


enclose2 :: (Ord t, Num t) => (t -> [Char] -> [Char]) -> t -> t -> [Char] -> [Char]
enclose2 f n p rest = if p >= n then '(':f 0 (')':rest) else f n rest
--enclose2 f n p rest = if p > n then '(':f 0 (')':rest) else f n rest
--enclose2 f n p rest = enclose (p > n) ('(':f 0 (')':rest)) (f n)


{-
instance Ord z => Ord (State z) where
   compare (State b1 k1 e1) (State j2 k2 e2) = compare (b1, k1, e1) (j2, k2, e2)
-}
digit = Range '0' '9'
lower = Range 'a' 'z'
upper = Range 'A' 'Z'
under = Token '_'
alpha = Alt [upper, lower]
--num = Many digit
--alnum = Alt [alpha, digit]
op = Alt [Token '+', Token '-', Token '*', Token '/']

--ident = Name "ident" $ cons (Alt [alpha, under]) ident1
--ident1 = Many (Alt [alpha, under, digit])

sexpr = Name "sexpr" $ Alt [Seq [sexpr, Token '+', sexpr], Token 'a']


test = parseED sexpr "a+a+a"

linePos = M.fromList . zip [(0::Int)..] . (0:) . map (+1) . findIndices (=='\n')

--p s t = tree s $ parseE s t

getLC x lps = let Just (n, s) = M.lookupLE x lps in (n, x - s)
{-
pD s t = do
   states <- parseED s t
   tableZ t $ concatMap S.toList states
   tableZ t $ filter isCompleteZ $ concatMap S.toList states
   return $ tree s states
-}
parseE r t =
   let
      lps = linePos t
      items  = predictA (0, 0) (SL.singleton $ State 0 0 (start r))
      states = parseE0 lps [items] t items [1 .. length t]
      done   = mapMaybe (\s -> ifJust (from s == 0 && rule (item s) == r && passi (item s)) (result (item s))) (SL.list $ last states)
   in if length states == length t && done /= []
         then done
         else trace (tableZ t $ concatMap SL.list states) []

parseED r t = do
   let lps = linePos t
   items <- comppredD lps [SL.singleton $ State 0 0 (start r)] (SL.singleton $ State 0 0 (start r))
   states <- parseE0D lps [items] t items [1 .. length t]
   let done   = mapMaybe (\s -> ifJust (from s == 0 && rule (item s) == r && passi (item s)) (result (item s))) (SL.list $ last states)
   putStrLn (tableZ t $ concatMap SL.list states)
   return done

parseE0 lps states _ items [] = states
parseE0 lps states t items (c : ks) = let
   itemsnew = parseE1A lps states t c items
   in parseE0 lps (states ++ [itemsnew]) t itemsnew ks

parseE0D lps states _ items [] = return states
parseE0D lps states t items (c : ks) = do
   itemsnew <- parseE1D lps states t c items
   parseE0D lps (states ++ [itemsnew]) t itemsnew ks

-- scanM :: Monad m => (m [a] -> b -> m a) -> m [a] -> [b] -> m [a]
scanM f z = foldl (\a b -> do ra <- a; rr <- f (last ra) b; return $ ra ++ [rr]) (return [z])

data States t = States [State t] (I.IntMap (S.Set (State t)))

putss newlist = do
   States all bytok <- St.get
   St.put $ States (all ++ newlist) (foldr fu bytok newlist)

fu s m = I.insert (to s) (S.insert s (fromMaybe S.empty (I.lookup (to s) m))) m

parseE1A lps states t c items = let
   scan1 = scanA c t items
   comp1 = completeA states $ SL.fromList scan1
   in if c < length t
      then predictA (getLC c lps) comp1
      else comp1

parseE1D lps states t c items = do
   putStrLn $ "SCAN " ++ take 75 (cycle "/\\")
   let scan1 = scanA c t items
   putStrLn $ "scan1=" ++ show scan1
   comp1 <- comppredD (getLC c lps) states $ SL.fromList scan1
   --putStrLn $ "comp1=" ++ show comp1
   return comp1

conseq (a : b : cs)
   | a == b = a
   | otherwise = conseq (b : cs)

closure f items = closure1 f items items

closure1 f done current =
   let
      new = f done current
      newdone = S.union done new
      newnew = new S.\\ done
   in
      if S.null new then done else closure1 f newdone newnew

closureA f items = closureA1 f items items

closureA1 f done current =
   let
      (a, newdone) = f done current
      newnew  = SL.fromList $ catMaybes a
   in
      if SL.null newnew then done else closureA1 f newdone newnew

closureD f items = closureD1 f items items

closureD1 f done current = do
   putStrLn $ replicate 80 '='
   (a, newdone) <- f done current
   let newnew = SL.fromList $ catMaybes a
   if SL.null newnew then return done else closureD1 f newdone newnew

closureDD :: (Show a, Ord a) => (SL.SetList a -> SL.SetList a -> IO ([Maybe a], SL.SetList a)) -> (SL.SetList a -> SL.SetList a -> IO ([Maybe a], SL.SetList a)) -> SL.SetList a -> IO (SL.SetList a)
closureDD f g items = closureDD1 f g items items

closureDD1 f g done1 active1 = do
   putStrLn $ "COMPLETE " ++ replicate 71 '='
   --print active1
   (a, done2) <- f done1 active1
   let active2 = SL.fromList $ catMaybes a
   let active3 = SL.union active1 active2
   putStrLn $ "PREDICT " ++ replicate 72 '='
   --print active3
   (b, done3) <- g done2 active3
   let active4 = SL.fromList $ catMaybes b
   let active5 = SL.union active2 active4
   if SL.null active5 then return done3 else closureDD1 f g done3 active5

processA f old current = St.runState (mapM insertA $ concatMap (\x -> map (x,) $ f x) $ SL.list current) old

processD f old current = St.runStateT (mapM insertD $ concatMap (\x -> map (x,) $ f x) $ SL.list current) old

processDD f old current = St.runStateT (do fxs <- mapM (\x -> do fx <- St.liftIO $ f x; return $ map (x,) fx) $ SL.list current; mapM insertD $ concat fxs) old

--refoldD f z xs = do (rs, zs) <- unzip <$> zipWithM f (z : zs) xs; return (rs, zs)
refoldD f z [] = return []
refoldD f z (x : xs) = do (a, b) <- f z x; c <- refoldD f b xs; return $ a : c

insertA (v, fv) = do
   old <- St.get
   if SL.member fv old
      then do
         return Nothing
      else do
         St.modify (SL.insert fv)
         return $ Just fv

insertD (v, fv) = do
   old <- St.get
   if SL.member fv old
      then do
         St.liftIO $ putStrLn $ show v ++ "  ==>  " ++ show fv
         return Nothing
      else do
         St.liftIO $ putStrLn $ show v ++ "  ==>  " ++ show fv ++ " *** NEW ***"
         St.modify (SL.insert fv)
         return $ Just fv

predictA :: (Ord tok) => (Int, Int) -> SL.SetList (State tok) -> SL.SetList (State tok)
predictA n items = closureA (processA (predict1 n)) items

predictD n items = do putStrLn $ "PREDICT "++take 72 (cycle "\\/"); closureD (processD (predict1 n)) items

comppredD n states items = closureDD (\old -> processDD (complete1D (states ++ [old])) old) (processD (predict1 n)) items

--completeD states state = closureD (\old -> processD (complete1A (states ++ [old])) old) state

--paux (Seq  as ) q = [as !! q | q < length as]
paux (Alt  as ) 0 = as
paux (Name a b) 0 = [b]
--paux (ManyTill a b) 0 = [a, b]
paux _ _ = []

predict1 n (State _ c i) = map (State c c) $ predict2 n i

predict2 n (Item (Seq    as   ) Running (ISeq j)) = [start (as !! j)]
predict2 n (Item (Alt    as   ) Running (IAlt 0)) = [start a | a <- as]
predict2 n (Item (Many   a    ) Running _       ) = [start a, Item (Many a) (Pass [toDyn ([]::[Dynamic])]) (IMany [])]
predict2 n (Item (Name   a b  ) Running _       ) = [start b]
predict2 n (Item (Apply  a b  ) Running _       ) = [start b]
predict2 n (Item (Set    a b  ) Running _       ) = [start b]
predict2 n (Item (Not    a    ) Running _       ) = [start a, Item (Not a) (Pass [toDyn ()]) Item2]
predict2 n (Item (Bind   a b  ) Running _       ) = [start a]
predict2 n (Item (Return a    ) Running _       ) = [Item (Return a) (Pass [a]) Item2]
predict2 n (Item  Pos           Running _       ) = [Item Pos (Pass [toDyn n]) Item2]
--predict2 (Item (Get   a  ) t _) = [Item (Get a) (Pass $ lookup a) Item2]
predict2 n (Item {}) = []

--start r@(Not a) = Item r (Pass $ toDyn ()) Item2
start r = Item r Running $ start1 r

start1 (Seq  a) = ISeq 0
start1 (Alt  a) = IAlt 0
start1 (Many a) = IMany []
start1 _ = Item2

scan c t items = mapMaybe (scan1 c (t !! (c - 1))) $ S.toList items

scanA c t items = mapMaybe (scan1 c (t !! (c - 1))) $ SL.list items

scan1 c ch (State j _ t) = scan2 c ch j c t

scan2 c ch j _ (Item r Running i2) = do sc <- saux ch r; return $ State j c $ Item r (if sc then Pass [toDyn ch] else Fail) i2
scan2 c ch _ _ _ = Nothing

saux ch (Token c  ) = Just $ ch == c
saux ch (Range c d) = Just $ ch >= c && ch <= d
saux _ _ = Nothing

completeA states = closureA (\old -> processA (complete1A (states ++ [old])) old)

completeD states state = do putStrLn $ "COMPLETE "++take 71 (cycle "\\/"); closureD (\old -> processDD (complete1D (states ++ [old])) old) state

complete1A states state =
   concatMap (complete2 states state) $ SL.list $ states !! from state

complete2 states substate@(State b c subitem1) mainstate@(State a b1 main) =
   if result subitem1 /= Running && result main == Running && subitem main subitem1
      then map (State a c) $ complete3 states mainstate substate
      else []

complete1D :: (Show tok, Eq tok, Typeable tok) => [SL.SetList (State tok)] -> State tok -> IO [State tok]
complete1D states state = do
   res <- mapM (complete2D states state) $ SL.list $ states !! from state
   return $ concat res

complete2D :: (Show tok, Eq tok, Typeable tok) => [SL.SetList (State tok)] -> State tok -> State tok -> IO [State tok]
complete2D states sub@(State b c subitem1) mainstate@(State a b1 main) = do
   --print (sub, main, subitem main subitem1)
   if result subitem1 /= Running && result main == Running && subitem main subitem1
      then do
         let r = map (State a c) $ complete3 states mainstate sub
         --print (sub, main, r)
         return r
      else return []

complete3 states mainstate@(State a b1 main@(Item r@(Seq as) s (ISeq n))) substate
   | result sub == Fail = [Item r Fail    (ISeq  n     )]
   | n + 1 == length as = [Item r res1    (ISeq (n + 1))]
   | otherwise          = [Item r Running (ISeq (n + 1))]
      where
         sub = item substate
         --res1 = if passi sub then states !! from substate
         res1 = if passi sub then Pass $ retrieve2 states mainstate else Fail

complete3 states mainstate@(State a b1 main@(Item x@(Alt as) q (IAlt n))) substate
   | passi sub     = [Item x (result sub) (IAlt  n     )]
   | n < length as = [Item x Running      (IAlt (n + 1))]
   | otherwise     = [Item x (result sub) (IAlt  n     )]
      where
         sub = item substate

complete3 states mainstate@(State a b1 main@(Item x@(Name d e) q i2)) substate = [Item x (result sub) i2]
   where
      sub = item substate

complete3 states mainstate@(State a b1 main@(Item x@(Apply d e) q _)) substate =
   case result sub of
      Pass reslist -> do
         res <- reslist
         case x of
            Apply iso _ ->
               case apply iso res of
                  Just j  -> [Item x (Pass [j]) Item2]
                  Nothing -> [Item x  Fail      Item2]
      Fail -> [Item x Fail Item2]
   where
      sub = item substate

complete3 states mainstate@(State a b1 main@(Item x@(Not d) q i2)) substate = [Item x (case result sub of { Pass p -> Fail; Fail -> Pass [toDyn ()] } ) Item2]
   where
      sub = item substate

complete3 states mainstate@(State a b1 main@(Item x@(Bind i (j, k)) q _)) substate =
   case result sub of
      Pass reslist -> do
         res <- reslist
         case j res of
            r2 -> [start r2]
      Fail -> [Item x Fail Item2]
   where
      sub = item substate

complete3 states mainstate@(State a b1 main@(Item x@(Many m) q (IMany done))) substate =
--   Item x (Pass $ toDyn done) Item2 :
   case result sub of
      Pass reslist -> do
         res <- reslist
         [Item x Running (IMany $ done ++ [res])]
      Fail     -> []
   where
      sub = item substate
{-
retrieve states mainseq n sub = reverse $ retrieve1 states mainseq n sub

retrieve1 states mainseq n sub = let
   prev1 = S.filter (\prevst -> pass (result $ item prevst) && rule (item prevst) == mainseq !! n) $ SL.set (states !! from sub)
   prev  = only $ S.toList prev1
   in ast (result $ item sub) : if n > 0 then retrieve1 states mainseq (n-1) prev else []
-}
retrieve2 :: (HasCallStack, Eq tok) => [SL.SetList (State tok)] -> State tok -> [Dynamic]
retrieve2 states state = do
   seq <- children states state
   let seqf = reverse seq
   asts1 <- map (asts . result . item) seqf
   return $ toDyn asts1

caux (Seq as) q y = as !! q == y
caux (Alt as) q y = y `elem` as
caux (Name a b) q y = b == y
--caux (ManyTill a b) q y = a == y
caux _ _ _ = False

subitem (Item (Seq   as ) _ (ISeq n)) sub = n < length as && as !! n == rule sub
subitem (Item (Then  a b) _ (ISeq n)) sub = case n of { 0 -> a == rule sub; 1 -> b == rule sub }
subitem (Item (Alt   as ) _ _       ) sub = rule sub `elem` as
subitem (Item (Name  a b) _ _       ) sub = b == rule sub
subitem (Item (Apply a b) _ _       ) sub = b == rule sub
subitem (Item (Not   a  ) _ _       ) sub = a == rule sub
subitem (Item (Many  a  ) _ _       ) sub = a == rule sub
subitem (Item {                    }) sub = False

slength (Seq as) = length as
slength _ = 1

isCompleteZ (State _ _ i) = result i /= Running
{-
predict3 c (Item (Alt  as  ) 0) = [Item a 0 | a <- as]
predict3 c (Item (Seq  as  ) d) = [Item (as !! d) 0 | d < length as]
predict3 c (Item (Name a  b) 0) = [Item b 0]
predict3 c t = []
-}
{-
makeLR e = let
   items = predict (S.singleton $ State e 0 0 0)
   in makeLRa [items] [items]

makeLRa old [] = old
makeLRa old current = let
   c = 0
   new = concatMap (makeLRb old) current
   oldcore = S.fromList $ map core old
   newnew = filter (\x -> not $ S.member (core x) oldcore) new
   in
   makeLRa (old ++ new) newnew

core = S.map core1

core1 (State r b c d) = Item r d

makeLRb states items = let
   states1 = scan2 (length states) items
   states2 = map (complete states) states1
   in if True --c < length t
      then map predict states2
      else states2

scan2 c items = map (S.fromList . catMaybes) $ crossWith
   (uncurry scan1)
   (zip [c..] $ S.toList $ S.fromList $ mapMaybe scan3 $ S.toList items)
   (S.toList items)

makeLRc states items = let
   tokens = scan5 items
   oldcore = S.fromList $ map core states
   in foldl (makeLRd items) states tokens

makeLRd items states token = let
   c = length states
   statescore = S.fromList $ map core states
   newstate = S.fromList $ mapMaybe (scan1 c token) $ S.toList items
   {-
   newstates = if S.member (core newstate) statescore
      then states
      else states ++ [newstate]
   -}
   in (states ++) $ map predict $ complete3 states newstate

data NumSet a b = NumSet (I.IntMap a) (S.Set b) (a -> b)

size (NumSet a b f) = fst $ fromJust $ I.lookupMax a

add a ns@(NumSet sa sb sf) = let
   b = sf a
   there = S.member b sb
   s = size ns
   in (not there, if not there
                     then NumSet (I.insert s a sa) (S.insert b sb) sf
                     else ns)
{-
add1 a (ns, collect) = let
   (nt, ns1) = add a ns
-}
--addList as ns = foldr add ns as

scan5 items = S.toList $ S.fromList $ mapMaybe scan3 $ S.toList items

{-
scan2a c items = let
   toks = S.toList $ M.fromList $ mapMaybe (uncurry scan4) $ zip [c..] $ S.toList items
   map 
-}
scan3 (State r@(Token c   ) b _ 0) = Just c
--scan3 c (State r@(Range c d) b _ 0) = ifJust (ch >= c && ch <= d) (State r b c 1)
scan3 t = Nothing

scan4 c (State r@(Token ch   ) b _ 0) = Just (ch, State r b c 1)
--scan3 c (State r@(Range c d) b _ 0) = ifJust (ch >= c && ch <= d) (State r b c 1)
scan4 c t = Nothing

children states (State f t i) = do
   s1@(State f1 t1 i1) <- S.toList $ states !! t
   if subitem i i1 && pass (result i1)
         then do
            s2@(State f2 t2 i2) <- S.toList $ states !! f1
            if rule i2 == rule i && f2 == f && pos i2 == pos i - 1
                  then if pos i2 > 0 then map (s1:) $ children states s2 else [[s1]]
            else []
         else []

-}

children :: (HasCallStack, Eq tok) => [SL.SetList (State tok)] -> State tok -> [[State tok]]
children states (State f t i) = do
   s1@(State f1 t1 i1) <- SL.list $ states !! t
   if subitem i i1 && pass (result i1)
         then do
            s2@(State f2 t2 i2) <- SL.list $ states !! f1
            if rule i2 == rule i && f2 == f && pos i2 == pos i - 1
                  then if pos i2 > 0 then map (s1:) $ children states s2 else [[s1]]
            else []
         else []

data Tree z = Tree (State z) [Tree z] | Trees [Tree z] deriving (Eq, Ord)

tree start states =
   let
      [end] = filter ((0 ==) . from) $ SL.list $ last states
   in
      tree1 states end

tree1 states end = Tree end $ tree2 $ reverse $ map reverse $ map2 (tree1 states) $ children states end

tree2 [x] = x

tree2 xs = map Trees xs

only [x] = x

instance Show z => Show (Tree z) where
   show tree = MyPretty2.format1 1 $ convTree tree

convTree (Tree a b) = MyPretty2.Data (show a) $ map convTree b
convTree (Trees b) = MyPretty2.Data "TREES" $ map convTree b
{-
transeq :: Foldable t => S.Set ETrans -> t a -> (Rule tok) -> [[ETrans]]
>>> p expr "a+a+a"
-}

mergeStrs a b = zipWith (\x y -> if x == ' ' then y else x) a b ++ if length a > length b then drop (length b) a else drop (length a) b

t2 (State _ t _) = t

{-
scanlr c t states = S.fromList $ mapMaybe (scan1 c (t !! (c - 1))) $ S.toList states
scanlr c ch (State r@(Token c   ) b _ 0) = ifJust (ch == c) (State r b c 1)
scanlr c ch (State r@(Range c d) b _ 0) = ifJust (ch >= c && ch <= d) (State r b c 1)
-}
combinescans c@(Token cr, cs) d@(Token dr, ds) =
   if cr == dr
      then [(Token cr, cs ++ ds)]
      else [c, d]
combinescans c@(Token cr, cs) d@(Range d1 d2, ds) =
   if cr >= d1 && cr <= d2
      then [(Range d1 (pred cr), ds), (Token cr, cs ++ ds), (Range (succ cr) d2, ds)]
      else [c, d]
combinescans c@(Range c1 c2, cs) d@(Range d1 d2, ds) =
   if c2 >= d1 && c1 <= d2 then
      let
         o1 = max c1 d1
         o2 = min c2 d2
      in if c1 < d1
               then [(Range c1 (pred o1), cs), (Range o1 o2, cs ++ ds), (Range (succ o2) d2, ds)]
               else [(Range d1 (pred o1), ds), (Range o1 o2, cs ++ ds), (Range (succ o2) c2, cs)]
   else
      [c, d]
-- start from start rule
-- thats your first kernel
-- closure i with predictions (if you have completions there's a problem)
-- scan all possible tokens into separate states
-- same token may have multiple meanings, combine them into one state
-- those are your new kernels
-- closure them with completions and predictions
-- 
-- how kernels are compared for equality defines what sort of parser it is
-- some record little context, some account for a lot, if it accounts for all context it can't do recursive grammars

tableZ str states =
   let
      shown = map show states
      range = [0..length str]
      nums  = map show [0..length str]
      numls = 0 : map length nums
      ends  = 0 : map (\a -> maximum $ zipWith (taux1D ends numls a) shown states) [0..length str]
      show1 = zipWith (taux2D ends numls) shown states
      nums1 = map (taux3D ends nums ) [0..length str]
      toks  = map (taux4D ends str  ) [0..length str-1]
      axis  = Data.List.foldr Parser3.mergeStrs "" (nums1 ++ toks)
   in
      unlines $ axis : show1 ++ [axis]
{- 
ends !! 0 = 0
ends !! 1 = 
-}
taux1D ends numls a sh st@(State f t i) = numls !! a +
   if f == t
      then let
         l = length sh + 2
         in if | t     == a -> ends !!  f      +      div l 2
               | t + 1 == a -> ends !! (f + 1) + (l - div l 2)
               | True       -> 0
      else let
         l = length sh + 4
         in if | t     == a -> ends !! (f + 1) + l
               | True       -> 0

taux2D ends numls sh st@(State f t i) = let
   l  = length sh
   in if f == t
            then replicate (ends !! (f + 1) - div l 2) ' ' ++ "(" ++ sh ++ ")"
            else let
               l2 = ends !! (f + 2) - ends !! (f + 1)
               l3 = l2 - l
               l4 = div l3 2
               l5 = l3 - l4
               in replicate (ends !! (f + 1) + numls !! (f + 1)) ' ' ++ replicate l4 '-' ++ sh ++ replicate l5 '-'

taux3D ends nums a = replicate (ends !! (a + 1) + 1) ' ' ++ nums !! a

taux4D ends str a = let
   sh = show $ str !! a
   l  = length sh
   in replicate (div (ends !! (a + 1) + ends !! (a + 2) - l) 2) ' ' ++ sh



data Doc str = DStr str | DGroup [Doc str] | DSeq [Doc str] deriving (Eq, Ord, Show)

data Doc2 str = Doc2 {docWidth :: Int, docHeight :: Int, docText :: [str]} deriving (Eq, Ord)

fp p e = format <$> print1 p e

fp2 p e = format <$> print2 (translate p) e

print1 :: RuleR t a -> a -> Maybe (Doc [t])
print1 (SeqR   as ) e = mergeSeq <$> zipWithM print1 as e
print1 (AltR   as ) e = firstJust1 $ map (\a -> print1 a e) as
print1 (ApplyR a b) e = unapply a e >>= print1 b
print1 (ManyR  a  ) e = mergeSeq <$> mapM (print1 a) e
print1 (BindR  p (g, h)) b = let a = h b in do t1 <- print1 p a; t2 <- print1 (g a) b; return $ DSeq [t1, t2]
print1 (NameR  a b) e = print1 b e
print1 (TokenR t  ) e = ifJust (t == e) $ DStr [t]
print1 AnyTokenR    e = Just $ DStr [e]
--print1 other        e = error $ show other

print2 :: Rule Char -> Dynamic -> Maybe (Doc [Char])
print2 (Seq   as ) e = do m <- zipWithM print2 as (fromDyn1 e :: [Dynamic]); return $ mergeSeq m
print2 (Alt   as ) e = firstJust1 $ map (\a -> print2 a e) as
print2 (Apply a b) e = unapply a e >>= print2 b
print2 (Many  a  ) e = mergeSeq <$> mapM (print2 a) (fromDyn1 e :: [Dynamic])
print2 (Bind  p (g, h)) b = let a = h b in do t1 <- print2 p a; t2 <- print2 (g a) b; return $ DSeq [t1, t2]
print2 (Name  a b) e = print2 b e
print2 (Token t  ) e = ifJust (t == fromDyn1 e) $ DStr [t]
print2 AnyToken    e = Just $ DStr [fromDyn1 e]
print2 other       e = error $ show other

format = format1 0

format1 ind (DStr string) = string
format1 ind (DGroup group) = concatMap (\item -> "\n" ++ replicate ind ' ' ++ format1 (ind + 3) item) group
format1 ind (DSeq docs) = concatMap (format1 ind) docs

mergeDocs (DSeq s) = mergeSeq $ mergeStrs1 $ map mergeDocs s
mergeDocs (DGroup a) = DGroup $ map mergeDocs a
mergeDocs (DStr s) = DStr s

mergeStrs1 [] = []
mergeStrs1 (DStr a : DStr b : cs) = mergeStrs1 (DStr (a ++ b) : cs)
mergeStrs1 (a : as) = a : mergeStrs1 as

mergeSeq1 [] = []
mergeSeq1 (DSeq a : DSeq b : cs) = mergeSeq1 (DSeq (a ++ b) : cs)
mergeSeq1 (a : as) = a : mergeSeq1 as

mergeSeq s = case mergeSeq1 $ mergeStrs1 s of
   [a] -> a
   b   -> DSeq b

wrap w d = d

t s = Doc2 (length s) 1 [s]
ts ss = Doc2 (maximum $ map length ss) (length ss) ss

toString (Doc2 _ _ ss) = unlines $ padcoll0 ss

minWidthDoc = minimumBy (compare `on` docWidth)
minHeightDoc ds = minimumBy (compare `on` docHeight) ds

minWidth ds = minimum $ map docWidth ds
minHeight ds = minimum $ map docHeight ds

maxWidth ds = maximum $ map docWidth ds
maxHeight ds = maximum $ map docHeight ds

sumHeight ds = sum $ map docHeight ds
sumWidth ds = sum $ map docWidth ds

fit :: Int -> [Doc2 a] -> Doc2 a
fit w opts = let f = filter ((<= w) . docWidth) opts in if null f then minWidthDoc opts else head f

indent1 n (Doc2 w h t) = Doc2 (w + n) h $ indent2 n t
indent2 n = map (replicate n ' ' ++)

-- sequential append
sapp (Doc2 aw ah at) (Doc2 bw bh (bth : btt)) =
   let
   ati = init at
   atl = last at
   in
   ts $ ati ++ ((atl ++ bth) : indent2 (length atl) btt)

(<\>) = sapp

scat ds = Prelude.foldl1 sapp ds

vcat ds =
   Doc2
   (maximum $ map docWidth ds)
   (sum $ map docHeight ds)
   (concat $ map docText ds)

vapp a b = vcat [a, b]

iapp a b = vapp a $ indent1 3 b

visitPost :: (forall a. Rule a -> Rule a) -> Rule a -> Rule a
visitPost f x = f $ visit (visitPost f) x

visitPre :: (forall a. Rule a -> Rule a) -> Rule a -> Rule a
visitPre f x = visit (visitPre f) (f x)

visit :: (forall a. Rule a -> Rule a) -> Rule a -> Rule a
visit f (Apply iso  x  ) = Apply iso  $ visit f x
visit f (Alt        xs ) = Alt        $ map (visit f) xs
visit f (Seq        xs ) = Seq        $ map (visit f) xs
visit f (Many       x  ) = Many       $ visit f x
visit f (ManyTill   x y) = ManyTill    (visit f x) (visit f y)
visit f (Bind   a (b,c)) = Bind        (visit f a) (\d -> visit f (b d), c)
--visit f (SetR  name  x  ) = SetR name   $ visit f x
--visit f (SetM names x  ) = SetM names $ visit f x
visit f (And        xs ) = And        $ map (visit f) xs
visit f (Not        x  ) = Not        $ visit f x
--visit f (        x :+ y) = visit f x :+ visit f y
--visit f (Count      x y) = Count        x (visit f y)
visit f (Name name  x  ) = Name name  $ visit f x
--visit f (Ignore     x  ) = Ignore     $ visit f x
--visit f (Try        x  ) = Try        $ visit f x
visit f other            = other

visitPostR :: (forall t a. RuleR t a -> RuleR t a) -> RuleR t a -> RuleR t a
visitPostR f x = f $ visitR (visitPostR f) x

visitPreR :: (forall t a. RuleR t a -> RuleR t a) -> RuleR t a -> RuleR t a
visitPreR f x = visitR (visitPreR f) (f x)

visitR :: (forall t a. RuleR t a -> RuleR t a) -> RuleR t a -> RuleR t a
visitR f (ApplyR iso  x  ) = ApplyR iso  $ visitR f x
visitR f (AltR        xs ) = AltR        $ map (visitR f) xs
visitR f (ManyR       x  ) = ManyR       $ visitR f x
visitR f (ManyTillR   x y) = ManyTillR    (visitR f x) (visitR f y)
--visitR f (SetR  name  x  ) = SetR name   $ visitR f x
--visitR f (SetM names x  ) = SetM names $ visitR f x
visitR f (AndR        x y) = AndR         (visitR f x) (visitR f y)
visitR f (NotR        x  ) = NotR        $ visitR f x
--visitR f (        x :+ y) = visitR f x :+ visitR f y
--visitR f (Count      x y) = Count        x (visitR f y)
visitR f (NameR name  x  ) = NameR name  $ visitR f x
--visitR f (Ignore     x  ) = Ignore     $ visitR f x
--visitR f (Try        x  ) = Try        $ visitR f x
visitR f other            = other

