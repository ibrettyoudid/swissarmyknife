{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use const" #-}
{- HLINT ignore "Use tuple-section" -}

module Parser7 where

import Favs hiding (indent1, indent2)
import qualified MyPretty2
import {-# SOURCE #-} MHashDynamic3 hiding (Apply, expr)
import NewTuple
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

import Data.IORef
import Graphics.Rendering.Cairo (RectangleInt(x))
import System.IO.Unsafe
import Foreign.C (CWchar)

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

sepBy x sep = Parser7.pure []
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

{-
thenisod = Iso "theniso" f g 
   where
      f d = do
         [a, b] <- fromDynamic d :: Maybe [Dynamic]
         return $ toDyn $ a :- b
      g d = do
         a :- b <- fromDynamic d :: Maybe (Dynamic :- Dynamic)
         return $ toDyn [a, b]

theniso :: forall a b. (Typeable a, Typeable b) => Iso Dynamic (a :- b)
theniso = Iso "theniso" f g
   where
      f d = do
         [ad, bd] <- fromDynamic d :: Maybe [Dynamic]
         a <- fromDynamic ad :: Maybe a
         b <- fromDynamic bd :: Maybe b
         return $ a :- b
      g ab = Just $ toDyn ab

thenisod :: forall a b. (Typeable a, Typeable b) => a -> b -> Iso Dynamic Dynamic
thenisod a b = Iso "thenisod" f g
   where
      f d = do
         [ad, bd] <- fromDynamic d :: Maybe [Dynamic]
         a <- fromDynamic ad :: Maybe a
         b <- fromDynamic bd :: Maybe b
         return $ toDyn $ a :- b
      g d = do
         a :- b <- fromDynamic d :: Maybe (a :- b)
         let ad = toDyn a
         let bd = toDyn b
         return $ toDyn [ad, bd]

-}




translate :: (Typeable a, HasCallStack) => RuleR t a -> Rule t
translate (AltR as) = Alt $ map translate as
translate (SeqR as) = Seq $ map translate as
translate (ThenR (a :: RuleR t a) (b :: RuleR t b)) = Apply thenisod $ Seq [translate a, translate b]
   where 
      thenisod = Iso "thenisod" f g
         where
            f d = let
               [ad, bd] = fromDyn1 d :: [Dynamic]
               a = fromDyn1 ad :: a
               b = fromDyn1 bd :: b
               in Just $ toDyn $ ad :- bd
            g d = let
               ad :- bd = fromDyn1 d :: (Dynamic :- Dynamic)
               --ad = toDyn a
               --bd = toDyn b
               in Just $ toDyn [ad, bd]
--translate (ThenR a b) = Then (translate a) (translate b)
translate (a :/  b) = Apply ifst $ Seq [translate a, translate b]
translate (a :// b) = Apply isnd $ Seq [translate a, translate b]
translate (ApplyR a b) = Apply (isod a) $ translate b
translate (NameR a b) = Name a $ translate b
translate (NameStubR a) = NameStub a
translate (BindR a (b, c)) = Bind (translate a) (\a -> translate (b $ fromDyn1 a), \b -> toDyn $ c $ fromDyn1 b)
translate (TokenR a) = Token a
translate AnyTokenR = AnyToken
translate (RangeR a b) = Range a b
translate (ManyR (a :: RuleR t a)) = Apply manyiso $ Many $ translate a
   where
      manyiso = Iso "manyiso" f g
      f d = Just $ toDyn (map (\a -> fromDyn1 a :: a) (fromDyn2 "not [Dynamic]" d :: [Dynamic]))
      g d = Just $ toDyn $ map toDyn (fromDyn2 "not [a] in g" d :: [a])
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

parseT r t = parseE (translate r) t

parseEnv env r t = parseE (doLookups env r) t
{-   
parse (Token a) = a
parse (Ignore a) = 
format (Token a) = a
-}
--    EIso    :: Iso alpha beta -> Rule alpha -> Rule beta
fd f x = do d <- fromDynamic x; r <- f d; return $ toDyn r
fe f x = case f $ fromMaybe (error $ "wrong type sent to Iso f="++show (toDyn f)++" x="++show x) (fromDynamic x) of
   Just  j -> Just $ toDyn j
   Nothing -> Nothing

isod (Iso n f g) = Iso (n++"d") (fe f) (fe g)

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
   compare (Then     {}) (Name     {}) = GT
   compare (Then     {}) (Seq      {}) = GT
   compare (Then     {}) (Alt      {}) = GT
   compare (Then     {}) (Token    {}) = GT
   compare (Then     {}) (Range    {}) = GT
   compare (Then     {}) (AnyToken {}) = GT
   compare (Then     {}) (Apply    {}) = GT
   compare (Then     {}) (Many     {}) = GT
   compare (Then     {}) (Bind     {}) = GT
   compare (Then     {}) (Return   {}) = GT
   compare (Then     {}) _             = LT
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

data State tok = Scan     { item :: Item tok, from :: SL.SetList (State tok) }
               | Predict  { item :: Item tok, from :: SL.SetList (State tok) }
               | Scanned  { item :: Item tok,                                 prev :: SL.SetList (State tok) }
               | Early    { item :: Item tok,                                 prev :: SL.SetList (State tok) }
               | Complete { item :: Item tok, from :: SL.SetList (State tok) } --, prev :: [State tok] }
               | From     { item :: Item tok, from :: SL.SetList (State tok), prev :: SL.SetList (State tok) }
               deriving (Eq, Ord, Show)
{-
instance Show z => Show (State z) where
   show s = show (item s)
-}
data Result = Running | Fail | Pass { asts :: [Dynamic] } deriving (Eq, Ord, Show)

data Item tok  = Item (Rule tok) Result (Item2 tok) deriving (Eq)

instance Ord tok => Ord (Item tok) where
   compare a b = compare (rule a, item2 a, result a) (rule b, item2 b, result b)

data Item2 tok = Item2
               | ISeq  [[Dynamic]]
               | IAlt  Int -- Int is how many have finished
               | IName
               | ITerm   --used for all terminal level ops
               | IMany [[Dynamic]]
               | INot
               | IAnd
               | IGet
               | ISet
               | IApply
               | IToken tok
               deriving (Eq, Ord)

result (Item r re i) = re
rule   (Item r re i) = r
item2  (Item r re i) = i

pos2 (ISeq seqs) = if null seqs then 0 else length $ head seqs
pos2 (IAlt n) = n

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
showRule (Then  a b) = \p -> enclose (p >= 3) (showRule a 3 $ (" then " ++) $ showRule b 3 "")
showRule (Alt   as ) = enclose1 (intercalate " | ") as 2
showRule (Name  a _) = \p -> ((a ++ " ") ++)
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
showItem (Seq as) (ISeq seqs) = \p -> enclose (p >= 3) (intercalate "," $ insertIndex (ilength seqs) "o" $ map (\x -> showsPrec p x "") as) . ((" ("++show seqs++")") ++)
showItem (Alt as) (IAlt n) = enclose1 ((++ ' ':show n) . intercalate " | ") as 2
showItem (Many a) (IMany as) = \p -> (("Many "++show a++"=") ++) . (show as ++)

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

exprsimple = Name "expr" $ Alt [Seq [exprsimple, Token '+', exprsimple], Token 'a']

test = parseE exprsimple "a+a+a"

linePos = M.fromList . zip [(0::Int)..] . (0:) . map (+1) . findIndices (=='\n')

--p s t = tree s $ parseE s t

getLC x lps = let Just (n, s) = M.lookupLE x lps in (n, x - s)

parseE r t = do
   let it = start r
   print it

   let state = Predict it SL.empty
   let map1 = M.fromList [(it, state)]

   done <- parseE2 map1 (0::Int, head t)

   res <- if null t
               then
                  return done
               else
                  foldM parseE1 done $ zip [(0::Int)..] t

   return $ map result $ filter ((r ==) . rule) $ map (item . snd) $ M.toList res

parseE1 map1 (n, t) = do
   putStrLn "END OF TOKEN"
   mapM_ (print . fst) $ M.toList map1
   putStrLn "SCANNING"
   let state = scan t map1
   putStrLn "SCANNED!!!!"
   print state

   let map2 = M.fromList [(item state, state)]

   parseE2 map2 (n, t)

parseE2 map2 (n, t) = do
   (_, done, _) <- parseE3 complete predict n t (True, map2, SL.fromList $ map snd $ M.toList map2)

   return done

{-
                     +====swap f,g====+
                     |                |
                     v                |
parseE2 ==> parseE3 ======> parseE4 ==+
              /  \              |
             /    \             |
            /      \            v
      (parseE5 f) (parseE5 g) (parseE5 f/g)
-}


parseE3 f g n t (changed, mapd, sln) = do
   a@(changed1, mapd1, sln1) <- parseE5 f n t (False, mapd, sln)
   b@(changed2, mapd2, sln2) <- parseE5 g n t (changed1, mapd1, SL.union sln sln1)
   if changed2
      then parseE4 f g n t (changed2, mapd2, SL.union sln1 sln2)
      else return (changed2, mapd2, SL.union sln1 sln2)

parseE4 f g n t (changed, mapd, sln) = do
   a@(changed1, mapd1, sln1) <- parseE5 f n t (False, mapd, sln)
   if changed1
      then parseE4 g f n t (True, mapd1, sln1)
      else return (changed || changed1, mapd1, sln1)

parseE5 f n t (changed, mapd, sln) = do
   a@(changed1, mapd1, sln1) <- f n t (False, mapd, SL.empty) sln
   putStrLn "Input"
   mapM_ (print . item) $ SL.toList sln
   putStrLn "Done"
   mapM_ (print . fst) $ M.toList mapd1
   putStrLn "Active"
   mapM_ (print . item) $ SL.toList sln1
   print changed1
   if changed1
      then do
         b@(changed2, mapd2, sln2) <- parseE5 f n t (True, mapd1, sln1)
         return (changed2, mapd2, SL.union sln1 sln2)
      else return (changed || changed1, mapd1, sln1)

untilM pred mf x = if pred x
                        then return x
                        else mf x >>= untilM pred mf

process f s args@(changed, mapd, sln) items = do
   foldM (\(changed, mapd, sln) i ->
      case M.lookup i mapd of
                        Nothing -> do
                           let z = f i (SL.singleton s)
                           putStrLn $ "|\\  | +---- \\        /   "++show i
                           putStrLn   "| \\ | +--    \\  /\\  /"
                           putStrLn   "|  \\| +----   \\/  \\/"
                           putStr (showSequences z)
                           return (True, M.insert i z mapd, SL.insert z sln)
                        Just  e -> do
                           let y = from e
                           (if SL.member s y
                                 then return (changed, mapd, sln)
                                 else do
                                    let z = f i (SL.insert s y)
                                    putStrLn $ "***** NEW STATE "++show s++" ALSO LEADS TO ITEM "++show i
                                    return (changed, M.insert i z mapd, SL.insert z sln))) args items

predict n t args@(changed, mapd, sln) sla = do
   putStrLn "PREDICT ----------------------------"
   res <- foldM (predict1 n t) (changed, mapd, sln) sla
   foldM (completeEarly n t) res sla

predict1 n t args s = do
   --print s
   let items = predict2 n $ item s
   --putStrLn "items"
   --print items

   res <- process Predict s args items
   --putStrLn "result"
   --print res
   return res

completeEarly n t args s = do
   let items = completeEarly2 n $ item s

   res <- process Early s args items

   return res

complete n t args@(changed, mapd, sln) sla = do
   putStrLn "COMPLETE ---------------------------"
   foldM (complete1 n t) (changed, mapd, sln) sla

complete1 n t args sub = do
   --print sub

   let p1 = makeFrom n sub
   --putStrLn "mains"
   --print p1

   let items = concatMap (complete3 $ item sub) (SL.map item $ prev p1)
   --putStrLn "new main items"
   --print items

   if passi $ item sub then do
      res <- process Complete p1 args items
      --putStrLn "result"
      --print res
      return res
   else
      return args

scan ch soi = Scanned (Item (Token ch) (Pass [toDyn ch]) Item2) $ SL.fromList $
   mapMaybe (\(it, state) -> ifJust (saux ch (rule it)) state) $ M.toList soi

makeFrom n s = From (item s) (SL.singleton s) (SL.concat $ map from $ seqstart s)

seqstart state@(Predict {}) = return state
seqstart state@(Complete {})= do
   from1 <- SL.list $ from state
   prev1 <- SL.list $ prev from1
   seqstart prev1

seqstart state@(Scanned {}) = do
   prev1 <- SL.list $ prev state
   seqstart prev1

sequences state = map reverse $ sequences1 state

sequences1 state@(Predict {}) = return [state]

sequences1 state@(Complete {})= do
   from1 <- SL.list $ from state
   prev1 <- SL.list $ prev from1
   rest <- sequences1 prev1
   return $ state:rest

sequences1 state@(Scanned {}) = do
   prev1 <- SL.list $ prev state
   rest <- sequences1 prev1
   return $ state:rest

sequences1 state@(Early {}) = do
   prev1 <- SL.list $ prev state
   rest <- sequences1 prev1
   return $ state:rest

seqInitsHard state@(Predict  {}) = [state]
seqInitsHard state@(Complete {}) = concatMap seqInitsHard $ seqPrev state
seqInitsHard state@(Scanned  {}) = concatMap seqInitsHard $ seqPrev state
seqInitsHard state@(Early    {}) = concatMap seqInitsHard $ seqPrev state

initParents :: State tok -> [State tok]
initParents state@(Predict  {}) = SL.toList $ from state

sequencesHard state = map reverse $ sequencesHard1 state

sequencesHard1 :: State tok -> [[State tok]]
sequencesHard1 state@(Predict  {}) = return [state]
sequencesHard1 state = do
   seqPrev1 <- seqPrev state
   rest <- sequencesHard1 seqPrev1
   return $ state : rest
   
seqPrev :: State tok -> [State tok]
seqPrev state@(Complete {}) = do
   from1 <- SL.toList $ from state
   from2 <- SL.toList $ from from1
   seqInit1 <- seqInitsHard from2
   initParents seqInit1
seqPrev state@(Scanned  {}) = SL.toList $ prev state
seqPrev state@(Early    {}) = SL.toList $ prev state
seqPrev state@(Predict  {}) = error "doesn't make sense to call seqPrev on a Predict"
seqPrev state@(From     {}) = do
   from2 <- SL.toList $ from state
   seqInit1 <- seqInitsHard from2
   initParents seqInit1

--paux (Seq  as ) q = [as !! q | q < length as]
paux (Alt  as ) 0 = as
paux (Name a b) 0 = [b]
--paux (ManyTill a b) 0 = [a, b]
paux _ _ = []

predict2 n (Item (Seq    as   ) Running (ISeq j)) = [start (as !! ilength j)]
predict2 n (Item (Alt    as   ) Running (IAlt 0)) = [start a | a <- as]
predict2 n (Item (Many   a    ) Running (IMany done)) = [start a]
predict2 n (Item (Name   a b  ) Running _       ) = [start b]
predict2 n (Item (Apply  a b  ) Running _       ) = [start b]
predict2 n (Item (Set    a b  ) Running _       ) = [start b]
predict2 n (Item (Not    a    ) Running _       ) = [start a]
predict2 n (Item (Bind   a b  ) Running _       ) = [start a]
--predict2 (Item (Get   a  ) t _) = [Item (Get a) (Pass $ lookup a) Item2]
predict2 n (Item {}) = []

--start r@(Not a) = Item r (Pass $ toDyn ()) Item2
start r = Item r Running $ start1 r

start1 (Seq  a) = ISeq [[]]
start1 (Alt  a) = IAlt 0
start1 (Many a) = IMany [[]]
start1 _ = Item2

saux ch (Token c  ) = ch == c
saux ch (Range c d) = ch >= c && ch <= d
saux ch AnyToken    = True
saux _ _ = False


completeEarly2 n (Item (Many   a    ) Running (IMany done)) = if done == [[]] then [Item (Many a) (mytrace2 "completeEarly2: " $ Pass []) (IMany [[]]), start a] else []
completeEarly2 n (Item (Not    a    ) Running _           ) = [Item (Not a) (Pass [toDyn ()]) Item2]
completeEarly2 n (Item (Return a    ) Running _           ) = [Item (Return a) (Pass [a]) Item2]
completeEarly2 n (Item  Pos           Running _           ) = [Item Pos (Pass [toDyn n]) Item2]

completeEarly2 n (Item {}) = []
--retrieve = iterate prev

complete3 sub main@(Item r@(Seq as) s (ISeq seqs))
   | result sub == Fail = [Item r Fail      (ISeq  seqs   )]
   | otherwise          = let
      seqsNew = [seq ++ [ast] | seq <- seqs, ast <- asts $ result sub]
      in trace ("seqsNew = "++show seqsNew++" asts = "++show (asts (result sub))) $ if ilength seqsNew == length as 
            then [Item r (Pass $ map toDyn seqsNew) (ISeq  seqsNew)]
            else [Item r Running                    (ISeq  seqsNew)]

complete3 sub main@(Item x@(Alt as) q (IAlt n))
   | passi sub     = [Item x (result sub) (IAlt  n     )]
   | n < length as = [Item x Running      (IAlt (n + 1))]
   | otherwise     = [Item x (result sub) (IAlt  n     )]

complete3 sub main@(Item x@(Name d e) q i2) = [Item x (result sub) i2]

complete3 sub main@(Item x@(Apply d e) q _) =
   case result sub of
      Pass reslist -> 
         case x of
            Apply iso@(Iso n f g) _ -> trace ("calling Iso "++n++" with mapMaybe (apply iso) "++show reslist) $
               case mapMaybe (apply iso) reslist of
                  [] -> [Item x  Fail    Item2]
                  l  -> [Item x (Pass l) Item2]
      Fail -> [Item x Fail Item2]

complete3 sub main@(Item x@(Not d) q i2) = [Item x (case result sub of { Pass p -> Fail; Fail -> Pass [toDyn ()] } ) Item2]

complete3 sub main@(Item x@(Bind a (b, c)) q _) =
   case result sub of
      Pass reslist -> do
         res <- reslist
         case b res of
            r2 -> [start r2]
      Fail -> [Item x Fail Item2]

complete3 sub main@(Item x@(Many a) q (IMany done)) =
--   Item x (Pass $ toDyn done) Item2 :
   case result sub of
      Pass reslist -> 
         let newdone = [seq ++ [res] | seq <- done, res <- reslist]
         in [Item x Running (IMany newdone), Item x (mytrace2 ("complete3: done="++show done++" newdone="++show newdone++" reslist="++show reslist++" returns ") $ Pass $ map toDyn newdone) (IMany newdone)]
      Fail     -> []
{-
retrieve states mainseq n sub = reverse $ retrieve1 states mainseq n sub

retrieve1 states mainseq n sub = let
   prev1 = S.filter (\prevst -> pass (result $ item prevst) && rule (item prevst) == mainseq !! n) $ SL.set (states !! from sub)
   from7 = only $ S.toList prev1
   in ast (result $ item sub) : if n > 0 then retrieve1 states mainseq (n-1) from7else []
-}
mytrace2 m x = unsafePerformIO $ mytrace3 m x

mytrace3 m x = do
   putStrLn $ m ++ show x
   return x

caux (Seq as) q y = as !! q == y
caux (Alt as) q y = y `elem` as
caux (Name a b) q y = b == y
--caux (ManyTill a b) q y = a == y
caux _ _ _ = False

subitem (Item (Seq   as ) _ (ISeq seqs)) sub = let n = ilength seqs in n < length as && as !! n == rule sub
subitem (Item (Then  a b) _ (ISeq seqs)) sub = let n = ilength seqs in case n of { 0 -> a == rule sub; 1 -> b == rule sub }
subitem (Item (Alt   as ) _ _       ) sub = rule sub `elem` as
subitem (Item (Name  a b) _ _       ) sub = b == rule sub
subitem (Item (Apply a b) _ _       ) sub = b == rule sub
subitem (Item (Not   a  ) _ _       ) sub = a == rule sub
subitem (Item (Many  a  ) _ _       ) sub = a == rule sub
subitem (Item {                    }) sub = False

slength (Seq as) = length as
slength _ = 1

ilength as = if null as then 0 else length $ head as



showSequences state = unlines $ showSequences1 state

showSequences1 :: Show tok => State tok -> [String]
showSequences1 state@(Predict  {}) = showTree [[""]] (show $ item state)
showSequences1 state@(Complete {}) = showSequences2 state $ SL.toList $ from state
showSequences1 state@(Scanned  {}) = showSequences3 state $ SL.toList $ prev state

showSequences2 :: Show tok => State tok -> [State tok] -> [String]
showSequences2 state from1 = showTree (map (showSequences3 state . SL.toList . prev) from1) (show $ item state)

showSequences3 :: Show tok => State tok -> [State tok] -> [String]
showSequences3 state prev1 = showTree (map showSequences1 prev1) ""

showSequencesD1 :: Show tok => State tok -> [[String]]
showSequencesD1 state@(Predict  {}) = [[show (item state) ++ "==>"]]
showSequencesD1 state@(Complete {}) = map (showSequencesD2 state) $ SL.toList $ from state
showSequencesD1 state@(Scanned  {}) = map (showSequencesD3 state) $ SL.toList $ prev state

showSequencesD2 :: Show tok => State tok -> State tok -> [String]
showSequencesD2 state from1 = showTree (map (showSequencesD3 state) $ SL.toList $ prev from1) ""

showSequencesD3 :: Show tok => State tok -> State tok -> [String]
showSequencesD3 state prev1 = showTree (showSequencesD1A prev1) ""

showSequencesD1A :: Show tok => State tok -> [[String]]
showSequencesD1A state@(Predict  {}) = [[show (item state) ++ "==>"]]
showSequencesD1A state@(Complete {}) = map (showSequencesD2A state) $ SL.toList $ from state
showSequencesD1A state@(Scanned  {}) = map (showSequencesD3A state) $ SL.toList $ prev state

showSequencesD2A :: Show tok => State tok -> State tok -> [String]
showSequencesD2A state from1 = showTree (map (showSequencesD3A state) $ SL.toList $ prev from1) (show $ item state)

showSequencesD3A :: Show tok => State tok -> State tok -> [String]
showSequencesD3A state prev1 = showTree (showSequencesD1A prev1) ""


showVTree lines1 =
               ["  /",
                " / ",
                "/  "]
            ++ intercalate
               ["|  ",
                "|  ",
                "|  "]
                lines1
              
--showVTree lines1 = 

showTree :: [[String]] -> String -> [String]
showTree lines1 line4 = let
   lines2 = intercalate [""] lines1
   n = length lines2
   k = 0
   nh = (length (head lines1) - 1) `div` 2
   nl = (length (last lines1) - 1) `div` 2
   i = (n - nh - nl - 1) `div` 2
   lines3 = replicate nh "" ++
            map (\x ->  replicate (x+k) ' ' ++ "\\") [0..i-1] ++
                        [replicate (i+k) ' ' ++ if null line4 then line4 else line4 ++ "==>"] ++
            map (\x ->  replicate (x+k) ' ' ++ "/" ) [i-1, i-2..0] ++
            replicate nl ""

   shiftRight = maximum $ zipWith showTree2 lines2 lines3
   in zipWith (showTree1 shiftRight) lines2 lines3
      --else zipWith (++) lines1 [line3 ++ replicate c '=' ++ ">"]

showTree1 shiftRight line2 line3 = let
   l3 = length (takeWhile (==' ') line3)

   in if not (null line2) && last line2 /= ' '
      then line2 ++ replicate (l3 - length line2 + shiftRight) '=' ++ drop l3 line3
      else line2 ++ replicate (l3 - length line2 + shiftRight) ' ' ++ drop l3 line3

showTree2 line2 line3 = let
   l2 = length line2
   l3 = length (takeWhile (==' ') line3)

   in l2 - l3
   

showTree3 lines1 = let
   n = length lines1
   i = (n - 1) `div` 2
   x = 3
   lines2 =   (replicate i (replicate x ' ') ++
                           [replicate x '='] ++
               replicate i (replicate x ' '))
   in zipWith (++) lines1 lines2


showState state = unlines $ showState1 state

showState1 comp@(Complete {}) = concatMap (showState2  comp) $ SL.toList $ from comp
showState1 pred@(Predict  {}) =       map (showState2A pred) $ SL.toList $ from pred

showState2 comp from1 = let
   from2 = head $ SL.toList $ from from1

   in map (showState3 comp from1) $ SL.toList $ prev from1

showState3 comp from1 prev1 = "COMPLETE " ++ show (item prev1) ++ " \\\\ " ++ show (item from1) ++ " ---> " ++ show (item comp)

showState2A pred from1 = "PREDICT " ++ show (item from1) ++ " ---> " ++ show (item pred)
{-
showState2A comp from1 = let
   from2 = head $ SL.toList $ from from1
   
   in map (showState3 comp from1 from2) $ prev from2

showState3A comp from1 from2 prev1  
      
      
      [[ , comp],
         [



showState1 state@(Predict  {}) = [[from state], 
                                 [state]]

showState1 state@(Complete {}) = [[prev $ from state,          state],
                                 [prev $ from $ from state,   from $ from state]]  -- from $ from state IS A SINGLETON, WHAT WOULD HAVE BEEN from state

showState1 state@(Scanned  {}) = [[prev $ state,               state],

showState1 (head $ SL.toList $ from state) ++ ["   ^   ", "   |   ", "   |   ", show $ item state]
showState3 state = show (head $ SL.toList $ prev state) ++ " <----- " ++ show (item state) : ["   |   ", "   |   ", "   v   "] ++ showState1 (head $ SL.toList $ from state)

showState2 state = show (item state)
-}
{-

predictZ n states stateOfItem = foldr (predict1 n) stateOfItem states

predictY n s stateOfItem = let
      items = predict2 n $ item s
      in foldr (\i soi -> case M.lookup i soi of
                        Nothing -> M.insert i (Predict i [s]       ) soi
                        Just  p -> M.insert i (Predict i (s:from p)) soi) stateOfItem items

completeZ n [        ] stateOfItem = stateOfItem
completeZ n (s:states) stateOfItem = let
      p1 = makeFrom n s
      items = concatMap (complete3 $ item s) (map item $ prev p1)
      stateOfItemNew =
         foldl (\soi i -> case M.lookup i soi of
                        Nothing -> M.insert i (Complete i [p1]       ) soi
                        Just  c -> M.insert i (Complete i (p1:from c)) soi) stateOfItem items
   in completeZ n states stateOfItemNew
-}


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


data Doc char str = DStr                 str 
                  | DGroup     [Doc char str] 
                  | DSeq       [Doc char str] 
                  | DLines              [str] 
                  | DTab      [[Doc char str]] 
                  | DRows      [Doc char str] -- should be made up of DStrs and VStretches
                  | DCols      [Doc char str] -- should be made up of DStrs and HStretches
                  | HStretch            [str]
                  | VStretch             str
                  | CharStr         char
                  deriving (Eq, Ord, Show)

data Doc2 str = Doc2 {docWidth :: Int, docHeight :: Int, docText :: [str]} deriving (Eq, Ord)

fp p e = format <$> print1 p e

fp2 p e = format <$> print2 (translate p) e

print1 :: RuleR t a -> a -> Maybe (Doc t [t])
print1 (SeqR   as ) e = mergeSeq <$> zipWithM print1 as e
print1 (AltR   as ) e = firstJust1 $ map (\a -> print1 a e) as
print1 (ApplyR a b) e = unapply a e >>= print1 b
print1 (ManyR  a  ) e = mergeSeq <$> mapM (print1 a) e
print1 (BindR  p (g, h)) b = let a = h b in do t1 <- print1 p a; t2 <- print1 (g a) b; return $ DSeq [t1, t2]
print1 (NameR  a b) e = print1 b e
print1 (TokenR t  ) e = ifJust (t == e) $ DStr [t]
print1 AnyTokenR    e = Just $ DStr [e]
--print1 other        e = error $ show other

print2 :: Rule Char -> Dynamic -> Maybe (Doc Char [Char])
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

sizes (DTab tab) = let
   (cws, rhs) = tabSizes tab

   in (sum cws, sum rhs)

tabSizes tab = let 
   sizes1 = map2 sizes tab
   colWidths = map maximum $ map2 fst sizes1
   rowHeights = map maximum $ transpose $ map2 snd sizes1

   in (colWidths, rowHeights)

format2 (DTab tab) = let
   (colWidths, rowHeights) = tabSizes tab

   in zipWith (\cw col -> format3 cw rowHeights col) colWidths tab

format3 cw rh col = zipWith (format4 cw) rh col

format4 w h (DLines   lines1) = map (padRWith1 ' ' w) $ padRWith1 "" h lines1
format4 w h (HStretch lines1) = map (take w . cycle) lines1
format4 w h (VStretch line1 ) = replicate h line1

mergeDocs (DSeq   s) = mergeSeq $ mergeStrs1 $ map mergeDocs s
mergeDocs (DGroup a) = DGroup $ map mergeDocs a
mergeDocs (DStr   s) = DStr s

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

