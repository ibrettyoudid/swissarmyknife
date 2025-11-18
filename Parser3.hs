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
import NewTuple
import qualified SetList as SL
import Iso hiding (foldl, (!!))
import qualified Iso
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
import Data.Text.Internal.Encoding.Fusion (restreamUtf16BE)

data Rule tok =
   Seq [Rule tok]
   | Then (Rule tok) (Rule tok)
   | Alt [Rule tok]
   | Name String (Rule tok)
   | Token tok
   | Range tok tok
   | Many (Rule tok)
   | ManyTill (Rule tok) (Rule tok)
   | Apply (Iso Dynamic Dynamic) (Rule tok)
   | Bind (Rule tok) (Dynamic -> Rule tok, Dynamic -> Dynamic) -- parsing, the iso turns the result of the nested rule into a new rule. printing, we have the result of the new rule but not the new rule itself. we need to use the nested rule and the result to recreate it
   | Return Dynamic
   | StrRes String Dynamic
   | Not (Rule tok)
   | And (Rule tok)
   | Set String (Rule tok)
   | Get String

data IgnoreMe a = IgnoreMe a

data RuleR t r where
   BindR     :: (Typeable a, Typeable b)                        =>  RuleR t a   -> (a -> RuleR t b, b -> a) -> RuleR t b
   ApplyR    :: (Typeable a, Typeable b) => Iso a b             ->  RuleR t a                 -> RuleR t b
   OptionR   :: (Typeable a, Typeable (Maybe a))                =>  RuleR t a                 -> RuleR t (Maybe a)
   EithR     :: (Typeable a, Typeable b, Typeable (Either a b)) =>  RuleR t a   -> RuleR t b  -> RuleR t (Either a s)
   ThenR     :: (Typeable a, Typeable b, Typeable (a :- b))     =>  RuleR t a   -> RuleR t b  -> RuleR t (a :- b)
   (:/)      :: (Typeable a, Typeable b)                        =>  RuleR t a   -> RuleR t b  -> RuleR t  b
   (://)     :: (Typeable a, Typeable b)                        =>  RuleR t a   -> RuleR t b  -> RuleR t  a
   CountR    :: Typeable b                                      =>  RuleR t Int -> RuleR t b  -> RuleR t [b]
   ManyTillR :: Typeable a                                      =>  RuleR t a   -> RuleR t b  -> RuleR t [a]
   ManyR     :: Typeable a                                      =>  RuleR t a                 -> RuleR t [a]
   SeqR      :: Typeable a                                      => [RuleR t a]                -> RuleR t [a]
   AltR      :: Typeable a                                      => [RuleR t a]                -> RuleR t a
   AndR      :: Typeable a                                      => [RuleR t a]                -> RuleR t a
   NotR      :: Typeable a                                      =>  RuleR t a                 -> RuleR t a
   IgnoreR   :: Typeable a                                      =>  RuleR t a                 -> RuleR t (IgnoreMe a)
   AnyTillR  :: Typeable a                                      =>  RuleR t a                 -> RuleR t [t]
   TryR      :: Typeable a                                      =>  RuleR t a                 -> RuleR t a
   BuildR    :: Typeable a                      =>  f           ->  RuleR t a                 -> RuleR t f
   LambdaR   :: Typeable a                      =>  f           ->  RuleR t a                 -> RuleR t a
   CallR     :: Typeable a                      =>  f           ->  RuleR t a                 -> RuleR t a
   NameR     :: Typeable a                      =>  String      ->  RuleR t a                 -> RuleR t a
   PureR     :: Typeable a                      =>  a           ->  RuleR t a
   TokenR    :: (Typeable t, Eq t)              =>  t           ->  RuleR t t
   RangeR    :: Typeable t                      =>  t ->  t     ->  RuleR t t
   AnyTokenR :: Typeable t                                      =>  RuleR t t
   StringR   :: Typeable t                      =>  [t]         ->  RuleR t [t]
   RestR     :: Typeable [t]                                    =>  RuleR t [t]
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
many1 p = cons >$< p >*< many p

sepBy x sep = Parser3.pure []
          <|> cons >$< x >*< many (sep *< x)

groupOf i = sepBy i (text ";") -- <|> aligned i


chainl1 arg op f = Iso.foldl f >$< (arg >*< (many (op >*< arg)))

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
translate (ThenR a b) = Seq [translate a, translate b]
translate (ApplyR a b) = Apply (isod a) $ translate b
translate (NameR a b) = Name a $ translate b
translate (BindR a (b, c)) = Bind (translate a) (\a -> translate (b $ fromDyn1 a), \b -> toDyn $ c $ fromDyn1 b)

parset r t = parseE (translate r) t
{-   
parse (Token a) = a
parse (Ignore a) = 
format (Token a) = a
-}
--    EIso    :: Iso alpha beta -> Rule alpha -> Rule beta
fd f x = do d <- fromDynamic x; r <- f d; return $ toDyn r

isod (Iso f g) = Iso (fd f) (fd g)

totald f g = isod (total f g)

strOfChars :: [Dynamic] -> String
strOfChars = map fromDyn1

charsOfStr :: String -> [Dynamic]
charsOfStr = map toDyn

chars = totald strOfChars charsOfStr

--repl1 r = do n <- St.get; return $ Seq $ replicate (fromDyn n 0 :: Int) r

replParse rule n = SeqR $ replicate n rule

replPrint rule res = length res

repl rule = (replParse rule, replPrint rule)

intiso :: Iso String Int
intiso = total read show

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
   Name  a _ == Name  b _ = a == b
   Token a   == Token b   = a == b
   Range a b == Range c d = (a, b) == (c, d)
   _ == _ = False

instance Ord tok => Ord (Rule tok) where
--   compare (Seqd a b) (Seqd c d) = compare (a, b) (c, d)
   compare (Alt   as ) (Alt   bs ) = compare as bs
   compare (Name  a _) (Name  b _) = compare a b
   compare (Token a  ) (Token b  ) = compare a b
   compare (Range a b) (Range c d) = compare (a, b) (c, d)
   compare (Not     a) (Not     b) = compare a b
   compare (Name   {}) _           = LT
   compare (Seq    {}) (Name   {}) = GT
   compare (Seq    {}) _           = LT
   compare (Alt    {}) (Name   {}) = GT
   compare (Alt    {}) (Seq    {}) = GT
   compare (Alt    {}) _           = LT
   compare (Token  {}) (Name   {}) = GT
   compare (Token  {}) (Seq    {}) = GT
   compare (Token  {}) (Alt    {}) = GT
   compare (Token  {}) _           = LT
   compare (Range  {}) (Name   {}) = GT
   compare (Range  {}) (Seq    {}) = GT
   compare (Range  {}) (Alt    {}) = GT
   compare (Range  {}) (Token  {}) = GT
   compare (Range  {}) _           = LT
   compare (Not    {}) _           = GT
--   compare (Range{}) _ = LT

instance Show tok => Show (Rule tok) where
   show = outershow Nothing

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

ii (Just d) = insertIndex d "."
ii Nothing = id

data State tok = State Int Int (Item tok) deriving (Eq, Ord)

item  (State b c i) = i
from2 (State b c i) = b
to2   (State b c i) = c

instance Show z => Show (State z) where
   show (State b c i) = show i ++ " " ++ unwords (map show [b, c])

data Result = Running | Fail | Pass Dynamic deriving (Eq, Ord, Show)

data Item tok  = Item   (Rule tok) Result Item2 deriving (Eq, Ord)

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
               deriving (Eq, Ord)

result (Item r re i) = re
rule   (Item r re i) = r
item2  (Item r re i) = i

pos2 (ISeq n) = n
pos2 (IAlt n) = n

pos (Item r s i) = pos2 i

pass (Pass _) = True
pass _        = False

passi = pass . result

type ItemSet tok = S.Set (Item tok)

instance Show tok => Show (Item tok) where
   showsPrec p (Item rule res i2) = showItem rule i2 p . shows res
   {-
   showsPrec p (Item (Seq   as ) res (ISeq n)) = enclose (p > 1) $ unwords $ insertIndex n "." $ map show as
   showsPrec p (Item (Alt   as ) res (IAlt n)) = enclose (p > 2) $ intercalate " | " $ map show as
   showsPrec p (Item rule        res Item2   ) = enclose1 1 p rule res
   showsPrec d (Name  a _) = unwords $ ii d [a]
   showsPrec d (Token a  ) = unwords $ ii d [show a]
   showsPrec d (Range a b) = unwords $ ii d ["[" ++ show a ++ ".." ++ show b ++ "]"]
   showsPrec d (Not     a) = "Not " ++ innershow Nothing a
   show (Item r s1 (IAlt n)) = outershow (Just n) r
   show (Item r s1 _) = outershow Nothing r
   -}

--showItem (Seq as) (ISeq n) = \p rest -> unwords $ insertIndex n "." $ map (\x -> showsPrec p x "") as
showItem (Seq as) (ISeq n) = enclose2 (unwords . insertIndex n ".") as 3
showItem (Alt as) (IAlt n) = enclose2 (intercalate " | ") as 2

showItem rule Item2 = enclose1 (`showsPrec` rule) 1

enclose flag str = if flag then "("++str++")" else str

enclose1 f n p rest = if p > n then '(':f 0 (')':rest) else f n rest

enclose2 f as n p rest = enclose (p > n) $ f $ map (\x -> showsPrec p x "") as

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

--expr = Name "expr" $ Alt [expr `Then` Token '+' `Then` expr], Token 'a']

test = parseED (Seq [Not (Token 'b'), Token 'a']) "a"

--p s t = tree s $ parseE s t

{-
pD s t = do
    states <- parseED s t
    tableZ t $ concatMap S.toList states
    tableZ t $ filter isCompleteZ $ concatMap S.toList states
    return $ tree s states
-}
parseE r t =
   let
      items  = predict (S.singleton $ State 0 0 (start r))
      states = parseE0 [items] t items [1 .. length t]
      done   = mapMaybe (\s -> ifJust (from2 s == 0 && rule (item s) == r && passi (item s)) (result (item s))) (S.toList $ last states)
   in if length states == length t && done /= []
         then done
         else trace (tableZ t $ concatMap S.toList states) []

parseED r t = do
   items <- predictD (SL.singleton $ State 0 0 (start r))
   parseE0D [items] t items [1 .. length t]

parseE0 states _ items [] = states
parseE0 states t items (c : ks) = let
   itemsnew = parseE1 states t c items
   in parseE0 (states ++ [itemsnew]) t itemsnew ks

parseE0D states _ items [] = return states
parseE0D states t items (c : ks) = do
   itemsnew <- parseE1D states t c items
   parseE0D (states ++ [itemsnew]) t itemsnew ks

-- scanM :: Monad m => (m [a] -> b -> m a) -> m [a] -> [b] -> m [a]
scanM f z = foldl (\a b -> do ra <- a; rr <- f (last ra) b; return $ ra ++ [rr]) (return [z])

data States t = States [State t] (I.IntMap (S.Set (State t)))

putss newlist = do
   States all bytok <- St.get
   St.put $ States (all ++ newlist) (foldr fu bytok newlist)

fu s m = I.insert (to2 s) (S.insert s (fromMaybe S.empty (I.lookup (to2 s) m))) m

parseE1 states t c items = let
   scan1 = scan c t items
   comp1 = complete states $ S.fromList scan1
   in if c < length t
      then predict comp1
      else comp1

parseE1D states t c items = do
   let scan1 = scanA c t items
   putStrLn $ "scan1=" ++ show scan1
   comp1 <- completeD states $ SL.fromList scan1
   putStrLn $ "comp1=" ++ show comp1
   if c < length t
      then do
         pred1 <- predictD comp1
         putStrLn $ "pred1=" ++ show pred1
         return pred1
      else return comp1

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
      new     = f done current
      newdone = SL.union done new
      newnew  = new SL.\\ done
    in
      if SL.null newnew then done else closureA1 f newdone newnew

closureD f items = closureD1 f items items

closureD1 f done current = do
   putStrLn $ replicate 80 '='
   (a, newdone) <- f done current
   let newnew = SL.fromList $ catMaybes a
   if SL.null newnew then return done else closureD1 f newdone newnew

process f old current = foldr S.union S.empty $ S.map (S.fromList . f) current

processA f old current = St.runState (mapM insertA $ concatMap (\x -> map (x,) $ f x) $ SL.list current) old

processD f old current = St.runStateT (mapM insertD $ concatMap (\x -> map (x,) $ f x) $ SL.list current) old

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

predict :: Ord tok => S.Set (State tok) -> S.Set (State tok)
predict = closure (process predict1)

predictD items = closureD (processD predict1) items

--paux (Seq  as ) q = [as !! q | q < length as]
paux (Alt  as ) 0 = as
paux (Name a b) 0 = [b]
--paux (ManyTill a b) 0 = [a, b]
paux _ _ = []

predict1 (State _ c i) = map (State c c) $ predict2 i

predict2 (Item (Seq    as   ) Running (ISeq n)) = [start (as !! n)]
predict2 (Item (Alt    as   ) Running (IAlt 0)) = [start a | a <- as]
predict2 (Item (Many   a    ) Running _       ) = [start a]
predict2 (Item (Name   a b  ) Running _       ) = [start b]
predict2 (Item (Apply  a b  ) Running _       ) = [start b]
predict2 (Item (Set    a b  ) Running _       ) = [start b]
predict2 (Item (Not    a    ) Running _       ) = [Item (Not a) (Pass $ toDyn ()) Item2, start a]
predict2 (Item (Bind   a b  ) Running _       ) = [start a]
predict2 (Item (Return a    ) Running _       ) = [Item (Return a) (Pass a) Item2]
--predict2 (Item (Get   a  ) t _) = [Item (Get a) (Pass $ lookup a) Item2]
predict2 (Item {}) = []

--start r@(Not a) = Item r (Pass $ toDyn ()) Item2
start r = Item r Running $ start1 r

start1 (Seq  a) = ISeq 0
start1 (Alt  a) = IAlt 0
start1 (Many a) = IMany []
start1 _ = Item2

scan c t items = mapMaybe (scan1 c (t !! (c - 1))) $ S.toList items

scanA c t items = mapMaybe (scan1 c (t !! (c - 1))) $ SL.list items

scan1 c ch (State j _ t) = scan2 c ch j c t

scan2 c ch j _ (Item r Running i2) = Just $ State j c $ Item r (if saux ch r then Pass $ toDyn ch else Fail) i2
scan2 c ch _ _ _ = Nothing

saux ch (Token c  ) = ch == c
saux ch (Range c d) = ch >= c && ch <= d
saux _ _ = False

complete states = closure (\old -> process (complete1 (states ++ [old])) old)

completeD states = closureD (\old -> processD (complete1D (states ++ [old])) old)

complete1 states state =
   concatMap (complete2 state) $ S.toList $ states !! from2 state

complete1D states state =
   concatMap (complete2 state) $ SL.list $ states !! from2 state

complete2 (State b c sub) (State a b1 main) =
   if result sub /= Running && result main == Running && subitem main sub
      then map (State a c) $ complete3 main sub
      else []

complete3 main@(Item r@(Seq as) s (ISeq n)) sub
  | result sub == Fail = [Item r Fail         (ISeq  n     )]
  | n + 1 == length as = [Item r (result sub) (ISeq (n + 1))]
  | otherwise          = [Item r Running      (ISeq (n + 1))]

complete3 main@(Item x@(Alt as) q (IAlt n)) sub
  | passi sub     = [Item x (result sub) (IAlt  n     )]
  | n < length as = [Item x Running      (IAlt (n + 1))]
  | otherwise     = [Item x (result sub) (IAlt  n     )]

complete3 main@(Item x@(Name d e) q i2) sub = [Item x (result sub) i2]

complete3 main@(Item x@(Apply d e) q _) sub =
   case result sub of
      Pass res ->
         case x of
            Apply iso _ ->
               case apply iso res of
                  Just j  -> [Item x (Pass j) Item2]
                  Nothing -> [Item x  Fail    Item2]
      Fail -> [Item x Fail Item2]

complete3 main@(Item x@(Not d) q i2) sub = [Item x (case result sub of { Pass p -> Fail; Fail -> Pass (toDyn ()) } ) Item2]

complete3 main@(Item x@(Bind a (b, c)) q _) sub =
   case result sub of
      Pass res ->
         case b res of
            r2 -> [start r2]
      Fail -> [Item x Fail Item2]

complete3 main@(Item x@(Many a) q (IMany done)) sub =
   Item x (Pass $ toDyn done) Item2 :
   case result sub of
      Pass res -> [Item x Running (IMany $ done ++ [res])]
      Fail     -> []



caux (Seq as) q y = as !! q == y
caux (Alt as) q y = y `elem` as
caux (Name a b) q y = b == y
--caux (ManyTill a b) q y = a == y
caux _ _ _ = False

subitem (Item (Seq   as ) _ (ISeq n)) ch = as !! n == rule ch
subitem (Item (Then  a b) _ (ISeq n)) ch = case n of { 0 -> a == rule ch; 1 -> b == rule ch }
subitem (Item (Alt   as ) _ _       ) ch = rule ch `elem` as
subitem (Item (Name  a b) _ _       ) ch = b == rule ch
subitem (Item (Apply a b) _ _       ) ch = b == rule ch
subitem (Item (Not   a  ) _ _       ) ch = a == rule ch
subitem (Item (Many  a  ) _ _       ) ch = a == rule ch
subitem (Item {                    }) ch = False

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
-}

children states (State f t i) = do
    s1@(State f1 t1 i1) <- S.toList $ states !! t
    if subitem i i1 && pass (result i1)
         then do
             s2@(State f2 t2 i2) <- S.toList $ states !! f1
             if rule i2 == rule i && f2 == f && pos i2 == pos i - 1
                  then if pos i2 > 0 then map (s1:) $ children states s2 else [[s1]]
             else []
         else []

data Tree z = Tree (State z) [Tree z] | Trees [Tree z] deriving (Eq, Ord)

tree start states =
   let
      [end] = filter ((0 ==) . from2) $ S.toList $ last states
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

print1 :: forall t a. RuleR t a -> a -> Maybe (Doc [t])
print1 (SeqR  as)   e = mergeSeq <$> zipWithM print1 as e
print1 (AltR  as)   e = firstJust1 $ map (\a -> print1 a e) as
print1 (ApplyR a b) e = unapply a e >>= print1 b
print1 (ManyR a)    e = mergeSeq <$> mapM (print1 a) e
print1 (BindR  p (g, h)) b = let a = h b in do t1 <- print1 p a; t2 <- print1 (g a) b; return $ DSeq [t1, t2]
print1 (NameR  a b) e = print1 b e
print1 (TokenR t)   e = ifJust (t == e) $ DStr [t]
print1 AnyTokenR    e = Just $ DStr [e]

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

fp p e = format <$> print1 p e

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

