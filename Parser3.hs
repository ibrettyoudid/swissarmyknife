{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{- HLINT ignore "Use tuple-section" -}

module Parser3 (
   module Parser3,
   module Parser3Types
)
where

-- import Rule
-- import Syntax3 hiding (foldl, foldr)
-- import Shell (ch)

import Favs hiding (indent1, indent2)
import Iso2 hiding (foldl, (!!))
import HashDyn hiding (Apply, expr)
import qualified MyPretty2
import NewTuple hiding (apply)
import Parser3Types
import qualified SetList as SL
import qualified PosItems as PI
import qualified PosItemSeq as PIS

import Control.Monad
import Control.Monad.State
import qualified Data.IntMap as I
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Typeable
import Debug
import Debug.Trace
import GHC.Stack
import System.Random (RandomGen(next))

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
SeqR a >*<         b = SeqR (a ++ [b])
a         >*< SeqR b = SeqR (a:b)
a         >*<         b = SeqR [a, b]
-}

AltR a <|> AltR b = AltR (a ++ b)
AltR a <|> b = AltR (a ++ [b])
a <|> AltR b = AltR (a : b)
a <|> b = AltR [a, b]

(>$<) :: (Typeable a, Typeable b) => Iso a b -> RuleR t a -> RuleR t b
(>$<) = ApplyR

(<=>) :: (Typeable a) => String -> RuleR t a -> RuleR t a
(<=>) = NameR

token :: (Typeable t, Eq t) => t -> RuleR t t
token = TokenR

text xs = SeqR $ map token xs

anytoken :: (Typeable t) => RuleR t t
anytoken = AnyTokenR

pure :: (Typeable a) => a -> RuleR t a
pure = PureR

many :: (Typeable a) => RuleR t a -> RuleR t [a]
many = ManyR 0 0

many1 :: (Typeable a) => RuleR t a -> RuleR t [a]
many1 = ManyR 1 0

sepBy x sep = Parser3.pure [] <|> icons >$< x >*< many (sep *< x)

lc = PosR

groupOf i = sepBy i (text ";") -- <|> aligned i

aligned i = ialign >$< many1 (whiteSpace *< PosR >*< i)

ialign :: Iso [(Int, Int) :- c] [(Int, Int) :- c]
ialign = Iso "ialign" f g
   where
      f ((p :- i) : is) = ifJust (all (\(p1 :- i1) -> snd p == snd p1) is) ((p :- i) : is)
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
--translate1 :: (Typeable a) => RuleR t a -> Rule t
translate r = detranslate r $ translate1 r

translate1 :: Typeable a => RuleR t a -> Rule t
translate1 (AltR       as) = Alt $ map translate as
translate1 (SeqR       as) = Seq $ map translate as
translate1 (ThenR     a b) = Seq [translate a, translate b]
translate1 (a :/ b       ) = Seq [translate a, translate b]
translate1 (a :// b      ) = Seq [translate a, translate b]
translate1 (ApplyR    a b) = translate b
translate1 (NameR     a b) = Name a $ translate b
translate1 (NameStubR   a) = NameStub a
translate1 (BindR a (b, c)) = Bind (translate a) (\a -> translate (b $ fromDyn1 a), \b -> toDyn $ c $ fromDyn1 b)
translate1 (TokenR      a) = Token a
translate1 AnyTokenR = AnyToken
translate1 (RangeR    a b) = Range a b
translate1 (ManyR   a b c) = Many a b $ translate c
translate1 (PureR       a) = Return $ toDyn a
translate1 PosR = Pos
-- translate (OptionR a) = Option $ translate a
-- translate (EithR a b) = Eith (translate a) (translate b)
-- translate (CountR n a) = Count (translate n) (translate a)
translate1 (ManyTillR a b c d) = ManyTill a b (translate c) (translate d)
translate1 (AndR      a b) = error "AND" -- And (translate a) (translate b)
translate1 (NotR        a) = Not (translate a)
translate1 (IgnoreR     a) = Ignore (translate a)
-- translate other = error $ show $ (unsafeCoerce other :: RuleR
translate1 (AnyTillR a b c) = error "ANYTILL"
translate1 (TryR        a) = error "TRY"
translate1 (StringR     a) = Return $ toDyn a
translate1 RestR = error "REST"
translate1 other = error "BAD"

detranslate :: forall t a. Typeable a => RuleR t a -> Rule t -> Rule t
detranslate (AltR     as) = id
detranslate (SeqR     as) = Apply (Iso "seqR"  (\d -> let list = fromDyn1 d :: [HashDyn] in Just $ toDyn $ (map fromDyn1 list :: a)) Just)
detranslate (ThenR   a b) = Apply (Iso "thenR" (\d -> let list = fromDyn1 d :: [HashDyn] in Just $ toDyn $ ((fromDyn1 (list !! 0) :- fromDyn1 (list !! 1)) :: a)) Just)
detranslate (ApplyR  a b) = Apply (isod a)
detranslate (ManyR a b c) = Apply (Iso "manyR" (\d -> let list = fromDyn1 d :: [HashDyn] in Just $ toDyn $ (map fromDyn1 list :: a)) Just)
detranslate (a :/ b     ) = Apply isnd
detranslate (a :// b    ) = Apply ifst
detranslate (_          ) = id
{-
    EithR       :: (Typeable a, Typeable b, Typeable (Either a b)) =>   RuleR t a     -> RuleR t b     -> RuleR t (Either a s)
    CountR      :: Typeable b                                                         =>   RuleR t Int -> RuleR t b   -> RuleR t [b]
    ManyTillR   :: Typeable a                                                         =>   RuleR t a   -> RuleR t b   -> RuleR t [a]
    AndR        :: Typeable a                                                         =>  [RuleR t a]                        -> RuleR t a
    NotR        :: Typeable a                                                         =>   RuleR t a                         -> RuleR t a
    IgnoreR     :: Typeable a                                                         =>   RuleR t a                         -> RuleR t (IgnoreMe a)
    AnyTillR    :: Typeable a                                                         =>   RuleR t a                         -> RuleR t [t]
    TryR        :: Typeable a                                                         =>   RuleR t a                         -> RuleR t a
    BuildR      :: Typeable a                                 =>   f                  ->    RuleR t a                         -> RuleR t f
    LambdaR     :: Typeable a                                 =>   f                  ->    RuleR t a                         -> RuleR t a
    CallR       :: Typeable a                                 =>   f                  ->    RuleR t a                         -> RuleR t a
    NameStubR   :: Typeable a                                 =>   String             ->    RuleR t a
    StringR     :: Typeable t                                 =>   [t]                ->    RuleR t [t]
    RestR       :: Typeable [t]                                                       =>    RuleR t [t]
-}
{-
translate :: Typeable a => RuleR t a -> Rule t
translate (AltR as) = Alt $ map translate as
translate (ThenR a b) = Seq [translate a, translate b]
translate (ApplyR a b) = Apply (isod a) $ translate b
translate (NameR a b) = Name a $ translate b
translate (BindR a (b, c)) = Bind (translate a) (\a -> translate (b $ fromDyn1 a), \b -> toDyn $ c $ fromDyn1 b)

translate1 :: Typeable a => M.Map String (IORef (RuleR t a)) -> RuleR t a -> IO (Rule t)
translate1 e (AltR    as)          = Alt $ mapM (translate e) as
translate1 e (ThenR   a b)         = do
                                    ra <- translate e a
                                    rb <- translate e b
                                    return $ Seq [ra, rb]
translate1 e (ApplyR a b)         = Apply (isod a) <$> translate e b
translate1 e (NameR   a b)         = Name a <$> translate e b
translate1 e (BindR   a (b, c)) = do
    ra <- translate e a

    return $ Bind (translate e a) (\a -> translate (b $ fromDyn1 a), \b -> toDyn $ c $ fromDyn1 b)
-}

doLookups env start = evalState (doLookups2 start) (M.map doLookups1 env)

doLookups1 r = (r, 0, r)

getEnv n = M.lookup n <$> get

setEnv n v = modify (M.insert n v)

doLookups2 (NameStub a) = do
   av <- getEnv a
   case av of
      Just (s, 0, u) -> do
         -- we have to use the T.A.R.D.I.S. for this
         e <- get
         let (r, e2) = runState (doLookups2 s) (M.insert a (s, 1, r) e)
         put e2
         return $ Name a r
      Just (s, 1, r) -> return r
doLookups2 (Apply iso x) = Apply iso <$> doLookups2 x
doLookups2 (Alt xs) = Alt <$> mapM doLookups2 xs
doLookups2 (Seq xs) = Seq <$> mapM doLookups2 xs
doLookups2 (Many a b x) = Many a b <$> doLookups2 x
-- doLookups2 (SetR   name   x   ) = SetR name    $ doLookups2 f x
-- doLookups2 (SetM names x   ) = SetM names $ doLookups2 f x
doLookups2 (And xs) = And <$> mapM doLookups2 xs
doLookups2 (Not x) = Not <$> doLookups2 x
-- doLookups2 (            x :+ y) = doLookups2 f x :+ doLookups2 f y
-- doLookups2 (Count         x y) = Count            x (doLookups2 f y)
doLookups2 (Name name x) = Name name <$> doLookups2 x
-- doLookups2 (Ignore       x   ) = Ignore       $ doLookups2 f x
-- doLookups2 (Try            x   ) = Try            $ doLookups2 f x
doLookups2 (ManyTill a b x y) = do
   xr <- doLookups2 x
   yr <- doLookups2 y
   return $ ManyTill a b xr yr
doLookups2 (Bind a (b, c)) = do
   ar <- doLookups2 a
   return $ Bind ar (b, c)
doLookups2 other = return other

-- doLookups2

{-
state
0       initial state, no expanded rules
1       we have started expanding this rule
2       we have finished expanding this rule, 3rd part is the dot
3..    only triggered if we try to lookup a rule while currently expanding it
-}
{-
parse (Token a) = a
parse (Ignore a) =
format (Token a) = a
-}
--      EIso      :: Iso alpha beta -> Rule alpha -> Rule beta
fd f x = do d <- fromDynamic x; r <- f d; return $ toDyn r

isod (Iso n f g) = Iso (n ++ "d") (fd f) (fd g)

totald n f g = isod (total n f g)

ifst = Iso "ifst" f g
   where
      f d = do
         l <- fromDynamic d :: Maybe [HashDyn]
         return $ l !! 0
      g d = Just $ toDyn [d, undefined]

isnd = Iso "isnd" f g
   where
      f d = do
         l <- fromDynamic d :: Maybe [HashDyn]
         return $ l !! 1
      g d = Just $ toDyn [undefined, d]

-- iconsd = isod icons

strOfChars :: [HashDyn] -> String
strOfChars = map fromDyn1

charsOfStr :: String -> [HashDyn]
charsOfStr = map toDyn

chars = totald "strOfChars" strOfChars charsOfStr

-- repl1 r = do n <- get; return $ Seq $ replicate (fromDyn n 0 :: Int) r

repParse rule n = SeqR $ replicate n rule

repPrint rule res = length res

rep rule = (repParse rule, repPrint rule)

intiso :: Iso String Int
intiso = total "intiso" read show

intisod = isod intiso

int1 :: RuleR Char Int
int1 = ApplyR intiso $ SeqR [RangeR '0' '9']

intn n = ApplyR intiso $ SeqR $ replicate n $ RangeR '0' '9'

-- test = int1 >>== repl (RangeR 'a' 'z')
-- icons = isod (\(x, xs) -> Just (x:xs)) (\(x:xs) -> Just (x, xs))

{-
simulate this:
do
    n <- int
    rep n anychar

which is

int >>= (\n -> rep n anychar)

-}

-- innershow d (Seqd   a b) = unwords $ ii d [innershow Nothing a, innershow Nothing b]
innershow :: (Show tok) => Maybe Int -> Rule tok -> [Char]
innershow d (Seq as) = unwords $ ii d $ map (innershow Nothing) as
innershow d (Alt as) = unwords $ ii d [intercalate " | " $ map (innershow Nothing) as]
innershow d (Name a _) = unwords $ ii d [a]
innershow d (Token a) = unwords $ ii d [show a]
innershow d (Range a b) = unwords $ ii d ["[" ++ show a ++ ".." ++ show b ++ "]"]
innershow d (Not a) = "Not " ++ innershow Nothing a

-- outershow d r@(Seqd   a b) = "Seqd " ++ innershow d r
outershow d r@(Seq as) = "Seq " ++ innershow d r
outershow d r@(Alt as) = "Alt " ++ innershow d r
outershow d r@(Name a b) = unwords $ ii d [a ++ " -> " ++ innershow Nothing b]
outershow d r@(Token a) = show a
outershow d r@(Range a b) = "[" ++ show a ++ ".." ++ show b ++ "]"
outershow d r@(Not a) = innershow Nothing r

ii (Just d) = insertIndex d "*"
ii Nothing = id

{-
instance Ord z => Ord (PosItem z) where
    compare (PosItem b1 k1 e1) (PosItem j2 k2 e2) = compare (b1, k1, e1) (j2, k2, e2)
-}
digit = Range '0' '9'

lower = Range 'a' 'z'

upper = Range 'A' 'Z'

under = Token '_'

alpha = Alt [upper, lower]

-- num = Many digit
-- alnum = Alt [alpha, digit]
op = Alt [Token '+', Token '-', Token '*', Token '/']

-- ident = Name "ident" $ cons (Alt [alpha, under]) ident1
-- ident1 = Many (Alt [alpha, under, digit])

sexpr = Name "sexpr" $ Alt [Seq [sexpr, Token '+', sexpr], Range 'a' 'z']

test = parseEE sexpr "a+b+c+d"

mintest = parseEE (Seq [Token 'a', Token 'b']) "ab"

num1 = parseEE (Many 1 0 $ Range '0' '9') "1234"

linePos = M.fromList . zip [(0 :: Int) ..] . (0 :) . map (+ 1) . findIndices (== '\n')

-- p s t = tree s $ parseE s t

getLC x lps = let Just (n, s) = M.lookupLE x lps in (n, x - s)

{-
pD s t = do
    stateslist <- parseED s t
    tableZ t $ concatMap S.toList stateslist
    tableZ t $ filter isCompleteZ $ concatMap S.toList stateslist
    return $ tree s stateslist
-}

parseT :: Typeable a => RuleR Char a -> [Char] -> IO [a]
parseT r t = do
   res <- parseEE (translate r) t
   return $ map fromDyn1 res

parseEnv env r t = parseEE (doLookups env r) t

parseE r t =
   let
      lps      = linePos t
      init     = SL.singleton $ maket 0 0 $ start r
      items    = predictA (0 :: Int, 0 :: Int) init
      stateslist = parseE0 lps [items] t items [1 .. length t] :: [SL.SetList (PosItemFT _)]
      done     = mapMaybe (\s -> ifJust (from s == 0 && rule (item s) == r && passi (item s)) (dot (item s))) (SL.list $ last stateslist)
   in if length stateslist == length t && done /= []
            then done
            else trace (table t $ PIS.fromElems $ map PI.fromSL stateslist) []

parseED r t = do
   let lps = linePos t
   let init = SL.singleton $ maket 0 0 $ start r :: SL.SetList (PosItemFT _)
   items <- comppredD lps [SL.singleton $ maket 0 0 (start r)] (SL.singleton $ maket 0 0 (start r))
   stateslist <- parseE0D lps [items] t items [1 .. length t] :: IO [SL.SetList (PosItemFT _)]
   let done = mapMaybe (\s -> ifJust (from s == 0 && rule (item s) == r && passi (item s)) (dot (item s))) (SL.list $ last stateslist)
   putStrLn (table t $ PIS.fromElems $ map PI.fromSL stateslist)
   return done
{-
parseEM r t = do
   let lps = linePos t
   let init = SL.singleton $ maket 0 0 $ start r
   let inits = PI.fromList [init]
   let initseq = PIS.fromList [(0, inits)]
   items <- comppredD lps [SL.singleton $ maket 0 0 (start r)] (SL.singleton $ maket 0 0 (start r))
   stateseq <- execPosItemT (zipWithM parseE0M [0 .. length t - 1] t) initseq :: IO [SL.SetList (PosItemFT _)]
   let done = mapMaybe (\s -> ifJust (from s == 0 && rule (item s) == r && passi (item s)) (dot (item s))) (SL.list $ last stateslist)
   putStrLn (table t stateseq)
   return done
-}
parseE0 lps stateslist _ items [] = stateslist
parseE0 lps stateslist t items (c : ks) =
   let itemsnew = parseE1A lps stateslist t c items
   in parseE0 lps (stateslist ++ [itemsnew]) t itemsnew ks

parseE0D lps stateslist _ items [] = return stateslist
parseE0D lps stateslist t items (c : ks) = do
   itemsnew <- parseE1D lps stateslist t c items
   parseE0D lps (stateslist ++ [itemsnew]) t itemsnew ks

parseE0M n t = do
   return []
   

-- scanM :: Monad m => (m [a] -> b -> m a) -> m [a] -> [b] -> m [a]
foldMH f z = foldl (\a b -> do ra <- a; rr <- f ra b; return rr) (return z)

-- data PosItems t = PosItems [PosItem t] (I.IntMap (S.Set (PosItem t)))
{-
putss newlist = do
    PosItems all bytok <- get
    put $ PosItems (all ++ newlist) (foldr fu bytok newlist)
-}
fu s m = I.insert (to s) (S.insert s (fromMaybe S.empty (I.lookup (to s) m))) m

parseE1A lps stateslist t c states =
   let scan1 = scanA c t states
       comp1 = completeA stateslist $ SL.fromList scan1
   in if c < length t
            then predictA (getLC c lps) comp1
            else comp1

parseE1D lps stateseq t c states = do
   putStrLn $ "SCAN " ++ take 75 (cycle "/\\")
   let scan1 = scanA c t states
   putStrLn $ "scan1=" ++ show scan1
   comp1 <- comppredD (getLC c lps) stateseq $ SL.fromList scan1
   -- putStrLn $ "comp1=" ++ show comp1
   return comp1

parseEE r t = do
   let
      lps = linePos t
      positem = maket 0 0 $ start r :: PosItemFT Char
      active = SL.singleton positem
      init = PIS.singleton $ positem :: PIS.PosItemSeq Char (PosItemFT Char)

      comppredE n active = do
         liftIO $ putStrLn $ "comppredE active="++show active
         completed <- completeE2 n SL.empty active
         predicted <- predictE2 n SL.empty completed
         completeE1 n predicted

      completeE1 n active 
         | SL.null active = return SL.empty
         | otherwise = do
            liftIO $ putStrLn $ "   completeE1 active="++show active
            completed <- completeE2 n SL.empty active
            predictE1 n completed

      predictE1 n active 
         | SL.null active = return SL.empty
         | otherwise = do
            liftIO $ putStrLn $ "   predictE1 active="++show active
            predicted <- predictE2 n SL.empty active
            completeE1 n predicted

      completeE2 n collect active 
         | SL.null active = return collect
         | otherwise = do
            liftIO $ putStrLn $ "      completeE2 active="++show active
            completed <- completeE3 n active
            completeE2 n (SL.concat [collect, completed]) completed

      predictE2 n collect active
         | SL.null active = return collect
         | otherwise = do
            liftIO $ putStrLn $ "      predictE2 active="++show active
            predicted <- predictE3 n active
            predictE2 n (SL.concat [collect, predicted]) predicted

      completeE3 n active = do
         SL.concat <$> (SL.mapM (\subpositem -> do
            liftIO $ putStrLn $ "         completeE3 sub="++show subpositem
            piseq <- get
            let
               subfrom = from subpositem
               subitem = item subpositem
               subdot  = dot  subitem
               subrule = rule subitem
            SL.fromList . concat <$> (SL.mapM (\mainpositem -> do
               liftIO $ putStrLn $ "         completeE3    main="++show mainpositem
               let
                  mainfrom = from mainpositem
                  mainitem = item mainpositem
                  maindot  = dot  mainitem
                  mainrule = rule mainitem
                  
                  f is = g $ Item mainrule is
                  g i  = PIS.insertC (maket mainfrom n i) subpositem
               if not (finished maindot) && finished subdot && anysubitem mainitem subitem
                  then do
                     --liftIO $ putStrLn $ show (mainrule, maindot)
                     case (mainrule, maindot) of
                        (Seq as, ISeq x) -> if
                           | subdot == Fail  -> f $ Fail
                           | x + 1 == length as ->
                              if pass subdot
                                 then do
                                    let
                                       seqs = childrenE piseq mainpositem
                                       oldasts = map2 (asts . dot . item) seqs
                                       ret = map crossList oldasts
                                       cret = concat ret
                                       newasts = map toDyn $ map reverse $ concat $ crossWith (:) (asts subdot) cret
                                    liftIO $ putStrLn $ "         seqs="++show seqs++" oldasts="++show oldasts++" ret="++show ret++" cret="++show cret++" pass="++show (asts subdot)++" newasts="++show newasts
                                    f $ Pass newasts
                              else
                                 f $ Fail
                           | otherwise          -> f $ ISeq (x + 1)
                        (Alt as, IAlt x) -> f $ if
                           | pass subdot -> subdot
                           | x < length as  -> IAlt (x + 1)
                           | otherwise      -> Fail
                        (Name {}, Running) -> f subdot
                        (Apply {}, Running) -> 
                           case subdot of
                              Pass reslist -> do
                                 concat <$> mapM (\res -> f $ case mainrule of
                                    Apply iso _ ->
                                       case apply iso res of
                                          Just j  -> Pass [j]
                                          Nothing -> Fail) reslist
                              Fail -> f Fail
                        (Not {}, Running) -> f $
                           case subdot of 
                              Pass p -> Fail
                              Fail   -> Pass [toDyn ()]
                        (Bind i (j, k), Running) -> 
                           case subdot of
                              Pass reslist -> concat <$> mapM (\res -> g $ start $ j res) reslist
                              Fail -> f Fail
                        (Many minn maxn sr, IMany oldasts) ->
                              if pass subdot
                                 then do
                                    let
                                       newasts = concat $ crossWith (:) (asts subdot) oldasts
                                       maxok = if maxn == 0 then newasts else filter ((<= maxn) . length) newasts
                                       minok = filter ((>= minn) . length) maxok
                                    liftIO $ putStrLn $ "         oldasts="++show oldasts++" pass="++show (asts subdot)++" newasts="++show newasts
                                    r1 <- if not (null maxok) then f $ IMany maxok                        else return []
                                    r2 <- if not (null minok) then f $ Pass $ map (toDyn . reverse) minok else return []
                                    return $ r1++r2
                              else
                                 f $ Fail
                        _ -> error $ "no entry in case for "++show mainrule ++ " " ++ show maindot
                  else
                     return []
               ) (fromMaybe SL.empty $ PI.lookup subrule (piseq PIS.! subfrom)))
            ) $ active)

      predictE3 n active = do
         SL.concat <$> (SL.mapM (\mainpositem -> do
            liftIO $ putStrLn $ "         predictE3 mainpositem="++show mainpositem
            let subitems = predict1 n mainpositem 
            liftIO $ putStrLn $ "         subitems="++show subitems
            r <- mapM (PIS.insertP mainpositem) subitems
            return $ (SL.fromList $ (concat r :: [PosItemFT Char]))) active)

   positemseq <- execStateT (do
      active <- predictE1 0 active
      piseq <- get :: StateT (PIS.PosItemSeq tok positem) IO (PIS.PosItemSeq tok positem)
      liftIO $ putStrLn $ table t piseq
      comppredE 0 active
      mapM_ (\n -> do
         piseq <- get :: StateT (PIS.PosItemSeq tok positem) IO (PIS.PosItemSeq tok positem)
         liftIO $ putStrLn $ table t piseq
         liftIO $ putStrLn $ "SCAN " ++ take 75 (cycle "/\\") ++ " " ++ t
         liftIO $ putStrLn $ replicate (80+n) ' ' ++ "^"
         let scanned = mapMaybe (scan1 n (t !! (n-1))) $ SL.list $ PI.setlist $ piseq PIS.! (n-1)
         mapM (\main -> PIS.insertC main main) scanned
         liftIO $ putStrLn $ "scanned=" ++ show scanned
         comppredE n $ SL.fromList scanned) [1..length t]
      ) init
   
   putStrLn (table t positemseq)
   return $ concat $ mapMaybe (\s -> ifJust (from s == 0 && rule (item s) == r && passi (item s)) (asts $ dot (item s))) (SL.list $ PI.setlist $ positemseq PIS.! length t)

--data Main = Main { lps :: [(Int, Int)], stateseq :: PosItemSeq state tok }

untilM pred mf x = if pred x
                        then return x
                        else mf x >>= untilM pred mf

processA f old current = runState (mapM insertA $ concatMap (\x -> map (x,) $ f x) $ SL.list current) old

-- refoldD f z xs = do (rs, zs) <- unzip <$> zipWithM f (z : zs) xs; return (rs, zs)
refoldD f z [] = return []
refoldD f z (x : xs) = do (a, b) <- f z x; c <- refoldD f b xs; return $ a : c

insertA (v, fv) = do
   old <- get
   if SL.member fv old
      then do
         return Nothing
      else do
         modify (SL.insert fv)
         return $ Just fv

insertD (v, fv) = do
   old <- get
   if SL.member fv old
      then do
         liftIO $ putStrLn $ show v ++ "   ==>   " ++ show fv
         return Nothing
      else do
         liftIO $ putStrLn $ show v ++ "   ==>   " ++ show fv ++ " *** NEW ***"
         modify (SL.insert fv)
         return $ Just fv

predictA n items = closureA (processA (predict1 n)) items

predictD n items = do putStrLn $ "PREDICT " ++ take 72 (cycle "\\/"); closureD (processD (predict1 n)) items

comppredD n stateslist items = closureDD (\old -> processDD (complete1D (stateslist ++ [old])) old) (processD (predict1 n)) items

scan n t items = mapMaybe (scan1 n (t !! n)) $ S.toList items

scanA n t items = mapMaybe (scan1 n (t !! n)) $ SL.list items

completeA stateslist = closureA (\old -> processA (complete1A (stateslist ++ [old])) old)

completeD stateslist states = do putStrLn $ "COMPLETE " ++ take 71 (cycle "\\/"); closureD (\old -> processDD (complete1D (stateslist ++ [old])) old) states

closureA f items = closureA1 f items items

closureA1 f done current = let
   (a, newdone) = f done current
   newnew = SL.fromList $ catMaybes a

   in if SL.null newnew then done else closureA1 f newdone newnew

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
   -- print active1
   (a, done2) <- f done1 active1
   let active2 = SL.fromList $ catMaybes a
   let active3 = SL.union active1 active2
   putStrLn $ "PREDICT " ++ replicate 72 '='
   -- print active3
   (b, done3) <- g done2 active3
   let active4 = SL.fromList $ catMaybes b
   let active5 = SL.union active2 active4
   if SL.null active5 then return done3 else closureDD1 f g done3 active5

processD f old current = runStateT (mapM insertD $ concatMap (\x -> map (x,) $ f x) $ SL.list current) old

processDD f old current = runStateT (do fxs <- mapM (\x -> do fx <- liftIO $ f x; return $ map (x,) fx) $ SL.list current; mapM insertD $ concat fxs) old

-- completeD stateslist state = closureD (\old -> processD (complete1A (stateslist ++ [old])) old) state

-- paux (Seq   as ) q = [as !! q | q < length as]
paux (Alt as) 0 = as
paux (Name a b) 0 = [b]
-- paux (ManyTill a b) 0 = [a, b]
paux _ _ = []

predict1 n st = let c = to st in map (maket c c) $ predict2 n (item st)

predict2 n (Item (Seq     as) (ISeq   j)) = [start (as !! j)]
predict2 n (Item (Alt     as) (IAlt   0)) = [start a | a <- as]
predict2 n (Item (Many a b c) (IMany as)) = [start c] ++ if as == [[]] && a == 0 then [Item (Many a b c) (Pass $ map toDyn as)] else []
predict2 n (Item (Name   a b)  Running  ) = [start b]
predict2 n (Item (Apply  a b)  Running  ) = [start b]
predict2 n (Item (Set    a b)  Running  ) = [start b]
predict2 n (Item (Not      a)  Running  ) = [start a, Item (Not a) (Pass [toDyn ()])]
predict2 n (Item (Bind   a b)  Running  ) = [start a]
predict2 n (Item (Return   a)  Running  ) = [Item (Return a) (Pass [a])]
predict2 n (Item  Pos          Running  ) = [Item Pos (Pass [toDyn n])]
-- predict2 (Item (Get    a   ) t _) = [Item (Get a) (Pass $ lookup a) Item2]
predict2 n (Item {}) = []

-- start r@(Not a) = Item r (Pass $ toDyn ()) Item2
start r = Item r $ start1 r

start1 (Seq  a) = ISeq  0
start1 (Alt  a) = IAlt  0
start1 (Many a b c) = IMany [[]]
start1 _ = Running

scan1 n ch st = scan2 n ch (from st) (item st)

scan2 n ch j (Item r Running) = do sc <- saux ch r; return $ maket j n $ Item r (if sc then Pass [toDyn ch] else Fail)
scan2 n ch _ _ = Nothing

saux ch (Token c) = Just $ ch == c
saux ch (Range c d) = Just $ ch >= c && ch <= d
saux _ _ = Nothing

complete1A stateslist state =
   concatMap (complete2 stateslist state) $ SL.list $ stateslist !! from state

complete2 stateslist substate mainstate = let
   subitem1 = item substate
   main = item mainstate

   in if dot subitem1 /= Running && dot main == Running && subitem main subitem1
            then map (maket (from mainstate) (to substate)) $ complete3 stateslist (rule main) (dot main) mainstate substate
            else []

complete1D stateslist substate = do
   res <- mapM (complete2D stateslist substate) $ SL.list $ stateslist !! from substate
   return $ concat res

complete2D stateslist substate mainstate = do
   -- print (sub, main, subitem main subitem1)
   let subitem1 = item substate
   let main = item mainstate
   if dot subitem1 /= Running && dot main == Running && subitem main subitem1
      then do
         let r = map (maket (from mainstate) (to substate)) $ complete3 stateslist (rule main) (dot main) mainstate substate
         -- print (sub, main, r)
         return r
      else return []

complete3 stateslist r@(Seq as) (ISeq n) mainstate substate
   | dot sub == Fail = [Item r Fail]
   | n + 1 == length as = [Item r res1]
   | otherwise          = [Item r (ISeq (n + 1))]
   where
      sub = item substate
      -- res1 = if passi sub then stateslist !! from substate
      res1 = if passi sub then Pass $ retrieve2 stateslist mainstate substate else Fail
complete3 stateslist r@(Alt as) (IAlt n) mainstate substate
   | passi sub     = [Item r (dot sub)]
   | n < length as = [Item r (IAlt (n + 1))]
   | otherwise     = [Item r Fail]
   where
      sub  = item substate
      main = item mainstate
complete3 stateslist r@(Name {}) Running mainstate substate = [Item r (dot sub)]
   where
      sub  = item substate
      main = item mainstate
complete3 stateslist r@(Apply {}) Running mainstate substate =
   case dot sub of
      Pass reslist -> do
         res <- reslist
         case r of
            Apply iso _ ->
               case apply iso res of
                  Just j  -> [Item r (Pass [j])]
                  Nothing -> [Item r Fail]
      Fail -> [Item r Fail]
   where
      sub  = item substate
      main = item mainstate
complete3 stateslist r@(Not {}) Running mainstate substate = 
   [Item r (case dot sub of 
               Pass p -> Fail
               Fail   -> Pass [toDyn ()])]
      where
         sub  = item substate
         main = item mainstate
complete3 stateslist r@(Bind i (j, k)) Running mainstate substate =
   case dot sub of
      Pass reslist -> do
         res <- reslist
         case j res of
            r2 -> [start r2]
      Fail -> [Item r Fail]
      where
         sub = item substate
         main = item mainstate
complete3 stateslist r@(Many a b sr) Running mainstate substate =
   --    Item x (Pass $ toDyn done) Item2 :
   case dot sub of
      Pass reslist -> do
         res <- reslist
         [Item r Running]
      Fail -> []
   where
      sub  = item substate
      main = item mainstate

{-
retrieve stateslist mainseq n sub = reverse $ retrieve1 stateslist mainseq n sub

retrieve1 stateslist mainseq n sub = let
    prev1 = S.filter (\prevst -> pass (dot $ item prevst) && rule (item prevst) == mainseq !! n) $ SL.set (stateslist !! from sub)
    prev   = only $ S.toList prev1
   in ast (dot $ item sub) : if n > 0 then retrieve1 stateslist mainseq (n-1) prev else []
-}
retrieve2 stateslist mainstate substate = do
   seq <- siblings stateslist mainstate substate
   let seqf = reverse seq
   asts1 <- map (asts . dot . item) seqf
   return $ toDyn asts1

caux (Seq as) q y = as !! q == y
caux (Alt as) q y = y `elem` as
caux (Name a b) q y = b == y
-- caux (ManyTill a b) q y = a == y
caux _ _ _ = False

-- subItem parent child -> Bool
subitem (Item (Seq    as) (ISeq n)) sub = n < length as && as !! n == rule sub
subitem (Item (Then  a b) (ISeq n)) sub = case n of 0 -> a == rule sub; 1 -> b == rule sub
subitem (Item (Alt    as) _) sub = rule sub `elem` as
subitem (Item (Name  a b) _) sub = b == rule sub
subitem (Item (Apply a b) _) sub = b == rule sub
subitem (Item (Not     a) _) sub = a == rule sub
subitem (Item (Many a b c) _) sub = c == rule sub
subitem (Item {           }) sub = False

anysubitem (Item (Seq     as) _) sub = rule sub `elem` as
anysubitem (Item (Then   a b) _) sub = rule sub == a || rule sub == b
anysubitem (Item (Alt     as) _) sub = rule sub `elem` as
anysubitem (Item (Name   a b) _) sub = b == rule sub
anysubitem (Item (Apply  a b) _) sub = b == rule sub
anysubitem (Item (Not      a) _) sub = a == rule sub
anysubitem (Item (Many a b c) _) sub = c == rule sub
anysubitem (Item {            }) sub = False

slength (Seq as) = length as
slength _ = 1

isCompleteZ state = dot (item state) /= Running

{-
predict3 c (Item (Alt   as   ) 0) = [Item a 0 | a <- as]
predict3 c (Item (Seq   as   ) d) = [Item (as !! d) 0 | d < length as]
predict3 c (Item (Name a   b) 0) = [Item b 0]
predict3 c t = []
-}
{-
makeLR e = let
    items = predict (S.singleton $ PosItem e 0 0 0)
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

core1 (PosItem r b c d) = Item r d

makeLRb stateslist items = let
    states1 = scan2 (length stateslist) items
    states2 = map (complete stateslist) states1
   in if True --c < length t
         then map predict states2
         else states2

scan2 c items = map (S.fromList . catMaybes) $ crossWith
    (uncurry scan1)
    (zip [c..] $ S.toList $ S.fromList $ mapMaybe scan3 $ S.toList items)
    (S.toList items)

makeLRc stateslist items = let
    tokens = scan5 items
    oldcore = S.fromList $ map core stateslist
   in foldl (makeLRd items) stateslist tokens

makeLRd items stateslist token = let
    c = length stateslist
    statescore = S.fromList $ map core stateslist
    newstate = S.fromList $ mapMaybe (scan1 c token) $ S.toList items
    {-
    newstates = if S.member (core newstate) statescore
         then stateslist
         else stateslist ++ [newstate]
    -}
   in (stateslist ++) $ map predict $ complete3 stateslist newstate

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
scan3 (PosItem r@(Token c    ) b _ 0) = Just c
--scan3 c (PosItem r@(Range c d) b _ 0) = ifJust (ch >= c && ch <= d) (PosItem r b c 1)
scan3 t = Nothing

scan4 c (PosItem r@(Token ch    ) b _ 0) = Just (ch, PosItem r b c 1)
--scan3 c (PosItem r@(Range c d) b _ 0) = ifJust (ch >= c && ch <= d) (PosItem r b c 1)
scan4 c t = Nothing

children stateslist (PosItem f t i) = do
    s1@(PosItem f1 t1 i1) <- S.toList $ stateslist !! t
    if subitem i i1 && pass (dot i1)
             then do
                  s2@(PosItem f2 t2 i2) <- S.toList $ stateslist !! f1
                  if rule i2 == rule i && f2 == f && pos i2 == pos i - 1
                           then if pos i2 > 0 then map (s1:) $ children stateslist s2 else [[s1]]
                  else []
             else []

-}
retrieveE stateseq mainpositem = do
   seq <- childrenE stateseq mainpositem
   map (asts . dot . item) seq

-- returns all children from the state you give it, back
childrenE positemseq next = do
   sub <- SL.list $ PI.setlist $ positemseq PIS.! to next
   if anysubitem (item next) (item sub) && pass (dot $ item sub)
      then siblingsE positemseq next sub
      else []

siblingsE positemseq next sub = do
   main <- SL.list $ PI.setlist $ positemseq PIS.! from sub
   if rule (item main) == rule (item next) && from main == from next && pos (item main) == pos (item next) - 1
      then do
         more <- if pos (item main) > 0
            then childrenE positemseq main
            else if from sub == from main 
               then [[]]
               else []
         [sub : more]
      else []

children stateslist parent = do
   s1 <- SL.list $ stateslist !! to parent
   if subitem (item parent) (item s1) && pass (dot $ item s1)
      then siblings stateslist parent s1
      else []

siblings stateslist parent s1 = do
   s2 <- SL.list $ stateslist !! from s1
   if rule (item s2) == rule (item parent) && from s2 == from parent && pos (item s2) == pos (item parent) - 1
      then
         if pos (item s2) > 0
            then do
               more <- children stateslist s2
               return $ s1 : more
            else
               [[]]
      else []

-- parents

data Tree z = Tree z [Tree z] | Trees [Tree z] deriving (Eq, Ord)

tree start stateslist =
   let [end] = filter ((0 ==) . from) $ SL.list $ last stateslist
   in tree1 stateslist end

tree1 stateslist end = Tree end $ tree2 $ reverse $ map reverse $ map2 (tree1 stateslist) $ children stateslist end

tree2 [x] = x
tree2 xs = map Trees xs

only [x] = x

instance (Show z) => Show (Tree z) where
   show tree = MyPretty2.format1 1 $ convTree tree

convTree (Tree a b) = MyPretty2.Data (show a) $ map convTree b
convTree (Trees b) = MyPretty2.Data "TREES" $ map convTree b

{-
transeq :: Foldable t => S.Set ETrans -> t a -> (Rule tok) -> [[ETrans]]
>>> p expr "a+a+a"
-}

mergeStrs a b = zipWith (\x y -> if x == ' ' then y else x) a b ++ if length a > length b then drop (length b) a else drop (length a) b

{-
scanlr c t stateslist = S.fromList $ mapMaybe (scan1 c (t !! (c - 1))) $ S.toList stateslist
scanlr c ch (PosItem r@(Token c    ) b _ 0) = ifJust (ch == c) (PosItem r b c 1)
scanlr c ch (PosItem r@(Range c d) b _ 0) = ifJust (ch >= c && ch <= d) (PosItem r b c 1)
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
   if c2 >= d1 && c1 <= d2
      then let
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
-- scan all possible tokens into separate stateslist
-- same token may have multiple meanings, combine them into one state
-- those are your new kernels
-- closure them with completions and predictions
--
-- how kernels are compared for equality defines what sort of parser it is
-- some record little context, some account for a lot, if it accounts for all context it can't do recursive grammars

table str stateseq = tableFT str stateseq $ PIS.domain stateseq

tableFT str stateseq (from, to) = let
   range = PIS.toList $ PIS.range (from, to) stateseq
   shown = map show range
   nums  = map show [from .. to]
   numls = 0 : map length nums

   -- map over token range with inner zipWith over the stateslist filtering the ones that end at that token
   ends  = 0 : map (\n -> maximum $ zipWith (taux1D ends numls from n) shown range) [from .. to]
   --ends  = 0 : map (\n -> maximum $ zipWith (taux1D ends numls from n) shown $ SL.toList $ PIS.setlist $ fromJust $ PIS.lookup n stateslist) [from .. to]
   show1 = zipWith (taux2D ends numls from) shown range
   nums1 = map     (taux3D ends nums  from) [from .. to]
   toks  = map     (taux4D ends str   from) [from .. to - 1]
   axis  = foldr Parser3.mergeStrs "" (nums1 ++ toks)

   in unlines $ axis : show1 ++ [axis]

{-
ends !! 0 = 0
ends !! 1 =
-}
taux1D ends numls fromTok tokn sh st =
   numls !! tokn + let
      f = from st
      t = to st

      in if f == t
            then
               let l = length sh + 2
               in if
                     | t     == tokn -> ends !! (f - fromTok    ) +      div l 2
                     | t + 1 == tokn -> ends !! (f - fromTok + 1) + (l - div l 2)
                     | True -> 0
            else
               let l = length sh + 4
               in if
                     | t     == tokn -> ends !! (f - fromTok + 1) + l
                     | True -> 0

taux2D ends numls fromTok sh st = let
   l = length sh
   f = from st
   t = to st
   in if f == t
            then replicate (ends !! (f - fromTok + 1) - div l 2) ' ' ++ "(" ++ sh ++ ")"
            else let
               l2 = ends !! (t - fromTok + 1) - ends !! (f - fromTok + 1)
               l3 = l2 - l
               l4 = div l3 2
               l5 = l3 - l4

               in replicate (ends !! (f - fromTok + 1) + numls !! (f - fromTok + 1)) ' ' ++ replicate l4 '-' ++ sh ++ replicate l5 '-'

taux3D ends nums from a = replicate (ends !! (a - from + 1) + 1) ' ' ++ nums !! (a - from)

taux4D ends str from a = let
   sh = show $ str !! a
   l = length sh

   in replicate (div (ends !! (a - from + 1) + ends !! (a - from + 2) - l) 2) ' ' ++ sh

data Doc str = DStr str | DGroup [Doc str] | DSeq [Doc str] deriving (Eq, Ord, Show)

data Doc2 str = Doc2 {docWidth :: Int, docHeight :: Int, docText :: [str]} deriving (Eq, Ord)

fp p e = format <$> print1 p e

fp2 :: (Typeable a, Show (Rule Char)) => RuleR Char a -> HashDyn -> Maybe [Char]
fp2 p e = format <$> print2 (translate p) e

print1 :: Ord t => RuleR t a -> a -> Maybe (Doc [t])
print1 (SeqR as) e = mergeSeq <$> zipWithM print1 as e
print1 (AltR as) e = firstJust1 $ map (\a -> print1 a e) as
print1 (ApplyR a b) e = unapply a e >>= print1 b
print1 (ManyR a b c) e = mergeSeq <$> mapM (print1 c) e
print1 (BindR p (g, h)) b = let a = h b in do t1 <- print1 p a; t2 <- print1 (g a) b; return $ DSeq [t1, t2]
print1 (NameR a b) e = print1 b e
print1 (TokenR t) e = ifJust (t == e) $ DStr [t]
print1 AnyTokenR e = Just $ DStr [e]
print1 PosR e = Just $ DStr []
print1 (ThenR a b) (e :- f) = do ta <- print1 a e; tb <- print1 b f; return $ DSeq [ta, tb]
print1 (a :/ b) f = do ta <- print1 a undefined; tb <- print1 b f; return $ DSeq [ta, tb]
print1 (a :// b) e = do ta <- print1 a e; tb <- print1 b undefined; return $ DSeq [ta, tb]
print1 (RangeR a b) e = ifJust (e >= a && e <= b) $ DStr [e]
print1 (PureR a) e = Just $ DStr []
--print1 other            e = error $ show other

print2 :: Rule Char -> HashDyn -> Maybe (Doc [Char])
print2 (Seq as) e = do m <- zipWithM print2 as (fromDyn1 e :: [HashDyn]); return $ mergeSeq m
print2 (Alt as) e = firstJust1 $ map (\a -> print2 a e) as
print2 (Apply a b) e = unapply a e >>= print2 b
print2 (Many a b c) e = mergeSeq <$> mapM (print2 c) (fromDyn1 e :: [HashDyn])
print2 (Bind p (g, h)) b = let a = h b in do t1 <- print2 p a; t2 <- print2 (g a) b; return $ DSeq [t1, t2]
print2 (Name a b) e = print2 b e
print2 (Token t) e = ifJust (t == fromDyn1 e) $ DStr [t]
print2 AnyToken e = Just $ DStr [fromDyn1 e]
print2 other e = error $ show other

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
   b -> DSeq b

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
sapp (Doc2 aw ah at) (Doc2 bw bh (bth : btt)) = let
   ati = init at
   atl = last at

   in ts $ ati ++ ((atl ++ bth) : indent2 (length atl) btt)

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
visit f (Apply iso x) = Apply iso $ visit f x
visit f (Alt xs) = Alt $ map (visit f) xs
visit f (Seq xs) = Seq $ map (visit f) xs
visit f (Many a b x) = Many a b $ visit f x
visit f (ManyTill a b x y) = ManyTill a b (visit f x) (visit f y)
visit f (Bind a (b, c)) = Bind (visit f a) (\d -> visit f (b d), c)
-- visit f (SetR   name   x   ) = SetR name    $ visit f x
-- visit f (SetM names x   ) = SetM names $ visit f x
visit f (And xs) = And $ map (visit f) xs
visit f (Not x) = Not $ visit f x
-- visit f (            x :+ y) = visit f x :+ visit f y
-- visit f (Count         x y) = Count            x (visit f y)
visit f (Name name x) = Name name $ visit f x
-- visit f (Ignore       x   ) = Ignore       $ visit f x
-- visit f (Try            x   ) = Try            $ visit f x
visit f other = other

visitPostR :: (forall t a. RuleR t a -> RuleR t a) -> RuleR t a -> RuleR t a
visitPostR f x = f $ visitR (visitPostR f) x

visitPreR :: (forall t a. RuleR t a -> RuleR t a) -> RuleR t a -> RuleR t a
visitPreR f x = visitR (visitPreR f) (f x)

visitR :: (forall t a. RuleR t a -> RuleR t a) -> RuleR t a -> RuleR t a
visitR f (ApplyR iso x) = ApplyR iso $ visitR f x
visitR f (AltR xs) = AltR $ map (visitR f) xs
visitR f (ManyR a b x) = ManyR a b $ visitR f x
visitR f (ManyTillR a b x y) = ManyTillR a b (visitR f x) (visitR f y)
-- visitR f (SetR   name   x   ) = SetR name    $ visitR f x
-- visitR f (SetM names x   ) = SetM names $ visitR f x
visitR f (AndR x y) = AndR (visitR f x) (visitR f y)
visitR f (NotR x) = NotR $ visitR f x
-- visitR f (            x :+ y) = visitR f x :+ visitR f y
-- visitR f (Count         x y) = Count            x (visitR f y)
visitR f (NameR name x) = NameR name $ visitR f x
-- visitR f (Ignore       x   ) = Ignore       $ visitR f x
-- visitR f (Try            x   ) = Try            $ visitR f x
visitR f other = other
{-
conseq (a : b : cs)
   | a == b = a
   | otherwise = conseq (b : cs)

closure f items = closure1 f items items

closure1 f done current = let
   new = f done current
   newdone = S.union done new
   newnew = new S.\\ done

   in if S.null new then done else closure1 f newdone newnew

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
   putStrLn $ take 80 $ cycle "/\\"
   putStrLn $ "SCANNING TOKEN "++show n++"="++show t
   state <- scan t map1
   putStrLn "SCANNED!!!!"
   print state

   let map2 = M.fromList [(item state, state)]

   res <- parseE2 map2 (n, t)
   
   putStrLn $ take 80 $ cycle "="
   putStrLn $ "END OF TOKEN SUMMARY FOR TOKEN "++show n++ "="++show t
   putStrLn "map1"
   mapM_ print map1
   putStrLn "map2"
   mapM_ print map2
   putStrLn "res"
   mapM_ print res
   putStrLn $ take 80 $ cycle "/\\"
   
   return res

parseE2 map2 (n, t) = do
   (_, done, _) <- parseE3 complete predict n t stateseq (SL.fromList $ map snd $ M.toList map2)

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
union1 = SL.union

parseE3 f g n t stateseq new = do
   a@(changed1, stateseq1, new1) <- parseE5 f n t stateseq  new
   b@(changed2, stateseq2, new2) <- parseE5 g n t stateseq1 $ SL.union new new1
   if changed2
      then do
         c@(changed3, stateseq3, new3) <- parseE4 f g n t stateseq2 new2
         return (changed1 || changed2 || changed3, stateseq3, SL.unions [new1, new2, new3])
      else return (changed1 || changed2, stateseq2, SL.union new1 new2)

parseE4 f g n t stateseq new = do
   a@(changed1, stateseq1, new1) <- parseE5 f n t stateseq new
   if changed1
      then do
         b@(changed2, stateseq2, new2) <- parseE4 g f n t stateseq1 new1
         return (changed2, stateseq2, SL.union new1 new2)
      else return (changed1, stateseq1, new1)

parseE5 f n t stateseq new = do
   a@(changed1, stateseq1, new1) <- f n t stateseq SL.empty new
   --putStrLn "Input"
   --mapM_ (print . item) $ SL.toList new
   --putStrLn "Done"
   --mapM_ print stateseq1
   --putStrLn "Active"
   --mapM_ (print . item) $ SL.toList new1
   --print changed1
   if changed1
      then do
         b@(changed2, stateseq2, new2) <- parseE5 f n t stateseq1 new1
         return (changed1 || changed2, stateseq2, SL.union new1 new2)
      else return (changed1, stateseq1, new1)

-}
