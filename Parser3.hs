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

test = parseE sexpr "a+b+c+d"

mintest = parseE (Seq [Token 'a', Token 'b']) "ab"

num1 = parseE (Many 1 0 $ Range '0' '9') "1234"

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
   res <- parseE (translate r) t
   return $ map fromDyn1 res

parseEnv env r t = parseE (doLookups env r) t

-- scanM :: Monad m => (m [a] -> b -> m a) -> m [a] -> [b] -> m [a]
foldMH f z = foldl (\a b -> do ra <- a; rr <- f ra b; return rr) (return z)

parseE r t = do
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
         let scanned = mapMaybe (scan1 n (t !! (n-1))) $ SL.toList $ PI.setlist $ piseq PIS.! (n-1)
         mapM (\main -> PIS.insertC main main) scanned
         liftIO $ putStrLn $ "scanned=" ++ show scanned
         comppredE n $ SL.fromList scanned) [1..length t]
      ) init
   
   putStrLn (table t positemseq)
   return $ concat $ mapMaybe (\s -> ifJust (from s == 0 && rule (item s) == r && passi (item s)) (asts $ dot (item s))) (SL.toList $ PI.setlist $ positemseq PIS.! length t)

--data Main = Main { lps :: [(Int, Int)], stateseq :: PosItemSeq state tok }

untilM pred mf x = if pred x
                        then return x
                        else mf x >>= untilM pred mf

-- refoldD f z xs = do (rs, zs) <- unzip <$> zipWithM f (z : zs) xs; return (rs, zs)
refoldD f z [] = return []
refoldD f z (x : xs) = do (a, b) <- f z x; c <- refoldD f b xs; return $ a : c

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

scan n t items = mapMaybe (scan1 n (t !! n)) $ SL.toList items

scan1 n ch st = scan2 n ch (from st) (item st)

scan2 n ch j (Item r Running) = do sc <- scan3 ch r; return $ maket j n $ Item r (if sc then Pass [toDyn ch] else Fail)
scan2 n ch _ _ = Nothing

scan3 ch (Token c) = Just $ ch == c
scan3 ch (Range c d) = Just $ ch >= c && ch <= d
scan3 _ _ = Nothing

scan4 (Token c) = True
scan4 (Range c d) = True
scan4 AnyToken = True


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

retrieveE stateseq mainpositem = do
   seq <- childrenE stateseq mainpositem
   map (asts . dot . item) seq

-- returns all children from the state you give it, back
childrenE positemseq next = do
   sub <- SL.toList $ PI.setlist $ positemseq PIS.! to next
   if anysubitem (item next) (item sub) && pass (dot $ item sub)
      then siblingsE positemseq next sub
      else []

siblingsE positemseq next sub = do
   main <- SL.toList $ PI.setlist $ positemseq PIS.! from sub
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
   s1 <- SL.toList $ stateslist !! to parent
   if subitem (item parent) (item s1) && pass (dot $ item s1)
      then siblings stateslist parent s1
      else []

siblings stateslist parent s1 = do
   s2 <- SL.toList $ stateslist !! from s1
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
   let [end] = filter ((0 ==) . from) $ SL.toList $ last stateslist
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
combinescans rules = let
   new    = concatMap (\(r:rs) -> concatMap (combinescans1 r) rs) $ take (length rules - 1) $ tails rules
   in if S.fromList new == S.fromList rules
         then rules
         else combinescans new

combinescans1 c@(Token cr, cs) d@(Token dr, ds) =
   if cr == dr
      then [(Token cr, cs ++ ds)]
      else [c, d]
combinescans1 c@(Token cr, cs) d@(Range d1 d2, ds) =
   if cr >= d1 && cr <= d2
      then [(Range d1 (pred cr), ds), (Token cr, cs ++ ds), (Range (succ cr) d2, ds)]
      else [c, d]
combinescans1 c@(Range c1 c2, cs) d@(Range d1 d2, ds) =
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

r0 = sums1

sums1 = Name "sums1" $ Alt [sums2, Name "sums3" products]

sums2 = Seq [sums1, Token '+', products]

products = Name "products" $ Alt [products1, value1]

products1 = Seq [products, Token '*', value1]

value1 = Name "value" $ Alt [id1, int9]

id1 = Name "id" $ Token 'a'

int9 = Name "int" $ Token '1'

r1 = sums2
r2 = sums1
r3 = products1
r4 = products
r5 = value1
r6 = value1

lr = LRParser $ I.fromList [
   (0, LRState (M.fromList [("1", Shift   8), ("a", Shift   9)                 ]) (M.fromList [(sums1, 1), (sums2, 1), (products, 4), (products1, 4), (value1, 7)]) SL.empty SL.empty),
   (2, LRState (M.fromList [("1", Shift   8), ("a", Shift   9)                 ]) (M.fromList [(products, 3), (products1, 3), (value1, 7)])                         SL.empty SL.empty),
   (5, LRState (M.fromList [("1", Shift   8), ("a", Shift   9)                 ]) (M.fromList [(value1, 6)])                                                        SL.empty SL.empty),
   (1, LRState (M.fromList [("+", Shift   2),                   ([], Accept   )])  M.empty                                                                          SL.empty SL.empty),
   (3, LRState (M.fromList [("*", Shift   5), ("+", Reduce r1), ([], Reduce r1)])  M.empty                                                                          SL.empty SL.empty),
   (4, LRState (M.fromList [("*", Shift   5), ("+", Reduce r2), ([], Reduce r2)])  M.empty                                                                          SL.empty SL.empty),
   (6, LRState (M.fromList [("*", Reduce r3), ("+", Reduce r3), ([], Reduce r3)])  M.empty                                                                          SL.empty SL.empty),
   (7, LRState (M.fromList [("*", Reduce r4), ("+", Reduce r4), ([], Reduce r4)])  M.empty                                                                          SL.empty SL.empty),
   (8, LRState (M.fromList [("*", Reduce r5), ("+", Reduce r5), ([], Reduce r5)])  M.empty                                                                          SL.empty SL.empty),
   (9, LRState (M.fromList [("*", Reduce r6), ("+", Reduce r6), ([], Reduce r6)])  M.empty                                                                          SL.empty SL.empty)]

data LRParser token = LRParser { lrParser :: I.IntMap (LRState token) }

data LRState token = LRState { acts :: M.Map [token] (Action token), gotos :: M.Map (Rule token) Int, kernel :: SL.SetList (Item token), closure :: SL.SetList (Item token) } deriving Show

data Action token = Shift Int | Reduce (Rule token) | Accept deriving Show

data LRStackItem token = LRStackItem { lrstatenum :: Int, ast :: LRAST token } deriving Show

data LRAST token = Tok token | AST [LRStackItem token] deriving Show

showStack parser stack = MyPretty2.GridV $ map (showStackItem parser) stack

showStackItem parser stackitem = [show $ lrstatenum stackitem, show $ ast stackitem]

parseLR (LRParser parser) stack toks = do
   putStrLn toks
   let 
      (top : _) = stack
      lrs = parser I.! lrstatenum top
      acts1 = acts lrs
      act = case toks of
         [   ] -> fromMaybe (error "unexpected end of file") $ M.lookup [ ] acts1
         (x:_) -> fromMaybe (error $ "unexpected "++show x)  $ M.lookup [x] acts1
   print $ showStack parser stack
   print acts1
   print act
   case act of
      Accept   -> return stack
      Shift  n -> parseLR (LRParser parser) (LRStackItem n (Tok $ head toks) : stack) (tail toks)
      Reduce r -> do
         let
            l = case r of
               Seq xs -> length xs
               Alt xs -> 1
               Token x -> 1
               Name a b -> 1
            (removed, old) = splitAt l stack
            (oldtop:_) = old
            statenum = lrstatenum oldtop
            lrs = parser I.! statenum
            goto = M.lookup r $ gotos lrs
         
         putStrLn $ "rule "++show r++" in LRState "++show statenum++"="++show lrs
         let
            newstatenum = case goto of
               Just  j -> j
               Nothing -> error $ "no entry found for rule "++show r++" in LRState "++show statenum++"="++show lrs
            newstack = LRStackItem newstatenum (AST removed) : old
         
         parseLR (LRParser parser) newstack toks

test2 = parseLR lr [LRStackItem 0 (AST [])] "a+1*1"

data LRMonad token = LRMonad { lrstates :: I.IntMap (LRState token), itemsets :: M.Map (Item token) Int }

newState n lrstate = do
   modify (\x -> x { lrstates = I.insert n lrstate $ lrstates x } )

nextStateNum :: MonadState (LRMonad token) m => m I.Key
nextStateNum = do
   lrs <- lrstates <$> get 
   return $ fst $ fromJust $ I.lookupMax lrs

makeLR start = execStateT (do
   makeLR1 start) $ LRMonad I.empty M.empty

makeLR1 kernel = do
   n <- nextStateNum
   let
      closure  = SL.closure (predict2 n)   kernel
      scans    = SL.filter  (scan4) $ SL.map rule closure
      combined = combinescans $ map (\s -> (s, [s])) $ SL.toList scans
   
   concat <$> mapM makeLR1 pred

makeLR2 itemset = 0
