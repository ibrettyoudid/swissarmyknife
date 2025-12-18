-- Copyright 2025 Brett Curtis
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE OverlappingInstances #-} -- deprecated
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}

module MHash where

import Favs hiding (ifP)
import MHashDynamic3
import MyPretty2

import NewTuple

import Data.IORef

import Data.List

import Control.Applicative ((<|>))
import Control.Applicative qualified as A
import Control.DeepSeq
import Control.Monad
import Control.Monad.State

import System.IO

import NumberParsers hiding (try, (<|>))
import Text.ParserCombinators.Parsec hiding (State, (<|>))
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token qualified as T

import qualified Parser7 as P

type VarName = String

infixr 0 <<=
(<<=) :: (Monad m) => (a -> m b) -> m a -> m b
a <<= b = b >>= a

{-}
infixr 0 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = (A.<$>)
-}
mmap a b = b >>= (return . a)

{-
instance Functor (State s) where
   fmap a b = do b1 <- b; return (a b1)
-}
untilM done action seed = do
   new <- action seed
   if done new then return new else untilM done action new

main = repl

repl :: IO ()
repl = do
   env <- convEnv env1
   repl1 [env]

x = (:) <$> getLine

repl1 :: Env -> IO ()
repl1 env = do
   putStr "M#> "
   -- ex <- parseH <$> getLine
   hSetBinaryMode stdin True
   li <- getLine
   li2 <- if li == "\\"
   then let 
      aux = do
         l <- getLine
         if null l then return [] else do r <- aux; return (l:r)
      in unlines <$> aux 
   else return li
   putStrLn li2
   putStrLn "parsing"
   (P.Pass exd:_) <- P.parseT num li2
   let ex = fromDyn1 $ head exd
   --let ex = Value u 0
   putStrLn "doing vars"
   let ex1 = runDoVars ex (map fconstr env)
   putStrLn "print ex"
   print ex
   putStrLn "print ex1"
   print ex1
   putStrLn "unparsing"
   case P.fp2 expr $ toDyn ex1 of
      Just j -> putStrLn j
      Nothing -> putStrLn "unparsing failed"
   putStrLn ""
   (res, env3) <- case ex1 of
      Block _ constr exprs -> do
         fr <- makeFrame constr
         let env2 = fr : env
         res <- eval2 env2 exprs
         return (res, env2)
      _ -> do
         res <- eval env ex1
         return (res, env)
   putDynLn res
   repl1 env3

eval :: Env -> Expr -> IO Dynamic
eval env stat = do
   putStrLn ("eval -> " ++ show stat)
   res <- eval1 env stat
   putStr "<- eval="
   putDynLn res
   return res

eval1 :: Env -> Expr -> IO Dynamic
eval1 env expr = case expr of
   Value _ v -> return v
   VarRef _ name nFr nV -> do
      return $ items (env !! nFr) !! nV
   VarRef1 t "quit" -> error "quit"
   VarRef1 t "env" -> return $ toDyn env
   VarRef1 t "frame" -> return $ toDyn $ head env
   VarRef1 t name -> return $ lookups name env
   Lambda _ args stat -> return $ toDyn $ Closure args stat env
   If _ cases -> evalIf cases
   Case t of1 cases -> do ofres <- eval env of1; evalCase ofres cases
   Else -> return $ toDyn True
   Apply t exprs -> do
      res <- mapM (eval env) exprs
      if fromDyn1 (head res) == mymem
         then apply (take 2 res ++ [toDyn (exprs !! 2)])
         else apply res
      --apply res
   Let t co exps stat -> do
      newfr <- makeFrame co
      let newenv = newfr : env
      zipWithM_ (\ref ex -> do v <- eval newenv ex; writeIORef (fromDyn1 ref) v) (items newfr) exps
      eval newenv stat
   Block t co exprs -> do
      newfr <- makeFrame co
      eval2 (newfr : env) exprs
   where
      evalIf [] = return $ toDyn ()
      evalIf ((cond :- then1) : rest) = do
         condres <- eval env cond
         case fromDynamic condres of
            Nothing -> error "if condition must be boolean!"
            Just True -> eval env then1
            Just False -> evalIf rest

      evalCase caseval [] = return $ toDyn ()
      evalCase caseval ((when1 :- then1) : others) = do
         whenval <- eval env when1
         if caseval == whenval
            then eval env then1
            else evalCase caseval others

eval2 env exprs = do
   res <- mapM (eval env) exprs
   return $ last res

makeFrame co = do
   newvals <- replicateM (length $ members co) (newIORef $ toDyn ())
   return $ Frame co $ map toDyn newvals

apply fxs = do
   putStr "apply -> "
   out <- mapM showDynIO fxs
   putAnyLn $ intercalate "," out
   res <- apply1 fxs
   putStr "<- apply="
   putDynLn res
   return res

apply1 [f] = return f
apply1 (f : x : xs) =
   if isFunc $ dynTypeRep f
   then do
      case dynApply f $ convertArg1 f x of
         Just j -> do
            k <- case fromDynamic j :: Maybe (IO Dynamic) of
               Just l -> l
               Nothing -> return j
            apply1 (k : xs)
   else apply1a (f : x : xs)

apply1a :: [Dynamic] -> IO Dynamic
apply1a (f : xs) =
   {-
   case dynTypeRep f of
      typeOf (undefined :: Maybe (IORef Dynamic)) ->
      -}
   case fromDynamic f :: Maybe (IORef Dynamic) of
   Just ref -> do putStrLn "readIORef"; dyn <- readIORef ref; apply1 (dyn : xs)
   Nothing -> case fromDynamic f :: Maybe NamedValue of
      Just named -> return $ nvalue named
      Nothing -> case fromDynamic f :: Maybe Closure of
         Just (Closure con stat env) -> do
            let lx = length xs
            let ly = length $ members con
            if lx >= ly
               then do
                  xrefs <- mapM newIORef $ take ly xs
                  ev <- eval (Frame con (map toDyn xrefs) : env) stat
                  apply (ev : drop ly xs)
               else
                  return $ toDyn $ PartApply xs con stat env
         Nothing -> case fromDynamic f :: Maybe PartApply of
            Just (PartApply oldxs con stat env) -> do
               let allxs = oldxs ++ xs
               let lx = length allxs
               let ly = length $ members con
               if lx >= ly
                  then do
                  xrefs <- mapM newIORef $ take ly allxs
                  ev <- eval (Frame con (map toDyn xrefs) : env) stat
                  apply (ev : drop ly allxs)
                  else
                  return $ toDyn $ PartApply allxs con stat env
            Nothing -> do
               case fromDynamic f :: Maybe Multimethod of
                  Just m -> do
                     putStrLn "multimethod"
                     a <- applyMultimethodIO m xs
                     case a of
                        Left e -> return $ error e
                        Right r -> return r
                  Nothing -> do
                     putStrLn $ "could not apply " ++ showDyn f
                     return $ toDyn (0 :: Int)

showType typ = show (typeRepTyCon typ)

pushVar t name = do
   fs <- get
   let i = 0
   let Constr cname tl members = fs !! i
   let f = members ++ [Member t name]
   put (replaceIndex i (Constr cname tl f) fs)
   return $ length members

push x = modify (x :)

-- pop = state (\(x : xs) -> (x, xs))
pop :: State [Constr] Constr
pop = do
   r <- get
   case r of
      (x : xs) -> do put xs; return x
      [] -> error "nothing to pop"

runDoVars :: Expr -> [Constr] -> Expr
runDoVars stat env = evalState (do wrapBlock stat) env

deepseq1 a = deepseq a a

deriving instance NFData (State [Constr] Expr)

wrapBlock :: Expr -> State [Constr] Expr
wrapBlock expr = do
   push $ Co "" []
   sres <- deepseq1 $ doVars expr
   fnew <- pop
   return $ Block u fnew [expr]

{-}
   error "hello"
   return $ case fnew of
   Constr _ _ [] -> sres
   fnew1 -> Block u fnew1 sres
-}
findWithIndex p xs = find (p . snd) $ zip [0..] xs

doVars :: Expr -> State [Constr] Expr
doVars stat = case stat of
   Lambda t vs stat -> do
      push vs
      sres <- wrapBlock stat
      pop
      return $ Lambda t vs sres
   VarRef1 t name -> do
      if name `elem` ["env", "frame", "quit"]
         then return $ VarRef1 t name
         else do
            e <- get
            case blah2 name e of
               Just v -> return v
               Nothing -> VarRef t name 0 <$> pushVar t name
   Apply t exprs -> do
      done <- mapM doVars exprs
      if varname (head exprs) == "@"
         then do
            if isUnknown $ etype $ done !! 1
            then return $ Apply u done
            else do
               push $ constr $ etype $ done !! 1
               done2 <- doVars $ done !! 2
               pop
               return $ Apply (etype done2) (take 2 done ++ [done2])
         else return $ Apply t done
   If t cases -> do
      x <- mapM (\(a :- b) -> do
         ra <- doVars a
         rb <- doVars b
         return $ ra :- rb) cases
      return $ If t x
   Value t v -> return $ Value t v
   other -> error (show other)

isUnknown typ = typ == u

blah2 name e = case findWithIndex isJust $ map (findWithIndex (\x -> mname x == name) . members) e of
   Just (i, Just (j, v2)) -> Just $ VarRef (mtype v2) (mname v2) i j
   Nothing -> Nothing

-- look up name in a list of frames
-- lookups :: String -> [[(String, a)]] -> a
lookups name e = items (e !! frameIndex vr) !! memIndex vr
   where
      vr = fromJust $ blah2 name $ map fconstr e

mymem = NamedValue "mymem" $ toDyn $ \frame vr -> case vr of
   VarRef1 _ name -> lookups name [frame]
   VarRef mtype mname i j -> items frame !! j

convEnv env = do
   (ns, rs) <- unzip <$> mapM envNewRef env
   return $ Frame (Co "env" ns) rs

convEnvRef env = newIORef <<= convEnv env

envNewRef (n, x) = do return (Member u n, x)

-- count the arguments of a function TypeRep
-- argCount :: Num n => TypeRep -> n
-- nubMulti :: Ord k => [(k,a)] -> [(k,[a])] collects pairs in a list together by left element
{--- collects functions with the same name and creates multimethods
createMultimethod1 [f] = f
createMultimethod1 fs  = case fst $ maxOn (argCount . dynTypeRep) fs of
                     0 -> error "not a function"
                     1 -> toDyn (\a     -> dynApplyLL fs [a])
                     2 -> toDyn (\a b   -> dynApplyLL fs [a,b])
                     3 -> toDyn (\a b c -> dynApplyLL fs [a,b,c])
-}
returnIO :: a -> IO a
returnIO = return

{-
class Typeable a => Blah a where
   myToDyn :: a -> Dynamic

instance (Typeable a, Typeable b) => Blah (a -> b) where
   myToDyn = myToDyn1

instance (Typeable a, Typeable b, Typeable c) => Blah (a -> b -> c) where
   myToDyn = myToDyn2

myToDyn1 f = toDyn (\x -> returnIO $ toDyn $ f $ fromJust $ fromDynamic x)

myToDyn2 f = toDyn (\x -> returnIO $ myToDyn1 $ f $ fromJust $ fromDynamic x)

myApplyL :: Dynamic -> [Dynamic] -> IO Dynamic
myApplyL f xs = foldM myApply1 f xs

myApply1 :: Dynamic -> Dynamic -> IO Dynamic
myApply1 f x = fromJust $ fromDynamic $ fromJust $ dynApply f $ toDyn x
--instance Blah (a -> IO b) where

plus2 = myToDyn2 ((+) :: Int -> Int -> Int)

plus1 = myToDyn1 ((+1):: Int -> Int)

i77 = toDyn (77 :: Int)
-}
-- convert (args -> IO b) to (args -> IO Dynamic)
toDynIO f = toDyn $ do r <- f; return $ toDyn r
toDynIO1 f = toDyn $ \x -> do r <- f x; return $ toDyn r
toDynIO2 f = toDyn $ \x y -> do r <- f x y; return $ toDyn r
toDynIO3 f = toDyn $ \x y z -> do r <- f x y z; return $ toDyn r

{-
--apply a list of functions to a list of arguments, return the first successful result
dynApplyLL :: [Dynamic] -> [Dynamic] -> Dynamic
dynApplyLL fs xs = head $ foldl dynApplyL fs xs

--not used, not sure what it does
dynApplyL2 :: [Dynamic] -> Dynamic -> Dynamic -> Dynamic
dynApplyL2 fs x y = head ((fs `dynApplyL` x) `dynApplyL` y)

--apply a list of functions to a single argument, returning all successful results
dynApplyL :: [Dynamic] -> Dynamic -> [Dynamic]
dynApplyL  fs x  = catMaybes $ map (\f -> dynApply f x) fs

equalDyn = dynApplyLL equalL
-}
{-
equalL =
   [toDyn ((==) :: Integer -> Integer -> Bool)
   ,toDyn ((==) :: String  -> String  -> Bool)
   ,toDyn ((==) :: Bool    -> Bool    -> Bool)]
-}
env1 =
   [ ("getStr", toDynIO getLine)
   , --   ("getAny"  , toDynIO  (parseH <$> getLine)),
   ("putDyn", toDynIO1 putDyn)
   , ("putDynLn", toDynIO1 putDynLn)
   , ("print", toDynIO1 (putDynLn :: Dynamic -> IO ()))
   , ("putStr", toDynIO1 (putStr :: String -> IO ()))
   , (":", toDyn ((:) :: Dynamic -> [Dynamic] -> [Dynamic]))
   , ("[]", toDyn ([] :: [Dynamic]))
   , ("head", toDyn (head :: [Dynamic] -> Dynamic))
   , ("tail", toDyn (tail :: [Dynamic] -> [Dynamic]))
   , ("++", toDyn ((++) :: [Dynamic] -> [Dynamic] -> [Dynamic]))
   , ("map", toDyn mymap)
   , --   ("foldl"  , toDyn    (\f x ys -> foldM (\x y -> apply1 [f,x,y]) x ys)),

   ("False", toDyn False)
   , ("True", toDyn True)
   , ("()", toDyn ())
   , ("=", toDyn mySet)
   , ("@", toDyn mymem)
   ]
   ++ numberOps

mySet :: IORef Dynamic -> Dynamic -> IO Dynamic
mySet r x = do
   writeIORef r x
   return x

mymap f xs = mapM (\x -> apply [f, x]) (xs :: [Dynamic])

{-
   ("+"      , toDyn    ((+)    :: Integer -> Integer -> Integer)),
   ("-"      , toDyn    ((-)    :: Integer -> Integer -> Integer)),
   ("*"      , toDyn    ((*)    :: Integer -> Integer -> Integer)),
   ("negate" , toDyn    (negate :: Integer -> Integer)),
   ("abs"    , toDyn    (abs    :: Integer -> Integer)),
   ("signum" , toDyn    (signum :: Integer -> Integer)),
   ("quot"   , toDyn    (quot   :: Integer -> Integer -> Integer)),
   ("rem"    , toDyn    (rem    :: Integer -> Integer -> Integer)),
   ("div"    , toDyn    (div    :: Integer -> Integer -> Integer)),
   ("mod"    , toDyn    (mod    :: Integer -> Integer -> Integer)),
   ("quotRem", toDyn    (quotRem:: Integer -> Integer -> (Integer, Integer))),
   ("divMod" , toDyn    (divMod :: Integer -> Integer -> (Integer, Integer)))
-}

type Env2 = [(VarName, Dynamic)]

numberOps = collectMulti (nOps ++ iOps ++ niOps ++ fOps)

nOps = nOps1 nOpsL

nOps1 :: (forall n. (Typeable n, Num n) => n -> Env2) -> Env2
nOps1 f =
   concat
   [ f (0 :: Int)
   , f (0 :: Integer)
   , f (0 :: Rational)
   , f (0 :: Float)
   , f (0 :: Double)
   ]

nOpsL a =
   [ ("+", toDyn (\b c -> b + c `asTypeOf` a))
   , ("-", toDyn (\b c -> b - c `asTypeOf` a))
   , ("*", toDyn (\b c -> b * c `asTypeOf` a))
   ]

testfn :: Integer -> Integer -> Integer
testfn a b = a + b

{-
class Integral a where
   quot        :: a -> a -> a
   rem         :: a -> a -> a
   div         :: a -> a -> a
   mod         :: a -> a -> a
   quotRem     :: a -> a -> (a, a)
   divMod      :: a -> a -> (a, a)
   toInteger   :: a -> Integer
-}

infixr 0 @=
a @= t = asTypeOf a t

iOps = iOps1 iOpsL

-- yes it does seem to need this weird type
iOps1 :: (forall i. (Typeable i, Integral i) => i -> Env2) -> Env2
iOps1 f = concat [f (0 :: Int), f (0 :: Integer)]

iOpsL :: (Typeable t, Integral t) => t -> Env2
iOpsL t =
   [ ("quot", toDyn (\a b -> quot (a @= t) (b @= t) @= t))
   , ("rem", toDyn (\a b -> rem (a @= t) (b @= t) @= t))
   , ("div", toDyn (\a b -> div (a @= t) (b @= t) @= t))
   , ("mod", toDyn (\a b -> mod (a @= t) (b @= t) @= t))
   , ("quotRem", toDyn (\a b -> quotRem (a @= t) (b @= t) @= (t, t)))
   , ("divMod", toDyn (\a b -> divMod (a @= t) (b @= t) @= (t, t)))
   , ("toInteger", toDyn (\a -> toInteger (a @= t) :: Integer))
   , ("even", toDyn (\a -> even (a @= t) :: Bool))
   , ("odd", toDyn (\a -> odd (a @= t) :: Bool))
   , ("gcd", toDyn (\a b -> gcd (a @= t) (b @= t) @= t))
   , ("lcm", toDyn (\a b -> lcm (a @= t) (b @= t) @= t))
   ]

niOps = nOps1 (\n -> iOps1 (niOpsL n))

niOpsL :: (Num n, Integral i, Typeable n, Typeable i) => n -> i -> Env2
niOpsL n i =
   [ ("^", toDyn (\a b -> (^) (a @= n) (b @= i) @= n))
   , ("fromIntegral", toDyn (\a -> fromIntegral (a @= i) @= n))
   ]

{-
even           :: Integral -> Bool
odd            :: Integral -> Bool
gcd            :: Integral -> Integral -> Integral
lcm            :: Integral -> Integral -> Integral
(^)            :: Num        -> Integral -> Num
(^^)           :: Fractional -> Integral -> Fractional
fromIntegral   :: Integral -> Num
realToFrac     :: Real     -> Fractional

class Floating a
   pi       :: a
   exp      :: a -> a
   log      :: a -> a
   sqrt     :: a -> a
   sin      :: a -> a
   cos      :: a -> a
   tan      :: a -> a
   asin     :: a -> a
   acos     :: a -> a
   atan     :: a -> a
   sinh     :: a -> a
   cosh     :: a -> a
   tanh     :: a -> a
   asinh    :: a -> a
   acosh    :: a -> a
   atanh    :: a -> a
   (**)     :: a -> a -> a
   logBase  :: a -> a -> a
-}
fOps = fOpsL (0 :: Float) ++ fOpsL (0 :: Double)

fOpsL t =
   -- [("pi"      , toDyn (pi @= t))
   [ ("exp", toDyn (\a -> exp (a @= t) @= t))
   , ("log", toDyn (\a -> log (a @= t) @= t))
   , ("sqrt", toDyn (\a -> sqrt (a @= t) @= t))
   , ("sin", toDyn (\a -> sin (a @= t) @= t))
   , ("cos", toDyn (\a -> cos (a @= t) @= t))
   , ("tan", toDyn (\a -> tan (a @= t) @= t))
   , ("asin", toDyn (\a -> asin (a @= t) @= t))
   , ("acos", toDyn (\a -> acos (a @= t) @= t))
   , ("atan", toDyn (\a -> atan (a @= t) @= t))
   , ("sinh", toDyn (\a -> sinh (a @= t) @= t))
   , ("cosh", toDyn (\a -> cosh (a @= t) @= t))
   , ("tanh", toDyn (\a -> tanh (a @= t) @= t))
   , ("asinh", toDyn (\a -> asinh (a @= t) @= t))
   , ("acosh", toDyn (\a -> acosh (a @= t) @= t))
   , ("atanh", toDyn (\a -> atanh (a @= t) @= t))
   , ("**", toDyn (\a b -> (**) (a @= t) (b @= t) @= t))
   , ("logBase", toDyn (\a b -> logBase (a @= t) (b @= t) @= t))
   ]

test =
   "if 1 -> 1\n"
   ++ "   2 -> 2\n"

test2 =
   "if blah1; blah2 -> blah3; blah4 | foo1; foo2 -> foo3; foo4 else snorg1; snorg2"

{-
parseH t = parseC expr t
parseC p t = right $ runParser p (1, 1, False) "" t

schemeDef = haskellStyle
   { reservedOpNames = ["->", "\\", "|", ";"]
   , reservedNames   = ["if", "then", "elif", "else", "case", "of", "when"]
   }

scheme = T.makeTokenParser schemeDef

symbol     s  = do checkC; T.symbol        scheme s
natural       = do checkC; T.natural       scheme
lexeme     l  = do checkC; T.lexeme        scheme l
identifier    = do checkC; T.identifier    scheme
reserved   r  = do checkC; T.reserved      scheme r
reservedOp r  = do checkC; T.reservedOp    scheme r
operator      = do checkC; T.operator      scheme
stringLiteral = do checkC; T.stringLiteral scheme

stats = do
   s <- sepEndBy1 expri $ reservedOp ";"
   return $ case s of
      [s1] -> s1
      ss   -> Stats ss

expri = begini expr
expr = buildExpressionParser optable terms

terms = do t <- many1 term; return (case t of [t1] -> t1; ts -> Apply ts)

term =     do f <- try forceFloating; return $ Value $ toDyn f
      <|> do n <- natural; return $ Value $ toDyn n
      <|> do s <- stringLiteral; return $ Value $ toDyn s
      <|> do i <- identifier; return $ VarRef1 i
      <|> do symbol "("; s <- stats; symbol ")"; return s
      <|> do reservedOp "\\"; is <- many1 identifier; reservedOp "->"; s <- stats; return $ Lambda (Constr "lambda" is) s
      <|> listP
      <|> ifP
      <|> caseP

listP = do
         symbol "["
         ss <- sepBy stats (symbol ",")
         symbol "]"
         return $ foldr (\x y -> Apply [VarRef1 ":",x,y]) (VarRef1 "[]") ss
{-
letP = do
   try $ reserved "let"
   i <- identifier
   symbol "="
   s <- stats
-}

ifP = do
   try $ reserved "if"
   a <- do
      cond <- stats
      reserved "then" <|> reservedOp "->"
      then1 <- stats
      return (cond, then1)
   b <- many $ do
      optional $ reservedOp "|" <|> reserved "elif"
      cond <- stats
      reserved "then" <|> reservedOp "->"
      then1 <- stats
      return (cond, then1)
   c <- option [] $ do
      reserved "else"
      optional $ reservedOp "->"
      else1 <- stats
      return [(Else, else1)]
   return $ If ((a : b) ++ c)

caseP = do
   try $ reserved "case"
   c <- stats
   optional $ reserved "of"
   begin $ do
      x <- many1 $ do
         optional $ reservedOp "|" <|> reserved "when"
         cond <- stats
         reserved "then" <|> reservedOp "->"
         then1 <- stats
         return (cond, then1)
      y <- option [] $ do
         reserved "else" <|> reserved "otherwise"
         optional $ reservedOp "->"
         else1 <- stats
         return [(Else, else1)]
      return $ Case c (x ++ y)

type PState = (Int, Int, Bool)

begin p = do
   pos <- getPosition
   oldState <- getState
   setState (sourceLine pos, sourceColumn pos, False)
   res <- p
   setState oldState
   return res

-- use this to check subsequent lines are more indented
begini p = do
   pos <- getPosition
   oldState <- getState
   setState (sourceLine pos, sourceColumn pos, True)
   res <- p
   setState oldState
   return res

getLineCol = do
   pos <- getPosition
   return (sourceLine pos, sourceColumn pos)

checkC = do
   (minLine, minCol, indent) <- getState
   (curLine, curCol) <- getLineCol
   guard (curLine >= minLine)
   if indent && curLine > minLine
      then guard (curCol > minCol)
      else guard (curCol >= minCol)

operatorN name = try $ do o <- operator; guard (o == name); return o

-- optable :: OperatorTable Char (Bool, [Column]) Dyn
optable   = [ doinfixr 9 [".", "!!"]
            , doinfixr 8 ["^", "^^", "**"]
            , doinfixl 7 ["*", "/"] --, `quot`, `rem`, `div`, `mod`
            , doinfixl 6 ["+", "-"]
            , doinfixr 5 [":", "++"]
            , doinfix  4 ["==", "/=", "<", "<=", ">=", ">"]
            , doinfixr 3 ["&&"]
            , doinfixr 2 ["||"]
            , doinfixl 1 [">>", ">>="]
            , doinfixr 1 ["=<<"]
            , doinfixr 0 ["$", "$!"] --, `seq`
            ]

-- doinfixa assoc p = map (\n -> Infix (do n1 <- operator; guard n == n1; return ) assoc)
doinfixa assoc p = map (\n -> Infix (do reservedOp n; return (\a1 a2 -> Apply [VarRef1 n, a1, a2])) assoc)
doinfixl = doinfixa AssocLeft
doinfixr = doinfixa AssocRight
doinfix  = doinfixa AssocNone

putf w s = putStrLn $ formatS w s
-}
{-
formatS w (Value v) = showDyn v
formatS w (VarRef1 v) = v

formatS w (If cases) = fit w
   ["if " ++ (indent1 3 $ intercalate " | " $ map (formatCase w)       cases)
   ,"if " ++ (indent1 3 $ intercalate "\n"  $ map (formatCase (w - 3)) cases)
   ]

formatS w (Stats ss) = fit w
   [intercalate "; " $ map (formatS w) ss
   ,intercalate "\n" $ map (formatS w) ss
   ]

formatCase w (cond, exec) = let
   fcond = formatS (w - 4) cond
   fexec = formatS (w - 4) exec
   res1   = fcond ++ " -> " ++ fexec
   res2   = fcond ++ "\n -> " ++ indent1 4 fexec

   in if width res1 <= w
         then res1
         else res2

fit w [o]      = o
fit w (o:opts) = if width o <= w then o else fit w opts

width = width1

width1 s = maximum $ map length $ lines s

height s = count $ lines s
-}

formatS w s = toString $ formatStat w s

data Doc
   = Doc {docWidth :: Int, docHeight :: Int, docText :: [String]}
   | Doc :-: Doc
   | Doc :\: Doc
   | Doc :^: Doc
   | Doc :|: Doc
   | DocFail
   deriving (Eq, Ord, Show)

t s = Doc (length s) 1 [s]
ts :: [String] -> Doc
ts ss = Doc (maximum $ map length ss) (length ss) ss

toString (Doc _ _ ss) = unlines $ padcoll0 ss

minWidthDoc docs = snd $ minimum $ map (\d -> (docWidth d, d)) docs
minHeightDoc docs = snd $ minimum $ map (\d -> (docHeight d, d)) docs

minWidth ds = minimum $ map docWidth ds
minHeight ds = minimum $ map docHeight ds

maxWidth ds = maximum $ map docWidth ds
maxHeight ds = maximum $ map docHeight ds

sumHeight ds = sum $ map docHeight ds
sumWidth ds = sum $ map docWidth ds

fit :: Int -> [Doc] -> Doc
fit w opts =
   let
   f1 = filter ((<= w) . docWidth) opts
   f2 = filter ((<= 1) . docHeight) f1
   in
   if f2 /= []
      then minWidthDoc f2
      else
         if f1 /= []
         then minHeightDoc f1
         else minWidthDoc opts

indent n (Doc w h t) = Doc w h $ map (replicate n ' ' ++) t

sapp (Doc aw ah at) (Doc bw bh (bth : btt)) =
   let
   ati = init at
   atl = last at
   in
   ts $ ati ++ ((atl ++ bth) : map (replicate (length atl) ' ' ++) btt)

(<\>) = sapp

scat ds = foldl1 sapp ds

hcat ds =
   Doc
   (sumWidth ds)
   (maxHeight ds)
   (map concat $ transpose $ map (padh (maxHeight ds)) ds)

padh maxh (Doc w h strs) = map (padl w) (strs ++ replicate (maxh - h) "")

vcat ds =
   Doc
   (maximum $ map docWidth ds)
   (sum $ map docHeight ds)
   (concat $ map docText ds)

vapp a b = vcat [a, b]

a <-> b = hcat [a, b]
a <:> b = vcat [a, b]

formatStat :: Int -> Expr -> Doc
formatStat w (Value _ v) = t $ showDyn v
formatStat w (VarRef1 _ n) = t n
formatStat w (Else) = t "else"
formatStat w (Apply _ (f : xs)) =
   fit
   w
   [ scat $ intersperse (t " ") $ map (formatStat w) (f : xs)
   , formatStat w f <\> (vcat $ map (formatStat w) xs)
   ]
formatStat w (If _ cases) =
   t "if "
   <\> fit
      (w - 3)
      [ scat $ zipWith (formatCase (w - 3)) [0 ..] cases
      , vcat $ zipWith (formatCase (w - 3)) (repeat 0) cases
      ]
formatStat w (Case _ c cases) =
   t "case "
   <\> formatStat (w - 5) c
   <\> t " of "
   <\> fit
      (w - 5)
      [ scat $ zipWith (formatCase (w - 3)) [0 ..] cases
      , vcat $ zipWith (formatCase (w - 3)) (repeat 0) cases
      ]
formatStat w x = t $ show x

formatCase :: Int -> Int -> (Expr :- Expr) -> Doc
formatCase w n (cond :- exec) =
   fit w $
   (if n > 0 && notElse cond then map (t " | " <->) else id) $
      [ formatStat (w - 4) cond <\> t " -> " <\> formatStat (w - 4) exec
      , formatStat w cond <:> (t " -> " <\> formatStat (w - 4) exec)
      ]

notElse Else = False
notElse _ = True

fmtStat (Value _ v) = t $ showDyn v
fmtStat (VarRef1 _ n) = t n
fmtStat (Else) = t "else"
fmtStat (Apply _ (f : xs)) =
   (foldr1 (:-:) $ intersperse (t " ") $ map fmtStat (f : xs))
   :|: (foldr1 (:^:) $ map fmtStat (f : xs))

fmt w h d@(Doc dw dh dt)
   | dw <= w && (dh > 1) <= h = [d]
   | True = []
fmt w h DocFail = []
fmt w h (as :-: bs) = do
   a <- fmt w False as
   b <- fmt (w - docWidth a) False bs
   return (a <-> b)
fmt w h (as :^: bs) = do
   guard h
   a <- fmt w h as
   b <- fmt w h bs
   return (a <:> b)
fmt w h (as :|: bs) = fmt w h as ++ fmt w h bs

data CData = CData
   { structDecls :: String
   , funcDecls :: String
   , structDefns :: String
   , funcDefns :: String
   , lambdaCounter :: Int
   }
   deriving (Eq, Show)

addStructDecl :: String -> State CData ()
addStructDecl x = modify (\s -> s{structDecls = structDecls s ++ x})
addStructDefn :: String -> State CData ()
addStructDefn x = modify (\s -> s{structDefns = structDefns s ++ x})
addFuncDecl :: String -> State CData ()
addFuncDecl x = modify (\s -> s{funcDecls = funcDecls s ++ x})
addFuncDefn :: String -> State CData ()
addFuncDefn x = modify (\s -> s{funcDefns = funcDefns s ++ x})
incLambda :: State CData ()
incLambda = modify (\s -> s{lambdaCounter = lambdaCounter s + 1})

testCompile p = do
   let (v, s) = runState p (CData "" "" "" "" 0)
   putStr $ structDecls s
   putStr $ funcDecls s
   putStr $ structDefns s
   putStr $ funcDefns s
   return v

type CWriter = State CData

interpre s xs = map (s ++) xs
interpost s xs = map (++ s) xs
interabs a xs b s = intermap (\x -> a ++ x ++ b) xs s
intermap f xs s = intercalate s $ map f xs
interas a xs s = interabs a xs [] s
interbs xs b s = interabs [] xs b s
interab a xs b = interabs a xs b []

{-
compileConstr c = do
   addStructDecl ("struct " ++ cname c ++ ";\n")
   addStructDefn
   ( "struct "
         ++ cname c
         ++ "\n"
         ++ "{\n"
         ++ interab "   Any " (inames c) ";\n"
         ++ cname c
         ++ "("
         ++ interas "Any " (inames c) ","
         ++ ") : "
         ++ intermap (\x -> x ++ "(" ++ x ++ ")") (inames c) ","
         ++ " {}\n"
         ++ "};\n"
         ++ "\n"
   )

-- compile :: [[String]] -> Expr -> State CData String
compile env (Apply _ stats) = do
   r <- mapM (compile env) stats
   return ((head r) ++ ".as<LambdaStr>()(" ++ intercalate "," (tail r) ++ ")")

{-
compile env (VarRef1 n) = do
   let Just i = findIndex (elem n) env
   return ("env->" ++ concat (replicate i "parent->") ++ n)
-}

compile env (Lambda _ vs s) = do
   inner <- compile (vs : env) s
   lc <- lambdaCounter <$> get
   incLambda
   let func = "lambda" ++ show lc
   let decl = "Any " ++ func ++ "(" ++ interas "Any " (inames vs) "," ++ ");"
   let struct = "lambdaFrame" ++ show lc
   compileConstr vs
   addFuncDecl (decl ++ ";")
   addFuncDefn
   ( decl
         ++ "\n"
         ++ "{\n"
         ++ "   env = new "
         ++ struct
         ++ "(env, "
         ++ intercalate "," (inames vs)
         ++ ");\n"
         ++ "   return "
         ++ inner
         ++ ";\n"
         ++ "}\n"
         ++ "\n"
   )
   return ("LambdaStr(" ++ func ++ ", env)")
-}
{-

compile env (If cases) = let
   aux a b = compile env a++" ? "++compile env b++" : "
   in case last cases of
         (Else, e) -> concatMap aux (init cases) ++ compile env e
         others    -> concatMap aux cases ++ " 0"

-}
{-
formatH w open sep close terms = let
   fterms  = map (formatH (w - indentStep)) terms
   res     = open ++ concat fterms ++ close

   in

   if length res <= w
      then res
      else open ++ "\n" ++ (indent indentStep $ intercalate "\n" $ fterms) ++ "\n" ++ close
      {-
      else if allEqual clens
               then
               then open ++ indent1 (length open) (intercalate (sep ++ "\n") fterms) ++ close
      -}
-}
{-
if a subitem can't fit into the w,
must trigger the outer to not fit
   it will as long as string length >= when spread across lines (check the definitions of res)
must also redo all the sub items
or could format them originally for spreading as the spread indent is <= the open+close tag

(a (b (c (c c c c) d (d d d d)) (e f) (g h)) (i (j k) (l m)))

(a (b (c (c c c c)
      d (d d d d))
      (e f)
      (g h))
   (i (j k) (l m)))

(a (b (c d)
      (e f))
   (i (j k) (l m)))

on a line, unmatched close brackets must not be followed by anything
=> if a sub item spans lines, there must be a new line after it
+ there ought to be a new line between all of the sub items
+ there ought to be a new line after the last one
-}
{-
repl :: IO ((), Env)
repl = runStateT (do
   a <- lift $ mapM (mapM envNewRef) env1
   put a
   repl1) []

repl1 :: StateT Env IO ()
repl1 = do
   lift $ putStr ">"
   line <- lift getLine
   let ex = parseH line
   lift $ print ex
   res <- eval ex
   lift $ print res
   repl1

eval x = do
   lift $ print "eval"
   lift $ print x
   res <- eval1 x
   lift $ print res
   return res

eval1 :: Expr -> StateT Env IO Dynamic
eval1 (Value v) = return v
eval1 (VarRef name nFrame nVar) = toDyn <$> (\e -> e !! nFrame !! nVar) <$> get
eval1 (VarRef1 name) = (lift . readIORef) <<= lookup1 name <$> get
{-
eval1 (VarRef1 name) = do
   val <- lookup1 name <$> get
   lift $ readIORef val
-}
eval1 (Apply exprs) = mapM eval exprs >>= apply
eval1 (Lambda args stat) = toDyn <$> Closure args stat <$> get

apply x = do
   lift $ print "apply"
   lift $ print x
   res <- apply1 x
   lift $ print res
   return res

apply1 :: [Dynamic] -> IO Dynamic
--apply1 [f,x] = return $ fromJust $ dynApply f x
apply1 (f:xs) =
   case fromDynamic f :: Maybe Closure of
      Just (Closure args stat env) ->
   fl =
   in c
   rd = dynApply1 f xs
   r1 = fromDynamic rd :: Maybe (IO Dynamic)
   in case r1 of
      Just rio -> lift rio
      Nothing  -> return rd

-- apply (f:e:exprs) = apply ((fromJust $ dynApply f e):exprs)
-- apply = apply
-}

{-
niOpsA :: [(VarName, Dynamic)]
niOpsA = nOpsA (iOpsA niOpsLA)

nOpsA :: (forall n. (Typeable n, Num n) => n -> Env2) -> Env2
nOpsA f = concat
   [f (0::Int)
   ,f (0::Integer)
   ,f (0::Rational)
   ,f (0::Float)
   ,f (0::Double)]

iOpsA :: (forall i. (Integral i, Typeable i) => n -> i -> Env2) -> n -> Env2
iOpsA f n = concat [f n (0::Int), f n (0::Integer)]

niOpsLA :: (Num n, Integral i, Typeable n, Typeable i) => n -> i -> Env2
niOpsLA n i =
   [("^"           , toDyn (\a b -> (^) (a @= n) (b @= i) @= n))
   ,("fromIntegral", toDyn (\a   -> fromIntegral (a @= i) @= n))]

niOpsX :: [Integer]
niOpsX = nOpsX (\n -> iOpsX (niOpsLX n))

nOpsX :: Num a1 => (a1 -> [a]) -> [a]
nOpsX f = concat [f 1, f 2, f 3]

iOpsX :: Num a1 => (a1 -> [a]) -> [a]
iOpsX f = concat [f 1, f 2]

niOpsLX :: t -> t -> [t]
niOpsLX n i = [n, i]

niOps :: [(VarName, Dynamic)]
niOps = concat
   [niOps1 (0::Int     )
   ,niOps1 (0::Integer )
   ,niOps1 (0::Rational)
   ,niOps1 (0::Float   )
   ,niOps1 (0::Double  )]

niOps1 :: (Typeable n, Num n) => n -> [(VarName, Dynamic)]
niOps1 n = concat [niOpsL n (0::Int), niOpsL n (0::Integer)]
-}
