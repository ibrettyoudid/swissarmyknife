-- Copyright 2025 Brett Curtis
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE LambdaCase #-}

module Prolog2 where

import Favs 

import Control.Monad.State
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.List
import qualified Data.List.Class as L
import Data.Functor.Identity
import Debug.Trace

import Control.Monad.ListT

-- does Haskell make this Prolog interpreter lazy?

data Expr = Apply [Expr] | I Int | Sym String | Str String | Var Int | Name String | Cons Expr Expr | List [Expr] | Imp [Expr] Expr | Braced Expr deriving (Eq, Ord, Show)

data Env = Env Env1 Int deriving (Eq, Ord, Show)

type Env1 = I.IntMap Expr

db =
   [ Apply [Apply [Sym "append", Sym "nil", Name "Bs", Name "Bs"]]
   , Apply [Apply [Sym "append", Apply [Sym "cons", Name "A", Name "As"], Name "Bs", Apply [Sym "cons", Name "A", Name "Cs"]], Apply [Sym "append", Name "As", Name "Bs", Name "Cs"]]
   ]

test = [Apply [Sym "append", Sym "nil", expandSym (cycle "abc"), Name "Y"]]
{- >>> run db test
[[("X",List []),("Y",List [Sym "a",Sym "b",Sym "c",Sym "d"])],[("X",List [Sym "a"]),("Y",List [Sym "b",Sym "c",Sym "d"])],[("X",List [Sym "a",Sym "b"]),("Y",List [Sym "c",Sym "d"])],[("X",List [Sym "a",Sym "b",Sym "c"]),("Y",List [Sym "d"])],[("X",List [Sym "a",Sym "b",Sym "c",Sym "d"]),("Y",List [])]]
-}
{-
runM :: (MonadState Env (ListT.ListT IO)) => [Expr] -> [Expr] -> IO [[(String, Expr)]]
runM db goals =  
   map (\(vars1, Env env n) -> let
         vars2 = I.fromList $ map (\(name, num) -> (num, name)) vars1
         in map (\(name, num) -> (name, contractList (rename1 env vars2 $ env I.! num) env)) vars1) <$> 
   ListT.toList
      (runStateT
         (do
               (Apply new, vars1) <- instantiate $ Apply goals
               doAnd db new
               return vars1
         )
         $ Env I.empty 0
      )
   -}

run db goals = 
   map 
      (\(vars1, Env env n) -> let
         vars2 = I.fromList $ map (\(name, num) -> (num, name)) vars1
         in map (\(name, num) -> (name, contractList (rename1 env vars2 $ env I.! num) env)) vars1) 
      (runListT
         (runState
            (do
               (Apply new, vars1) <- instantiate $ Apply goals
               doAnd (convert db) new
               return vars1
            )
            $ Env I.empty 0)) -- :: Identity [([(String, Int)], Env)])

doClause db goal clause = do
   new <- instantiate clause
   let Apply (head1 : newgoals) = fst new
   unify goal head1
   doAnd db newgoals

inst goal = runStateT (instantiate goal) $ Env I.empty 0

instantiate goal = do
   Env env vn <- lift get
   let names = getNames goal
   let newvars = zip names [vn..]
   let new = rename (M.fromList newvars) goal
   lift $ put $ Env env $ vn + length newvars
   return (new, newvars)

{-
>>> run test
No instance for `Show ([Expr] -> [[(String, Expr)]])'
   arising from a use of `evalPrint'
   (maybe you haven't applied a function to enough arguments?)
In a stmt of an interactive GHCi command: evalPrint it_a6xvi

(MonadState Env (t []), MonadTrans t) => t [] ()

use lift to bring state monad stuff into the list monad
-}

doAnd db = mapM_ (doOr db)

doOr db newgoal = db >>= doClause db newgoal

convert db = foldr (<|>) mzero $ map return db

getNames (Name n) = [n]
getNames (Apply as) = concatMap getNames as
getNames _ = []

rename env (Name n) = Var $ env M.! n
rename env (Apply as) = Apply $ map (rename env) as
rename _ x = x

rename1 env renv (Var n) = fromMaybe (maybe (Var n) Name $ I.lookup n renv) $ I.lookup n env
rename1 env renv (Apply as) = Apply $ map (rename1 env renv) as
rename1 _ _ x = x

setv var val = lift $ modify (\(Env env n) -> Env (I.insert (getvarref var env) val env) n)

getvarref var env = case I.lookup var env of
   Just (Var next) -> getvarref next env
   _ -> var

getv var = do
   Env env n <- lift get
   return $ getvarsub var env

getvarsub (Var var) env = case I.lookup var env of
   Just next -> getvarsub next env
   Nothing -> Var var
getvarsub other env = other

assert a = if a then return () else mzero

{- >>> execStateT (unify (Var 1) (Sym "nil")) M.empty
Just (fromList [(1,Sym "nil")])

>>> execStateT (unify (prepare 0 test) (head db)) M.empty
Nothing

>>> head db
Apply [Apply [Sym "append",Sym "nil",Name "Bs",Name "Bs"]]
-}

unify1 (I     a ) (I     b ) = assert $ a == b
unify1 (Sym   a ) (Sym   b ) = assert $ a == b
unify1 (Str   a ) (Str   b ) = assert $ a == b
unify1 (Apply as) (Apply bs) = do assert $ length as == length bs; zipWithM_ unify as bs
unify1 (Var    a) b          = setv a b
unify1 a          (Var   b ) = setv b a
unify1 _          _          = mzero

unify a b = do
   a1 <- getv a
   b1 <- getv b
   trace ("a=" ++ show a ++ " b=" ++ show b) $ unify1 a1 b1

visitPost f (Apply xs) = f $ Apply $ map (visitPost f) xs
visitPost f (List  xs) = f $ List  $ map (visitPost f) xs
visitPost f other      = f other

visitPre f x = case f x of
   Apply xs -> Apply $ map (visitPre f) xs
   List  xs -> List  $ map (visitPre f) xs
   other    -> other

expandList xs = foldr (\x y -> Apply [Sym "cons", x, y]) (Sym "nil") xs

expandList1 xs y = foldr (\x y -> Apply [Sym "cons", x, y]) y xs

expandList2 (List xs  ) = expandList   xs
expandList2 (Imp  xs y) = expandList1 xs y
expandList2 other       = other

expandLists x = visitPost expandList2 x

expandSym = expandList . map (Sym . singleton)

expandSym1 xs y = expandList1 (map (Sym . singleton) xs) y

expandStr = expandList . map (I . ord)

parser = 
   Apply [Apply [Sym "=", Name "X", Name "X"]] : map expandTerm [
      Apply [Sym "-->", Apply [Sym "num1"]],
      Apply [Sym "-->", Apply [Sym "num1"], Apply [Sym "num"]],
      Apply [Sym "-->", Apply [Sym "num"], List [Sym "0"], Apply [Sym "num1"]]
      ]
{-
>>> parser
[Apply [Apply [Sym "=",Name "X",Name "X"]],Apply [Apply [Sym "num1",Name "ET1",Name "ET1"]],Apply [Apply [Sym "num1",Name "ET1",Name "ET2"],Apply [Sym "num",Name "ET1",Name "ET2"]],Apply [Apply [Sym "num",Name "ET1",Name "ET3"],Apply [Sym "=",Name "ET1",Imp [Sym "0"] (Name "ET2")],Apply [Sym "num1",Name "ET2",Name "ET3"]]]

>>> run parser [Apply [Sym "=", Name "X", Sym "sdafsd"]]
[[("X",Sym "sdafsd")]]



>>> parse [Apply [Sym "num"]] "0000"
[Apply [Sym "=",Name "ET1",Apply [Sym "cons",Sym "0",Apply [Sym "cons",Sym "0",Apply [Sym "cons",Sym "0",Apply [Sym "cons",Sym "0",Name "ET2"]]]]],Apply [Sym "num",Name "ET1",Name "ET2"]]

>>> run parser [Apply [Sym "=",Name "ET1",Apply [Sym "cons",Sym "0",Apply [Sym "cons",Sym "0",Apply [Sym "cons",Sym "0",Apply [Sym "cons",Sym "0",Name "ET2"]]]]]]
IntMap.!: key 1 is not an element of the map
-}

parse xs s = let
   l      = (+1) $ length $ filter (\case { Braced {} -> False; _ -> True }) xs
   vars = map (\n -> Name ("ET" ++ show n)) [1..l] 
   vars1 = init vars       
   vars2 = tail vars

   es = expandTerm0 xs vars1 vars2

   s1 = Apply [Sym "=", head vars, expandSym1 s $ last vars]

   in trace (show $ s1 : es) $ run parser $ s1 : es

expandTerm (Apply (Sym "-->":xs)) = let
   l     = length $ filter (\case { Braced {} -> False; _ -> True }) xs
   vars  = map (\n -> Name ("ET" ++ show n)) [1..l] 
   vars1 = head vars:init vars       
   vars2 = last vars:tail vars

   in Apply (expandTerm0 xs vars1 vars2) 

{-
>>> expandTerm (Apply [Sym "-->", Sym "np", Sym "adj", Sym "n"])
/home/brett/swissarmyknife/Prolog.hs:(121,1)-(125,94): Non-exhaustive patterns in function expandTerm1
-}

expandTerm0 xs as bs = trace ("expandTerm1 "++show xs++" "++show as++" "++show bs) $ expandTerm1 xs as bs

expandTerm1 [             ] [    ] [    ] = []
expandTerm1 (x@(Sym {}):xs) (a:as) (b:bs) = Apply [x, a, b]             : expandTerm0 xs as bs
expandTerm1 ((Apply  x):xs) (a:as) (b:bs) = Apply (x ++ [a, b])         : expandTerm0 xs as bs
expandTerm1 ((List   x):xs) (a:as) (b:bs) = Apply [Sym "=", a, Imp x b] : expandTerm0 xs as bs
expandTerm1 ((Braced x):xs)    as     bs  = x                           : expandTerm0 xs as bs

expandTermD (Apply (Sym "-->":xs)) = let
   l     = length $ filter (\case { Braced {} -> False; _ -> True }) xs
   vars  = map (\n -> Name ("ET" ++ show n)) [1..l] 
   vars1 = head vars:init vars
   vars2 = last vars:tail vars
   in do
      print l
      z <- expandTerm0D xs vars1 vars2
      return $ Apply (Sym "-->":z)

{-
>>> expandTerm (Apply [Sym "-->", Sym "np", Sym "adj", Sym "n"])
Apply [Sym "-->",Apply [Sym "np",Name "ET1",Name "ET3"],Apply [Sym "adj",Name "ET1",Name "ET2"],Apply [Sym "n",Name "ET2",Name "ET3"]]
-}

expandTerm0D xs as bs = do
   putStrLn $ "expandTerm1 "++show xs++" "++show as++" "++show bs
   z <- expandTerm1D xs as bs
   putStrLn $ " = "++show z
   return z

expandTerm1D [             ] [    ] [    ] = return []
expandTerm1D (x@(Sym {}):xs) (a:as) (b:bs) = do z <- expandTerm0D xs as bs; return (Apply [x, a, b]             :z)
expandTerm1D ((Apply  x):xs) (a:as) (b:bs) = do z <- expandTerm0D xs as bs; return (Apply (x ++ [a, b])         :z)
expandTerm1D ((List   x):xs) (a:as) (b:bs) = do z <- expandTerm0D xs as bs; return (Apply [Sym "=", a, Imp x b] :z)
expandTerm1D ((Braced x):xs)    as     bs  = do z <- expandTerm0D xs as bs; return (x                           :z)

contractEnv env = I.map (contractSym . (`contractList` env)) env

contractEnv1 (Env env _) = contractEnv env

contractEList = map contractEnv

contractList (Sym "nil") env = List []
contractList (Apply [Sym "cons", x, xs]) env = Cons (getvarsub x env) $ contractList xs env
contractList (Var var) env = case I.lookup var env of
   Just next -> contractList next env
   Nothing -> Var var
contractList other env = other

contractList1 (Sym "nil") env = List []
contractList1 (Apply [Sym "cons", x, xs]) env = cons (getvarsub x env) $ contractList xs env
contractList1 (Var var) env = case I.lookup var env of
   Just next -> contractList next env
   Nothing -> Var var
contractList1 other env = other

contractSym (List xs) = if all isSym xs then Sym $ concatMap unsym xs else List xs
contractSym other = other

isSym (Sym {}) = True
isSym _ = False

unsym (Sym s) = s

cons x (List xs) = List (x:xs)
cons x (Imp xs t) = Imp (x:xs) t
cons x other = Imp [x] other

{-
unify (Var a) b = do a1 <- getvarstate a; case a1 of Nothing -> setv a b; Just a2 -> unify a2 b
unify a (Var b) = do b1 <- getvarstate b; case b1 of Nothing -> setv b a; Just b2 -> unify a b2
unify _ _ = assert False
-}
{- The result is a list of var assignments for each thread
-
- append([], Bs, Bs).
- append([A|As], Bs, [A|Cs]) :- append(As, Bs, Cs).
-
- ? append(X, Y, "hello")
- X = ""
- Y = "hello"
-
- X = "h"
- Y = "ello"
-
- how to share data between threads
- need to keep track of which point threads have split at
-
- need to unify assignments made since split on rejoining
- create new variables where they're different
-
- make sure to end up with the same number of answers as threads
- don't cross-contaminate
- X = "h"
- Y = "hello"
- is not a valid answer
-
- -}
