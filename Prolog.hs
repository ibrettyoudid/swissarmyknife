-- Copyright 2025 Brett Curtis
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Prolog where

import Control.Monad.State
import Data.Char
import Data.Map qualified as M
import Data.IntMap qualified as I
import Data.List

data Expr = Apply [Expr] | I Int | Sym String | Str String | Var Int | Name String | List [Expr] | Imp [Expr] Expr deriving (Eq, Ord, Show)

data Env1 = Env1 Env Int deriving (Eq, Ord, Show)

type Env = I.IntMap Expr

db =
  [ Apply [Apply [Sym "append", Sym "nil", Name "Bs", Name "Bs"]]
  , Apply [Apply [Sym "append", Apply [Sym "cons", Name "A", Name "As"], Name "Bs", Apply [Sym "cons", Name "A", Name "Cs"]], Apply [Sym "append", Name "As", Name "Bs", Name "Cs"]]
  ]

test = Apply [Sym "append", Name "X", Name "Y", expandSym "abcd"]

{- >>> contractEList $ run db test
[fromList [(0,Sym ""),(1,Sym "abcd"),(3,Sym "abcd")],fromList [(0,Sym "a"),(1,Sym "bcd"),(5,Sym "a"),(7,Sym ""),(8,Sym "bcd"),(9,Sym "bcd"),(11,Sym "bcd")],fromList [(0,Sym "ab"),(1,Sym "cd"),(5,Sym "a"),(7,Sym "b"),(8,Sym "cd"),(9,Sym "bcd"),(13,Sym "b"),(15,Sym ""),(16,Sym "cd"),(17,Sym "cd"),(19,Sym "cd")],fromList [(0,Sym "abc"),(1,Sym "d"),(5,Sym "a"),(7,Sym "bc"),(8,Sym "d"),(9,Sym "bcd"),(13,Sym "b"),(15,Sym "c"),(16,Sym "d"),(17,Sym "cd"),(21,Sym "c"),(23,Sym ""),(24,Sym "d"),(25,Sym "d"),(27,Sym "d")],fromList [(0,Sym "abcd"),(1,Sym ""),(5,Sym "a"),(7,Sym "bcd"),(8,Sym ""),(9,Sym "bcd"),(13,Sym "b"),(15,Sym "cd"),(16,Sym ""),(17,Sym "cd"),(21,Sym "c"),(23,Sym "d"),(24,Sym ""),(25,Sym "d"),(29,Sym "d"),(31,Sym ""),(32,Sym ""),(33,Sym ""),(35,Sym "")]]
-}
run db goal =
  execStateT
    ( do
        new <- instantiate goal
        doOr db new
    )
    $ Env1 I.empty 0

doClause db goal clause = do
  new <- instantiate clause
  let Apply (head1 : newgoals) = new
  Env1 env vn <- get
  case execStateT (unify goal head1) env of
    Nothing -> lift []
    Just newenv -> do
      put $ Env1 newenv vn
      doAnd db newgoals

inst goal = runStateT (instantiate goal) $ Env1 I.empty 0

instantiate goal = do
  Env1 env vn <- get
  let names = getNames goal
  let new = rename (M.fromList $ zip names [vn ..]) goal
  put $ Env1 env $ vn + length names
  return new

{-
>>> inst test
(Apply [Sym "append",Var 0,Var 1,Apply [Sym "cons",Sym "a",Apply [Sym "cons",Sym "b",Sym "nil"]]],Env1 (fromList []) 2)

(MonadState Env (t []), MonadTrans t) => t [] ()

use lift to bring list monad stuff into the state monad
-}

doAnd db = mapM_ (doOr db)

doOr db newgoal = lift db >>= doClause db newgoal

visitPost f (Apply xs) = f $ Apply $ map (visitPost f) xs
visitPost f (List  xs) = f $ List  $ map (visitPost f) xs
visitPost f other      = f other

visitPre f x = case f x of
  Apply xs -> Apply $ map (visitPre f) xs
  List  xs -> List  $ map (visitPre f) xs
  other    -> other

expandList xs = foldr (\x y -> Apply [Sym "cons", x, y]) (Sym "nil") xs

expandList1 xs y = foldr (\x y -> Apply [Sym "cons", x, y]) y xs

expandList2 (List xs  ) = expandList  xs
expandList2 (Imp  xs y) = expandList1 xs y
expandList2 other       = other

expandLists x = visitPost expandList2 x

expandSym = expandList . map (Sym . singleton)

expandStr = expandList . map (I . ord)

expandTerm (Apply (Sym "-->":xs)) = let
  vars = map (\n -> Name ("ET" ++ show n)) [1..length xs] 

  in Apply (Sym "-->":zipWith3 expandTerm1 xs (head vars:vars) (last vars:tail vars))

{-
>>> expandTerm (Apply [Sym "-->", Sym "np", Sym "adj", Sym "n"])
Apply [Sym "-->",Apply [Sym "np",Name "ET1",Name "ET3"],Apply [Sym "adj",Name "ET1",Name "ET2"],Apply [Sym "n",Name "ET2",Name "ET3"]]
-}

expandTerm1 s@(Sym {}) a b = Apply [s, a, b]
expandTerm1 (Apply xs) a b = Apply (xs ++ [a, b])
expandTerm1 (List  xs) a b = Apply [Sym "=", a, Imp xs b]

contractEnv env = I.map (contractSym . (`contractList` env)) env

contractEnv1 (Env1 env _) = contractEnv env

contractEList = map contractEnv1

contractList (Sym "nil") env = List []
contractList (Apply [Sym "cons", x, xs]) env = cons (getvarsub x env) $ contractList xs env
contractList (Var var) env = case I.lookup var env of
  Just next -> contractList next env
  Nothing -> Var var
contractList other env = other

contractSym (List xs) = if all isSym xs then Sym $ concatMap unsym xs else List xs
contractSym other = other

isSym (Sym {}) = True
isSym _ = False

unsym (Sym s) = s

cons x (List xs) = List (x:xs)
cons x (Imp xs t) = Imp (x:xs) t
cons x other = Imp [x] other

getNames (Name n) = [n]
getNames (Apply as) = concatMap getNames as
getNames _ = []

rename env (Name n) = Var $ env M.! n
rename env (Apply as) = Apply $ map (rename env) as
rename _ x = x

setv var val = modify (\env -> I.insert (getvarref var env) val env)

getvarstate var = gets (getvar var)

getvar var env = I.lookup (getvarref var env) env

getvarref var env = case I.lookup var env of
  Just (Var next) -> getvarref next env
  _ -> var

getvarsub (Var var) env = case I.lookup var env of
  Just next -> getvarsub next env
  Nothing -> Var var
getvarsub other env = other

assert a = if a then lift $ Just () else lift Nothing

{- >>> execStateT (unify (Var 1) (Sym "nil")) M.empty
Just (fromList [(1,Sym "nil")])

>>> execStateT (unify (prepare 0 test) (head db)) M.empty
Nothing

>>> head db
Apply [Apply [Sym "append",Sym "nil",Name "Bs",Name "Bs"]]
-}

unify :: Expr -> Expr -> StateT Env Maybe ()
unify (I      a) (I      b) = assert $ a == b
unify (Sym    a) (Sym    b) = assert $ a == b
unify (Str    a) (Str    b) = assert $ a == b
unify (Apply as) (Apply bs) = do assert $ length as == length bs; zipWithM_ unify as bs
unify (Var    a) (Var    b) = do
  a1 <- getvarstate a
  case a1 of
    Nothing -> setv a (Var b)
    Just a2 -> do
      b1 <- getvarstate b
      case b1 of
        Nothing -> setv b a2
        Just b2 -> unify a2 b2
unify (Var a) b = do a1 <- getvarstate a; case a1 of Nothing -> setv a b; Just a2 -> unify a2 b
unify a (Var b) = do b1 <- getvarstate b; case b1 of Nothing -> setv b a; Just b2 -> unify a b2
unify _ _ = assert False

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
