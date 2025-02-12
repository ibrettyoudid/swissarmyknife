-- Copyright 2025 Brett Curtis
{-# LANGUAGE FlexibleContexts #-}

module Prolog where

import Control.Monad.State
import Data.Map qualified as M

data Expr = Apply [Expr] | Int1 Int | String1 String | Var Int | VarName String deriving (Eq, Ord, Show)

data Env1 = Env1 Env Int deriving (Eq, Ord, Show)

type Env = M.Map Int Expr

db =
  [ Apply [Apply [String1 "append", String1 "nil", VarName "Bs", VarName "Bs"]]
  , Apply [Apply [String1 "append", Apply [String1 "cons", VarName "A", VarName "As"], VarName "Bs", Apply [String1 "cons", VarName "A", VarName "Cs"]], Apply [String1 "append", VarName "As", VarName "Bs", VarName "Cs"]]
  ]

test = Apply [String1 "append", VarName "X", VarName "Y", Apply [String1 "cons", String1 "a", Apply [String1 "cons", String1 "b", String1 "nil"]]]

{- >>> run db test
[Env1 (fromList [(0,String1 "nil"),(1,Var 3),(3,Apply [String1 "cons",String1 "a",Apply [String1 "cons",String1 "b",String1 "nil"]])]) 4,Env1 (fromList [(0,Apply [String1 "cons",Var 5,Var 7]),(1,Var 8),(5,String1 "a"),(7,String1 "nil"),(8,Var 11),(9,Apply [String1 "cons",String1 "b",String1 "nil"]),(11,Apply [String1 "cons",String1 "b",String1 "nil"])]) 12,Env1 (fromList [(0,Apply [String1 "cons",Var 5,Var 7]),(1,Var 8),(5,String1 "a"),(7,Apply [String1 "cons",Var 13,Var 15]),(8,Var 16),(9,Apply [String1 "cons",String1 "b",String1 "nil"]),(13,String1 "b"),(15,String1 "nil"),(16,Var 19),(17,String1 "nil"),(19,String1 "nil")]) 20]
-}
run db goal =
  execStateT
    ( do
        new <- instantiate goal
        doOr db new
    )
    $ Env1 M.empty 0

doClause db goal clause = do
  new <- instantiate clause
  let Apply (head1 : newgoals) = new
  Env1 env vn <- get
  case execStateT (unify goal head1) env of
    Nothing -> lift []
    Just newenv -> do
      put $ Env1 newenv vn
      doAnd db newgoals

inst goal = runStateT (instantiate goal) $ Env1 M.empty 0

instantiate goal = do
  Env1 env vn <- get
  let names = getNames goal
  let new = rename (M.fromList $ zip names [vn ..]) goal
  put $ Env1 env $ vn + length names
  return new

{-
>>> inst test
(Apply [String1 "append",Var 0,Var 1,Apply [String1 "cons",String1 "a",Apply [String1 "cons",String1 "b",String1 "nil"]]],Env1 (fromList []) 2)

(MonadState Env (t []), MonadTrans t) => t [] ()

use lift to bting list monad stuff into the state monad
-}

doAnd db = mapM_ (doOr db)

doOr db newgoal = lift db >>= doClause db newgoal

getNames (VarName n) = [n]
getNames (Apply as) = concatMap getNames as
getNames _ = []

rename env (VarName n) = Var $ env M.! n
rename env (Apply as) = Apply $ map (rename env) as
rename _ x = x

setv var val = modify (\env -> M.insert (getv1 env var) val env)

getv1 env var = case M.lookup var env of
  Just (Var next) -> getv1 env next
  _ -> var

getv var = do
  env <- get
  return $ M.lookup (getv1 env var) env

assert a = if a then lift $ Just () else lift Nothing

{- >>> execStateT (unify (Var 1) (String1 "nil")) M.empty
Just (fromList [(1,String1 "nil")])

>>> execStateT (unify (prepare 0 test) (head db)) M.empty
Nothing

>>> head db
Apply [Apply [String1 "append",String1 "nil",VarName "Bs",VarName "Bs"]]
-}

unify :: Expr -> Expr -> StateT Env Maybe ()
unify (Int1 a) (Int1 b) = assert $ a == b
unify (String1 a) (String1 b) = assert $ a == b
unify (Apply as) (Apply bs) = do assert $ length as == length bs; zipWithM_ unify as bs
unify (Var a) (Var b) = do
  a1 <- getv a
  case a1 of
    Nothing -> setv a (Var b)
    Just a2 -> do
      b1 <- getv b
      case b1 of
        Nothing -> setv b a2
        Just b2 -> unify a2 b2
unify (Var a) b = do a1 <- getv a; case a1 of Nothing -> setv a b; Just a2 -> unify a2 b
unify a (Var b) = do b1 <- getv b; case b1 of Nothing -> setv b a; Just b2 -> unify a b2
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
