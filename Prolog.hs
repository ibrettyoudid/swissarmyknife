-- Copyright 2025 Brett Curtis
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE LambdaCase #-}

module Prolog where

import Control.Monad.State
import Data.Char
import Data.Map qualified as M
import Data.IntMap qualified as I
import Data.List
import Debug.Trace

data Expr = Apply [Expr] | I Int | Sym String | Str String | Var Int | Name String | List [Expr] | Imp [Expr] Expr | Braced Expr deriving (Eq, Ord, Show)

data Env1 = Env1 Env Int deriving (Eq, Ord, Show)

type Env = I.IntMap Expr

db =
  [ Apply [Apply [Sym "append", Sym "nil", Name "Bs", Name "Bs"]]
  , Apply [Apply [Sym "append", Apply [Sym "cons", Name "A", Name "As"], Name "Bs", Apply [Sym "cons", Name "A", Name "Cs"]], Apply [Sym "append", Name "As", Name "Bs", Name "Cs"]]
  ]

test = [Apply [Sym "append", Name "X", Name "Y", expandSym "abcd"]]
{- >>> contractEList $ run db test
Couldn't match type `[(String, Expr)]' with `Env1'
Expected: [Env1]
  Actual: [[(String, Expr)]]
In the second argument of `($)', namely `run db test'
In the expression: contractEList $ run db test
In an equation for `it_aotQe':
    it_aotQe = contractEList $ run db test
-}

parser = 
  Apply [Sym "=", Name "X", Name "X"] : map expandTerm [
    Apply [Sym "-->", Apply [Sym "num1"]],
    Apply [Sym "-->", Apply [Sym "num1"], Apply [Sym "num"]],
    Apply [Sym "-->", Apply [Sym "num"], List [Sym "0"], Apply [Sym "num1"]]]
{-
>>> parser
[Apply [Sym "=",Name "X",Name "X"],Apply [Sym "-->",Apply [Sym "num1",Name "ET1",Name "ET1"]],Apply [Sym "-->",Apply [Sym "num1",Name "ET1",Name "ET2"],Apply [Sym "num",Name "ET1",Name "ET2"]],Apply [Sym "-->",Apply [Sym "num",Name "ET1",Name "ET3"],Apply [Sym "=",Name "ET1",Imp [Sym "0"] (Name "ET2")],Apply [Sym "num1",Name "ET2",Name "ET3"]]]


>>> parse [Apply [Sym "num"]] "0000"
[]
-}

parse xs s = let
  l    = (+1) $ length $ filter (\case { Braced {} -> False; _ -> True }) xs
  vars = map (\n -> Name ("ET" ++ show n)) [1..l] 
  vars1 = init vars     
  vars2 = tail vars

  es = expandTerm0 xs vars1 vars2

  s1 = Apply [Sym "=", head vars, expandSym1 s $ last vars]
  in run parser $ s1 : es

run db goals = map (\(vars1, Env1 env n) -> let
  vars2 = I.fromList $ map (\(name, num) -> (num, name)) vars1
  in map (\(name, num) -> (name, rename1 vars2 $ env I.! num)) vars1) $
    runStateT
      (do
          (Apply new, vars1) <- instantiate $ Apply goals
          doAnd db new
          return vars1
      )
      $ Env1 I.empty 0
  
  

doClause db goal clause = do
  new <- instantiate clause
  let Apply (head1 : newgoals) = fst new
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
  let newvars = zip names [vn..]
  let new = rename (M.fromList newvars) goal
  put $ Env1 env $ vn + length newvars
  return (new, newvars)

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

expandSym1 xs y = expandList1 (map (Sym . singleton) xs) y

expandStr = expandList . map (I . ord)

expandTerm (Apply (Sym "-->":xs)) = let
  l    = length $ filter (\case { Braced {} -> False; _ -> True }) xs
  vars = map (\n -> Name ("ET" ++ show n)) [1..l] 
  vars1 = head vars:init vars     
  vars2 = last vars:tail vars

  in Apply (Sym "-->":expandTerm0 xs vars1 vars2) 

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
  l    = length $ filter (\case { Braced {} -> False; _ -> True }) xs
  vars = map (\n -> Name ("ET" ++ show n)) [1..l] 
  vars1 = head vars:init vars
  vars2 = last vars:tail vars
  in do
    print l
    z <- expandTerm0D xs vars1 vars2
    return $ Apply (Sym "-->":z)

{-
>>> expandTerm (Apply [Sym "-->", Sym "np", Sym "adj", Sym "n"])
/home/brett/swissarmyknife/Prolog.hs:(121,1)-(125,94): Non-exhaustive patterns in function expandTerm1
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

rename1 renv (Var n) = Name $ renv I.! n
rename1 renv (Apply as) = Apply $ map (rename1 renv) as
rename1 _ x = x

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
