-- Copyright 2025 Brett Curtis
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Eta reduce" #-}

module Prolog1 where -- one environment

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.State
import Data.Char
import Data.Map qualified as M
import Data.Set qualified as S
import Data.IntMap qualified as I
import Data.List
import Data.Maybe
import MHash (Doc(docWidth))
import TestBoomerang (id3Base)
--import qualified BString as S
{-
data Expr = 
   Apply [Expr]
    | I Int
    | Sym String 
    | Str String 
    | Var (TVar Assign)
    | Name String 
    | List [Expr] 
    | Imp [Expr] Expr
    deriving (Eq)

data Assign = Blank | Assign Branch Expr (TVar Assign)

data Branch = Root | Branch { bt::BranchT, parent::Branch, threadId::TVar ThreadId, kids::TVar [Branch] }

data BranchT = Or | And

data Env1 = Env1 { eenv :: Env, evar :: Int, ebranch :: Int } deriving (Eq)

type Env = I.IntMap Expr

db =
   [ Apply [Apply [Sym "append", Sym "nil", Name "Bs", Name "Bs"]]
   , Apply [Apply [Sym "append", Apply [Sym "cons", Name "A", Name "As"], Name "Bs", Apply [Sym "cons", Name "A", Name "Cs"]], Apply [Sym "append", Name "As", Name "Bs", Name "Cs"]]
   ]

test = Apply [Sym "append", Name "X", Name "Y", expandSym "abcd"]

{- >>> contractEList $ run db test
[fromList [(0,Sym ""),(1,Sym "abcd"),(3,Sym "abcd")],fromList [(0,Sym "a"),(1,Sym "bcd"),(5,Sym "a"),(7,Sym ""),(8,Sym "bcd"),(9,Sym "bcd"),(11,Sym "bcd")],fromList [(0,Sym "ab"),(1,Sym "cd"),(5,Sym "a"),(7,Sym "b"),(8,Sym "cd"),(9,Sym "bcd"),(13,Sym "b"),(15,Sym ""),(16,Sym "cd"),(17,Sym "cd"),(19,Sym "cd")],fromList [(0,Sym "abc"),(1,Sym "d"),(5,Sym "a"),(7,Sym "bc"),(8,Sym "d"),(9,Sym "bcd"),(13,Sym "b"),(15,Sym "c"),(16,Sym "d"),(17,Sym "cd"),(21,Sym "c"),(23,Sym ""),(24,Sym "d"),(25,Sym "d"),(27,Sym "d")],fromList [(0,Sym "abcd"),(1,Sym ""),(5,Sym "a"),(7,Sym "bcd"),(8,Sym ""),(9,Sym "bcd"),(13,Sym "b"),(15,Sym "cd"),(16,Sym ""),(17,Sym "cd"),(21,Sym "c"),(23,Sym "d"),(24,Sym ""),(25,Sym "d"),(29,Sym "d"),(31,Sym ""),(32,Sym ""),(33,Sym ""),(35,Sym "")]]
-}
run db goal = do
   new <- inst goal
   kids <- newTVarIO []
   doClauses db new $ Branch And Root 0 kids

doClause db goal clause br = do
   new <- atomically $ instantiate clause
   let Apply (head1 : newgoals) = new
   res <- atomically $ unify br goal head1
   case res of
      False -> return False
      True  -> doGoals db newgoals br

inst goal = atomically $ instantiate goal

instantiate goal = do
   --Env1 env vn br <- get
   let names = getNames goal
   vars <- replicateM (length names) (newTVar Blank)
   let new = rename (M.fromList $ zip names vars) goal
   --put $ Env1 env (vn + length names) br
   return new

{-
>>> inst test
(Apply [Sym "append",Var 0,Var 1,Apply [Sym "cons",Sym "a",Apply [Sym "cons",Sym "b",Sym "nil"]]],Env1 (fromList []) 2)

(MonadState Env (t []), MonadTrans t) => t [] ()

use lift to bring list monad stuff into the state monad
-}
branch bt br actions = do
   z <- mapM (\action -> do
      v1 <- newTVarIO 0
      v2 <- newTVarIO []
      let b = Branch bt br v1 v2
      a <- async (do my <- myThreadId; atomically $ writeTVar v1 my; action b)
      return (b, a)) actions
   let (bs, as) = unzip z
   atomically $ writeTVar (kids br) bs
   return as

doGoals db goals br = do
   as <- branch And br $ map (doClauses db) goals
   doGoals1 as

doGoals1 [ ] = return True
doGoals1 as = do
   (a, re) <- waitAny as
   if not re 
      then do mapM_ cancel as; return False
      else doGoals1 $ delete a as

doClauses db goal br = do
   as <- branch Or br $ map (doClause db goal) db
   doClauses1 as

doClauses1 [ ] = return False
doClauses1 as = do
   (a, re) <- waitAny as
   doClauses1 $ delete a as

expandList xs = foldr (\x y -> Apply [Sym "cons", x, y]) (Sym "nil") xs

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

contractEnv env = I.map (contractSym . (`contractList` env)) env

contractEnv1 (Env1 env _ _) = contractEnv env

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

assert a = if a then lift $ Just () else lift Nothing

{- >>> execStateT (unify (Var 1) (Sym "nil")) M.empty
Just (fromList [(1,Sym "nil")])

>>> execStateT (unify (prepare 0 test) (head db)) M.empty
Nothing

>>> head db
Apply [Apply [Sym "append",Sym "nil",Name "Bs",Name "Bs"]]
-}



unifyList bra as bs = 
   if | null as && null bs -> return True
       | null as || null bs -> return False
       | otherwise -> do 
               next <- unify bra (head as) (head bs)
               if next
                  then unifyList bra (tail as) (tail bs)
                  else return False

unifyList1 bra as bs = foldl (\done (a, b) -> do d <- done; if d then unify bra a b else return False) (return True) $ zip as bs

--unifyList bra brb as bs = foldM (\done (a, b) -> do d <- done; if d then unify bra brb a b else return False) (return True) $ zip as bs

unify :: Branch -> Expr -> Expr -> STM Bool
unify _   (I      a) (I      b) = return $ a == b
unify _   (Sym    a) (Sym    b) = return $ a == b
unify _   (Str    a) (Str    b) = return $ a == b
unify bra (Apply as) (Apply bs) = do 
   if length as /= length bs 
      then return False 
      else unifyList bra as bs

unify bra (Var a) b = doVar bra a b
unify bra a (Var b) = doVar bra b a
unify bra _ _ = return False

doVar bra a b = do
   a1 <- readTVar a
   case a1 of
      Blank -> do
         a2 <- newTVar Blank
         writeTVar a $ Assign bra b a2
      Assign brc exc elc -> do
         b1 <- readTVar b
         case b1 of
            Blank -> setv b a2
            b2 -> unify bra a2 b2


   -- it only matters what type of branch the most recent common ancestor is
   -- if it's an AND then they must unify
   -- if it's an OR they may both be true but have different values so don't unify (unless rejoining?)
      -- is there any point recording this event? i don't think so
common bra brb = let 
   seq1 br = iterate (\(x, y) -> (parent x, S.insert x y)) (br, S.empty) 
   seqa = seq1 bra
   seqb = seq1 brb
   in case catMaybes $ zipWith (\(a, sa) (b, sb) -> 
         if | S.elem a sb -> Just a
            | S.elem b sa -> Just b
            | otherwise   -> Nothing) seqa seqb of
         []    -> error "branches are not connected"
         (x:_) -> x

{- The result is a list of var assignments for each thread

unifyList bra brb as bs = 
   if | null as && null bs -> return True
       | null as || null bs -> return False
       | otherwise -> do 
               next <- unify bra brb (head as) (head bs)
               if next
                  then unifyList bra brb (tail as) (tail bs)
                  else return False

unifyList1 bra brb as bs = foldl (\done (a, b) -> do d <- done; if d then unify bra brb a b else return False) (return True) $ zip as bs

--unifyList bra brb as bs = foldM (\done (a, b) -> do d <- done; if d then unify bra brb a b else return False) (return True) $ zip as bs

unify :: Branch -> Branch -> Expr -> Expr -> STM Bool
unify _   _   (I      a) (I      b) = return $ a == b
unify _   _   (Sym    a) (Sym    b) = return $ a == b
unify _   _   (Str    a) (Str    b) = return $ a == b
unify bra brb (Apply as) (Apply bs) = do 
   if length as /= length bs 
      then return False 
      else unifyList bra brb as bs

unify bra brb (Var    a) (Var    b) = do
   a1 <- readTVar a
   case a1 of
      Blank -> do
         a2 <- newTVar Blank
         setv a $ Assign bra (Var b) a2
      Assign  -> do
         b1 <- readTVar b
         case b1 of
            Blank -> setv b a2
            b2 -> unify bra brb a2 b2
unify bra brb (Var a) b = do a1 <- readTVar a; case a1 of E -> setv a b; a2 -> unify bra brb a2 b
unify bra brb a (Var b) = do b1 <- readTVar b; case b1 of E -> setv b a; b2 -> unify bra brb a b2
unify bra brb _ _ = return False
 -
 - append([], Bs, Bs).
 - append([A|As], Bs, [A|Cs]) :- append(As, Bs, Cs).
 -
 - ? append(X, Y, "hello")
 - append(X=[], Y=Bs1, Bs1="hello")
 - X = ""
 - Y = "hello"
 -
 - append(X=[A1=h|As1], Y=Bs2, [A1=h|Cs1="ello"])
 - append(As1=[], Bs2=Bs3, Bs3=Cs1="ello").
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

sent --> np, vp.

np --> det, n.

det --> [a]

det --> [the]

n --> [car]

n --> [bus]

vp --> vit

det(E1, E2) :- E1 = [the | E2]
det(E3, E4) :- E3 = [a    | E4]
n   (E5, E6) :- E5 = [bus | E6]
n   (E7, E8) :- E7 = [car | E8]
np (EA, EC) :- det(EA, EB), n(EB, EC)

? phrase([the,bus], X)

np(X, [])
det(X=EA, EB)
det(X=EA=E1, EB=E2)
det(X=EA=E1=[the|E2], EB=E2)

det(X=EA=




branch(A, a(A, A)).
branch(B, b(B, B, B)).



 - -}

-}