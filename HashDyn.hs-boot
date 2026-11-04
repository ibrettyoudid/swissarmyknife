module HashDyn where

import Dyn
import NewTuple

import Data.IORef

newtype HashDyn = HashDyn Dynamic

instance Dyn HashDyn

data Expr
   = Value   { etype :: MType, value :: HashDyn }
   | VarRef  { etype :: MType, name :: String, frameIndex :: Int, memIndex :: Int }
   | Let1    { etype :: MType, name :: String, frameIndex :: Int, memIndex :: Int }
   | VarDef  { xtype :: Expr , name :: String, frameIndex :: Int, memIndex :: Int }
   | VarRef1 { etype :: MType, name :: String }
   | Lambda  { etype :: MType, econstr :: Constr,                     subexpr :: Expr }
   | Let     { etype :: MType, econstr :: Constr, subexprs :: [Expr], subexpr :: Expr }
   | Block   { etype :: MType, econstr :: Constr, subexprs :: [Expr] }
   | Apply   { etype :: MType,                    subexprs :: [Expr] }
   | Split   { etype :: MType,                    subexprs :: [Expr] }
   | SplitJ  { etype :: MType,                    subexprs :: [Expr] }
   | Exprs   { etype :: MType,                    subexprs :: [Expr] }
   | If      { etype :: MType,                                  clauses :: [(Expr :- Expr)] }
   | If1     { etype :: MType, then1 :: Expr,                   clauses :: [(Expr :- Expr)] }
   | Case    { etype :: MType, of1   :: Expr,                   clauses :: [(Expr :- Expr)] }
   | Case1   { etype :: MType,                                  clauses :: [(Expr :- Expr)] }
   | Case2   { etype :: MType, of2   :: HashDyn,                clauses :: [(Expr :- Expr)] }
   | Case3   { etype :: MType, of2   :: HashDyn, then1 :: Expr, clauses :: [(Expr :- Expr)] }
   | Else
   | Keyword { key :: String }
   | Join    { ejoiner :: Expr, eres :: Expr }
   | Join1   { eres :: Expr }
   | Join2   { joiner :: Joiner }    
   | SetJ    { joiner :: Joiner }
   | Apply2  { }
   | CreateJ (IORef Int) (IORef HashDyn) [IORef HashDyn] 

data Joiner = Joiner { arg :: HashDyn, counter :: IORef Int, resref :: IORef HashDyn, resrefs :: [IORef HashDyn] }

data Closure = Closure Constr Expr Env 

type Env = [Frame]

data Frame = Frame {fconstr :: Constr, items :: [HashDyn]}

data PartApply = PartApply [HashDyn] Constr Expr Env

data TC = Trans | Cum

data Person = Tyrone | Danny | James | David

data Field = Borrowed | Profit | Owed

instance Eq Expr

instance Eq HashDyn

instance Ord HashDyn

instance Show HashDyn

toDyn :: Typeable a => a -> HashDyn
fromDyn :: Typeable a => HashDyn -> a -> a
fromDynamic :: Typeable a => HashDyn -> Maybe a
