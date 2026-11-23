module HashDyn where

import Dyn
import NewTuple

import Data.IORef
import qualified Data.Set as S

newtype HashDyn = HashDyn Dynamic

instance Dyn HashDyn

data Expr
   = Value   { etype :: MType, resIndex :: Int, value :: HashDyn }
   | VarRef  { etype :: MType, resIndex :: Int, name :: String, frameIndex :: Int, memIndex :: Int }
   | Let1    { etype :: MType, resIndex :: Int, name :: String, frameIndex :: Int, memIndex :: Int }
   | VarDef  { xtype :: Expr , resIndex :: Int, name :: String, frameIndex :: Int, memIndex :: Int }
   | VarRef1 { etype :: MType, resIndex :: Int, name :: String }
   | Lambda  { etype :: MType, resIndex :: Int, econstr :: Constr,                     subexpr :: Expr }
   | Let     { etype :: MType, resIndex :: Int, econstr :: Constr, subexprs :: [Expr], subexpr :: Expr }
   | Block   { etype :: MType, resIndex :: Int, econstr :: Constr, subexprs :: [Expr] }
   | Apply   { etype :: MType, resIndex :: Int,                    subexprs :: [Expr] }
   | Split   { etype :: MType, resIndex :: Int,                    subexprs :: [Expr] }
   | SplitJ  { etype :: MType, resIndex :: Int,                    subexprs :: [Expr] }
   | Exprs   { etype :: MType, resIndex :: Int,                    subexprs :: [Expr] }
   | If      { etype :: MType, resIndex :: Int,                                  clauses :: [(Expr :- Expr)] }
   | If1     { etype :: MType, resIndex :: Int, then1 :: Expr,                   clauses :: [(Expr :- Expr)] }
   | Case    { etype :: MType, resIndex :: Int, of1   :: Expr,                   clauses :: [(Expr :- Expr)] }
   | Case1   { etype :: MType, resIndex :: Int,                                  clauses :: [(Expr :- Expr)] }
   | Case2   { etype :: MType, resIndex :: Int, of2   :: HashDyn,                clauses :: [(Expr :- Expr)] }
   | Case3   { etype :: MType, resIndex :: Int, of2   :: HashDyn, then1 :: Expr, clauses :: [(Expr :- Expr)] }
   | Else    { etype :: MType, resIndex :: Int }
   | Keyword { key :: String }
   | Join    { ejoiner :: Expr, eres :: Expr }
   | Join1   { eres :: Expr }
   | Join2   { joiner :: Joiner }    
   | SetJ    { joiner :: Joiner }
   | Apply2  { }
   | Tokens  (S.Set Char)

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
