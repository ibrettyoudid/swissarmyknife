module HashDyn where

import Dyn
import NewTuple

newtype HashDyn = HashDyn Dynamic

instance Dyn HashDyn

data Expr
   = Value   {etype :: MType, value :: HashDyn}
   | VarRef  {etype :: MType, name :: String, frameIndex :: Int, memIndex :: Int}
   | VarDef  {xtype :: Expr , name :: String, frameIndex :: Int, memIndex :: Int}
   | VarRef1 {etype :: MType, name :: String}
   | Lambda  {etype :: MType, econstr :: Constr,                     subexpr :: Expr}
   | Let     {etype :: MType, econstr :: Constr, subexprs :: [Expr], subexpr :: Expr}
   | Block   {etype :: MType, econstr :: Constr, subexprs :: [Expr]}
   | Apply   {etype :: MType,                    subexprs :: [Expr]}
   | If      {etype :: MType,                clauses :: [(Expr :- Expr)]}
   | Case    {etype :: MType, case1 :: Expr, clauses :: [(Expr :- Expr)]}
   | Else
   | Exprs   {etype :: MType, exprs1 :: [Expr]}
   | Keyword String

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
