-- Copyright 2025 Brett Curtis
import Data.IORef
import Data.Map.Lazy qualified as M

-- import Data.Typeable
-- import Type.Reflection hiding (TypeRep, typeRepTyCon)

data Expr
  = Value Dynamic
  | VarRef String Int Int
  | VarRef1 String
  | Lambda Constr Expr
  | Let Constr [Expr] Expr
  | Block Constr Expr
  | Apply [Expr]
  | If [(Expr, Expr)]
  | Case Expr [(Expr, Expr)]
  | Else
  | Stats [Expr]
  deriving (Show, Typeable)

data Closure = Closure Constr Expr Env deriving (Typeable)

type Env = [Frame]

data Frame = Frame {constr :: Constr, items :: [IORef Dynamic]}

data Constr = Constr {cname :: String, inames :: [String]} deriving (Show)

-- data Multimethod = Multimethod { name :: String, funcs :: M.Map [SomeTypeRep] Dynamic } deriving (Show, Typeable)

data PartApply = PartApply [Dynamic] Constr Expr Env deriving (Typeable)
