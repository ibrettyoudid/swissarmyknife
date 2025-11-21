{-# LANGUAGE TemplateHaskell #-}

--module Control.Isomorphism.Partial.Constructors 
module Iso
(
  module Iso,
  module SyntaxCIPU
)
where

import SyntaxTH
import SyntaxCIPU

import NewTuple

import Prelude hiding (foldl, foldr, iterate, id, (.))

import qualified Prelude as P

import Data.Bool (Bool, otherwise)
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)))
import Data.Maybe 
import Data.List qualified

-- module Control.Isomorphism.Partial.Derived 
--import Prelude ()

--module Control.Isomorphism.Partial.Prim
--import Prelude ()

import Control.Monad (liftM2, (>=>), fmap, mplus)
import Control.Category (Category (id, (.)))

--import Data.Bool (Bool, otherwise)
--import Data.Either (Either (Left, Right))
--import Data.Eq (Eq ((==)))
--import Data.Maybe (Maybe (Just, Nothing))

-- module Control.Isomorphism.Partial.TH 
import Language.Haskell.TH
--import Control.Monad
import Data.List (find)
import Data.Char (toLower)
import Chess (coln)
import Favs (crossList)

-- module Control.Isomorphism.Partial.Unsafe

--import Prelude ()
--import Data.Maybe (Maybe ())

-- module Control.Isomorphism.Partial.Unsafe

instance Show (Iso a b) where
  show i = "iso"

$(defineIsomorphisms ''Either)
$(defineIsomorphisms ''Maybe)

-- module Control.Isomorphism.Partial.Derived 
foldl :: Iso (alpha :- beta) alpha -> Iso (alpha :- [beta]) alpha
foldl i = inverse unit 
        . (id *** inverse nil) 
        . iterate (step i) where

  step i = (i *** id) 
         . associate 
         . (id *** inverse cons)

-- module Control.Isomorphism.Partial.Constructors 
-- why is nil a function?

-- nil () = []

nil :: Iso () [alpha]
nil = Iso f g where
  f ()  =  Just []
  g []  =  Just ()
  g _   =  Nothing

cons :: Iso (alpha :- [alpha]) [alpha]
cons = Iso f g where
  f (x :- xs) =  Just (x :  xs)
  g (x : xs)  =  Just (x :- xs)
  g _         =  Nothing
  
listCases :: Iso (Either () (alpha :- [alpha])) [alpha]
listCases = Iso f g
  where
    f (Left ())        =  Just []
    f (Right (x :- xs))  =  Just (x : xs)
    g []               =  Just (Left ())
    g (x:xs)           =  Just (Right (x :- xs))

--module Control.Isomorphism.Partial.Prim

inverse :: Iso alpha beta -> Iso beta alpha
inverse (Iso f g) = Iso g f

apply :: Iso alpha beta -> alpha -> Maybe beta
apply (Iso f g) = f

unapply  ::  Iso alpha beta -> beta -> Maybe alpha
unapply  =   apply . inverse

instance Category Iso where
  g . f  =  Iso  (apply f >=> apply g) 
                 (unapply g >=> unapply f)
  id     =  Iso  Just Just

{-
infix 5 <$>
class IsoFunctor f where
  (<$>) :: Iso alpha beta -> (f alpha -> f beta)
-}
ignore :: alpha -> Iso alpha ()
ignore x = Iso f g where
  f _   =  Just ()
  g ()  =  Just x

-- | the product type constructor `(:-)` is a bifunctor from 
-- `Iso` $\times$ `Iso` to `Iso`, so that we have the 
-- bifunctorial map `***` which allows two separate isomorphisms 
-- to work on the two components of a tuple.
(***) :: Iso alpha beta -> Iso gamma delta -> Iso (alpha :- gamma) (beta :- delta)
i *** j = Iso f g where
  f (a :- b) = liftM2 (:-) (apply i a) (apply j b) 
  g (c :- d) = liftM2 (:-) (unapply i c) (unapply j d) 

-- | The mediating arrow for sums constructed with `Either`.
-- This is not a proper partial isomorphism because of `mplus`.
(|||) :: Iso alpha gamma -> Iso beta gamma -> Iso (Either alpha beta) gamma
i ||| j = Iso f g where
  f (Left x) = apply i x
  f (Right x) = apply j x
  g y = (Left `fmap` unapply i y) `mplus` (Right `fmap` unapply j y)

 
-- | Nested products associate. 
associate :: Iso (alpha :- (beta :- gamma)) ((alpha :- beta) :- gamma)
associate = Iso f g where
  f (a :- (b :- c)) = Just ((a :- b) :- c)
  g ((a :- b) :- c) = Just (a :- (b :- c))

-- | Products commute.
commute :: Iso (alpha :- beta) (beta :- alpha)
commute = Iso f f where
  f (a :- b) = Just (b :- a)

-- | `()` is the unit element for products. 
unit :: Iso alpha (alpha :- ())
unit = Iso f g where
  f a = Just (a :- ())
  g (a :- ()) = Just a

-- | Products distribute over sums.
distribute  ::  Iso (alpha :- Either beta gamma) (Either (alpha :- beta) (alpha :- gamma))
distribute  =   Iso f g where
  f (a :- Left   b)    =  Just (Left   (a :- b))
  f (a :- Right  c)    =  Just (Right  (a :- c))
  g (Left   (a :- b))  =  Just (a :-  Left   b)
  g (Right  (a :- b))  =  Just (a :-  Right  b)
  
-- | `element x` is the partial isomorphism between `()` and the 
-- singleton set which contains just `x`.
element :: Eq alpha => alpha -> Iso () alpha
element x = Iso 
  (\a -> Just x)
  (\b -> if x == b then Just () else Nothing)

-- | For a predicate `p`:- `subset p` is the identity isomorphism
-- restricted to elements matching the predicate.
subset :: (alpha -> Bool) -> Iso alpha alpha
subset p = Iso f f where
  f x | p x = Just x | otherwise = Nothing

iterate :: Iso alpha alpha -> Iso alpha alpha
iterate step = Iso f g where
  f = Just . driver (apply step)
  g = Just . driver (unapply step)
  
  driver :: (alpha -> Maybe alpha) -> (alpha -> alpha)
  driver step state 
    =  case step state of
         Just state'  ->  driver step state'
         Nothing      ->  state

match c = Iso (\x -> Just c) (\x -> if x == c then Just () else Nothing)

match1 c = satisfy (==c)

total f g = Iso (Just . f) (Just . g)

satisfy p = Iso f f where f x = if p x then Just x else Nothing

(!!) list = Iso (Just . (list Prelude.!!)) (`Data.List.elemIndex` list)

(!!!) lists = Iso 
  (\[row, col] -> Just (lists P.!! row P.!! col))
  (\e -> do (row, blah) <- findWithIndex2 (== e) lists; (col, val) <- blah; return [row, col])

(!!!!) lists = Iso 
  (\[page, row, col] -> Just (lists P.!! page P.!! row P.!! col))
  (\e -> findWithIndex3 (== e) lists)
{-}    
map (\[a, b, c] -> lists P.!! page P.!! row P.!! col)
  crossList [[0..pages-1], [0..rows-1], [0..cols-1]]
  from
  find isJust $ zip [0..] $ map (find ((>= 0) . snd) $ zip [0..] $ map (fromMaybe (-1) . Data.List.elemIndex e)) lists)
-}
findWithIndex p xs = find (p . snd) $ zip [0..] xs

findWithIndex2 p xss = findWithIndex isJust $ map (findWithIndex p) xss

findWithIndex3 p xsss = case findWithIndex isJust $ map (findWithIndex isJust . map (findWithIndex p)) xsss of
  Just (a, Just (b, Just (c, d))) -> Just [a, b, c]
  _ -> Nothing
xx listss [page, row, col] = listss P.!! page P.!! row P.!! col
-- module Control.Isomorphism.Partial.TH 
{-
-- | Extract the name of a constructor:- e.g. ":" or "Just".
conName :: Con -> Name
conName (NormalC name fields)       =   name
conName (RecC name fields)          =   name
conName (InfixC lhs name rhs)       =   name
conName (ForallC vars context con)  =   conName con

-- | Extract the types of the constructor's fields.
conFields :: Con -> [Type]
conFields (NormalC name fields)       =   map (\(s:- t) -> t) fields
conFields (RecC name fields)          =   map (\(n:- s:- t) -> t) fields
conFields (InfixC lhs name rhs)       =   map (\(s:- t) -> t) [lhs:- rhs]
conFields (ForallC vars context con)  =   conFields con

-- | Extract the constructors of a type declaration
decConstructors :: Dec -> Q [Con]
decConstructors (DataD _ _ _ cs _)    =  return cs
decConstructors (NewtypeD _ _ _ c _)  =  return [c]
decConstructors _                      
  = fail "partial isomorphisms can only be derived for constructors of data type or newtype declarations."

-- | Construct a partial isomorphism expression for a constructor:- 
-- given the constructor's name.
constructorIso :: Name -> ExpQ
constructorIso c = do
  DataConI n _ d _  <-  reify c
  TyConI dec        <-  reify d
  cs                <-  decConstructors dec
  let Just con      =   find (\c -> n == conName c) cs
  isoFromCon (wildcard cs) con

wildcard :: [Con] -> [MatchQ]
wildcard cs 
  =  if length cs > 1
     then  [match (wildP) (normalB [| Nothing |]) []]
     else  []

-- | Converts a constructor name (starting with an upper-case
--   letter) into a function name (starting with a lower-case
--   letter).
rename :: Name -> Name
rename n 
  = mkName (toLower c : cs) where c : cs = nameBase n

-- | Construct partial isomorphism definitions for all 
--   constructors of a datatype:- given the datatype's name.
--   The names of the partial isomorphisms are constructed by
--   spelling the constructor names with an initial lower-case
--   letter.
defineIsomorphisms :: Name -> Q [Dec]
defineIsomorphisms d = do
  TyConI dec  <-  reify d
  cs          <-  decConstructors dec
  mapM (defFromCon (wildcard cs)) cs

-- | Constructs a partial isomorphism definition for a
--   constructor:- given information about the constructor.
--   The name of the partial isomorphisms is constructed by
--   spelling the constructor name with an initial lower-case
--   letter.
defFromCon :: [MatchQ] -> Con -> DecQ
defFromCon wildcard con
  = funD (rename (conName con)) 
      [clause [] (normalB (isoFromCon wildcard con)) []]

-- | Constructs a partial isomorphism expression for a
--   constructor:- given information about the constructor.
isoFromCon :: [MatchQ] -> Con -> ExpQ
isoFromCon wildcard con = do
  let c     =   conName con
  let fs    =   conFields con
  let n     =   length fs
  (ps:- vs)  <-  genPE n
  v         <-  newName "x"
  let f     =   lamE [nested tupP ps] 
                  [| Just $(foldl appE (conE c) vs) |]
  let g     =   lamE [varP v] 
                  (caseE (varE v) $ 
                    [ match (conP c ps) 
                        (normalB [| Just $(nested tupE vs) |]) []
                    ] ++ wildcard)
  [| Iso $f $g |]



genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids:- map varE ids)

nested tup []      =  tup [] 
nested tup [x]     =  x
nested tup (x:xs)  =  tup [x:- nested tup xs]

-}