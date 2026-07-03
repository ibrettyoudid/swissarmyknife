module ArrayDyn where

import {-# SOURCE #-} Dyn

import qualified Data.Map as M
import qualified Data.Array.IArray as A

data Dimension name dyn
   = DimInt {dimName :: name, dimLower :: Int, dimUpper :: Int, dimMult :: Int}
   | DimMap {dimName :: name, dimLower :: Int, dimUpper :: Int, dimMult :: Int, dimMap1 :: M.Map Int dyn, dimMap2 :: M.Map dyn Int}
--            | DimCat { dimDim::Dimension, dimDiv::[Int], dimMult1::[Int] }

data SubArrayD e dimName dyn = SubArrayD {dims :: [Dimension dimName dyn], offset :: Int, payload :: A.Array Int e}

