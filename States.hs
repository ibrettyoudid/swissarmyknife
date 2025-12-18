module States where

import Parser3Types

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

--data states  a = states  { set :: S.Set a, list :: [a]}
newtype StateSeq tok = StateSeq (M.Map int (States tok)) 

data State tok = Scan     { item :: Item tok, from :: [State tok] }
               | Predict  { item :: Item tok, from :: [State tok] } 
               | Scanned  { tk   ::      tok, prev :: [State tok] }
               | Complete { item :: Item tok, from :: [State tok], ast :: Dynamic }
               | Previous { item :: Item tok, prev :: [State tok] }
               deriving (Eq, Ord)

{-
prediction: need to be able to check if we already have predicted something

scanning: need to be able to find all the scan items for each token

completion: need to be able to find all the running mains from a finished sub
            and then all the other subs
            and then to create a finished main,
               which contains a tree of all possible parses
            and, to continue
            need to be able to find all the running supers from that main

            need to be able to tell if we've already completed something

choices:
            in summary, subs know their mains and previous sub/alternative subs, and mains know their last sub
               but don't want to have to build a parse tree every time we do an Apply
            in summary, subs know their mains, and mains know their subs 
               mains know their subs only through the AST

                                                         parse end
            start symbol                                 complete
               ^                                             |
               |                                             |
               |                                             v
            seq start        seq next                    seq end
            predict          complete <--+                complete
               []           [A]  |        \                  |
               ^                 |         \                 |
               |                 |          \                |
               |                 v           \               v
               predict A <----- scanned    predict <----- scanned

-}

data States tok = States { set :: S.Set (State tok), list :: [State tok], byFrom :: M.Map Int (State tok), bySubRule :: M.Map Rule (State tok) }

empty = States S.empty [] M.empty M.empty

insert v states   | member v states  = states
                  | otherwise        = states  (S.insert v $ set states) (v:list states)

insertD states  (v, fv) | member fv states  = ((False, v, fv), states )
                        | otherwise         = ((True , v, fv), insert fv states )

fromList l = L.foldr insert empty l

fromSet s = states  s $ S.toList s

member v states  = S.member v $ set states 

map f sl = fromList $ L.map f $ list sl

union a b = let
   s = S.union (set a) (set b)
   l = list a ++ filter (\x -> not $ S.member x $ set a) (list b)
   in states  s l

a \\ b = let
   s = set a S.\\ set b
   l = L.filter (flip S.member s) (list a)
   in states  s l

null a = S.null $ set a

singleton a = states  (S.singleton a) (L.singleton a)

instance Show a => Show (states  a) where
   show a = "fromList "++show (list a)

