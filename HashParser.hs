-- Copyright 2025 Brett Curtis
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{- HLINT ignore "Use map with tuple-section" -}

module HashParser where

import MyPretty2
import Favs
import Numeric
import NewTuple hiding (delete)
import qualified BString as B
import qualified HTTPTypes
import Dyn hiding (toDyn, fromDyn, fromDyn1, fromDyn2, fromDynamic)
import HashDyn
import qualified NumberParsers as NP
import Show1

import Parser3 hiding (Apply, pure)
import qualified Parser3 as P
import Iso2 hiding (foldl, foldr, right, (!!), ignore)

import Data.List
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import Data.IORef
import Data.Time.Calendar
import qualified Data.Array.IArray as A

import qualified Data.Dynamic as D
import Data.Typeable

import Type.Reflection hiding (TypeRep, typeOf, typeRepTyCon)

import GHC.Stack
--import Control.Exception

lexer = num <|> oplex <|> var <|> charlit <|> strlit

oplex = varIso >$< many1 (satisfy (`elem` "!£$%^&*-=+;@~#<>/?|") >$< anytoken)

punc = satisfy (`elem` "([{}]),.")

charlit = valueIso >$< text "'" *< anytoken >* text "'"

strlit = valueIso >$< text "\"" *< many anytoken >* text "\""

sepSpace = token ' '

singIso = Iso "singIso" (\a -> Just [a]) (\case [a] -> Just a; _ -> Nothing)

u = unknown

readShow = total "readShow" read show

num = "num" <=> valueIso >$< (readShow :: Iso String Int) >$< many1 (RangeR '0' '9')

numIso = valueIso :: Iso Int Expr

valueIso = Iso "valueIso" (Just . Value u . toDyn) (\case Value _ n -> fromDynamic n; _ -> Nothing)

varIso = Iso "varIso" (Just . VarRef1 u) varfn

varIsoT = Iso "varIsoT" (\(v :- t) -> Just $ VarRef1 t v) varfnt

var = "var" <=> varIso >$< identifier

vardefiso = Iso "vardef" (\(n :- t) -> Just $ VarDef t n 0 0) (\case VarDef t n _ _ -> Just (n :- t); _ -> Nothing)

vardef = "vardef" <=> vardefiso >$< text "var" *< identifier >*< ((text "::" *< expr) <|> (valueIso >$< P.pure u))

identifier = many1 (RangeR 'a' 'z')

typeanno = expr >*< text "::" *< expr

mem = Iso "mem" (Just . Member u) (\(Member _ n) -> Just n) >$< identifier

varfn (VarRef1 _ v) = Just v
varfn (VarRef _ v _ _) = Just v
varfn _ = Nothing

varfnt (VarRef t v _ _) = Just (v :- t)
varfnt (VarRef1 t v) = Just (v :- t)
varfnt _ = Nothing

app (Apply _ l) = l

--prediso p = Iso (ifPred p) (ifPred p)

opiso :: [String] -> Iso (Expr :- (String :- Expr)) Expr
opiso ops =
   Iso
      "opiso"
      (\(a :- op :- b) -> Just $ Apply u [VarRef1 u op, a, b])
      ( \case
         Apply _ [varfn -> Just op, a, b] -> ifJust (op `elem` ops) (a :- op :- b)
         _ -> Nothing
      )

opc :: [[Char]] -> RuleR Char String
opc ops = Prelude.foldr1 (<|>) $ map text ops

opl :: [[Char]] -> RuleR Char Expr -> RuleR Char Expr
opl ops term = chainl1 term (opc ops) $ opiso ops

opr :: [[Char]] -> RuleR Char Expr -> RuleR Char Expr
opr ops term = (chainr1) (term :: RuleR Char Expr) (opc ops :: RuleR Char String) (opiso ops :: Iso (Expr :- (String :- Expr)) Expr)

ops = ["+", "-", "*", "/", "$", ".", "=", "=="]

op = "op" <=> varIso >$< text "(" *< opc ops >* text ")"

leftsec = "leftsec" <=>
   Iso
      "leftsec"
      (\(a :- op) -> Just $ Lambda u (Co "lam" [Member u "b"]) (Apply u [VarRef1 u op, a, VarRef1 u "b"]))
      ( \case
         (Lambda _ (Co _ locals) (Apply _ [VarRef1 _ op, a, VarRef1 _ b])) -> ifJust (b `elem` map mname locals) (a :- op)
         _ -> Nothing
      )
      >$< text "(" *< term >*< opc ops >* text ")"

rightsec = "rightsec" <=>
   Iso
      "rightsec"
      (\(op :- b) -> Just $ Lambda u (Co "lam" [Member u "a"]) (Apply u [VarRef1 u op, VarRef1 u "a", b]))
      ( \case
         (Lambda _ (Co _ locals) (Apply _ [VarRef1 _ op, VarRef1 _ a, b])) -> ifJust (a `elem` map mname locals) (op :- b)
         _ -> Nothing
      )
      >$< text "(" *< opc ops >*< term >* text ")"

parens = text "(" *< expr >* text ")"

list = "list" <=> text "[" *< list2 >* text "]"

list3 = chainr1 expr (text ",")
list2 = Iso "list" (Just . Apply u . (VarRef1 u "list" :)) (\(Apply _ (f : xs)) -> ifJust (f == VarRef1 u "list") xs) >$< expr `sepBy` text ","

term = "term" <=> num <|> var <|> leftsec <|> rightsec

varname (VarRef1 _ v) = v
varname (VarRef _ v _ _) = v
varname _ = ""

applic =
   "application"
      <=> Iso
         "application"
         (\ts -> Just $ if length ts >= 2 then Apply u ts else head ts)
         ( \case
            Apply _ (f : xs) | notElem (varname f) ops -> Just (f : xs)
            x -> Just [x]
         )
      >$< sepBy term sepSpace

-- a :: A @ b :: B @ c :: C

expr10 = "expr10" <=> opiso ["@", "::"] >$< applic >*< opc ["@", "::"] >*< applic <|> applic

expr9 = "expr9" <=> opiso ["."] >$< expr9 >*< opc ["."] >*< expr10 <|> expr10

expr8 = "expr8" <=> expr9

expr7 = "expr7" <=> opiso ["*", "/"] >$< expr7 >*< opc ["*", "/"] >*< expr8 <|> expr8

-- expr7 = "expr7" <=> (opiso ["*"] >$< term >*< text1 "*" >*< expr7) <|> term

expr6 = "expr6" <=> opiso ["+", "-"] >$< expr6 >*< opc ["+", "-"] >*< expr7 <|> expr7

expr5 = "expr5" <=> expr6 

expr4 = "expr4" <=> opiso ["<", "<=", "==", ">=", ">", "/="] >$< expr5 >*< opc ["<", "<=", "==", ">=", ">", "/="] >*< expr5 <|> expr5

expr3 = "expr3" <=> opiso ["&&", "||"] >$< expr4 >*< opc ["&&", "||"] >*< expr3 <|> expr4

expr0 = "expr0" <=> opiso ["$", "="] >$< expr3 >*< opc ["$", "="] >*< expr0 <|> expr3

ifSyn = "ifSyn" <=>
   Iso
      "if"
      (Just . If u)
      (\case If _ blah -> Just blah; _ -> Nothing)
      >$< text "if"
      *< groupOf (expr0 >*< text "->" *< expr0 :: RuleR Char (Expr :- Expr))

conIso = Iso "conIso" (Just . Co "data") (\case Co _ members -> Just members)

lambdaSyn = "lambdaSyn" <=>
   Iso "lambdaSyn"
      (\(params :- exp) -> Just $ Lambda u (Co "" params) exp)
      (\case Lambda _ (Co _ params) exp -> Just (params :- exp); _ -> Nothing)
         >$< text "\\" *< many mem >*< text "->" *< expr0

blockSyn = "blockSyn" <=>
   Iso
      "blockSyn"
      (\exp -> Just $ Block u (Co "" []) exp)
      (\case Block _ _ exp -> Just exp; _ -> Nothing)
      >$< groupOf expr0

dataSyn = valueIso >$< conIso >$< text "data" *< sepSpace *< mem `sepBy` sepSpace

expr = "expr" <=> (ifSyn <|> lambdaSyn <|> blockSyn <|> expr0)

exprs = groupOf expr

