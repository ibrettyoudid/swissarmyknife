{-# LANGUAGE FlexibleInstances #-}
-- Copyright 2025 Brett Curtis
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Syntax3 where

import Favs hiding (indent, indent1, indent2, left, mode, right, swap)

import SyntaxCIPU
import SyntaxTH

import Prelude hiding (foldl, foldr, id, iterate, print, pure, (*<), (.), (>$<), (>*), (>*<))
import Prelude qualified

import Control.Category
import Control.Monad
import Data.Bits
import Data.Char
import Data.Kind
import Data.List qualified as L

{-}
import Text.Syntax
import Text.Syntax.Parser.Naive
import Text.Syntax.Printer.Naive

import qualified GHC.Cmm as Text.Syntax.Parser

-}
-- module Text.Syntax.Classes where

-- import qualified Language.Haskell.TH as Control.Isomorphism.Partial
-- import qualified GHC.Runtime.Eval as Control.Isomorphism.Partial
{-
import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.Unsafe
import Control.Isomorphism.Partial.TH
-}
import Data.Bool (Bool, otherwise)
import Data.Either (Either (Left, Right))
import Data.Maybe (Maybe (Just, Nothing))

import Data.Attoparsec.ByteString qualified as A
import Data.ByteString qualified as B

import System.IO.Unsafe

infix 2 <=>
infixl 3 <|>

-- infixl 4 <+>
infix 5 >$<
infixr 6 >*<

class ProductFunctor f where
  (>*<) :: f alpha -> f beta -> f (alpha, beta)

class Alternative f where
  (<|>) :: f alpha -> f alpha -> f alpha
  empty :: f alpha

class IsoFunctor f where
  (>$<) :: Iso alpha beta -> f alpha -> f beta

class (IsoFunctor delta, ProductFunctor delta, Alternative delta) => Syntax delta where
  -- (>$<)   ::  Iso alpha beta -> delta alpha -> delta beta
  -- (>*<)   ::  delta alpha -> delta beta -> delta (alpha, beta)
  -- (<|>)   ::  delta alpha -> delta alpha -> delta alpha
  -- empty   ::  delta alpha
  pure :: (Eq alpha) => alpha -> delta alpha
  token :: delta Char
  ci :: delta alpha -> delta alpha
  groupOf :: delta alpha -> delta [alpha]
  (<=>) :: String -> delta alpha -> delta alpha

class (Syntax delta) => SyntaxA string delta where
  textA :: string -> delta ()

{-
\*********************************************************************************
SINCE I KNOW NOTHING ABOUT TEMPLATE HASKELL I HAVE COMMENTED THIS BIT OUT
and tried to guess the 4 definitions below
-}
defineIsomorphisms ''Either
defineIsomorphisms ''Maybe

{-
just :: Iso alpha (Maybe alpha)
just = Iso (Just . Just) id

nothing :: Iso () (Maybe alpha)
nothing = Iso (const Nothing) (\Nothing -> Just ())

left :: Iso alpha (Either alpha beta)
left =
  Iso
    (Just . Left)
    ( \case
        Left x -> Just x
        Right x -> Nothing
    )

right :: Iso beta (Either alpha beta)
right =
  Iso
    (Just . Right)
    ( \case
        Left x -> Nothing
        Right x -> Just x
    )
-}
instance Category Iso where
  g . f =
    Iso
      (apply f >=> apply g)
      (unapply g >=> unapply f)
  id = Iso Just Just

-- derived combinators
many :: (Syntax delta) => delta alpha -> delta [alpha]
many p =
  nil
    >$< pure ()
    <|> cons
    >$< p
    >*< many p

many1 :: (Syntax delta) => delta alpha -> delta [alpha]
many1 p = cons >$< p >*< many p

(<+>) :: (Syntax delta) => delta alpha -> delta beta -> delta (Either alpha beta)
p <+> q = (left >$< p) <|> (right >$< q)

-- | `text` parses\/prints a fixed text and consumes\/produces a unit value.
text :: (Syntax delta) => String -> delta ()
text [] = pure ()
text (c : cs) =
  inverse (match ((), ()))
    >$< (inverse (match c) >$< token)
    >*< text cs

{- | This variant of `>*<` ignores its left result.
In contrast to its counterpart derived from the `Applicative` class, the ignored
parts have type `delta ()` rather than `delta beta` because otherwise information relevant
for pretty-printing would be lost.
-}
(*<) :: (Syntax delta) => delta () -> delta alpha -> delta alpha
p *< q = inverse unit . commute >$< p >*< q

{- | This variant of `>*<` ignores its right result.
In contrast to its counterpart derived from the `Applicative` class, the ignored
parts have type `delta ()` rather than `delta beta` because otherwise information relevant
for pretty-printing would be lost.
-}
(>*) :: (Syntax delta) => delta alpha -> delta () -> delta alpha
p >* q = inverse unit >$< p >*< q

-- | The `between` function combines `*<` and `>*` in the obvious way.
between :: (Syntax delta) => delta () -> delta () -> delta alpha -> delta alpha
between p q r = p *< r >* q

{- | The `chainl1` combinator is used to parse a
left-associative chain of infix operators.
-}
chainl1 :: (Syntax delta) => delta alpha -> delta beta -> Iso (alpha, (beta, alpha)) alpha -> delta alpha
chainl1 arg op f = foldl f >$< arg >*< many (op >*< arg)

chainr1 :: (Syntax delta) => delta alpha -> delta beta -> Iso (alpha, (beta, alpha)) alpha -> delta alpha
chainr1 arg op f = f >$< arg >*< op >*< chainr1 arg op f <|> arg

optional :: (Syntax delta) => delta alpha -> delta (Maybe alpha)
optional x = just >$< x <|> nothing >$< text ""

sepBy :: (Syntax delta) => delta alpha -> delta () -> delta [alpha]
sepBy x sep =
  nil
    >$< text ""
    <|> cons
    >$< x
    >*< many (sep *< x)

comma :: (Syntax delta) => delta ()
comma = text ","

dot :: (Syntax delta) => delta ()
dot = text "."

-- Expressing whitespace
-- ---------------------
--
-- Parsers and pretty printers treat whitespace
-- differently. Parsers
-- specify where whitespace is allowed or required to occur, while
-- pretty printers specify how much whitespace is to be inserted at
-- these locations. To account for these different roles of
-- whitespace, the following three syntax descriptions provide
-- fine-grained control over where whitespace is allowed, desired or
-- required to occur.

{- | `skipSpace` marks a position where whitespace is allowed to
occur. It accepts arbitrary space while parsing, and produces
no space while printing.
-}
skipSpace :: (Syntax delta) => delta ()
skipSpace = ignore [] >$< many (text " ")

{- | `optSpace` marks a position where whitespace is desired to occur.
It accepts arbitrary space while parsing, and produces a
single space character while printing.
-}
optSpace :: (Syntax delta) => delta ()
optSpace = ignore [()] >$< many (text " ")

{- | `sepSpace` marks a position where whitespace is required to
occur. It requires one or more space characters while parsing,
and produces a single space character while printing.
-}
sepSpace :: (Syntax delta) => delta ()
sepSpace = text " " >* skipSpace

-------------------------------------------------------------------------------
{-
PARSERS                                                                 PARSERS

PARSERS                                                                 PARSERS
-}
-------------------------------------------------------------------------------
-- Text.Syntax.Parser.Naive

{-
012345
if | a ->
     b
   | c -> d

start off in Inner 0
parse if
enter Group 3
enter Item 3
parse |
Inner 4
parse a -> b
reach 2nd |
fail and return to Item 3
parse |
Inner 4
parse c -> d
fail and return to Item 3

-}
data PMode = First | Next | Inner

data PState = PState {todo :: String, column :: Int, mode :: PMode, indent :: Int}

newtype Parser alpha = Parser (PState -> [(alpha, PState)])

parse :: Parser alpha -> String -> [alpha]
parse (Parser p) s = [x | (x, PState "" _ _ _) <- p $ PState s 0 Inner -1]

parseM :: (MonadFail m) => Parser alpha -> String -> m alpha
parseM p s =
  case parse p s of
    [] -> fail "parse error"
    [result] -> return result
    _ -> fail "ambiguous input"

instance IsoFunctor Parser where
  iso >$< Parser p =
    Parser
      ( \s0 ->
          [ (y, s1)
          | (x, s1) <- p s0
          , Just y <- [apply iso x]
          ]
      )

instance ProductFunctor Parser where
  Parser p >*< Parser q =
    Parser
      ( \s0 ->
          [ ((x, y), s2)
          | (x, s1) <- p s0
          , (y, s2) <- q s1
          ]
      )

instance Alternative Parser where
  Parser p <|> Parser q = Parser (\s -> p s ++ q s)
  empty = Parser (\s -> [])

instance Syntax Parser where
  pure x = Parser (\s -> [(x, s)])
  token =
    Parser
      ( \s -> case todo s of
          [] -> []
          (t : ts) -> [(t, PState ts (case t of '\n' -> 0; _ -> column s + 1) (mode s) (indent s))]
      )
  ci (Parser p) =
    Parser
      ( \s ->
          case mode s of
            First -> p s{mode = Inner, indent = column s}
            Next -> if column s == indent s then p s{mode = Inner} else []
            Inner -> if column s > indent s then p s else []
      )

  -- groupOf (Parser p) = Parser (\s -> groupOf1 p s { mode = First })

  groupOf = groupOfLB
  n <=> p = p

setMode m = Parser (\s -> [((), s{mode = m})])

setColumn c = Parser (\s -> [((), s{column = c})])

setState f = Parser (\s -> [((), f s)])

groupOf1 p s0 = let s1s = p s0 in map (\(r1, s1) -> ([r1], s1)) s1s ++ concatMap (\(r1, s1) -> map (\(r2, s2) -> (r1 : r2, s2)) $ groupOf1 p s1{mode = Next}) s1s

groupOfLB p = groupOfL p <|> groupOfB p

groupOfB p = text "{" *< (cons >$< p >*< many (text ";" *< p {- >* text ";" -})) >* text "}"

groupOfL p =
  Parser
    ( \s ->
        let
          Parser p1 =
            cons
              >$< setMode First
              *< p
              >*< many (setMode Next *< p)
              >* setState (\s2 -> s2{mode = mode s, column = column s})
         in
          p1 s
    )

-------------------------------------------------------------------------------
------------------------------------------------------------------------------- Atto
-------------------------------------------------------------------------------
instance ProductFunctor A.Parser where
  a >*< b = do
    ra <- a
    rb <- b
    return (ra, rb)

instance Alternative A.Parser where
  a <|> b = A.choice [a, b]
  empty = mzero

instance IsoFunctor A.Parser where
  f >$< a = do
    ra <- a
    case apply f ra of
      Just j -> return j
      Nothing -> empty

instance Syntax A.Parser where
  -- (>$<)   ::  Iso alpha beta -> A.Parser alpha -> A.Parser beta
  -- (>*<)   ::  A.Parser alpha -> A.Parser beta -> A.Parser (alpha, beta)
  -- (<|>)   ::  A.Parser alpha -> A.Parser alpha -> A.Parser alpha
  -- empty   ::  A.Parser alpha
  pure = return
  token = chr . fromIntegral <$> A.anyWord8

instance SyntaxA B.ByteString A.Parser where
  textA t = do A.string t; return ()

-------------------------------------------------------------------------------
------------------------------------------------------------------------------- Earley
-------------------------------------------------------------------------------
{-
data Earley :: Type -> Type where
   EProd  :: Earley alpha -> Earley beta -> Earley (alpha, beta)
   EAlt   :: Earley alpha -> Earley alpha -> Earley alpha
   EEmpty :: Earley alpha
--   EIso   :: Iso alpha beta -> Earley alpha -> Earley beta
   EPure  :: Eq alpha => alpha -> Earley alpha
   EToken :: Earley Char

instance ProductFunctor Earley where
   a >*< b = EProd a b

instance Alternative Earley where
   a <|> b = EAlt a b
   empty   = EEmpty

instance IsoFunctor Earley where
   (>$<) = EIso

instance Syntax Earley where
   pure = EPure
   token = EToken

parsees :: Earley alpha -> String -> alpha
parsees e (s:ss) = case e of
   EPure  p -> p
   EToken   -> s
-}

-------------------------------------------------------------------------------
{-
PRINTERS                                                                PRINTERS

PRINTERS                                                                PRINTERS
-}
-------------------------------------------------------------------------------

data Doc = DStr String | DGroup [Doc] | DSeq [Doc] deriving (Eq, Ord, Show)

data Doc2 = Doc2 {docWidth :: Int, docHeight :: Int, docText :: [String]} deriving (Eq, Ord)

-- snarf (DSeq (DStr a:DSeq (DStr b:c))) = let

-- Text.Syntax.Printer.Naive
newtype Printer alpha = Printer (alpha -> Maybe Doc)

print :: Printer alpha -> alpha -> Maybe Doc
print (Printer p) = p

printM :: (MonadFail m) => Printer alpha -> alpha -> m Doc
printM p x = maybe (fail "print error") return $ print p x

instance IsoFunctor Printer where
  iso >$< Printer p = Printer (\b -> unapply iso b >>= p)

instance ProductFunctor Printer where
  Printer p >*< Printer q = Printer (\(x, y) -> DSeq <$> sequence [p x, q y])

instance Alternative Printer where
  Printer p <|> Printer q = Printer (\s -> mplus (p s) (q s))
  empty = Printer (\s -> Nothing)

instance Syntax Printer where
  pure x =
    Printer
      ( \y ->
          if x == y
            then Just $ DStr ""
            else Nothing
      )
  token = Printer (\s -> Just $ DStr [s])
  ci = id
  groupOf (Printer p) = Printer (\a -> DGroup <$> mapM p a)
  n <=> p = p

-------------------------------------------------------------------------------
------------------------------------------------------------------------------- PrinterIO
-------------------------------------------------------------------------------
newtype PrinterIO alpha = PrinterIO (alpha -> IO (Maybe Doc))

printIO :: PrinterIO alpha -> alpha -> IO (Maybe Doc)
printIO (PrinterIO p) = p

instance IsoFunctor PrinterIO where
  --   iso >$< PrinterIO p = PrinterIO (\b -> p Prelude.>$< unapply iso b)
  iso >$< PrinterIO p = PrinterIO (\b -> case unapply iso b of Just j -> p j; Nothing -> return Nothing)

instance ProductFunctor PrinterIO where
  PrinterIO p >*< PrinterIO q =
    PrinterIO
      ( \(x, y) ->
          do
            mp <- p x
            case mp of
              Just j -> do
                mq <- q y
                case mq of
                  Just k -> return $ Just $ DSeq [j, k]
                  Nothing -> return Nothing
              Nothing -> return Nothing
      )

--   PrinterIO p >*< PrinterIO q = PrinterIO (\(x, y) -> let a = fmap (fmap DSeq . sequence) $ sequence [p x, q y] in a)
--   PrinterIO p >*< PrinterIO q = PrinterIO (\(x, y) -> DSeq Prelude.>$< fmap sequence $ sequence [p x, q y])

instance Alternative PrinterIO where
  PrinterIO p <|> PrinterIO q =
    PrinterIO
      ( \s ->
          do
            mp <- p s
            case mp of
              Just j -> return $ Just j
              Nothing -> do
                mq <- q s
                case mq of
                  Just k -> return $ Just k
                  Nothing -> return Nothing
      )

  empty = PrinterIO (\s -> return Nothing)

instance Syntax PrinterIO where
  pure x =
    PrinterIO
      ( \y ->
          return $
            if x == y
              then Just $ DStr ""
              else Nothing
      )
  token = PrinterIO (\s -> return $ Just $ DStr [s])
  ci = id
  groupOf (PrinterIO p) = PrinterIO (fmap (fmap DGroup . sequence) . mapM p)
  n <=> (PrinterIO p) =
    PrinterIO
      ( \s ->
          do
            putStrLn ("-> " ++ n)
            r <- p s
            putStrLn ("<- " ++ n ++ "=" ++ case r of Just j -> format j; _ -> "Nothing")
            return r
      )

wrap w d = d

t s = Doc2 (length s) 1 [s]
ts :: [String] -> Doc2
ts ss = Doc2 (maximum $ map length ss) (length ss) ss

toString (Doc2 _ _ ss) = unlines $ padcoll0 ss

minWidthDoc = L.minimumBy (compare `on` docWidth)
minHeightDoc = L.minimumBy (compare `on` docHeight)

minWidth ds = minimum $ map docWidth ds
minHeight ds = minimum $ map docHeight ds

maxWidth ds = maximum $ map docWidth ds
maxHeight ds = maximum $ map docHeight ds

sumHeight ds = sum $ map docHeight ds
sumWidth ds = sum $ map docWidth ds

fit :: Int -> [Doc2] -> Doc2
fit w opts = let f = filter ((<= w) . docWidth) opts in if null f then minWidthDoc opts else head f

indent1 n (Doc2 w h t) = Doc2 (w + n) h $ indent2 n t
indent2 n = map (replicate n ' ' ++)

sapp (Doc2 aw ah at) (Doc2 bw bh (bth : btt)) =
  let
    ati = init at
    atl = last at
   in
    ts $ ati ++ ((atl ++ bth) : indent2 (length atl) btt)

(<\>) = sapp

scat ds = Prelude.foldl1 sapp ds

vcat ds =
  Doc2
    (maximum $ map docWidth ds)
    (sum $ map docHeight ds)
    (concat $ map docText ds)

vapp a b = vcat [a, b]

iapp a b = vapp a $ indent1 3 b

format = format1 0

format1 ind (DStr string) = string
format1 ind (DGroup group) = concatMap (\item -> "\n" ++ replicate ind ' ' ++ format1 (ind + 3) item) group
format1 ind (DSeq docs) = concatMap (format1 ind) docs

mergeDocs (DSeq s) = mergeSeq $ mergeStrs $ map mergeDocs s
mergeDocs (DGroup a) = DGroup $ map mergeDocs a
mergeDocs (DStr s) = DStr s

mergeStrs [] = []
mergeStrs (DStr a : DStr b : cs) = mergeStrs (DStr (a ++ b) : cs)
mergeStrs (a : as) = a : mergeStrs as

mergeSeq [DStr a] = DStr a
mergeSeq a = DSeq a

fp p e = format <$> print p e

text1a [] = pure []
text1a (t : ts) = cons >$< (match1 t >$< token) >*< text1a ts

text1 t = "text " ++ t <=> text1a t

lexi l = optSpace *< l

letter = subset isLetter >$< token
digit = subset isDigit >$< token

-- identifier = cons >$< letter >*< many (letter <|> digit)
identifier = many1 letter

number :: (Syntax f) => f Int
number = total read show >$< many1 digit

intStr = total charToWord8 word8ToChar >$< token
intChar = total ord chr >$< token

doMvid m (a, b) = a * m + b

base m = total (doMvid m) (`divMod` m)

int2 m = base m >$< intChar >*< intChar

int3 m = base (m * m) >$< intChar >*< int2 m

int4 m = base (m * m) >$< int2 m >*< int2 m

int 0 m = pure 0
int n m = total (\(a, b) -> a * m ^ (n - 1) + b) (`divMod` m) >$< intChar >*< int (n - 1) m

word = invol fromIntegral >$< intChar

flags =
  total
    (\a -> (a .&. 0x80 /= 0, a .&. 0x40 /= 0, a .&. 0x20 /= 0, a .&. 0x10 /= 0))
    (\(a, b, c, d) -> if a then 0x80 else 0 + if b then 0x40 else 0 + if c then 0x20 else 0 + if d then 0x10 else 0)
    >$< word

file = tag >*< many token

tag = header >*< many frame23

header = text "ID3" >*< intChar >*< intChar >*< flags >*< int4 0x80

rep p 0 = pure []
rep p n = total (uncurry (:)) (\(a : b) -> (a, b)) >$< p >*< rep p (n - 1)

frame23 =
  subset (\(a, (b, (c, d))) -> length d == b)
    >$< rep token 4
    >*< int4 0x100
    >*< int2 0x100
    >*< many token

x = unsafePerformIO $ readFile "D:\\Music\\Artists\\Paradise Lost\\2020 - Obsidian\\01 Darker Thoughts.mp3"

y = parse file x

frameText = intChar >*< many token

charToWord8 = fromIntegral . ord

word8ToChar = chr . fromIntegral

-- convert a total function and its inverse to a partial isomorphism
total f g = Iso (Just . f) (Just . g)

-- an involution is a function that is its own inverse, such as reverse or unary minus
-- convert an involution to an Iso
invol f = total f f

foldr :: Iso (alpha, beta) beta -> Iso (beta, [alpha]) beta
foldr i = foldl (i . commute) . (id *** invol reverse)

-- cant really expect to run an "if" backwards
isoif = Iso (\(c, t, e) -> if c then t else e) undefined

-- instance Alternative (Iso a b) where
f </> g =
  Iso
    (\x -> apply f x `mplus` apply g x)
    (\y -> unapply f y `mplus` unapply g y)

-- a <=> b = Iso (swap a b) (swap b a)

swap a b c = if c == a then Just b else Nothing

-------------------------------------------------------------------------------
-- Control.Isomorphism.Partial.Unsafe
-- moved to SyntaxCIPU.hs

-- Control.Isomorphism.Partial.Prim
inverse :: Iso alpha beta -> Iso beta alpha
inverse (Iso f g) = Iso g f

apply :: Iso alpha beta -> alpha -> Maybe beta
apply (Iso f _) = f

unapply :: Iso alpha beta -> beta -> Maybe alpha
unapply = apply . inverse

ignore :: alpha -> Iso alpha ()
ignore x = Iso f g
 where
  f _ = Just ()
  g () = Just x

-- | the product type constructor `(,)` is a bifunctor from

-- `Iso` $\times$ `Iso` to `Iso`, so that we have the

-- bifunctorial map `***` which allows two separate isomorphisms

-- to work on the two components of a tuple.

(***) :: Iso alpha beta -> Iso gamma delta -> Iso (alpha, gamma) (beta, delta)
i *** j = Iso f g
 where
  f (a, b) = liftM2 (,) (apply i a) (apply j b)
  g (c, d) = liftM2 (,) (unapply i c) (unapply j d)

-- | The mediating arrow for sums constructed with `Either`.

-- This is not a proper partial isomorphism because of `mplus`.

(|||) :: Iso alpha gamma -> Iso beta gamma -> Iso (Either alpha beta) gamma
i ||| j = Iso f g
 where
  f (Left x) = apply i x
  f (Right x) = apply j x
  g y = (Left `fmap` unapply i y) `mplus` (Right `fmap` unapply j y)

-- | Nested products associate.
associate :: Iso (alpha, (beta, gamma)) ((alpha, beta), gamma)
associate = Iso f g
 where
  f (a, (b, c)) = Just ((a, b), c)
  g ((a, b), c) = Just (a, (b, c))

-- | Products commute.
commute :: Iso (alpha, beta) (beta, alpha)
commute = Iso f f
 where
  f (a, b) = Just (b, a)

-- | `()` is the unit element for products.
unit :: Iso alpha (alpha, ())
unit = Iso f g
 where
  f a = Just (a, ())
  g (a, ()) = Just a

-- | Products distribute over sums.
distribute :: Iso (alpha, Either beta gamma) (Either (alpha, beta) (alpha, gamma))
distribute = Iso f g
 where
  f (a, Left b) = Just (Left (a, b))
  f (a, Right c) = Just (Right (a, c))
  g (Left (a, b)) = Just (a, Left b)
  g (Right (a, b)) = Just (a, Right b)

-- | `element x` is the partial isomorphism between `()` and the

-- singleton set which contains just `x`.

match :: (Eq alpha) => alpha -> Iso () alpha
match x =
  Iso
    (\() -> Just x)
    (\b -> if x == b then Just () else Nothing)

match1 x =
  Iso
    (\x1 -> if x == x1 then Just x else Nothing)
    (\x1 -> if x == x1 then Just x else Nothing)

-- | For a predicate `p`, `subset p` is the identity isomorphism

-- restricted to elements matching the predicate.

subset :: (alpha -> Bool) -> Iso alpha alpha
subset p = Iso f f
 where
  f x | p x = Just x | otherwise = Nothing

iterate :: Iso alpha alpha -> Iso alpha alpha
iterate step = Iso f g
 where
  f = Just . driver (apply step)
  g = Just . driver (unapply step)

  driver :: (alpha -> Maybe alpha) -> (alpha -> alpha)
  driver step' state =
    case step' state of
      Just state' -> driver step' state'
      Nothing -> state

-- Control.Isomorphism.Partial.Derived
foldl :: Iso (alpha, beta) alpha -> Iso (alpha, [beta]) alpha
foldl i =
  inverse unit
    . (id *** inverse nil)
    . iterate (step i)
 where
  step i' =
    (i' *** id)
      . associate
      . (id *** inverse cons)

-- Control.Isomorphism.Partial.Constructors
{-
module Control.Isomorphism.Partial.Constructors
  ( nil
  , cons
  , listCases
  , left
  , right
  , nothing
  , just
  ) where
import Prelude ()

import Data.Either (Either (Left, Right))
import Data.Maybe (Maybe (Just, Nothing))
-}

nil :: Iso () [alpha]
nil = Iso f g
 where
  f () = Just []
  g [] = Just ()
  g _ = Nothing

cons :: Iso (alpha, [alpha]) [alpha]
cons = Iso f g
 where
  f (x, xs) = Just (x : xs)
  g (x : xs) = Just (x, xs)
  g _ = Nothing

listCases :: Iso (Either () (alpha, [alpha])) [alpha]
listCases = Iso f g
 where
  f (Left ()) = Just []
  f (Right (x, xs)) = Just (x : xs)
  g [] = Just (Left ())
  g (x : xs) = Just (Right (x, xs))

-------------------------------------------------------------------------------
foldl1 i =
  -- alpha
  inverse unit
    -- (alpha, ())
    . (id *** inverse nil)
    -- (alpha, [beta])
    . iterate (step i)

-- (alpha, [beta])

step i =
  (i *** id)
    . associate
    . (id *** inverse cons)

-------------------------------------------------------------------------------
{-
myfoldr f = (z, []    ) <=> []
        <|> (z, (x:xs)) <=> (f x (myfoldr f z xs))
-}

{-
\a -> blah $ foo a

blah . foo

foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys

foldr k z []     = z
foldr k z (y:ys) = y `k` foldr k z ys
foldr k z (y:ys) = k y (foldr k z ys)

foldl f z0 xs0 = lgo z0 xs0
             where
                lgo z []     =  z
                lgo z (x:xs) = lgo (f z x) xs

foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl f z l = if l == [] then z else foldl f (f z x) xs

iterate :: Iso alpha alpha -> Iso alpha alpha
iterate step = Iso f g where
  f = Just . driver (apply step)
  g = Just . driver (unapply step)

  driver :: (alpha -> Maybe alpha) -> (alpha -> alpha)
  driver step state
    =  case step state of
         Just state'  ->  driver step state'
         Nothing      ->  state

-}

{-
Iso b c . Iso a b = Iso a c

foldl :: Iso (alpha, beta) alpha -> Iso (alpha, [beta]) alpha
foldl i = inverse unit
        . (id *** inverse nil)    -- Iso (alpha, [beta]) (alpha, ())
        . iterate (step i) where  -- Iso (alpha, [beta]) (alpha, [beta])
-}
{-
newtype Parser u alpha
  = Parser ((String, u) -> [(alpha, (String, u))])

parse :: Parser u alpha -> String -> u -> [alpha]
parse (Parser p) s u = [ x | (x, ("", u)) <- p (s, u) ]

parseM :: Monad m => Parser u alpha -> String -> u -> m alpha
parseM p s u
  =  case parse p s u of
       []        ->  fail "parse error"
       [result]  ->  return result
       _         ->  fail "ambiguous input"

instance IsoFunctor (Parser u) where
  iso >$< Parser p
    = Parser (\s ->  [  (y, s1)
                     |  (x, s1)  <-  p s
                     ,  Just y   <-  [apply iso x] ])

instance ProductFunctor (Parser u) where
  Parser p >*< Parser q
    = Parser (\s ->  [  ((x, y), s2)
                     |  (x,  s1)  <- p  s
                     ,  (y,  s2)  <- q  s1 ])

instance Alternative (Parser u) where
  Parser p <|> Parser q = Parser (\s -> p s ++ q s)
  empty = Parser (\s -> [])

instance Syntax (Parser u) where
  pure x  =  Parser (\s -> [(x, s)])
  token   =  Parser f where
    f ([]    , _)  =  []
    f ((t:ts), u)  =  [(t, (ts, u))]
-}
{-
data Type = TypeName String
          | Ptr { to :: Type }
          | Ref { to :: Type }
          | Hole
          | Func Type [String]
          deriving (Eq, Ord, Show)

ctype1   = typeName >$< identifier

--ctype2a :: Syntax f => f Type

--qqq :: Syntax f => f Type -> f (Iso Type Type)

--ctype2a :: Syntax f => f (Iso Type Type)
ctype2a = ptr >$< text "*" *< ctype2a
      <|> ref >$< text "&" *< ctype2a
      <|> hole >$< text ""

ctype2c = func >$< (isoswap >$< (text "(" *< sepBy arg (text ",") >* text ")") >*< ctype2c)
      <|> pure Hole

(>**<) = flip (>*<)

swap (a,b) = (b,a)

isoswap = Iso f f where f (a,b) = Just (b,a)

arg = identifier

-- No instance for Eq (Iso Type Type)
-- h1 :: Iso () (Iso Type Type)
-- h1 = element isoid

{-
blah = Iso
   (\c bt -> apply c $ apply ptr bt)
   (\c x  -> unapply ptr $ unapply c x)
-}

isoid = Iso Just Just

sepBy1 p sep = cons >$< p >*< many (sep *< p)

dtype2a bt = text "*" *< (dtype2a (Ptr bt))

-- sepBy  p sep = sepBy1 p sep <|> empty
-}
{-

-- parser for a c++ type and an identifier since they are intermixed "char f[50]".
-- the supplied qid parameter parses the ident (use qident,noident or maybeident for this)
ctype2 qid    = do t       <- ctype2a
                   (i, t1) <-
                                 -- parentheses around the identifier/pointer
                                 -- dont match an empty ( ) here
                                 -- can occur in an "operator Type ()"
                                 do notFollowedBy (do symbol "("; symbol ")"); -- fail if ( ) would match
                                    symbol "("
                                    c <- ctype2 qid
                                    symbol ")"
                                    return c
                               <|>
                                 -- this bit is ok
                                 do i <- qid; return (i, id)
                   t2      <- ctype2c
                   return (i, t1 . t2 . t)

-- stuff before the identifier
ctype2a       =

                         do try $ symbol "*"; cv <- cvMod; c <- ctype2a; return (\bt -> c $ cv $ Ptr bt)
                     <|> do try $ symbol "&"; cv <- cvMod; c <- ctype2a; return (\bt -> c $ cv $ Ref bt)
                     <|> do try $ reserved "__cdecl"   ; c <- ctype2a; return (\bt -> c (CDecl    bt))
                     <|> do try $ reserved "__thiscall"; c <- ctype2a; return (\bt -> c (ThisCall bt))
                     <|> try (do ct <- ctype1; reservedOp "::"; reservedOp "*"; cv <- cvMod; c <- ctype2a; return (\bt -> c $ cv $ PtrMem ct bt))
                     <|> return id
                     <?> "ctype2a"

-- stuff after the identifier
ctype2c       =          do reservedOp "["; n <- eexpr; reservedOp "]"; c <- ctype2c; return (\bt -> Array n (c bt))
                     <|> try (do symbol "(";
                                 a1 <- sepBy declArg (symbol ",")
                                 a <- option a1 (do symbol "..."; return (a1 ++ [VarArgs]))
                                 symbol ")";
                                 try $ many (try (try (reserved "const") <|> try (reserved "volatile") <|> try (throwSpec)))
                                 c <- ctype2c;
                                 return (\bt -> FuncT (c bt) a))
                     <|> return id
                     <?> "ctype2c"

throwSpec = do
   reserved "throw"
   symbol "("
   sepBy (identifier <|> symbol "...") (symbol ",")
   symbol ")"

ctype qid = do
   bt      <- ctype1
   (i, c)  <- ctype2 qid
   return (i, c bt)

ctypeni = do (_, t) <- ctype (return ()); return t
-}
