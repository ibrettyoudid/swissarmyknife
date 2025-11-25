{- Most of this is from partial-isomorphisms and invertible-syntax on hackage.com -}
{- Additions by Brett Curtis -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Syntax3 where

import Favs hiding (indent, indent1, indent2, left, mode, right, swap)

import NewTuple
import SyntaxCIPU

import Prelude hiding (foldl, foldr, id, iterate, print, pure, (*<), (.), (>$<), (>*), (>*<))
import Prelude qualified

import Control.Category
import Control.Monad
import Control.Monad.State
import Control.Applicative qualified as App

import ListT qualified
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
infixr 5 >$<
infixr 6 >*<

class ProductFunctor f st tok str where
   (>*<) :: f st tok str alpha -> f st tok str beta -> f st tok str (alpha :- beta)

class Alternative f st tok str where
   (<|>) :: f st tok str alpha -> f st tok str alpha -> f st tok str alpha
   empty :: f st tok str alpha

class IsoFunctor f st tok str where
   (>$<) :: (Show alpha, Show beta) => Iso alpha beta -> f st tok str alpha -> f st tok str beta

class (IsoFunctor delta st tok str, ProductFunctor delta st tok str, Alternative delta st tok str) => Syntax delta st tok str where
   -- (>$<)   ::  Iso alpha beta -> delta alpha -> delta beta
   -- (>*<)   ::  delta alpha -> delta beta -> delta (alpha, beta)
   -- (<|>)   ::  delta alpha -> delta alpha -> delta alpha
   -- empty   ::  delta alpha
   pure :: (Eq alpha) => alpha -> delta st tok str alpha
   token :: delta st tok str tok

class Syntax delta st tok str => SyntaxF delta st tok str where
   ci      ::                            delta st tok str alpha -> delta st tok str  alpha
   groupOf :: (Show alpha) =>            delta st tok str alpha -> delta st tok str [alpha]
   (<=>)   :: (Show alpha) => String ->  delta st tok str alpha -> delta st tok str  alpha

class SyntaxP f where
   (>><) :: f b c -> f a b -> f a c

class SyntaxToken delta tok where
{-
\*********************************************************************************
-}
-- derived combinators
many p =  nil  >$< pure ()
         <|> cons >$< p >*< many p

many1 p = cons >$< p >*< many p

p <+> q = (left >$< p) <|> (right >$< q)

-- | `text` parses\/prints a fixed text and consumes\/produces a unit value.
--text :: (Eq char, Show char, Syntax delta) => [char] -> delta ()
text [] = pure ()
text (c : cs) =
   inverse (match ((), ())) >$< (inverse (match c) >$< token) >*< text cs

{- | This variant of `>*<` ignores its left result.
In contrast to its counterpart derived from the `Applicative` class, the ignored
parts have type `delta ()` rather than `delta beta` because otherwise information relevant
for pretty-printing would be lost.
-}
p *< q = inverse unit . commute >$< p >*< q

{- | This variant of `>*<` ignores its right result.
In contrast to its counterpart derived from the `Applicative` class, the ignored
parts have type `delta ()` rather than `delta beta` because otherwise information relevant
for pretty-printing would be lost.
-}
p >* q = inverse unit >$< p >*< q

-- | The `between` function combines `*<` and `>*` in the obvious way.
between p q r = p *< r >* q

{- | The `chainl1` combinator is used to parse a
left-associative chain of infix operators.
-}
chainl1 arg op f = foldl f >$< arg >*< many (op >*< arg)

chainr1 arg op f = f >$< arg >*< op >*< chainr1 arg op f <|> arg

optional x = just >$< x <|> nothing >$< text []

sepBy x sep = nil >$< text ([] :: [alpha1])
   <|> cons >$< x >*< many (sep *< x)

--comma :: (Syntax delta) => delta ()
comma = text ","

--dot :: (Syntax delta) => delta ()
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
--skipSpace :: (Syntax delta) => delta ()
skipSpace = ignore [] >$< many (satisfy isSpace >$< token)

{- | `optSpace` marks a position where whitespace is desired to occur.
It accepts arbitrary space while parsing, and produces a
single space character while printing.
-}
--optSpace :: (Syntax delta) => delta ()
optSpace = ignore " " >$< many (satisfy isSpace >$< token)

{- | `sepSpace` marks a position where whitespace is required to
occur. It requires one or more space characters while parsing,
and produces a single space character while printing.
-}
--sepSpace :: (Syntax delta) => delta ()
sepSpace = ignore " " >$< many1 (satisfy isSpace >$< token)

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

data PState str = PState {todo :: str, column :: Int, mode :: PMode, indent :: Int}
{-
newtype Parser tok str alpha = Parser (PState str -> [(alpha, PState str)])

parse :: Parser tok String alpha -> String -> [alpha]
parse (Parser p) s = [x | (x, PState [] _ _ _) <- p $ PState s 0 Inner -1]

parseM :: (MonadFail m) => Parser tok String alpha -> String -> m alpha
parseM p s =
   case parse p s of
      [] -> fail "parse error"
      [result] -> return result
      _ -> fail "ambiguous input"

instance IsoFunctor Parser Char String where
   iso >$< Parser p =
      Parser
         ( \s0 ->
               [ (y, s1)
               | (x, s1) <- p s0
               , Just y <- [apply iso x]
               ]
         )

instance {-# OVERLAPPING #-} ProductFunctor Parser Char String where
   Parser p >*< Parser q =
      Parser
         ( \s0 ->
               [ ((x, y), s2)
               | (x, s1) <- p s0
               , (y, s2) <- q s1
               ]
         )

instance Alternative Parser Char String where
   Parser p <|> Parser q = Parser (\s -> p s ++ q s)
   empty1 = Parser (\s -> [])

instance SyntaxToken Parser Char where

instance Syntax Parser Char String where
   pure x = Parser (\s -> [(x, s)])
   token =
      Parser
         ( \s -> case todo s of
               [] -> []
               (t : ts) -> [(t, PState ts (case t of '\n' -> 0; _ -> column s + 1) (mode s) (indent s))]
         )

instance SyntaxF Parser Char String where
   ci (Parser p) =
      Parser
         ( \s ->
               case mode s of
                  First -> p s{mode = Inner, indent = column s}
                  Next  -> if column s == indent s then p s{mode = Inner} else []
                  Inner -> if column s >  indent s then p s else []
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

groupOfL p = Parser (\s -> let
                           Parser p1 = cons >$< setMode First *< p >*< many (setMode Next *< p) >* setState (\s2 -> s2{mode = mode s, column = column s})

                           in p1 s)
-}
-------------------------------------------------------------------------------
------------------------------------------------------------------------------- ParserIO
-------------------------------------------------------------------------------

--newtype ParserIO alpha = ParserIO (StateT String (ListT.ListT IO) (IO alpha))
newtype ParserIO st tok str alpha = ParserIO { parserIO :: ListT.ListT (StateT st IO) alpha }

type SII = (String, Int, Int)

parseIO (ParserIO p) str = evalStateT (ListT.toList p) str

parseLexIO p l str = parseIO l str >>= parseIO p

instance ProductFunctor ParserIO a b c where
   ParserIO a >*< ParserIO b = ParserIO $ do
      ra <- a
      rb <- b
      return (ra, rb)

instance Alternative ParserIO a b c where
   ParserIO a <|> ParserIO b = ParserIO $ a App.<|> b
   empty = ParserIO App.empty
   --ParserIO a <|> ParserIO b = ParserIO $ join $ ListT.cons a $ ListT.cons b App.empty

instance IsoFunctor ParserIO a b c where
   f >$< ParserIO a = ParserIO $ do
      ra <- a
      maybe App.empty return (apply f ra)

instance Syntax ParserIO [a] a [a] where
   pure = ParserIO . return
   token = ParserIO $ do
      (h:t) <- lift get
      lift $ put t
      return h

instance Syntax ParserIO SII Char String where
   pure = ParserIO . return
   token = ParserIO $ do
      (h:t, l, c) <- lift get
      lift $ put $ case h of
            '\n' -> (t, l+1, 0)
            _    -> (t, l, c+1)
      return h

instance SyntaxF ParserIO [SII] SII [SII] where
   groupOf i = sepBy i (Iso (\case (";", _, _) -> Just ()) (\() -> Just (";", 0::Int, 0::Int)) >$< token) <|>
                     aligned i
   n <=> p = do
      ParserIO $ liftIO $ putStrLn $ "-> " ++ n
      r <- p
      ParserIO $ liftIO $ putStrLn $ "<- " ++ n ++ " = " ++ show r
      return r

instance Functor (ParserIO [a] a [a]) where
   fmap f (ParserIO a) = ParserIO $ fmap f a

instance App.Applicative (ParserIO [a] a [a]) where
   pure a = ParserIO $ App.pure a
   ParserIO a <*> ParserIO b = ParserIO $ a <*> b

instance Monad (ParserIO [a] a [a]) where
   ParserIO a >>= b = ParserIO $ do
      ra <- a
      parserIO $ b ra

instance MonadFail (ParserIO [a] a [a]) where
   fail m = ParserIO $ fail m

aligned ::Show alpha => ParserIO [SII] SII [SII] alpha -> ParserIO [SII] SII [SII] [alpha]
aligned (ParserIO i) = do
   ((_, l, c):_) <- ParserIO $ lift get
   many $ ParserIO $ do
      ((_::String, l1::Int, c1::Int):_) <- lift get
      guard $ l1 > l && c == c1
      i

tokenAt = do
   ((_, l, c):_) <- lift get
   t <- token
   return (t, l, c)


-------------------------------------------------------------------------------
------------------------------------------------------------------------------- Atto
-------------------------------------------------------------------------------
{-
instance ProductFunctor A.Parser Char where
   a >*< b = do
      ra <- a
      rb <- b
      return (ra, rb)

instance Alternative A.Parser Char where
   a <|> b = A.choice [a, b]
   empty = mzero

instance IsoFunctor A.Parser Char where
   f >$< a = do
      ra <- a
      case apply f ra of
         Just j -> return j
         Nothing -> empty

instance SyntaxToken A.Parser Char where

instance {-# OVERLAPPING #-} Syntax A.Parser Char where
   token = chr . fromIntegral <$> A.anyWord8
   -- (>$<)   ::  Iso alpha beta -> A.Parser alpha -> A.Parser beta
   -- (>*<)   ::  A.Parser alpha -> A.Parser beta -> A.Parser (alpha, beta)
   -- (<|>)   ::  A.Parser alpha -> A.Parser alpha -> A.Parser alpha
   -- empty   ::  A.Parser alpha
   pure = return
-}
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

-- snarf (DSeq (DStr a:DSeq (DStr b:c))) = let

-- Text.Syntax.Printer.Naive
newtype Printer st tok str alpha = Printer (alpha -> Maybe (Doc str))

print :: Printer st tok str alpha -> alpha -> Maybe (Doc str)
print (Printer p) = p

printM :: (MonadFail m) => Printer st tok str alpha -> alpha -> m (Doc str)
printM p x = maybe (fail "print error") return $ print p x

instance IsoFunctor Printer st tok str where
   iso >$< Printer p = Printer (\b -> unapply iso b >>= p)

instance ProductFunctor Printer st tok [tok] where
   Printer p >*< Printer q = Printer (\(x, y) -> DSeq <$> sequence [p x, q y])

instance Alternative Printer st tok str where
   Printer p <|> Printer q = Printer (\s -> mplus (p s) (q s))
   empty = Printer (\s -> Nothing)

instance Syntax Printer st tok [tok] where
   token = Printer (\s -> Just $ DStr [s])
   pure x =
      Printer
         ( \y ->
               if x == y
                  then Just $ DStr []
                  else Nothing
         )

instance SyntaxF Printer st tok [tok] where
   ci = id
   groupOf (Printer p) = Printer (\a -> DGroup <$> mapM p a)
   n <=> p = p

-------------------------------------------------------------------------------
------------------------------------------------------------------------------- PrinterIO
-------------------------------------------------------------------------------
newtype PrinterIO st tok str alpha = PrinterIO (alpha -> IO (Maybe (Doc str)))

printIO :: PrinterIO st tok str alpha -> alpha -> IO (Maybe (Doc str))
printIO (PrinterIO p) = p

instance IsoFunctor PrinterIO st tok str where
   --   iso >$< PrinterIO p = PrinterIO (\b -> p Prelude.>$< unapply iso b)
   iso >$< PrinterIO p
      = PrinterIO
         (\b ->
            do
               Prelude.print b
               let b1 = unapply iso b
               Prelude.print b1
               case b1 of
                  Just j -> p j
                  Nothing -> return Nothing)

instance ProductFunctor PrinterIO st tok str where
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

instance Alternative PrinterIO st tok [tok] where
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

instance Syntax PrinterIO st tok [tok] where
   token = PrinterIO (\s -> return $ Just $ DStr [s])
   pure x =
      PrinterIO
         ( \y ->
               return $
                  if x == y
                     then Just $ DStr []
                     else Nothing
         )

instance SyntaxF PrinterIO st Char String where
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

data Doc str = DStr str | DGroup [Doc str] | DSeq [Doc str] deriving (Eq, Ord, Show)

data Doc2 str = Doc2 {docWidth :: Int, docHeight :: Int, docText :: [str]} deriving (Eq, Ord)

wrap w d = d

t s = Doc2 (length s) 1 [s]
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

fit :: Int -> [Doc2 a] -> Doc2 a
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

letter = satisfy isLetter >$< token
digit = satisfy isDigit >$< token

-- identifier = cons >$< letter >*< many (letter <|> digit)
identifier = many1 letter

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
   satisfy (\(a, (b, (c, d))) -> length d == b)
      >$< rep token 4
      >*< int4 0x100
      >*< int2 0x100
      >*< many token

x = unsafePerformIO $ readFile "D:\\Music\\Artists\\Paradise Lost\\2020 - Obsidian\\01 Darker Thoughts.mp3"

--y = parse file x

frameText = intChar >*< many token

charToWord8 = fromIntegral . ord

word8ToChar = chr . fromIntegral

-- convert a total function and its inverse to a partial isomorphism

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
