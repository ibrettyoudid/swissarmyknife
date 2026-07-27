{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module AttoAny where

import BString

import Data.String

import Data.Attoparsec.ByteString (Result)
import Control.Applicative

class (Functor parser, Alternative parser, Monad parser, BString str, BStringC char str) => Parser parser str char | parser -> str char, str -> parser where
   anyChar     :: parser char
--   anyCha    r1 :: parser Char
   char        ::  Char  -> parser char
   oneOf       :: [Char] -> parser char
   noneOf      :: [Char] -> parser char
   endOfInput  ::           parser ()  
   many        ::           parser a                    -> parser [a]
   many1       ::           parser a                    -> parser [a]
   manyTill    ::           parser a   -> parser b      -> parser [a]
   lookAhead   ::           parser a                    -> parser  a
   option      :: a ->      parser a                    -> parser  a
   try         ::           parser a                    -> parser  a
   choice      ::          [parser a]                   -> parser  a
   string      :: String                                -> parser str
   manyTill2   :: String                                -> parser str
   rest        ::                                          parser str
   takeWhile   :: (char -> Bool)                        -> parser str
   takeWhile1  :: (char -> Bool)                        -> parser str
   (<?>)       ::           parser a   -> String        -> parser  a 

   parse       ::           parser a   ->           str -> Result  a
   parse1      ::           parser a   -> str    -> str -> a
   parse2      ::           parser a   ->           str -> a
   parseOnly   ::           parser a   ->           str -> Either String a
   halves      ::           parser a   ->           str -> a
   parseNoFail :: a ->      parser a   ->           str -> a

class (Monoid builder, BString str, IsString builder, IsString str) => Builder builder str | builder -> str, str -> builder where
   fromString :: String -> builder
   fromText   :: str -> builder
   toText     :: builder -> str
   --(<>)       :: builder -> builder -> builder
   --bconcat    :: [builder] -> builder

class ParserC char where
   inClass     :: [Char] -> char -> Bool
   notInClass  :: [Char] -> char -> Bool

