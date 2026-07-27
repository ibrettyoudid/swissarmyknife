-- Copyright 2025 Brett Curtis
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
module NumberParsersAtto where

import Favs
import AttoAny
import BString

import Data.Char
import Control.Applicative ((<$>), (<|>))
import Control.Monad

--type Blah c = Stream s m c => ParsecT s u m c

forceFloatingC :: (ConvertChar char Char, Fractional a, Parser parser str char) => parser a
forceFloatingC = do 
   s <- sign
   n <- decimal digitOrComma
   fract <- option Nothing (Just <$> fraction)
   expo  <- option Nothing (Just <$> exponent')
   --only succeed if it can't be an integer
   guard (isJust fract || isJust expo)
   return (s (fromInteger n + fromMaybe 0.0 fract) * fromMaybe 1.0 expo)

floating :: (ConvertChar char Char, Fractional a, Parser parser str char) => parser a
floating = do 
   s <- sign
   n <- decimal digit
   fract <- option 0.0 fraction
   expo  <- option 1.0 exponent'
   return (s (fromInteger n + fract) * expo)

floatingC :: (ConvertChar char Char, Fractional a, Parser parser str char) => parser a
floatingC = do 
   s <- sign
   n <- decimal digitOrComma
   fract <- option 0.0 fraction
   expo  <- option 1.0 exponent'
   return (s (fromInteger n + fract) * expo)

-- decimal point, fractional part
fraction :: (Fractional a, ConvertChar char Char, Parser parser str char) => parser a
fraction = do 
   char '.'
   digits <- many1 digit <?> "fraction"
   return (foldr op 0.0 digits)
      where
         op d f = (f + fromIntegral (digitToInt $ convertChar d))/10.0

exponent' :: (Fractional a, ConvertChar char Char, Parser parser str char) => parser a
exponent' = do 
   oneOf "eE"
   s <- sign
   e <- decimal digit <?> "exponent"
   return $ power $ s e

   <?> "exponent"
      where
         power e  | e < 0      = 1.0/power (-e)
                  | otherwise  = fromInteger (10^e)


-- integers and naturals
integer :: (ConvertChar char Char, Parser parser str char) => parser Integer
integer = do 
   f <- sign
   f <$> nat digit

integerC :: (ConvertChar char Char, Parser parser str char) => parser Integer
integerC = do 
   f <- sign
   f <$> nat digitOrComma

int :: (Num a, ConvertChar char Char, Parser parser str char, Parser parser str char) => parser a
int = fromIntegral <$> integer

intC :: (Num a, ConvertChar char Char, Parser parser str char, Parser parser str char) => parser a
intC = fromIntegral <$> integerC

sign =
   (char '-' >> return negate)
   <|>      (char '+' >> return id)
   <|>      return id

nat digitType = zeroNumber digitType <|> decimal digitType

zeroNumber digitType = do 
   char '0'
   octal <|> return 0

decimal digitType = baseInteger 10 digitType

hexadecimal = do 
   oneOf "xX"
   baseInteger 16 hexDigit

octal = do 
   oneOf "oO"
   baseInteger 8  octDigit

baseInteger :: (Parser p s c, ConvertChar c Char) => Integer -> p c -> p Integer
baseInteger base baseDigit = do
   digits <- many1 baseDigit
   let digits1 = fromList digits
   let n = foldl (\x d -> x*base + toInteger (digitToInt $ convertChar d)) 0 digits
   seq n (return n)

digit = oneOf "0123456789"

digitOrComma = oneOf ",0123456789"
{-
digit = chr (satisfy (inClass "0123456789")) - chr '0'
-}
hexDigit = oneOf "0123456789ABCDEFabcdef"
{-
      return $ chr u - chr 'A' + 10) <|> (do
      l <- satisfy (inClass "abcdef")
      return $ chr l - chr 'a' + 10)
-}
octDigit = oneOf "01234567"
{-
digitToInt x
   | x >= 48 && x <=  57 = fromIntegral (x - 48)
   | x >= 65 && x <=  70 = fromIntegral (x - 55)
   | x >= 97 && x <= 112 = fromIntegral (x - 87)
   | otherwise = 0
-}
{- 

in Parsec.Token

-- naturalOrFloat :: CharParser st (Either Integer Double)
naturalOrFloat  = lexeme (natFloat) <?> "number"

float           = lexeme floating   <?> "float"
integer         = lexeme int        <?> "integer"
natural         = lexeme nat        <?> "natural"

main functions are:

floating :: GenParser Char st Double                  -- unsigned floating, decimal only
int      :: GenParser Char st Integer                 -- signed integer, dec/hex/oct
natFloat :: GenParser Char st (Either Integer Double) -- Either of the above


-}

