-- Copyright 2025 Brett Curtis
{-# LANGUAGE FlexibleContexts #-}

module NumberParsersT
(
   module NumberParsersT,
   
   try,
   parse
)
where

import Favs
import AttoT

import Data.Attoparsec.Text hiding (decimal, hexadecimal)
import Data.Char
import Control.Applicative ((<$>), (<|>))
import Control.Monad

--type Blah c = Stream s m c => ParsecT s u m c

--only succeed if it can't be an integer
forceFloatingC = do 
   s <- sign
   n <- decimal digitOrComma
   fract <- option Nothing (Just <$> fraction)
   expo  <- option Nothing (Just <$> exponent')
   guard (isJust fract || isJust expo)
   return (s (fromInteger n + fromMaybe 0.0 fract) * fromMaybe 1.0 expo)

floating = do 
   s <- sign
   n <- decimal digit
   fract <- option 0.0 fraction
   expo  <- option 1.0 exponent'
   return (s (fromInteger n + fract) * expo)

floatingC = do 
   s <- sign
   n <- decimal digitOrComma
   fract <- option 0.0 fraction
   expo  <- option 1.0 exponent'
   return (s (fromInteger n + fract) * expo)

-- decimal point, fractional part
fraction = do 
   char '.'
   digits <- many1 digit <?> "fraction"
   return (foldr op 0.0 digits)

   <?> "fraction"
      where
            op d f = (f + fromIntegral (digitToInt d))/10.0

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
integer = do 
   f <- sign
   f <$> nat digit

integerC = do 
   f <- sign
   f <$> nat digitOrComma

int :: Parser Int
int = fromIntegral <$> integer

intC :: Parser Int
intC = fromIntegral <$> integerC

sign :: Num a => Parser (a -> a)
sign =
   (char '-' >> return negate)
   <|>      (char '+' >> return id)
   <|>      return id

nat digitType = zeroNumber digitType <|> decimal digitType

zeroNumber digitType = do 
   char '0'
   hexadecimal <|> octal <|> decimal digit <|> return 0
      <?> ""

decimal digitType = baseInteger 10 digitType

hexadecimal = do 
   oneOf "xX"
   baseInteger 16 hexDigit

octal = do 
   oneOf "oO"
   baseInteger 8  octDigit

baseInteger base baseDigit = do
   digits <- many1 baseDigit
   let n = foldl (\x d -> x*base + toInteger (digitToInt d)) 0 digits
   seq n (return n)

digitOrComma = do many (oneOf ","); digit
{-
digit = chr (satisfy (inClass "0123456789")) - chr '0'
-}
hexDigit = satisfy (inClass "0123456789ABCDEFabcdef")
{-
      return $ chr u - chr 'A' + 10) <|> (do
      l <- satisfy (inClass "abcdef")
      return $ chr l - chr 'a' + 10)
-}
octDigit = satisfy (inClass "01234567")
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

