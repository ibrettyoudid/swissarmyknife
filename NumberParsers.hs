-- Copyright 2025 Brett Curtis
{-# LANGUAGE FlexibleContexts #-}

module NumberParsers
(
   module NumberParsers,
   (<|>),
   try
)
where

import Favs

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Control.Applicative ((<$>))
import Control.Monad
import Data.Char (digitToInt)

--type Blah c = Stream s m c => ParsecT s u m c
parse1 p filename txt = case parse p filename txt of
   Left l -> error $ show l
   Right r -> r
   
--only succeed if it can't be an integer
forceFloating :: Stream s m Char => ParsecT s u m Double
forceFloating =         do f <- sign
                           n <- decimal
                           fract <- option Nothing (Just <$> fraction)
                           expo  <- option Nothing (Just <$> exponent')
                           guard (isJust fract || isJust expo)
                           return ((fromInteger n + fromMaybe 0.0 fract) * fromMaybe 1.0 expo)

floating :: Stream s m Char => ParsecT s u m Double
floating =              do s <- sign
                           n <- decimal
                           fract <- option 0.0 fraction
                           expo  <- option 1.0 exponent'
                           return (s (fromInteger n + fract) * expo)

-- decimal point, fractional part
fraction :: Stream s m Char => ParsecT s u m Double
fraction =
                        do char '.'
                           digits <- many1 digit <?> "fraction"
                           return (foldr op 0.0 digits)

                  <?> "fraction"
                           where
                              op d f = (f + fromIntegral (digitToInt d))/10.0

exponent' :: Stream s m Char => ParsecT s u m Double
exponent' = do oneOf "eE"
               s <- sign
               e <- decimal <?> "exponent"
               return $ power $ s e

         <?> "exponent"
                  where
                     power e  | e < 0      = 1.0/power (-e)
                              | otherwise  = fromInteger (10^e)


-- integers and naturals
integer :: Stream s m Char => ParsecT s u m Integer
integer =
                  do f <- sign
                     f <$> nat

sign :: (Stream s m Char, Num n) => ParsecT s u m (n -> n)
sign =
                           (char '-' >> return negate)
                  <|>      (char '+' >> return id)
                  <|>      return id

nat :: Stream s m Char => ParsecT s u m Integer
nat =                zeroNumber <|> decimal

zeroNumber :: Stream s m Char => ParsecT s u m Integer
zeroNumber =      do char '0'
                     hexadecimal <|> octal <|> decimal <|> return 0
               <?> ""

decimal :: Stream s m Char => ParsecT s u m Integer
decimal =            baseInteger 10 digit

hexadecimal :: Stream s m Char => ParsecT s u m Integer
hexadecimal     = do oneOf "xX"
                     baseInteger 16 hexDigit



octal :: Stream s m Char => ParsecT s u m Integer
octal           = do oneOf "oO"
                     baseInteger 8  octDigit

baseInteger :: Stream s m Char => Integer -> ParsecT s u m Char -> ParsecT s u m Integer
baseInteger base baseDigit
   = do digits <- many1 baseDigit
        let n = foldl (\x d -> x*base + toInteger (digitToInt d)) 0 digits
        seq n (return n)


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

