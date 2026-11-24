module MSolve (
   msolveOutput,
   msolveRational,
   msolveIO,
   msolve
) 
where

import MPoly
import NumberParsers

import System.Process
import System.IO.Unsafe
import Data.List
import Data.Ratio

import Text.Parsec
import Text.Parsec.String (parseFromFile)

whiteSpace :: (Monad m) => ParsecT [Char] u m [Char]
whiteSpace = many (oneOf " \n\r\t")

lexeme l = do
   whiteSpace
   r <- l
   whiteSpace
   return r

list item = do
   lexeme $ char '['
   items <- sepBy item $ lexeme $ char ','
   lexeme $ char ']'
   return items

number :: Monad m => ParsecT [Char] u m (Ratio Integer)
number = do
   mantissa <- integer
   choice [
      do
         try $ lexeme $ char '*'
         lexeme $ char '2'
         lexeme $ char '^'
         n <- lexeme integer
         return $ mantissa * 2 ^ n % 1, 

      do
         try $ lexeme $ char '/'
         lexeme $ char '2'
         lexeme $ char '^'
         n <- lexeme integer
         return $ mantissa % 2 ^ n,
         
      return $ mantissa % 1]

msolveOutput :: Monad m => ParsecT [Char] u m [[[Ratio Integer]]]
msolveOutput = do
   lexeme $ char '['
   dims <- integer
   case dims of
      -1 -> return []
      0 -> do
         lexeme $ char ','
         lexeme $ char '['
         integer
         lexeme $ char ','
         items <- list $ list $ list number
         lexeme $ char ']'
         lexeme $ char ']'
         lexeme $ char ':'
         return items

msolveRational mpolys = do
   writeFile "/tmp/msolve_input.ms" $ 
      intercalate "," (vars $ head mpolys) ++ "\n" ++
      "0\n" ++
      intercalate ",\n" (map showmp1 mpolys) ++ "\n"
   callCommand "msolve -f /tmp/msolve_input.ms -o /tmp/msolve_output"
   output <- parseFromFile msolveOutput "/tmp/msolve_output"
   case output of
      Left l -> error $ show l
      Right r -> return r
--   createProcess (proc "msolve -f /tmp/msolve_input.ms -o /tmp/msolve_output" [])
   
msolveIO mpolys = do
   s3 <- msolveRational mpolys
   return $ map (map (map fromRational)) s3

msolve :: (Show coeff, Real coeff, Fractional coeff) => [MPoly coeff] -> [[[coeff]]]
msolve mps = unsafePerformIO $ msolveIO mps