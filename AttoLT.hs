module AttoLT where

import BString
import AttoAny 

import Prelude hiding (null, init, tail, head, elem, length, (++), (!!), toLower, split, last, take, drop, notElem, concat, takeWhile, dropWhile, putStrLn, putStr)

import qualified Data.Attoparsec.Text.Lazy as AttoLT
import qualified Data.Text.Lazy.Builder as B

import qualified Debug.Trace

--anyChar = AttoLT.anyWord8
{-
char :: Char -> AttoLT.Parser Char
char x = do
   c <- AttoLT.word8 $ convertChar x
   return $ convertChar c
-}
--anyChar1 = convertChar <$> AttoLT.anyWord8 :: AttoLT.Parser Char

--oneOf xs = choice $ map char xs
instance Parser AttoLT.Parser LText Char where
   char = AttoLT.char
   anyChar = AttoLT.anyChar
   oneOf xs = AttoLT.satisfy $ AttoLT.inClass xs

--noneOf :: String -> Parser Word8
--noneOf xs = choice $ map char $ S.toList $ allChars S.\\ S.fromList xs
   noneOf xs = AttoLT.satisfy $ AttoLT.notInClass xs

   many xs = AttoLT.many' xs

   parse1 p src txt = case AttoLT.parseOnly p txt of
      Left msg -> error $ "parse1: "++convertString src++": "++msg
      Right r  -> r

   parseNoFail z p txt = case AttoLT.parseOnly (p <* AttoLT.takeLazyText) txt of
      Left msg -> Debug.Trace.trace ("parseNoFail: "++msg++" input="++convertString txt) z
      Right r  -> r

   halves p txt = do
      let l = length txt
      case AttoLT.parseOnly p (take (div l 2) txt) of
         
   manyTill2 xs = convertString <$> AttoLT.takeWhile (AttoLT.notInClass xs)

instance Builder B.Builder LText where
   fromText = B.fromLazyText
   toText = B.toLazyText
   fromString = B.fromString