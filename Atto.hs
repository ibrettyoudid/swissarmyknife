module Atto where

import BString

import Prelude hiding (null, init, tail, head, elem, length, (++), (!!), toLower, split, last, take, drop, notElem, concat, takeWhile, dropWhile, putStrLn, putStr)

import qualified Data.Attoparsec.ByteString as AP

import qualified Debug.Trace

anyChar = AP.anyWord8

char :: Char -> AP.Parser Char
char x = do
   c <- AP.word8 $ convertChar x
   return $ convertChar c

anyChar1 = convertChar <$> AP.anyWord8 :: AP.Parser Char

--oneOf xs = choice $ map char xs
oneOf xs = AP.satisfy $ AP.inClass xs

--noneOf :: String -> Parser Word8
--noneOf xs = choice $ map char $ S.toList $ allChars S.\\ S.fromList xs
noneOf xs = AP.satisfy $ AP.notInClass xs

many xs = AP.many' xs

manyTill2 xs = AP.takeWhile (AP.notInClass xs)

parse1 p txt = case AP.parseOnly p txt of
   Left msg -> error $ "parse1: "++msg
   Right r  -> r

parseNoFail z p txt = case AP.parseOnly (p <* AP.takeByteString) txt of
   Left msg -> Debug.Trace.trace ("parseNoFail: "++msg++" input="++convertString txt) z
   Right r  -> r

halves p txt = do
   let l = length txt
   case AP.parseOnly p (take (div l 2) txt) of
      