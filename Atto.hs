module Atto where

import BString

import Prelude hiding (null, init, tail, head, elem, length, (++), (!!), toLower, split, last, take, drop, notElem, concat, takeWhile, dropWhile, putStrLn, putStr)

import Data.Attoparsec.ByteString as AP

import qualified Debug.Trace

anyChar = anyWord8

char x = word8 $ convertChar x

--oneOf xs = choice $ map char xs
oneOf xs = satisfy $ inClass xs

--noneOf :: String -> Parser Word8
--noneOf xs = choice $ map char $ S.toList $ allChars S.\\ S.fromList xs
noneOf xs = satisfy $ notInClass xs

many xs = many' xs

manyTill2 xs = AP.takeWhile (notInClass xs)

parse1 p txt = case parseOnly p txt of
   Left msg -> error $ "parse1: "++msg
   Right r  -> r

parseNoFail z p txt = case parseOnly (p <* takeByteString) txt of
   Left msg -> Debug.Trace.trace ("parseNoFail: "++msg++" input="++convertString txt) z
   Right r  -> r

