module Atto where

import BString

import Prelude hiding (takeWhile, many, many')

import Data.Attoparsec.ByteString.Lazy as AP


anyChar = anyWord8

char x = word8 $ convertChar x

--oneOf xs = choice $ map char xs
oneOf xs = satisfy $ inClass xs

--noneOf :: String -> Parser Word8
--noneOf xs = choice $ map char $ S.toList $ allChars S.\\ S.fromList xs
noneOf xs = satisfy $ notInClass xs

many xs = many' xs

manyTill2 xs = AP.takeWhile (notInClass xs)