module AttoT
(
   module AttoT,
   module AttoAny
) 
where

import BString
import AttoAny 

import Prelude hiding (null, init, tail, head, elem, length, (++), (!!), toLower, split, last, take, drop, notElem, concat, takeWhile, dropWhile, putStrLn, putStr)

import qualified Data.Attoparsec.Text as AttoT
import qualified Data.Attoparsec.Combinator as AttoT
import qualified Data.Text.Internal.StrictBuilder as B

import qualified Debug.Trace

--anyChar = AttoT.anyWord8
{-
char :: Char -> AttoT.Parser Char
char x = do
   c <- AttoT.word8 $ convertChar x
   return $ convertChar c
-}
--anyChar1 = convertChar <$> AttoT.anyWord8 :: AttoT.Parser Char

instance ParserC Char where
   inClass = AttoT.inClass
   notInClass = AttoT.notInClass

--oneOf xs = choice $ map char xs
instance Parser AttoT.Parser Text Char where
   char = AttoT.char
   anyChar = convertChar <$> AttoT.anyChar
   oneOf xs = AttoT.satisfy $ AttoT.inClass xs

--noneOf :: String -> Parser Word8
--noneOf xs = choice $ map char $ S.toList $ allChars S.\\ S.fromList xs
   noneOf xs = AttoT.satisfy $ AttoT.notInClass xs
   takeWhile = AttoT.takeWhile
   lookAhead = AttoT.lookAhead

   many xs = AttoT.many' xs

   parse1 p src txt = case AttoT.parseOnly p txt of
      Left msg -> error $ "parse1: "++convertString src++": "++msg
      Right r  -> r

   parse2 p txt = case AttoT.parseOnly p txt of
      Left msg -> error $ "parse1: "++msg
      Right r  -> r

   parseNoFail z p txt = case AttoT.parseOnly (p <* AttoT.takeLazyText) txt of
      Left msg -> Debug.Trace.trace ("parseNoFail: "++msg++" input="++convertString txt) z
      Right r  -> r

   halves p txt = do
      let l = length txt
      case AttoT.parseOnly p (take (div l 2) txt) of
         
   manyTill2 xs = convertString <$> AttoT.takeWhile (AttoT.notInClass xs)
{-}
instance Builder B.StrictTextBuilder Text where
   fromText = B.fromText
   toText = B.toText
   fromString s = mconcat $ map B.fromChar s
-}