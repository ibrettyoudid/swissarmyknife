module AttoB where

import BString
import AttoAny

import Prelude hiding (null, init, tail, head, elem, length, (++), (!!), toLower, split, last, take, drop, notElem, concat, takeWhile, dropWhile, putStrLn, putStr)
import qualified Data.Attoparsec.ByteString as AttoB
import qualified Data.Attoparsec.ByteString.Lazy as AttoL
import qualified Debug.Trace

import Data.Word
import Control.Applicative (Alternative)

import qualified Debug.Trace


instance ParserC Word8 where
   inClass = AttoB.inClass
   notInClass = AttoB.notInClass

instance Parser AttoB.Parser ByteString Word8 where
   many xs = AttoB.many' xs
   parse  p txt = AttoB.parse p txt
   parse1 p src txt = case AttoB.parseOnly p txt of
      Left msg -> error $ "parse1: "++convertString src++": "++msg
      Right r  -> r

   parse2 p txt = case AttoB.parseOnly p txt of
      Left msg -> error $ "parse2: "++msg
      Right r  -> r

   parseNoFail z p txt = case AttoB.parseOnly (p <* AttoB.takeByteString) txt of
      Left msg -> Debug.Trace.trace ("parseNoFail: "++msg++" input="++convertString txt) z
      Right r  -> r

   string s = AttoB.string $ convertString s

   manyTill2 xs = AttoB.takeWhile (AttoB.notInClass xs)

   halves p txt = do
      let l = length txt
      case AttoB.parseOnly p (take (div l 2) txt) of
         
   anyChar   = AttoB.anyWord8
--   anyChar1 = convertChar <$> AttoL.anyWord8 :: AttoL.Parser Char
   char c    = convertChar <$> AttoB.word8 (convertChar (c :: Char))
   oneOf xs  = AttoB.satisfy $ AttoB.inClass xs
   noneOf xs = AttoB.satisfy $ AttoB.notInClass xs

newtype AttoLB res = AttoLB { attoLB :: AttoB.Parser res }

{-
instance Parser AttoL.Parser where
   many xs = AttoL.many' xs
-}
instance Functor AttoLB where
   fmap f (AttoLB p) = AttoLB $ fmap f p

instance Applicative AttoLB where

instance Alternative AttoLB where

instance Monad AttoLB where
   return = AttoLB . return
   (AttoLB ma) >>= amb = AttoLB (ma >>= (\a -> attoLB $ amb a))

instance Parser AttoLB LByteString Word8 where
   anyChar                    = AttoLB anyChar
   char          c            = AttoLB $ char c
   oneOf         s            = AttoLB $ oneOf         s      
   noneOf        s            = AttoLB $ noneOf        s      
   many          (AttoLB p)   = AttoLB $ many          p       
   many1         (AttoLB p)   = AttoLB $ many1         p       
   manyTill      (AttoLB p) (AttoLB q) = AttoLB $ manyTill      p q       
   lookAhead     (AttoLB p)   = AttoLB $ lookAhead     p       
   option      r (AttoLB p)   = AttoLB $ option      r p        
   choice                 ps  = AttoLB $ choice        $ map attoLB ps      
   endOfInput                 = AttoLB $ endOfInput       
   parse         (AttoLB p) s = parse       p (convertString s)
   parse1      (AttoLB p) f s = parse1      p (convertString f) (convertString s)
   parse2        (AttoLB p) s = parse2        p (convertString s)     
   parseOnly     (AttoLB p) s = parseOnly     p (convertString s)
   halves        (AttoLB p) s = halves        p (convertString s)   
   parseNoFail r (AttoLB p) s = parseNoFail r p (convertString s)
   --(<?>)         (AttoLB p) s = (<?>)         p (convertString s :: ByteString)
   string                   s = convertString <$> (AttoLB $ string          s)
   manyTill2       s          = convertString <$> (AttoLB $ manyTill2       s)
   rest                       = convertString <$> (AttoLB $ rest)
   takeWhile     p            = convertString <$> (AttoLB $ AttoAny.takeWhile     p)
   takeWhile1    p            = convertString <$> (AttoLB $ takeWhile1    p    )
         
{-}
instance ParserC AttoL.Parser Word8 where
   anyChar   = AttoL.anyWord8
   char c    = convertChar <$> AttoL.word8 (convertChar (c :: Char))
   oneOf xs  = AttoL.satisfy $ AttoL.inClass xs
   noneOf xs = AttoL.satisfy $ AttoL.notInClass xs
-}
--oneOf xs = choice $ map char xs
--noneOf :: String -> Parser Word8
--noneOf xs = choice $ map char $ S.toList $ allChars S.\\ S.fromList xs

