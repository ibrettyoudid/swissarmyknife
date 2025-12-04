{-# LANGUAGE FlexibleInstances #-}
-- Copyright 2025 Brett Curtis
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BString
(
   module BString,
   B.ByteString
)
where

import qualified Favs as F

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Maybe
import System.IO
import qualified Prelude as P
import Prelude hiding (String, drop, find, head, tail, length, null, map, (++), splitAt)

import Data.Word

type LByteString = LB.ByteString

class Show s => BString s where
   empty :: s
   tail :: s -> s
   (++) :: s -> s -> s
   take :: Int -> s -> s
   drop :: Int -> s -> s
   init :: s -> s
   inits :: s -> [s]
   tails :: s -> [s]
   null :: s -> Bool
   length :: s -> Int
   intercalate :: s -> [s] -> s
   splitAt :: Int -> s -> (s, s)
   putStr :: s -> IO ()
   putStrLn :: s -> IO ()

class BString s => BStringC c s | s -> c where
   cons :: c -> s -> s
   head :: s -> c
   last :: s -> c
   (!!) :: s -> Int -> c
   find :: (c -> Bool) -> s -> Maybe c
   elem :: c -> s -> Bool
   notElem :: c -> s -> Bool

   smap  :: (c -> c) -> s -> s
   takeWhile :: (c -> Bool) -> s -> s
   dropWhile :: (c -> Bool) -> s -> s

   -- these are here because of the Eq class on the characters
   stripPrefix :: s -> s -> Maybe s
   isPrefixOf :: s -> s -> Bool
   isSuffixOf :: s -> s -> Bool
   isInfixOf :: s -> s -> Bool

instance Show a => BString [a] where
   empty = []
   tail = P.tail
   (++) = (P.++)
   take = P.take
   drop = P.drop
   init = P.init
   inits = L.inits
   tails = L.tails
   null = P.null
   length = P.length
   intercalate = L.intercalate

   splitAt = L.splitAt
--   split = F.split

instance {-# OVERLAPPING #-} BString [Char] where
   empty = []
   tail = P.tail
   (++) = (P.++)
   take = P.take
   drop = P.drop
   init = P.init
   inits = L.inits
   tails = L.tails
   null = P.null
   length = P.length
   intercalate = L.intercalate

   splitAt = L.splitAt
   putStr = P.putStr
   putStrLn = P.putStrLn

instance (Eq a, Show a) => BStringC a [a] where
   cons = (:)
   head = P.head
   last = P.last
   (!!) = (P.!!)
   find = L.find
   elem = L.elem
   notElem = L.notElem

   smap = L.map
   takeWhile = L.takeWhile
   dropWhile = L.dropWhile

   stripPrefix = L.stripPrefix
   isPrefixOf = L.isPrefixOf
   isSuffixOf = L.isSuffixOf
   isInfixOf = L.isInfixOf

instance BString B.ByteString where
   empty = B.empty
   tail = B.tail
   (++) = B.append
   take = B.take
   drop = B.drop
   init = B.init
   inits = B.inits
   tails = B.tails
   null = B.null
   length = B.length
   intercalate = B.intercalate
   splitAt = B.splitAt
   putStr = B.putStr
   putStrLn = B.putStr . (++"\n")

instance BStringC Word8 B.ByteString where
   cons = B.cons
   head = B.head
   last = B.last
   (!!) = B.index
   find = B.find
   elem = B.elem
   notElem = B.notElem

   smap = B.map
   takeWhile = B.takeWhile
   dropWhile = B.dropWhile

   stripPrefix p s = if isPrefixOf p s then Just $ drop (length p) s else Nothing
   isPrefixOf = B.isPrefixOf
   isSuffixOf = B.isSuffixOf
   isInfixOf = B.isInfixOf

instance BString LB.ByteString where
   empty = LB.empty
   tail = LB.tail
   (++) = LB.append
   take n = LB.take (fromIntegral n)
   drop n = LB.drop (fromIntegral n)
   init = LB.init
   inits = LB.inits
   tails = LB.tails
   null = LB.null
   length = fromIntegral . LB.length
   intercalate = LB.intercalate
   splitAt n = LB.splitAt (fromIntegral n)
   putStr = LB.putStr
   putStrLn = LB.putStr . (++"\n")

instance BStringC Word8 LB.ByteString where
   cons = LB.cons
   head = LB.head
   last = LB.last
   s !! n = LB.index s (fromIntegral n)
   find = LB.find
   elem = LB.elem
   notElem = LB.notElem

   smap = LB.map
   takeWhile = LB.takeWhile
   dropWhile = LB.dropWhile

   stripPrefix p s = if isPrefixOf p s then Just $ drop (length p) s else Nothing
   isPrefixOf = LB.isPrefixOf
   isSuffixOf = LB.isSuffixOf
   --isInfixOf = LB.isInfixOf

instance BString T.Text where
   empty = T.empty
   tail = T.tail
   (++) = T.append
   take = T.take
   drop = T.drop
   init = T.init
   inits = T.inits
   tails = T.tails
   null = T.null
   length = T.length
   intercalate = T.intercalate
   splitAt = T.splitAt
   --putStr = T.putStr
   --putStrLn = T.putStrLn

instance BStringC Char T.Text where
   cons = T.cons
   head = T.head
   last = T.last
   (!!) = T.index
   find = T.find
   elem = T.elem
   notElem c s = not $ T.elem c s

   smap = T.map
   takeWhile = T.takeWhile
   dropWhile = T.dropWhile

   stripPrefix = T.stripPrefix
   isPrefixOf = T.isPrefixOf
   isSuffixOf = T.isSuffixOf
   isInfixOf = T.isInfixOf

concat xs = foldr (++) empty xs

split sep = splitWith (stripPrefix sep)

-- splitSeps seps = splitWith (F.justPrefixs seps)

split1With pred str = F.firstJustElse (str, empty) (zipWith (\a b -> (a,) <$> pred b) (inits str) (tails str))

splitWith pred = L.unfoldr (\s -> F.ifJust (not $ null s) $ split1With pred s)

replace :: (Eq c, Eq s, BStringC c s) => s -> s -> s -> s
replace from to lst =
      let 
            l = length from
            fif x
               | null x = empty
               | otherwise = let
                  (tak, drp) = splitAt l x
      
                  in if tak == from
                        then to ++ fif drp
                        else cons (head x) (fif $ tail x)
      in fif lst

class ConvertString a b where
   convertString :: a -> b

instance ConvertString P.String B.ByteString where
   convertString = B.pack . L.map (fromIntegral . C.ord)

instance ConvertString B.ByteString P.String where
   convertString = L.map (C.chr . fromIntegral) . B.unpack

instance ConvertString B.ByteString LB.ByteString where
   convertString = B.fromStrict

instance ConvertString LB.ByteString B.ByteString where
   convertString = B.toStrict

instance ConvertString P.String LB.ByteString where
   convertString = LB.pack . L.map (fromIntegral . C.ord)

instance ConvertString LB.ByteString P.String where
   convertString = L.map (C.chr . fromIntegral) . LB.unpack

instance ConvertString P.String T.Text where
   convertString = T.pack

instance ConvertString T.Text P.String where
   convertString = T.unpack

instance ConvertString a a where
   convertString = id

instance ConvertString T.Text B.ByteString where
   convertString s = convertString (convertString s :: P.String)

instance ConvertString B.ByteString T.Text where
   convertString s = convertString (convertString s :: P.String)

class ConvertChar a b where
   convertChar :: a -> b

instance ConvertChar Char Word8 where
   convertChar = fromIntegral . C.ord

instance ConvertChar Word8 Char where
   convertChar = C.chr . fromIntegral

instance ConvertChar a a where
   convertChar = id

toLower :: (ConvertChar a Char, ConvertChar Char a) => a -> a
toLower = convertChar . C.toLower . convertChar

filename list = drop (length list - 1 - fromMaybe (length list - 1) (B.elemIndex (convertChar '/') (B.reverse list))) list

ext list = drop (length list - 1 - fromMaybe (negate 1) (B.elemIndex (convertChar '.') (B.reverse list))) list

b :: ConvertString a B.ByteString => a -> B.ByteString
b = convertString
s :: P.String -> P.String
s = id
l :: LByteString -> LByteString
l = id
t :: T.Text -> T.Text
t = id