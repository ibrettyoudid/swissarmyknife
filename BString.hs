{-# LANGUAGE FlexibleInstances #-}
-- Copyright 2025 Brett Curtis
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BString where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as LB
import Data.List qualified as L
import Data.Text qualified as T
import Favs qualified as F
import Prelude hiding (String, drop, find, head, length, null, (++))
import Prelude qualified as P

import Data.Char
import Data.Word

class BString s where
  empty :: s
  tail :: s -> s
  (++) :: s -> s -> s
  drop :: Int -> s -> s
  inits :: s -> [s]
  tails :: s -> [s]
  null :: s -> Bool
  length :: s -> Int
  intercalate :: s -> [s] -> s

class BStringC c s | s -> c where
  cons :: c -> s -> s
  head :: s -> c
  (!!) :: s -> Int -> c
  find :: (c -> Bool) -> s -> Maybe c
  elem :: c -> s -> Bool
  notElem :: c -> s -> Bool

  -- these are here because of the Eq class on the characters
  stripPrefix :: s -> s -> Maybe s
  isPrefixOf :: s -> s -> Bool
  isSuffixOf :: s -> s -> Bool
  isInfixOf :: s -> s -> Bool

instance BString [a] where
  empty = []
  tail = P.tail
  (++) = (P.++)
  drop = P.drop
  inits = L.inits
  tails = L.tails
  null = P.null
  length = P.length
  intercalate = L.intercalate

--   split = F.split

instance (Eq a) => BStringC a [a] where
  cons = (:)
  head = P.head
  (!!) = (P.!!)
  find = L.find
  elem = L.elem
  notElem = L.notElem
  stripPrefix = L.stripPrefix
  isPrefixOf = L.isPrefixOf
  isSuffixOf = L.isSuffixOf
  isInfixOf = L.isInfixOf

instance BString B.ByteString where
  empty = B.empty
  tail = B.tail
  (++) = B.append
  drop = B.drop
  inits = B.inits
  tails = B.tails
  null = B.null
  length = B.length
  intercalate = B.intercalate

instance BStringC Word8 B.ByteString where
  cons = B.cons
  head = B.head
  (!!) = B.index
  find = B.find
  elem = B.elem
  notElem = B.notElem

  stripPrefix p s = if isPrefixOf p s then Just $ drop (length p) s else Nothing
  isPrefixOf = B.isPrefixOf
  isSuffixOf = B.isSuffixOf
  isInfixOf = B.isInfixOf

instance BString T.Text where
  empty = T.empty
  tail = T.tail
  (++) = T.append
  drop = T.drop
  inits = T.inits
  tails = T.tails
  null = T.null
  length = T.length
  intercalate = T.intercalate

instance BStringC Char T.Text where
  cons = T.cons
  head = T.head
  (!!) = T.index
  find = T.find
  elem = T.elem
  notElem c s = not $ T.elem c s

  stripPrefix = T.stripPrefix
  isPrefixOf = T.isPrefixOf
  isSuffixOf = T.isSuffixOf
  isInfixOf = T.isInfixOf

concat xs = foldr (++) empty xs

split sep = splitWith (stripPrefix sep)

-- splitSeps seps = splitWith (F.justPrefixs seps)

split1With pred str = F.firstJustElse (str, empty) (zipWith (\a b -> (a,) <$> pred b) (inits str) (tails str))

splitWith pred = L.unfoldr (\s -> F.ifJust (not $ null s) $ split1With pred s)

class ConvertString a b where
  convertString :: a -> b

instance ConvertString P.String B.ByteString where
  convertString = B.pack . map (fromIntegral . ord)

instance ConvertString B.ByteString P.String where
  convertString = map (chr . fromIntegral) . B.unpack

instance ConvertString B.ByteString LB.ByteString where
  convertString = B.fromStrict

instance ConvertString LB.ByteString B.ByteString where
  convertString = B.toStrict

instance ConvertString P.String LB.ByteString where
  convertString = LB.pack . map (fromIntegral . ord)

instance ConvertString LB.ByteString P.String where
  convertString = map (chr . fromIntegral) . LB.unpack

instance ConvertString P.String T.Text where
  convertString = T.pack

instance ConvertString T.Text P.String where
  convertString = T.unpack

instance ConvertString P.String P.String where
  convertString = id

instance ConvertString T.Text B.ByteString where
  convertString s = convertString (convertString s :: P.String)

instance ConvertString B.ByteString T.Text where
  convertString s = convertString (convertString s :: P.String)

class ConvertChar a b where
  convertChar :: a -> b

instance ConvertChar Char Word8 where
  convertChar = fromIntegral . ord

instance ConvertChar Word8 Char where
  convertChar = chr . fromIntegral

bytestring :: B.ByteString
bytestring = "abra"

string = "hello"

test = bytestring ++ string
