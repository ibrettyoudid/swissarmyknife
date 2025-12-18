{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TestBoomerang where

import Control.Category (id, (.))
import Control.Monad (forever)
import Text.Boomerang (
   Boomerang (Boomerang, prs, ser),
   Parser (Parser),
   hhead,
   incMinor,
   parse,
   push,
   rCons,
   rNil,
   unparse,
   val,
   xmap,
   xmaph,
   xpure,
   type (:-) (..))

import Text.Boomerang.String hiding (int)
import Text.Boomerang.String qualified as B
import Text.Boomerang.TH
import Prelude hiding (id, (.))

import Data.Bits
import Data.Char
import System.IO.Unsafe

data File = File2 Header String

data Id3Base = Id3Base Header String

data Header = Header {verMajor :: Int, verMinor :: Int, unsync :: Bool, extHdr :: Bool, experi :: Bool, footer :: Bool, tagSize :: Int} deriving (Eq, Show)

data Frame
   = Frame {frameHeader :: FrameHeader, contents :: String}
   | Padding String
   deriving (Eq)

data FrameHeader = FrameHeader
   { frameID :: String
   , frameSize :: Int
   , tagAltPrsv :: Bool
   , fileAltPrsv :: Bool
   , readOnly :: Bool
   , grpIdent :: Bool
   , compression :: Bool
   , encryption :: Bool
   }
   deriving (Eq)

$(makeBoomerangs ''Id3Base)
$(makeBoomerangs ''Header)
$(makeBoomerangs ''Frame)
$(makeBoomerangs ''FrameHeader)

test = alpha <> digit

int = xmaph ord (Just . chr) anyChar

rep p 0 = rNil
rep p n = rCons . p . rep p (n - 1)

intn = intx 0x100

int2 = intn 2

int4 = intn 4

intx m n = xmaph (intf m) (Just . unintf n m) (rep int n)

intf m = foldl1 (\a b -> a * m + b)

unintf 0 _ _ = []
unintf n m x = let
   (q, r) = divMod x (m ^ (n - 1))
   
   in fromIntegral q : unintf (n - 1) m r

unbits n x = take n $ map odd $ iterate (`shift` (-1)) x

bits :: [Bool] -> Int
bits = foldl (\a x -> shift a 1 .|. (if x then 1 else 0)) 0

flags3 =
   xmap
      (\(f :- o) -> let us : eh : ex : _ = unbits 8 f in us :- eh :- ex :- o)
      (\(us :- eh :- ex :- o) -> Just (bits [us, eh, ex] :- o))
      int

flags4 =
   xmap
      (\(f :- o) -> let us : eh : ex : ft : _ = unbits 8 f in us :- eh :- ex :- ft :- o)
      (\(us :- eh :- ex :- ft :- o) -> Just (bits [us, eh, ex, ft] :- o))
      int

restp :: Parser StringError String (b -> String :- b)
restp = Parser $ \tokens pos -> [Right ((mypush tokens, []), incMinor (length tokens) pos)]

boo x = Boomerang x undefined

rest = Boomerang restp undefined

-- autorep p q = do f <- prs p; prs $ rep q $ mypop f
autorep :: (Eq hdr) => Boomerang e tok ([a1] :- stack2) (hdr :- [a1] :- stack2) -> Boomerang e tok ([a1] :- stack2) (a1 :- [a1] :- stack2) -> (hdr -> Int) -> (Int -> hdr -> hdr) -> Boomerang e tok stack2 (hdr :- [a1] :- stack2)
autorep p q f g =
   Boomerang
      ( do
         topf <- prs p
         prs (push (mytop topf) . rep q (f (mytop topf)))
      )
      -- undefined

      ( \r0 -> do
         (s1, r1) <- ser p r0
         let l = length $ hhead r1
         (s2, r2) <- ser (push (g l (hhead r0)) . rep q l) r0
         -- (s2, r2) <- ser (rep q (length r0) . push (hhead r0)) r0
         return (s1 . s2, r2)
      )

pass p q f g h = 
   Boomerang
      ( do
         topf <- prs p
         let l = mytop topf
         prs (push l . q (f l))
      )
      ( \r0 -> do
         (s1, r1) <- ser p r0
         let l = h $ hhead r1
         (s2, r2) <- ser (push (g l (hhead r0)) . q l) r0
         return (s1 . s2, r2)
      )
   -- q = rep q
   -- h = length

{-
autorep :: (Eq t1, Eq t2, Num t2) => Boomerang e tok ([a1] :- stack2) (t1 :- [a1] :- stack2) -> Boomerang e tok ([a1] :- stack2) (a1 :- [a1] :- stack2) -> (t1 -> t2) -> (t2 -> t1 -> t1) -> Boomerang e tok stack2 (t1 :- [a1] :- stack2)
autorep p q f g = Boomerang
   (do
      topf <- prs p
      prs (push (mytop topf) . rep q (f (mytop topf))))
   --undefined
   {-
   (\r0 -> do
      (s1, r1) <- ser p r0
      (s2, r2) <- ser (push (hhead r0) . rep q (length r0)) r0
      --(s2, r2) <- ser (rep q (length r0) . push (hhead r0)) r0
      return (s1 . s2, r2))
-}
-}

mytop :: (b -> a :- b) -> a
mytop h = hhead (h undefined)

mypush :: a -> b -> (a :- b)
mypush = (:-)

dup = xpure (\(a :- b) -> a :- a :- b) (\(a :- b :- c) -> Just (b :- c))

-- autorep p q = do prs dup; f <- prs p; prs $ rep q $ mypop f

-- Parser $ \tokens pos -> [Right ((\(top :- stack) -> prs $ rep q top), )]
-- doId3v2 = boo $ do

id3Base = rId3Base . autorep header anyChar tagSize (\x y -> y{tagSize = x})

header = rHeader . "ID3" . int . int . flags4 . intx 0x80 4

frameh = rFrameHeader . rep anyChar 4 . int4 . flags3 . flags3

frameBase = rFrame . autorep frameh anyChar frameSize (\x y -> y{frameSize = x})

t1 = autorep B.int anyChar id (\x y -> y)

t2 = parse t1 "3hello"
