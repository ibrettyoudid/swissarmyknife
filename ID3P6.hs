{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
-- Copyright 2025 Brett Curtis
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use zipWith" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
{-# HLINT ignore "Eta reduce" #-}

module ID3P6 where

import ApplyTuple
import NewTuple
import BString
import Favs hiding (range, split, split1With, splitWith)
import Iso hiding ((!!))
import MHashDynamic2 hiding (Apply, Frame, name, tl, value, Value, (!), (==))
import qualified MyPretty2
import Parser6 hiding (Frame, Range, int, low)
import Parser6 qualified as P
import Parser6Types qualified as P
import Shell hiding (contents, fields, main, year, (@))
import Show1
import ShowTuple

-- import Control.Monad.State
import Control.Applicative hiding (empty)
import Control.Monad
import System.Directory
import System.IO
import System.IO.Unsafe

-- import System.IO.Extra
import System.Posix.Files
import System.Process

-- import System.Win32.File
-- import System.Win32.Time

import Data.Binary
import Data.Bits
import Data.Char
import Data.List hiding (concat, take, drop, elem, find, groupBy, head, inits, intercalate, isInfixOf, isPrefixOf, isSuffixOf, last, length, notElem, null, stripPrefix, tail, tails, (!!), (++))
import Data.List qualified
import Prelude hiding (concat, take, drop, elem, head, length, notElem, null, tail, last, (!!), (++))

-- import Data.Algorithm.Diff

import Data.ByteString qualified as B
import Data.Fixed
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word qualified as W
import GHC.Generics hiding (Meta)

-- things to try
t1 db = putt $ artists db
t2 = p fta
t3 = filtree [inli "apc"] artistd
t4 = filtree [inis $= "dt", ininis "iaw"] artistd

baseDir = if linux then "/home/brett/Music" else "d:/music"

infixr 5 @
p @ s = if last p == convertChar '/' then p ++ s else p ++ cons (convertChar '/') s

backupDir = baseDir @ "Backup"
artistd = baseDir @ "Artists"
unsharedd = baseDir @ "Unshared"
compd = baseDir @ "Compilations"
misc = baseDir @ "Misc"

f = [Artist, Year, Album, Track, Song]
d = [baseDir, "/", " - ", "/", " - "]
m = do
   String baseDir
   ar <- upto "/"
   yr <- upto " - "
   al <- upto "/"
   tr <- upto " - "
   so <- upto "."
   return (ar, yr, al, tr, so)
{-
m1 = do
   tod Artist
   to Year " - "
   tod Album
   to Track " - "
   to Song "."
   toe Ext
-}

upto x = AnyTill $ String x
ft = fileTree
fta = fileTree artistd
fds = fieldsFromString f d
down = "/home/brett/Downloads/"

p = play
play files = runInteractiveProcess "vlc" ("--one-instance" : files) Nothing Nothing
pft f dir = play $ filtree f dir

low        = smap toLower
inlow x    = isInfixOf (low x) . low
prelow x   = isPrefixOf (low x) . low
inis x     = map head $ split " " $ low x
ininis x y = low x `isInfixOf` inis y
inli x y   = inlow x y || ininis x y

-- fm filt = filtree (filt . metaFromString f d)
{-
data Split = SFail | Partial | Match deriving (Eq, Ord, Show)
sp :: (String -> Bool) -> String -> (String -> Split) -> String -> Split
sp pred delim cont [] = Partial
sp pred delim cont str = case split1M delim str of
   Just (b, a) -> if pred b   then cont a  else SFail
   Nothing     -> if pred str then Partial else SFail

sp1 pred delim = sp pred delim ok
-}
{-
splitWith pred = unfoldr (\s -> ifJust (not $ null s) $ split1With pred s)

split1With pred str = firstJustElse (str, []) (zipWith (\a b -> (a,) <$> pred b) (inits str) (tails str))

split1 sep = split1With (stripPrefix sep)

split1M sep = split1WithM (stripPrefix sep)

split1WithM pred str = case catMaybes $ zipWith (\a b -> (a,) <$> pred b) (inits str) (tails str) of
   [] -> Nothing
   (x : _) -> Just x
-}

--ok = const Match
ok2 f s = True
ok3 a b c = True
filtree [] p = map (p @) (fileNames p) ++ concatMap (filtree [] . (p @)) (dirNames p)
filtree (pred : fds) p = map (p @) (filter pred $ fileNames p) ++ concatMap (filtree fds . (p @)) (filter pred $ dirNames p)
artistp a = filter (inlow a) $ dirPaths artistd
artistt = fileTree . (artistd ++)
albump a = filter (inlow a) $ cdirPaths $ dirPaths artistd
ismp3 = (".mp3" `isSuffixOf`) . low
mp3s = filter ismp3 . fileTree
artistmp3s a = mp3s $ artistd ++ a

tagTree = unsafePerformIO . tagTreeM
tagTreeM d = fromAssocsD . concat <$> tagTreeM1 d
tagTreeM1 d =
   mapM (\f -> map (\fr -> ([toDyn f, toDyn $ fst fr], snd fr)) . M.toList . metaOfFrames1 <$> readTagM (readSomeAudio 4) f) $
      mp3s d

-- test = afl . ta . tagTree2

para = artistd @ "Paradise Lost"
satyr = artistd @ "Satyricon"
super = artistd @ "Superior"
volc = satyr @ "2002 - Volcano"
obsid = para @ "2020 - Obsidian"
tf = obsid @ "02 Fall from Grace.mp3"

type MyString = B.ByteString
type IOString = B.ByteString

data ID3Tag
   = ID3Tag { 
      id3 :: MyString, 
      verMajor :: Int, 
      verMinor :: Int, 
      unsync :: Bool, 
      extHdr :: Bool, 
      experi :: Bool, 
      footer :: Bool, 
      tagSize :: Int, 
      tagBytes :: B.ByteString, 
      frames :: [Frame] }

data FrameHeader = 
   FrameHeader { 
      frameID1    :: IOString, 
      frameSize   :: Int, 
      tagAltPrsv  :: Bool, 
      fileAltPrsv :: Bool, 
      readOnly    :: Bool, 
      compression :: Bool, 
      encryption  :: Bool, 
      grouping    :: Bool, 
      unsyncFr    :: Bool, 
      dataLenI    :: Bool, 
      frameBytes  :: B.ByteString } deriving (Eq, Ord, Show, Read, Generic)

data Frame  = FText         { frameID :: FrameID, frameHeader :: FrameHeader, textEncoding :: Int, value :: T.Text }
            | FSyncLyrics   { frameID :: FrameID, frameHeader :: FrameHeader, textEncoding :: Int, value :: T.Text, language :: T.Text, timeFormat :: Int, contentType :: Int, syncedLyrics :: M.Map Int T.Text }
            | FUserText     { frameID :: FrameID, frameHeader :: FrameHeader, textEncoding :: Int, value :: T.Text,     description :: T.Text }
            | FUnsyncLyrics { frameID :: FrameID, frameHeader :: FrameHeader, textEncoding :: Int, value :: T.Text,     description :: T.Text, language :: T.Text }
            | FPicture      { frameID :: FrameID, frameHeader :: FrameHeader, textEncoding :: Int, picture :: IOString, description :: T.Text, mimeType :: T.Text, pictureType :: Int }

            | FrameTruncated
            | Invalid B.ByteString
            | Bytes Int
            | Nowt
            deriving (Eq, Ord, Show, Read, Generic)

data MPEGFrame = MPEGFrame {version :: Int, layer :: Int, bitRate :: Int, sampRate :: Int, mpegFrameBytes :: Int, mpegFrameTime :: Pico, mpegFrameAudio :: B.ByteString} deriving (Eq, Ord, Show)

data FileTimes = FileTimes {created :: Pico, written :: Pico, accessed :: Pico} deriving (Eq, Ord, Show, Read)

blankFT = FileTimes 0 0 0

data Meta = Meta
   { byId :: M.Map FrameID Dynamic
   , isDir :: Bool
   , path :: T.Text
   , audio :: T.Text
   , artist :: T.Text
   , album :: T.Text
   , albumartist :: T.Text
   , track :: T.Text
   , song :: T.Text
   , year :: Int
   , genre :: T.Text
   , times :: FileTimes
   , orig :: FileTimes
   }
   deriving (Eq, Ord, Show, Read)

n = track

data Encoding = ISO8859 | UCS2
   deriving (Eq, Ord, Show)

-- test1 = putGrid $ transpose1 $
test1 = differences $ fileTree $ unsharedd @ "Portion Control" @ "2007 - Onion Jack IV"
test1a = commonSubsequencesList $ fileTree $ unsharedd @ "Portion Control" @ "2007 - Onion Jack IV"

test2 db = play $ map (c . path) $ filter (album $= "Paradise Lost") $ tl db

{-
fixalbumname = do
   db <- loadDB
   let fs = filter (album $= "Symbol of Life") $ tl db
   let fs1 = map (\m -> m{album = "Symbol Of Life"}) fs
   return $ updateDB1 db fs1
-}
timeFrame = mpegFrameTime . getMPEGFrame

a $$ b = a . map b

fieldname ?= b = \fileframes -> field fieldname fileframes == b

{-
artists1 db =
   map (applyT (mode ?? "Artist", range ?? "Year", countUnique ?? "Album", length :: [[Frame]] -> Int, mode ?? "Genre")) $ -- , sum $$ timeFrame, mode $$ genre))
   -- \$ sortOn (minimum $$ yearInt)
      groupBy1 artist $
         tl db
         -}

artists db =
   map (applyT (head $$ artist, counts $$ year, countUnique $$ album)) $ -- , sum $$ timeFrame, mode $$ genre)) -- , sum $$ timeFrame, mode $$ genre)) -- , sum $$ timeFrame, mode $$ genre)) -- , sum $$ timeFrame, mode $$ genre)) -- , sum $$ timeFrame, mode $$ genre)) -- , sum $$ timeFrame, mode $$ genre)) -- , sum $$ timeFrame, mode $$ genre)) -- , sum $$ timeFrame, mode $$ genre))
      groupBy1 artist $
         tl db

artistalbums db =
   map (applyT (hd $$ artist, hd $$ year, hd $$ album, mode $$ genre)) $ -- sum $$ timeFrame, -- sum $$ timeFrame, -- sum $$ timeFrame, -- sum $$ timeFrame, -- sum $$ timeFrame, -- sum $$ timeFrame, -- sum $$ timeFrame, -- sum $$ timeFrame,
      groupBy1 (applyT (artist, year, album)) $
         tl db

albums db =
   map (applyT (range $$ year, range $$ album)) $ -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame)) -- , sum  timeFrame))
      sortOn (minimum $$ year) $
         groupBy1 album $
            tl db

putt a = MyPretty2.putGrid $ transpose $ map showT a

-- t = over mapped show (0.2, 15, "ae")
-- putr = putGrid . transpose . map t

-- artists f db = map ()
--   $ groupBy1 artist $ artistsalbums
filterfr = M.filter
tl = map snd . M.toList

select ft = map (applyT ft)

fieldA id meta = fromDyn1 $ field id meta

fieldText id meta = fromDyn1 $ field id meta :: MyString

fieldBool id meta = fromDyn1 $ field id meta :: Bool

setFieldA id val meta = setField id (toDyn val) meta

-- setFields name vals = (vals ++) . delFields name
delField id meta = meta{byId = M.delete id $ byId meta}

-- delFields name = filter (\case FrameText name1 _ | name `isPrefixOf` name1 -> False; _ -> True)

field id meta = fromMaybe (error $ "field " ++ show id ++ " not in " ++ show meta) $ M.lookup id $ fields2 meta

getField id meta = M.lookup id $ fields2 meta

fields2 meta = M.fromList $ fields1 meta

setField1 id val meta = meta{byId = M.alter (const $ Just $ toDyn val) id $ byId meta}

instance P.Frame FrameID Dynamic Meta where
   myget1 n f = fromJust $ getField n f
   myset1 = setField

setField id val meta = case id of
   Bisdir      -> meta { isDir       = fromDyn1 val }
   Bpath       -> meta { path        = fromDyn1 val }
   Baudio      -> meta { audio       = fromDyn1 val }
   Track       -> meta { track       = fromDyn1 val }
   Album       -> meta { album       = fromDyn1 val }
   Artist      -> meta { artist      = fromDyn1 val }
   AlbumArtist -> meta { albumartist = fromDyn1 val }
   Song        -> meta { song        = fromDyn1 val }
   Year        -> meta { year        = fromDyn1 val }
   Genre       -> meta { genre       = fromDyn1 val }
   Btimes      -> meta { times       = fromDyn1 val }
   Borig       -> meta { orig        = fromDyn1 val }
   _           -> setField1 id val meta

fields1 meta =
   [ (Bisdir     , toDyn $ isDir       meta)
   , (Bpath      , toDyn $ path        meta)
   , (Baudio     , toDyn $ audio       meta)
   , (Track      , toDyn $ track       meta)
   , (Album      , toDyn $ album       meta)
   , (Artist     , toDyn $ artist      meta)
   , (AlbumArtist, toDyn $ albumartist meta)
   , (Song       , toDyn $ song        meta)
   , (Year       , toDyn $ year        meta)
   , (Genre      , toDyn $ genre       meta)
   , (Btimes     , toDyn $ times       meta)
   , (Borig      , toDyn $ orig        meta)
   ]
      ++ M.toList (byId meta)

field1 id meta = fromDynamic $ case id of
   Bisdir      -> toDyn $ isDir  meta
   Bpath       -> toDyn $ path   meta
   Baudio      -> toDyn $ audio  meta
   Track       -> toDyn $ track  meta
   Album       -> toDyn $ album  meta
   AlbumArtist -> toDyn $ artist meta
   Song        -> toDyn $ song   meta
   Year        -> toDyn $ year   meta
   Genre       -> toDyn $ genre  meta
   Btimes      -> toDyn $ times  meta
   Borig       -> toDyn $ orig   meta
   _           -> fromJust $ M.lookup id $ byId meta

isfixed id = case id of
   Bisdir      -> True
   Bpath       -> True
   Baudio      -> True
   Track       -> True
   Album       -> True
   Artist      -> True
   AlbumArtist -> True
   Song        -> True
   Year        -> True
   Btimes      -> True
   Borig       -> True
   _           -> False

isFile = not . isDir

{-
settimes = setFields "FT."
setctimes = setFields "FT.Current"
setotimes = setFields "FT.Original"
-}
class Zero a where
   zero :: a

{-
instance Num a => Zero a where
      zero = 0
-}
instance Zero Int where
   zero = 0

instance Zero Integer where
   zero = 0

instance Zero [a] where
   zero = []

instance (Zero a, Zero b) => Zero (a, b) where
   zero = (zero, zero)

instance Zero T.Text where
   zero = ""

hd [] = zero
hd (a : as) = a

maxi [] = zero
maxi a = maximum a

mini [] = zero
mini a = minimum a

a <<= b = b >>= a
infixr 1 <<=

data Range a = Range a a

range l = Range (mini l) (maxi l)

instance (Eq a, Show a) => Show (Range a) where
   show (Range a b) = if a == b then show a else show a ++ "-" ++ show b

modez z [] = z
modez _ xs = mode xs

groupBy f = combine (:) [] . mapfxx f
groupBy1 f = map snd . groupBy f

applyU2 (f, g) x = (f x, g x)
applyU3 (f, g, h) x = (f x, g x, h x)
applyU4 (f, g, h, i) x = (f w, g x, h x, i x)
applyU5 (f, g, h, i, j) x = (f x, g x, h x, i x, j x)
applyU6 (f, g, h, i, j, k) x = (f x, g x, h x, i x, j x, k x)
applyU7 (f, g, h, i, j, k, l) x = (f x, g x, h x, i x, j x, k x, l x)

applyV7 f (t, u, v, w, x, y, z) = (f t, f u, f v, f w, f x, f y, f z)
applyL fs x = map ($ x) fs

-- partPart :: P.ParsecT String u e String
-- pathPart = P.manyTill (P.char '/') $ P.string "/"

{-
pattrn p = do field <- P.manyTill P.anyChar (P.string ">")
                     delim <- P.manyTill P.anyChar (P.string "<" <|> P.eof)
                     pattrn (do fieldvals <- p
                                    value <- P.manyTill P.anyChar $ P.string delim
                                    return $ (field, value):fieldvals)
-}
pathParts = splitWith (stripPrefix "/")
dirName str =
   let
      p = pathParts str
      l = length p
      in
      p !! (l - 2)

fileName = last . pathParts

elemIndexRev e f =
   let
      l = length f
      r = reverse f
      i = fromMaybe (-1) $ elemIndex e r
      in
      l - i - 1

extension str =
   let
      f = fileName str
      i = elemIndexRev '.' f
      in
      drop i f

interleave as bs = concat $ transpose [as, bs]

uninterleave = transpose . groupN 2

{-
stats = do
   st <- filter ("Statement-" `isPrefixOf`) <$> listDirectory down
   let template = ["Statement--601033-78450349--" "-" "-" "-" "-" "-" ".pdf"]
   let st1 = mapMaybe (\x -> (x :) <$> infoFromString2 template x) st
   putGrid $ transpose st1
   let st2 = filter (\x -> (< 2000) $ readInt $ x !! 2) st1
   putGrid $ transpose st2
   let template = ["Statement--601033-78450349--" "-" "-" "--" "-" "-" ".pdf"]
   let sta = map (\(f : _ : d1 : m1 : y1 : d2 : m2 : y2 : left) -> [f, concat $ Main.interleave template (y1 : m1 : d1 : y2 : m2 : d2 : left)]) st2
   putGrid $ transpose sta
   mapM_ (\[from, to] -> rename (down ++ from) (down ++ to)) sta

-- let st3 = map (\x -> concat $ Main.interleave template $ (\[f,_,d1,m1,y1,d2,m2,y2] -> [y1,m1,d1, y2,m2,d2,""]) x) st2
-- putGrid [st3]
-- putGrid $ transpose $ map (\x -> [x, show $ infoFromString2 ["Statement--601033-78450349--""-""-""-""-""-"".pdf"] x]) st
-}
differences strs = differences1 strs $ commonSubsequencesList strs

differences1 strs delims =
   let
      info = map (infoFromString delims) strs
      tran = transpose1 info
      in
      case diffcheck tran 0 of
         Nothing -> filter (not . all (== "")) tran
         Just n -> let (a, b : bs) = splitAt (n + 1) delims in differences1 strs (a ++ bs)

diffcheck (c1 : c2 : cs) n =
   case M.toList $ counts1 $ zipWith (\e1 e2 -> (e1 == "", e2 == "")) c1 c2 of
      [((False, True), a), ((True, False), b)] -> Just n
      _ -> diffcheck (c2 : cs) (n + 1)
diffcheck _ _ = Nothing

differences2 :: [String] -> [[String]]
differences2 strs = let delims = commonSubsequencesList strs in map (infoFromString delims) strs

metaFromString :: [FrameID] -> [String] -> String -> Meta
metaFromString fields delims str = metaOfFrames2 False (c str) blankFT blankFT $ fieldsFromString fields delims str

fieldsFromString fields delims str = M.fromList $ zip fields $ map toDyn $ infoFromString delims str

infoFromString delims str = tail $ infoFromString1 delims str

infoFromString1 :: [String] -> String -> [String]
infoFromString1 _ [] = []
infoFromString1 [] str = [str]
infoFromString1 (delim : ds) str =
   let
      (word, rest) = split1With (stripPrefix delim) str
      in
      word : infoFromString1 ds rest

infoFromString2 _ [] = Just []
infoFromString2 [] str = Just [str]
infoFromString2 (delim : ds) str = case split1WithM (stripPrefix delim) str of
   Just (found, left) -> (found :) <$> infoFromString2 ds left
   Nothing -> Nothing

infoFromStringA delims str = let (found, left) = unzip $ zipWith split1 delims (str : left) in found

commonSubsequencesList [] = []
commonSubsequencesList ss = splitWith (stripPrefix "*") $ foldl1 (\a b -> intercalate "*" $ commonSubsequences a b) ss

{-
commonSubsequences2 a b  = let
      c  = map (\an -> map (commonSubsequences2A an) [0..length b]) [0..length a]
      commonSubsequences2A an bn
         | an == 0 || bn == 0 = ([], 0, 0, True)
         | otherwise          = let
            (as, al, aw, _ ) = c !! (an-1) !! bn
            (bs, bl, bw, _ ) = c !! an !! (bn-1)
            (cs, cl, cw, cp) = c !! (an-1) !! (bn-1)

            ac = a !! (an-1)
            bc = b !! (bn-1)
            in if ac == bc
                        then if cp
                           then (ac:cs, cl+1, cw, True)
                           else (ac:'*':cs, cl+1, cw+1, True)
                        else if al > bl || al == bl && aw < bw
                           then (as, al, aw, False)
                           else (bs, bl, bw, False)
      (res, _, _, _) = c !! length a !! length b
      in reverse res
-}
dbpath = baseDir @ "haskelldb.bin"

-- dbroots = map (baseDir ++) ["Artists/Paradise Lost/2015 - The Plague Within"]
dbroots = map (artistd @) ["Paradise Lost", "Isis"] :: [T.Text]

type DB = M.Map T.Text Meta
type FS = Meta

newDB :: DB
newDB = M.empty

loadDB :: IO DB
loadDB = read <$> readFile dbpath

saveDB :: DB -> IO ()
saveDB = writeFile dbpath . show

-- readDB1 = decodeFile dbpath

-- writeDB1 = encodeFile dbpath
updateDB :: M.Map T.Text Meta -> IO (M.Map T.Text Meta)
updateDB db = do
   dbnew <- updateDB1 db <$> readDB db dbroots
   putStrLn "Writing..."
   saveDB dbnew
   putStrLn "Done"
   return dbnew

readDB db roots = mapOfList <$> readDB1 db dbroots

readDB1 db roots = concat <$> mapM (\k -> readFS db $ fromMaybe (makeDir k) $ M.lookup k db) roots

mapOfList = M.fromList . mapfxx path

blankMeta =
   Meta
      { isDir = False
      , path = empty
      , audio = empty
      , byId = M.empty
      , artist = empty
      , album = empty
      , albumartist = empty
      , track = empty
      , song = empty
      , year = 0
      , genre = empty
      , times = FileTimes 0 0 0
      , orig = FileTimes 0 0 0
      }
makeDir k = blankMeta{isDir = True, path = k}

updateDB1 bypathold bypathnew =
   let
      -- join on pathnames
      (bypathmatch, bypathnewmiss, bypatholdmiss) = myJoinWith newdataoldtimes bypathnew bypathold
      -- now try and match by audio
      (byaudiomatch, byaudionewmiss, byaudiooldmiss) = myJoinWith newdataoldtimes (newkeys audio bypathnewmiss) (newkeys audio bypatholdmiss)
      in
      -- give up on the remaining unmatched and just add them
      -- leave out nmdirs

      -- we keep everything that matched one way or the other plus new stuff with missing / nonmatching audio
      bypathmatch `M.union` newkeys path byaudiomatch `M.union` bypathnewmiss `M.union` newkeys path byaudionewmiss

updateDBMerge db listnew = M.union (M.fromList $ mapfxx path listnew) db

myJoin new old = (new `M.intersection` old, new M.\\ old, old M.\\ new)
myJoinWith f new old = (M.intersectionWith f new old, new M.\\ old, old M.\\ new)

newkeys f m = M.fromList $ map (\(k, a) -> (f a, a)) $ M.toList m

newdataoldtimes new old = new{orig = orig old}

{-
picosecondsToDiffTime :: Integer -> DiffTime

diffTimeToPicoseconds :: DiffTime -> Integer
-}
c = convertString
readFS :: M.Map T.Text Meta -> Meta -> IO [Meta]
readFS db f
   | isDir f = do
         putStrLn ("DIR " ++ c (path f))
         let path1 = path f ++ "/"
         (times1, orig1) <- readFileTimes f -- otimes1 is only different for a new dir
         -- if field "FT.Current.Written" times1 > field "FT.Current.Written" f
         putStrLn "hello"
         if on (>) written times1 (times f)
            then do
               rdir <- listDirectory $ c path1
               let rdir1 = map c rdir
               rnew <- mapM (\p -> do d <- doesDirectoryExist $ c p; return $ blankMeta{isDir = d, path = p}) $ map (path1 ++) rdir1
               let withnew = M.union (inDir path1 db) $ M.fromList $ mapfxx path rnew
               updated <- mapM (readFS db) withnew -- recurse into subdirectories and files
               return $ f{times = times1} : concat updated -- otimes1 is only updated if it's a new dir
            else do
               putStrLn "UH OH"
               updated <- mapM (readFSD db) $ inDir path1 db -- recurse into subdirectories only
               return $ f{times = times1} : concat updated
   | otherwise = do
         putStrLn $ c $ path f
         let path1 = path f
         (times1, orig1) <- readFileTimes f
         -- if on (>) written times1 (times f)
         if ((>) `on` written) times1 (times f)
            -- if times1 `on (>) written` times f
            then do
               rfr <- readTagM (readSomeAudio 8) path1
               return [metaOfFrames False path1 times1 orig1 rfr]
            else
               return [f]

readFSD db f
   | isDir f = readFS db f
   | otherwise = return [f]

--writeFS f = writeTag (path f) $ framesOfMeta f

zeroTime = MyPretty2.DateTime (posixSecondsToUTCTime 0)

-- intOfFT (FILETIME ft) = fromIntegral ft
-- https://www.ubereats.com/gb/orders/d79a668a-365e-46fa-8d75-af17bd713bb0

inDir d =
   let
      l = length d
      in
      M.takeWhileAntitone (\x -> isPrefixOf d x && notElem '/' (drop l x)) . M.dropWhileAntitone (not . isPrefixOf d)

inDir1 d = M.takeWhileAntitone (all (notElem '/') . stripPrefix d) . M.dropWhileAntitone (not . isPrefixOf d)

inDirRec d = M.takeWhileAntitone (isPrefixOf d) . M.dropWhileAntitone (not . isPrefixOf d)

-- fti = posixSecondsToUTCTime
fti = picoOfPosixTime

{-}
deriving instance Generic Ue
deriving instance Generic Day
deriving instance Generic DiffTime
instance Binary UTCTime
instance Binary Day
instance Binary DiffTime
-}
picoOfPosixTime = nominalDiffTimeToSeconds
utcTimeOfPico = posixSecondsToUTCTime . secondsToNominalDiffTime

readFileTimes fs = do
   let path1 = path fs
   fst <- getFileStatus $ c path1
   let fadc = fti $ statusChangeTimeHiRes fst -- created
   let fadw = fti $ modificationTimeHiRes fst -- written
   let fada = fti $ accessTimeHiRes fst -- accessed
   now <- fti <$> getPOSIXTime
   let times1 = FileTimes fadc fadw fada
   -- notice this only set otimes1 to what it's just read if FT.Original.Written is 0
   -- ie. its a new file/dir
   let otimes1 = if written (orig fs) == 0 then FileTimes fadc fadw now else orig fs
   -- let otimes1 = if zeroTime == zeroTime then getFileTimes1 fadc fadw now "Original" else otimes fs
   return (times1, otimes1)

-- xylistFromFS = concatMap (\f -> map (\fr -> ((path f, frid fr), val fr)) f)

justText = filter (\case FText {} -> True; _ -> False)

--            return $ FrameMPEG version layer bitRate sampRate mpegFrameSize mpegFrameTime
{-
overall bitrate should be total number of bits divided by total time
overall samprate should be total number of samples divided by total time
overall framesize should be total number of bytes divided by number of frames? or just total bytes
overall frametime should be total time

we have a bunch of frames of possibly different sizes
multiply everything by what proportion of the file it represents
are we talking about time or bytes
-}
{-
unzip3 :: [(a,b,c)] -> ([a],[b],[c])
unzip3   =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs)) ([],[],[])
unzip4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4   =  foldr (\(a,b,c,d) ~(as,bs,cs,ds) -> (a:as,b:bs,c:cs,d:ds)) ([],[],[],[])
unzip5 :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5   =  foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) -> (a:as,b:bs,c:cs,d:ds,e:es)) ([],[],[],[],[])
unzip6 :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
unzip6   =  foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) -> (a:as,b:bs,c:cs,d:ds,e:es,f:fs)) ([],[],[],[],[],[])
-}
readTagM readAudio f1 = do
   print f1
   let f = convertString f1 :: String
   h <- openBinaryFile f ReadMode
   hdat <- B.hGet h 65536
   return $ parseTag hdat 
      {-
      _ -> do
         hClose h
         return []
         -}
{-
writeTag f1 t = do
   let f = convertString f1 :: String
   h <- openBinaryFile f ReadWriteMode
   d <- hGet h 10
   let Done rest _ hd = parse header d
   let ts = myget1B TagSize hd
   let FDone fd _ = unparseFrames t
   let padLen = ts - length fd
   if myget1B Id3 hd == "ID3"
      then
         if padLen >= 0
            then do
               hSeek h AbsoluteSeek 0
               hPut h $ unparseTag t
               hClose h
            else shiftAudio h fd $ fromIntegral $ myget1B TagSize hd + 10
      else shiftAudio h fd 0
where
   shiftAudio h fd pos = do
      putStrLn "shifting audio"
      fs <- hFileSize h
      hSeek h AbsoluteSeek pos
      audio <- hGet h $ fromIntegral $ fs - pos
      hSeek h AbsoluteSeek 0
      hPut h $ unparseTag1 (length fd * 2) fd
      hPut h audio
      hClose h
-}
{-
readTagM readAudio f1 = do
   print f1
   let f = convertString f1 :: String
   h <- openBinaryFile f ReadMode
   hdat <- replicateM 10 $ hGetChar h
   case parse header hdat of
      Done rest _ dyn ->
         let hd = fromDyn1 dyn :: M.Map Var Dynamic
         in if myget1 Id3 hd == "ID3"
                  then do
                     rest <- replicateM (myget1B TagSize hd) $ hGetChar h
                     let rest1 = if myget1B VerMajor hd == 3 && myget1B Unsync hd then resync1 rest else rest
                     m <- readAudio h $ myget1B TagSize hd
                     fileSize <- hFileSize h
                     hClose h
                     --            mapM_ print m
                     let t = parseFrames (myget1B VerMajor hd) rest1
                     let audioSize = fromInteger fileSize - myget1B TagSize hd - 10
                     return $ hd : t -- ++ combineMPEGFrames audioSize (getMPEGFrames m)
                  else do
                     hClose h
                     return []
      _ -> do
         hClose h
         return []
writeTag f1 t = do
   let f = convertString f1 :: String
   h <- openBinaryFile f ReadWriteMode
   d <- hGet h 10
   let Done rest _ hd = parse header d
   let ts = myget1B TagSize hd
   let FDone fd _ = unparseFrames t
   let padLen = ts - length fd
   if myget1B Id3 hd == "ID3"
      then
         if padLen >= 0
            then do
               hSeek h AbsoluteSeek 0
               hPut h $ unparseTag1 ts fd
               hClose h
            else shiftAudio h fd $ fromIntegral $ myget1B TagSize hd + 10
      else shiftAudio h fd 0
where
   shiftAudio h fd pos = do
      putStrLn "shifting audio"
      fs <- hFileSize h
      hSeek h AbsoluteSeek pos
      audio <- hGet h $ fromIntegral $ fs - pos
      hSeek h AbsoluteSeek 0
      hPut h $ unparseTag1 (length fd * 2) fd
      hPut h audio
      hClose h
-}

-- format the frame data for writing. this has to be different to the parseTag method
-- because of having to move the audio
readAllAudio h ts = do
   fs <- fromIntegral <$> hFileSize h
   let as = fs - ts - 10
   audio <- hGet h as
   return $ parseMPEGFrame audio
{-
readAllAudio1 h ts = do
   fs <- fromIntegral <$> hFileSize h
   let as = fs - ts - 10
   audio <- hGet h as
   case parse mpegFramesP audio of
      Done _ _ fr -> return fr
      Fail i cs e -> error e
-}
readNoAudio h ts = return []

readSomeAudio nf h ts = do
   fs <- fromIntegral <$> hFileSize h
   let as = fs - ts - 10
   mapM (\n -> readSomeAudio1 h $ ts + 10 + n * (fs - ts - 10) `div` nf) [1 .. (nf - 1)]

hGet h n = replicateM n $ hGetChar h
hPut h s = hPutStr h s

readSomeAudio1 h pos = do
   hSeek h AbsoluteSeek $ fromIntegral $ pos - maxFrameSize
   audio <- hGet h $ maxFrameSize * 2
   case parse (rep mpegFrameP 8) audio of
      -- Left l        -> error ++ show l
      -- Right r       -> return r
      Done m _ _ -> return m

middle l = l !! (length l `div` 2)

getMPEGFrame fs = head $ getMPEGFrames fs

emptyMPEGFrame = MPEGFrame 0 0 0 0 0 0 BString.empty

{-
convertMPEGFrame f = [
      FrameText "MPEGVersion" $ Int1 $ version f,
      FrameText "MPEGLayer"   $ Int1 $ layer f,
      FrameText "MPEGBits"    $ Int1 $ framebits f,
      FrameText "MPEGSamples" $ Int1 $ framesamps f,
      FrameText "MPEGBytes"   $ Int1 $ mpegFrameSize f,
      FrameText "MPEGTime"    $ Int1 $ mpegFrameTime f]
-}
-- combineMPEGFrames1 :: Int -> [Frame] -> Frame
combineMPEGFrames totalBytes [] = []
combineMPEGFrames totalBytes frs =
   let
      readBytes = sum        $ map mpegFrameBytes frs
      readTime  = sum        $ map mpegFrameTime  frs
      readSamps = sum        $ map framesamps     frs
      mVersion  = mode       $ map version        frs
      mLayer    = mode       $ map layer          frs
      sampRate  = round      $ realToFrac   readBytes      / realToFrac   readSamps
      bitRate   = round      $ realToFrac   readBytes  * 8 / realToFrac   readTime
      totalTime = realToFrac $ fromIntegral totalBytes * 8 / fromIntegral bitRate / 1000
      in
      [MPEGFrame mVersion mLayer bitRate sampRate totalBytes totalTime empty]

{-
[ FrameText "MPEG Ver" $ c $ show $ mode $ map version frs
, FrameText "MPEG Layer" $ c $ show $ mode $ map layer frs
, FrameText "Bitrate" $ c $ show $ round bitRate
, FrameText "Audio bytes" $ c $ show totalBytes
, FrameText "Time" $ c $ show $ realToFrac totalTime
, FrameText "Sample rate" $ c $ show $ round sampRate
]
-}

getMPEGFrames = filter (\case MPEGFrame{} -> True; _ -> False)

{-
combineMPEGFrames2 fs []  = emptyMPEGFrame
combineMPEGFrames2 fs frs = let
      --uz = unzip7 $ map (applyU7 (version, layer, framebits, framesamps, mpegFrameSize, mpegFrameTime, mpegFrameAudio)) frs
      --(mVersion, mLayer, sBits, sSamples, sBytes, sTime, mAudio) = applyT7 (mode, mode, sum, sum, sum, sum, middle) uz
      --(mVersion, mLayer, sBits, sSamples, sBytes, sTime, mAudio)
      -- = applyT7 (mode, mode, sum, sum, sum, sum, middle) $ unzip7 $ map (applyU7 (version, layer, framebits, framesamps, mpegFrameSize, mpegFrameTime, mpegFrameAudio)) frs
      --{-
      mVersion = mode   $ map version        frs
      mLayer   = mode   $ map layer          frs
      sBits    = sum    $ map framebits      frs
      sSamples = sum    $ map framesamps     frs
      sBytes   = sum    $ map mpegFrameSize  frs
      sTime    = sum    $ map mpegFrameTime  frs
      mAudio   = middle $ map mpegFrameAudio frs
      ---}
      in
         FrameMPEG mVersion mLayer (sBytes*8000`div`sTime) (sSamples`div`sTime) sBytes (sTime*fs`div`sBytes) mAudio

framebits fr = bitRate fr * mpegFrameTime fr

combineMPEGFrames3 fs = combineMPEGFrames2 fs . filter (\case { FrameMPEG {} -> True; _ -> False })
-}
framesamps fr = fromIntegral (sampRate fr) * mpegFrameTime fr

myget1A :: FrameID -> M.Map FrameID Dynamic -> T.Text
myget1A id framemap = c (myget1B id framemap :: String)

myget1B id framemap = fromDyn1 $ fromJust $ M.lookup id framemap 

myget1C :: FrameID -> [M.Map Var Dynamic] -> Maybe (M.Map Var Dynamic)
myget1C id frames = Data.List.find (\f -> isJust $ do
   d <- M.lookup FrameID f
   s <- fromDynamic d
   j <- M.lookup s textIDMap
   guard $ id == id1 j) frames

metaOfFrames isDir1 path1 times1 orig1 = metaOfFrames2 isDir1 path1 times1 orig1 . metaOfFrames1

metaOfFrames1 tag = M.fromList $ map (\frame -> (frameID frame, P.mygetD Value frame)) $ frames tag


metaOfFrames2 isDir1 path1 times1 orig1 byId1 =
      Meta
         { isDir       = isDir1
         , path        = path1
         , audio       = ""
         , albumartist =               myget1A AlbumArtist byId1
         , artist      =               myget1A Artist      byId1
         , album       =               myget1A Album       byId1
         , track       =               myget1A Track       byId1
         , song        =               myget1A Song        byId1
         , year        = readInt $ c $ myget1A Year        byId1
         , genre       =               myget1A Genre       byId1
         , times       = times1
         , orig        = orig1
         , byId        = byId1
         }

{-
decodeText = decodeText1 . map fromIntegral . B.unpack

decodeText1 (0 : iso8859) = map chr iso8859
decodeText1 (1 : ucs2) = decodeUCS2 ucs2
decodeText1 x = []

decodeUCS2 (255 : 254 : rest) = decodeLE rest
decodeUCS2 (254 : 255 : rest) = decodeBE rest
decodeUCS2 str =
   let ez = countZero $ alternateChars str
         oz = countZero $ alternateChars $ tail str
      in if ez >= oz then decodeBE str else decodeLE str

decodeLE [] = []
decodeLE (a : b : rest) = chr (a + b * 0x100) : decodeLE rest
decodeBE [] = []
decodeBE (a : b : rest) = chr (a * 0x100 + b) : decodeBE rest
-}
itext enc = total (decodeText enc) (encodeText enc)

decodeText enc text = case enc of
   0 -> T.decodeLatin1 text
   1 -> decodeUCS2     text

encodeText enc text = case enc of
   0 -> T.encodeUtf8 text
   1 -> T.encodeUtf16LE text



decodeUCS2 str = case B.take 2 str of
   "\255\254" -> T.decodeUtf16LE $ B.drop 2 str
   "\254\255" -> T.decodeUtf16BE $ B.drop 2 str
   _ -> if countZeroAlt 0 str >= countZeroAlt 1 str then T.decodeUtf16BE str else T.decodeUtf16LE str

countZeroAlt off s = length $ filter (/= 0) $ map (s !!) [off .. length s - 1]

countZero = length . filter (== 0)

alternateChars [] = []
alternateChars [a] = [a]
alternateChars (a : b : rest) = a : alternateChars rest

encodeTextA = B.pack . map fromIntegral . encodeText1

encodeText1 x = 0 : map ord x

readFileU2 = unsafePerformIO . readFile

readTag = unsafePerformIO . readTag2
readTag2 f = parseTag <$> B.readFile f

unright (Right r) = r

parseTag str =
   let 
      Done rest1 _ hd = parse header str
      rest2 = take (tagSize hd) rest1
      rest = if verMajor hd == (3::Int) && unsync hd then resync rest2 else rest2
      in case id3 hd of
            "ID3" -> hd { frames = result $ parse body rest }
            _ -> hd

-- add the header onto the frame data
unparseTag :: ID3Tag -> IOString
unparseTag tg = 
   case P.format tag tg of
      FDone t _ -> t
      FFail t _ em -> error em

{-
unparseTag1 totalSize framedat =
   let
      frLen = length framedat
      padLen = totalSize - frLen
      padding = replicate padLen ' '
      in
      if padLen < 0
         then error "unparseTag1 called with totalSize less than frLen!"
         else let
            FDone r _ = P.format header hd
            in concat
               [ r
               , framedat
               , padding
               ]
-}

isValidID :: String -> Bool
isValidID = all (\c -> isDigit c || isAsciiUpper c)

resync = B.pack . resync1 . B.unpack
desync = B.pack . unsync1 . B.unpack

resync1 [] = []
resync1 [a] = [a]
resync1 (255 : 0 : rest) = 255 : resync1 rest
resync1 (a : rest) = a : resync1 rest

unsync1 [] = []
unsync1 (255 : rest) = 255 : 0 : unsync1 rest
unsync1 (a : rest) = a : unsync1 rest

resync2 [] = []
resync2 [a] = [a]
resync2 ('\255' : '\000' : rest) = '\255' : resync1 rest
resync2 (a : rest) = a : resync1 rest

unsync2 [] = []
unsync2 ('\255' : rest) = '\255' : '\0' : unsync1 rest
unsync2 (a : rest) = a : unsync1 rest

isync1 True  = total resync desync
isync1 False = total id id

--unparseFrames frs = B.concat $ map unframe frs

emptyTag :: ID3Tag
emptyTag = undefined
{-
tag :: Rule IOString Word8 () ID3Tag
tag = Build emptyTag (
   Id3K      <-- rep char 3 :/
   VerMajorK <-- int :/
   VerMinorK <-- int :/
   SetM (UnsyncK :- ExtHdrK :- ExperiK :- FooterK :- ZK :- ZK :- ZK :- ZK) (Apply tuple8 $ bits 8) :/
   TagSizeK  <-- int4 0x80 :/
   Anything 
      (\f i -> case parse1 (Get TagSizeK >>== tokens) f i of
         Done i1 f1 r1 -> parse1 (FramesK <-- Many frame) f1 r1) 
      (\f r -> case Parser6.format1 (FramesK <-- Many frame) f r of
         FDone t1 f1 -> Parser6.format1 (Get TagSizeK) f1 (length t1)))
-}
header :: Rule IOString Word8 () ID3Tag
header = Build emptyTag (
   Id3K      <-- rep char 3 :/
   VerMajorK <-- int :/
   VerMinorK <-- int :/
   SetM (UnsyncK :- ExtHdrK :- ExperiK :- FooterK :- ZK :- ZK :- ZK :- ZK) (Apply tuple8 $ bits 8) :/
   TagSizeK  <-- int4 0x80 )

body = Many frame

{-
let unsync = flags .&. 0x80 /= 0
let extHdr = flags .&. 0x40 /= 0 -- would mean compression in v2.2 if they finished it
let experi = flags .&. 0x20 /= 0 -- v2.3 & 2.4
let footer = flags .&. 0x10 /= 0 -- v2.4 only
-}

emptyFrame :: Frame
emptyFrame = undefined

frame = frameheader

frameheader :: Int -> Rule IOString Word8 x Frame
frameheader 2 =
   Build emptyFrame (
      FrameIDK    <-- rep char 3 :/
      FrameSizeK  <-- int4 0x100 :/
      FrameBytesK <-- (Get FrameSizeK >>== tokens))

frameheader 3 =
   Build emptyFrame (
      FrameIDK    <-- rep char 4 :/
      FrameSizeK  <-- int4 0x100 :/
      SetM (TagAltPrsvK :- FileAltPrsvK :- ReadOnlyK :- ZK :- ZK :- ZK :- ZK :- ZK) (Apply tuple8 $ bits 8) :/
      SetM (CompressionK :- EncryptionK :- GroupingK :- ZK :- ZK :- ZK :- ZK :- ZK) (Apply tuple8 $ bits 8) :/
      FrameBytesK <-- (Get FrameSizeK >>== tokens))

frameheader 4 =
   Build emptyFrame (
      FrameIDK    <-- rep char 4 :/
      FrameSizeK  <-- int4 0x80 :/
      SetM (ZK :- TagAltPrsvK :- FileAltPrsvK :- ReadOnlyK :- ZK :- ZK :- ZK :- ZK :- ()) (Apply tuple8 (bits 8)) :/
      SetM (ZK :- GroupingK :- ZK :- ZK :- CompressionK :- EncryptionK :- UnsyncFrK :- DataLenIK :- ()) (Apply tuple8 (bits 8)) :/
      FrameBytesK <-- (Get FrameSizeK >>== tokens))
      

textEnc     = TextEncodingK --> (\enc -> Apply (itext enc) Rest)

zeroTextEnc = TextEncodingK --> (\enc -> Apply (itext enc) $ case enc of
                                                      0 -> zeroText
                                                      1 -> AnyTill $ String "\0\0")

zeroText = AnyTill $ Token '\0'



framebody = \case
   T frameID2 -> Build (FText { }) (
      TextEncodingK <-- int :/
      ValueK        <-- textEnc)

   Txxx -> Build (FUserText { }) (
      TextEncodingK <-- int :/
      DescriptionK  <-- zeroTextEnc :/
      ValueK        <-- textEnc)

   Uslt -> Build (FUnsyncLyrics { }) (
      TextEncodingK <-- int :/
      LanguageK     <-- rep char 3 :/
      DescriptionK  <-- zeroTextEnc :/
      LyricsK       <-- textEnc)

   Apic -> Build (FPicture { }) (
      TextEncodingK <-- int :/
      MIMETypeK     <-- zeroText :/
      PictureTypeK  <-- int :/
      DescriptionK  <-- zeroTextEnc :/
      PictureK      <-- Rest)

   Sylt -> Build (FSyncLyrics { }) (
      TextEncodingK <-- int :/
      LanguageK     <-- rep int 3 :/
      TimeFormatK   <-- int :/
      ContentTypeK  <-- int :/
      SyncedLyricsK <-- Many (zeroTextEnc :+ int4 0x100))

{-
   | toLower (head textID) == 't' = Build (FText { frameID = id1 $ fromJust $ M.lookup textID textIDMap }) (
   | low textID == "txxx" = Build (FUserText { }) (
   | low textID == "uslt" = Build (FUnsyncLyrics { }) (
   | low textID == "apic" = Build (FPicture { }) (
   | low textID == "sylt" = Build (FSyncLyrics { }) (
-}
      --rest = if myget1C UnsyncFr then resync1 rest1 else rest1
   --"flags" <-- GetM ["tagAltPrsv" "fileAltPrsv" "readOnly" "compression" "encryption" "grouping" "unsyncFr" "dataLenI"]]
   --let dat1 = if unsyncFr then resync dat else dat

parseMPEGFrame audio = do
   case parse mpegFrameP audio of
      Done i _ r -> r : parseMPEGFrame i
      --AP.Partial c -> []

mpegFrameP = AnyTill mpegFrameOK

snarf [] = []
snarf (Invalid a : Invalid b : xs) = snarf $ Invalid (B.append a b) : xs
snarf (x : xs) = x : snarf xs

invalid = ManyTill char (Token (chr 0xFF))

data MPEGVersion = MPEGV1 | MPEGV2 | MPEGV2_5

mpegv1 = 1

mpegv2 = 2

mpegv2_5 = 3

mpeg12Layer1 =  [0,  32,  64,  96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448,   0] -- Layer 1
mpeg12Layer2 =  [0,  32,  48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320, 384,   0] -- Layer 2
mpeg12Layer3 =  [0,  32,  40,  48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320,   0] -- Layer 3
mpeg25Layer1 =  [0,  32,  48,  56,  64,  80,  96, 112, 128, 144, 160, 176, 192, 224, 256,   0] -- MPEG2.5 Layer 1
mpeg25Layer23 = [0,   8,  16,  24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160,   0] -- MPEG2.5 Layer 2/3
allZeros =      [0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0]

bitRates =       [[0,  32,  64,  96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448,   0], -- Layer 1
                           [0,  32,  48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320, 384,   0], -- Layer 2
                           [0,  32,  40,  48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320,   0], -- Layer 3
                           [0,  32,  48,  56,  64,  80,  96, 112, 128, 144, 160, 176, 192, 224, 256,   0], -- MPEG2.5 Layer 1
                           [0,   8,  16,  24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160,   0], -- MPEG2.5 Layer 2/3
                           [0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0]]

sampRates =      [[0, 44100, 22050, 11025],
                           [0, 48000, 24000, 12000],
                           [0, 32000, 16000,  8000],
                           [0,     0,     0,     0]]
    
--          !! sampRateEnc
--          !! version

sampPerFrames =  [[0,    0,    0,    0],
                           [0,  384,  384,  384],
                           [0, 1152, 1152, 1152],
                           [0, 1152,  576,  576]]
          

mpegFrameOK = Build emptyMPEGFrame (
   --AP.word8 0xFF
   SetM (AllOnes2K :- AllOnes1K :- AllOnes0K :- VersionEnc1K :- VersionEnc0K :- LayerEnc1K :- LayerEnc0K :- ZK :- BitRateEnc3K :- BitRateEnc2K :- BitRateEnc1K :- BitRateEnc0K :- SampRateEnc1K :- SampRateEnc0K :- ZK :- ChannelMode1K :- ChannelMode0K) (int3 0x100) :/
   AllOnesK     <-- Apply (inverse $ ibits 3) (GetM (AllOnes2K :- AllOnes1K :- AllOnes0K)) :/
   VersionEncK  <-- Apply (inverse $ ibits 2) (GetM (VersionEnc1K :- VersionEnc0K)) :/
   LayerEncK    <-- Apply (inverse $ ibits 2) (GetM (LayerEnc1K :- LayerEnc0K)) :/
   BitRateEncK  <-- Apply (inverse $ ibits 4) (GetM (BitRateEnc3K :- BitRateEnc2K :- BitRateEnc1K :- BitRateEnc0K)) :/
   SampRateEncK <-- Apply (inverse $ ibits 2) (GetM (SampRateEnc1K :- SampRateEnc0K)) :/
   ChannelModeK <-- Apply (inverse $ ibits 2) (GetM (ChannelMode1K :- ChannelMode0K)))
{-
mpegFrameF = do
   let version = [mpegv2_5, 0, mpegv2, mpegv1] !! versionEnc

   let layer = [0, 3, 2, 1] !! layerEnc

   let bitRate =
            ( if version == mpegv1
                  then [allZeros, mpeg12Layer1, mpeg12Layer2 , mpeg12Layer3 ]
                  else [allZeros, mpeg25Layer1, mpeg25Layer23, mpeg25Layer23]
            )
               !! layer
               !! bitRateEnc

   let sampRate =
            [ [    0,     0,     0, 0]
            , [44100, 48000, 32000, 0]
            , [22050, 24000, 16000, 0]
            , [11025, 12000,  8000, 0]
            ]
               !! version
               !! sampRateEnc

   let sampPerFrame =
            [ [0,    0,    0,    0]
            , [0,  384,  384,  384]
            , [0, 1152, 1152, 1152]
            , [0, 1152,  576,  576]
            ]
               !! layer
               !! version

   let bitsPerSamp = fromIntegral sampPerFrame / 8

   let slotSize = [0, 4, 1, 1] !! layer

   let mpegFrameBytes =
            floor (bitsPerSamp * fromIntegral bitRate * 1000 / fromIntegral sampRate)
               + if padding /= 0 then slotSize else 0

   let mpegFrameTime =
            realToFrac
               (fromIntegral mpegFrameBytes * 8 / fromIntegral bitRate)

   if allOnes == 0x7 && version > 0 && layer > 0 && bitRate > 0 && sampRate > 0
      then do
         audio <- AP.take (mpegFrameBytes - 4)
         return $ MPEGFrame version layer bitRate sampRate mpegFrameBytes mpegFrameTime audio
      else fail "not a valid mpeg frame"
-}
maxFrameSize = 5764 -- 1152 / 8 * 320 * 1000 / 8000 + 4

ibsofs = total bsofs sofbs

tuple8 = Iso (\[a, b, c, d, e, f, g, h] -> Just (a :- b :- c :- d :- e :- f :- g :- h :- ())) (\(a :- b :- c :- d :- e :- f :- g :- h :- ()) -> Just [a, b, c, d, e, f, g, h])

bits n = Apply (ibits n) int

ibits n = total (dobits n) undobits

dobits :: Int -> Int -> [Bool]
dobits n = take n . map odd . iterate (`shift` (-1))

-- curry $ unfoldr (\(bn, bx) -> ifJust (bn > 0) (bx .&. 1 /= 0, (bn-1, shift bx (-1))))

undobits :: [Bool] -> Int
undobits = foldl (\a x -> shift a 1 .|. (if x then 1 else 0)) 0

intn n m = Apply (total (dointn m) (undoint n m)) $ Count n int

dointn :: Int -> [Int] -> Int
dointn m = foldl (\a b -> a * m + b) 0

int = Apply (total fromIntegral fromIntegral) AnyToken
--int = fromIntegral <$> AP.anyWord8

--int2 m = Seq [int, int]

--int2 = intn 2
int3 = intn 3
int4 = intn 4

undoint 0 _ _ = []
undoint n m x =
   let (q, r) = divMod x (m ^ (n - 1))
      
   in q : undoint (n - 1) m r

rep p n = Seq $ replicate n p

--char = chr <$> int

char = AnyToken

-- char = AP.anyWord8

-- intn 0 _ = int
-- intn n m = do a <- int; b <- intn (n-1) m; return $ a*m^(n-1) + b
-- intn n m = do a <- intn (n-1) m; b <- int; return $ a*m + b
{-
data FS  = Dir  { dpath :: String, objs :: M.Map String FS, dTimes :: FileTime, doTimes :: FileTime }
            | File { fpath :: String, frames :: [Frame], fTimes :: FileTime, foTimes :: FileTime }
            deriving (Generic)
-}


data Var = 
   Id3          |
   VerMajor     |
   VerMinor     |
   Unsync       |
   ExtHdr       |
   Experi       |
   Footer       |
   TagSize      |
   Frames       |
   FrameID      |
   FrameSize    | 
   TagAltPrsv   |  
   FileAltPrsv  |   
   ReadOnly     |
   Grouping     |
   Compression  |   
   Encryption   |  
   UnsyncFr     |
   DataLenI     |
   Z            |   
   Dat          |      
   TagBytes     |
   FrameBytes   |
   Value        |
   TextEncoding |
   Language     |
   Description  |
   Lyrics       |
   MIMEType     |
   PictureType  |
   Picture      |
   TimeFormat   |
   ContentType  |
   SyncedLyrics |
   AllOnes2     |
   AllOnes1     |
   AllOnes0     |
   VersionEnc1  |   
   VersionEnc0  |   
   LayerEnc1    | 
   LayerEnc0    | 
   BitRateEnc3  |   
   BitRateEnc2  |   
   BitRateEnc1  |   
   BitRateEnc0  |   
   SampRateEnc1 |    
   SampRateEnc0 |    
   Padding      |
   ChannelMode1 |    
   ChannelMode0 |    
   AllOnes      |
   VersionEnc   |  
   LayerEnc     |
   BitRateEnc   |  
   SampRateEnc  |        
   ChannelMode  deriving (Eq, Ord, Show)        

data Id3K          = Id3K          deriving (Eq, Ord, Show)       
data VerMajorK     = VerMajorK     deriving (Eq, Ord, Show)     
data VerMinorK     = VerMinorK     deriving (Eq, Ord, Show)     
data UnsyncK       = UnsyncK       deriving (Eq, Ord, Show)   
data ExtHdrK       = ExtHdrK       deriving (Eq, Ord, Show)   
data ExperiK       = ExperiK       deriving (Eq, Ord, Show)   
data FooterK       = FooterK       deriving (Eq, Ord, Show)   
data TagSizeK      = TagSizeK      deriving (Eq, Ord, Show)    
data FramesK       = FramesK       deriving (Eq, Ord, Show)   
data FrameIDK      = FrameIDK      deriving (Eq, Ord, Show)    
data FrameSizeK    = FrameSizeK    deriving (Eq, Ord, Show)      
data TagAltPrsvK   = TagAltPrsvK   deriving (Eq, Ord, Show)       
data FileAltPrsvK  = FileAltPrsvK  deriving (Eq, Ord, Show)        
data ReadOnlyK     = ReadOnlyK     deriving (Eq, Ord, Show)     
data GroupingK     = GroupingK     deriving (Eq, Ord, Show)     
data CompressionK  = CompressionK  deriving (Eq, Ord, Show)        
data EncryptionK   = EncryptionK   deriving (Eq, Ord, Show)       
data UnsyncFrK     = UnsyncFrK     deriving (Eq, Ord, Show)     
data DataLenIK     = DataLenIK     deriving (Eq, Ord, Show)     
data ZK            = ZK            deriving (Eq, Ord, Show)        
data DatK          = DatK          deriving (Eq, Ord, Show)
data TagBytesK     = TagBytesK     deriving (Eq, Ord, Show)           
data FrameBytesK   = FrameBytesK   deriving (Eq, Ord, Show)
data ValueK        = ValueK        deriving (Eq, Ord, Show)
data TextEncodingK = TextEncodingK deriving (Eq, Ord, Show)
data LanguageK     = LanguageK     deriving (Eq, Ord, Show)
data DescriptionK  = DescriptionK  deriving (Eq, Ord, Show)
data LyricsK       = LyricsK       deriving (Eq, Ord, Show)
data MIMETypeK     = MIMETypeK     deriving (Eq, Ord, Show)
data PictureTypeK  = PictureTypeK  deriving (Eq, Ord, Show)
data PictureK      = PictureK      deriving (Eq, Ord, Show)
data TimeFormatK   = TimeFormatK   deriving (Eq, Ord, Show)
data ContentTypeK  = ContentTypeK  deriving (Eq, Ord, Show)
data SyncedLyricsK = SyncedLyricsK deriving (Eq, Ord, Show)
data AllOnes2K     = AllOnes2K     deriving (Eq, Ord, Show)     
data AllOnes1K     = AllOnes1K     deriving (Eq, Ord, Show)     
data AllOnes0K     = AllOnes0K     deriving (Eq, Ord, Show)     
data VersionEnc1K  = VersionEnc1K  deriving (Eq, Ord, Show)        
data VersionEnc0K  = VersionEnc0K  deriving (Eq, Ord, Show)        
data LayerEnc1K    = LayerEnc1K    deriving (Eq, Ord, Show)      
data LayerEnc0K    = LayerEnc0K    deriving (Eq, Ord, Show)      
data BitRateEnc3K  = BitRateEnc3K  deriving (Eq, Ord, Show)        
data BitRateEnc2K  = BitRateEnc2K  deriving (Eq, Ord, Show)        
data BitRateEnc1K  = BitRateEnc1K  deriving (Eq, Ord, Show)        
data BitRateEnc0K  = BitRateEnc0K  deriving (Eq, Ord, Show)        
data SampRateEnc1K = SampRateEnc1K deriving (Eq, Ord, Show)         
data SampRateEnc0K = SampRateEnc0K deriving (Eq, Ord, Show)         
data PaddingK      = PaddingK      deriving (Eq, Ord, Show)    
data ChannelMode1K = ChannelMode1K deriving (Eq, Ord, Show)         
data ChannelMode0K = ChannelMode0K deriving (Eq, Ord, Show)         
data AllOnesK      = AllOnesK      deriving (Eq, Ord, Show)    
data VersionEncK   = VersionEncK   deriving (Eq, Ord, Show)       
data LayerEncK     = LayerEncK     deriving (Eq, Ord, Show)     
data BitRateEncK   = BitRateEncK   deriving (Eq, Ord, Show)       
data SampRateEncK  = SampRateEncK  deriving (Eq, Ord, Show)        
data ChannelModeK  = ChannelModeK  deriving (Eq, Ord, Show)        

instance P.Frame Id3K MyString ID3Tag where
      myget1 Id3K = id3
      myset1 Id3K value frame = frame { id3 = value }

instance P.Frame VerMajorK Int ID3Tag where
      myget1 VerMajorK = verMajor
      myset1 VerMajorK value frame = frame { verMajor = value }

instance P.Frame VerMinorK Int ID3Tag where
      myget1 VerMinorK = verMinor
      myset1 VerMinorK value frame = frame { verMinor = value }

instance P.Frame UnsyncK Bool ID3Tag where
      myget1 UnsyncK = unsync
      myset1 UnsyncK value frame = frame { unsync = value }

instance P.Frame ExtHdrK Bool ID3Tag where
      myget1 ExtHdrK = extHdr
      myset1 ExtHdrK value frame = frame { extHdr = value }

instance P.Frame ExperiK Bool ID3Tag where
      myget1 ExperiK = experi
      myset1 ExperiK value frame = frame { experi = value }

instance P.Frame FooterK Bool ID3Tag where
      myget1 FooterK = footer
      myset1 FooterK value frame = frame { footer = value }

instance P.Frame TagSizeK Int ID3Tag where
      myget1 TagSizeK = tagSize
      myset1 TagSizeK value frame = frame { tagSize = value }

instance P.Frame TagBytesK B.ByteString ID3Tag where
      myget1 TagBytesK = tagBytes
      myset1 TagBytesK value frame = frame { tagBytes = value }

instance P.FrameD Var ID3Tag where
      mygetD name frame = case name of
         Id3       ->  toDyn $ id3       frame
         VerMajor  ->  toDyn $ verMajor  frame
         VerMinor  ->  toDyn $ verMinor  frame
         Unsync    ->  toDyn $ unsync    frame
         ExtHdr    ->  toDyn $ extHdr    frame
         Experi    ->  toDyn $ experi    frame
         Footer    ->  toDyn $ footer    frame
         TagSize   ->  toDyn $ tagSize   frame
         TagBytes  ->  toDyn $ tagBytes  frame
         Frames    ->  toDyn $ frames    frame
      mysetD name value frame = case name of
         Id3       ->  frame { id3      = fromDyn1 value }
         VerMajor  ->  frame { verMajor = fromDyn1 value }
         VerMinor  ->  frame { verMinor = fromDyn1 value }
         Unsync    ->  frame { unsync   = fromDyn1 value }
         ExtHdr    ->  frame { extHdr   = fromDyn1 value }
         Experi    ->  frame { experi   = fromDyn1 value }
         Footer    ->  frame { footer   = fromDyn1 value }
         TagSize   ->  frame { tagSize  = fromDyn1 value }
         TagBytes  ->  frame { tagBytes = fromDyn1 value }

instance P.FrameD Var FrameHeader where
   mygetD name frame = case name of
      FrameID     -> toDyn $ frameID1    frame 
      FrameSize   -> toDyn $ frameSize   frame
      TagAltPrsv  -> toDyn $ tagAltPrsv  frame 
      FileAltPrsv -> toDyn $ fileAltPrsv frame 
      ReadOnly    -> toDyn $ readOnly    frame 
      Compression -> toDyn $ compression frame 
      Encryption  -> toDyn $ encryption  frame 
      Grouping    -> toDyn $ grouping    frame 
      UnsyncFr    -> toDyn $ unsyncFr    frame 
      DataLenI    -> toDyn $ dataLenI    frame 
      FrameBytes  -> toDyn $ frameBytes  frame
   mysetD name value frame = case name of
      FrameID     -> frame { frameID1    = fromDyn1 value } 
      FrameSize   -> frame { frameSize   = fromDyn1 value }
      TagAltPrsv  -> frame { tagAltPrsv  = fromDyn1 value } 
      FileAltPrsv -> frame { fileAltPrsv = fromDyn1 value } 
      ReadOnly    -> frame { readOnly    = fromDyn1 value } 
      Compression -> frame { compression = fromDyn1 value } 
      Encryption  -> frame { encryption  = fromDyn1 value } 
      Grouping    -> frame { grouping    = fromDyn1 value } 
      UnsyncFr    -> frame { unsyncFr    = fromDyn1 value } 
      DataLenI    -> frame { dataLenI    = fromDyn1 value } 
      FrameBytes  -> frame { frameBytes  = fromDyn1 value }


instance P.FrameD Var Frame where
   mygetD name frame = case name of
      Value        -> toDyn $ value        frame
      TextEncoding -> toDyn $ textEncoding frame
      Language     -> toDyn $ language     frame
      Description  -> toDyn $ description  frame
      MIMEType     -> toDyn $ mimeType     frame
      PictureType  -> toDyn $ pictureType  frame
      Picture      -> toDyn $ picture      frame
      TimeFormat   -> toDyn $ timeFormat   frame
      ContentType  -> toDyn $ contentType  frame
      SyncedLyrics -> toDyn $ syncedLyrics frame

   mysetD name value1 frame = case name of
      Value        -> frame { value        = fromDyn1 value1 }
      TextEncoding -> frame { textEncoding = fromDyn1 value1 }
      Language     -> frame { language     = fromDyn1 value1 }
      Description  -> frame { description  = fromDyn1 value1 }
      MIMEType     -> frame { mimeType     = fromDyn1 value1 }
      PictureType  -> frame { pictureType  = fromDyn1 value1 }
      Picture      -> frame { picture      = fromDyn1 value1 }
      TimeFormat   -> frame { timeFormat   = fromDyn1 value1 }
      ContentType  -> frame { contentType  = fromDyn1 value1 }
      SyncedLyrics -> frame { syncedLyrics = fromDyn1 value1 }

textIDMap = M.fromList $ mapfxx textId frameIDList

idMap     = M.fromList $ mapfxx id1    frameIDList

descOfTextID textId = fromMaybe (error "textId " ++ textId ++ " not found") $ M.lookup textId descOfTextIDMap

descOfTextIDMap = M.fromList $ map (applyT (textId, desc)) frameIDList

--frameIDMap = M.fromList $ map (applyT (textId

data FrameIDEntry = FT { sec :: T.Text, id1 :: FrameID, textId :: T.Text, desc :: T.Text, longdesc :: T.Text }

data FrameID = T FrameID | Baudio | Bpath | Bisdir | Btimes | Borig | Aenc | Apic | Comm | Comr | Encr | Equa | Etco | Geob | Grid | Ipls | Link | Mcdi | Mllt | Owne | Priv | Pcnt | Popm | Poss | Rbuf | Rvad | Rvrb | Sylt | Sytc | Album | Tbpm | Tcom | Genre | Tcop | Tdat | Tdly | Tenc | Text | Tflt | Time | Tit1 | Song | Tit3 | Tkey | Tlan | Tlen | Tmed | Toal | Tofn | Toly | Tope | Tory | Town | Artist | AlbumArtist | Tpe3 | Tpe4 | Tpos | Tpub | Track | Trda | Trsn | Trso | Tsiz | Tsrc | Tsse | Year | Txxx | Ufid | User | Uslt | Wcom | Wcop | Woaf | Woar | Woas | Wors | Wpay | Wpub | Wxxx | Atxt | Chap | Ctoc | Rgad | Tcmp | Tso2 | Tsoc | Xrva | Ntrk | Aspi | Equ2 | Rva2 | Seek | Sign | Tden | Tdor | Tdrc | Tdrl | Tdtg | Tipl | Tmcl | Tmoo | Tpro | Tsoa | Tsop | Tsot | Tsst | Buf | Cnt | Crm deriving (Eq, Ord, Show, Read)

frameIDList =
   [ FT "4.20 " Aenc "AENC" "Audio encryption" ""
   , FT "4.15 " Apic "APIC" "Picture" "Attached picture"
   , FT "4.11 " Comm "COMM" "Comments" ""
   , FT "4.25 " Comr "COMR" "Commercial" "Commercial frame"
   , FT "4.26 " Encr "ENCR" "Encryption method registration" ""
   , FT "4.13 " Equa "EQUA" "Equalizn" "Equalization"
   , FT "4.6  " Etco "ETCO" "Events" "Event timing codes"
   , FT "4.16 " Geob "GEOB" "Object" "General encapsulated object"
   , FT "4.27 " Grid "GRID" "Group ID" "Group identification registration"
   , FT "4.4  " Ipls "IPLS" "People" "Involved people list"
   , FT "4.21 " Link "LINK" "Link" "Linked information"
   , FT "4.5  " Mcdi "MCDI" "CD ID" "Music CD identifier"
   , FT "4.7  " Mllt "MLLT" "Loc. Grid" "MPEG location lookup Grid"
   , FT "4.24 " Owne "OWNE" "Owner" "Ownership frame"
   , FT "4.28 " Priv "PRIV" "Private" "Private frame"
   , FT "4.17 " Pcnt "PCNT" "# Plays" "Play counter"
   , FT "4.18 " Popm "POPM" "Popularimeter" ""
   , FT "4.22 " Poss "POSS" "Position synchronisation frame" ""
   , FT "4.19 " Rbuf "RBUF" "Recommended buffer size" ""
   , FT "4.12 " Rvad "RVAD" "Relative volume adjustment" ""
   , FT "4.14 " Rvrb "RVRB" "Reverb" ""
   , FT "4.10 " Sylt "SYLT" "Synchronized lyric/text" ""
   , FT "4.8  " Sytc "SYTC" "Synchronized tempo codes" ""
   , FT "4.2.1" Album "TALB" "Album" "Album/Movie/Show title"
   , FT "4.2.1" Tbpm "TBPM" "BPM" "BPM [beats per minute]"
   , FT "4.2.1" Tcom "TCOM" "Composer" ""
   , FT "4.2.1" Genre "TCON" "Genre" "Content type"
   , FT "4.2.1" Tcop "TCOP" "Copyright" "Copyright message"
   , FT "4.2.1" Tdat "TDAT" "Date" ""
   , FT "4.2.1" Tdly "TDLY" "Playlist delay" ""
   , FT "4.2.1" Tenc "TENC" "Encoded by" ""
   , FT "4.2.1" Text "TEXT" "Lyrics by" "Lyricist/Text writer"
   , FT "4.2.1" Tflt "TFLT" "File type" ""
   , FT "4.2.1" Time "TIME" "Time" ""
   , FT "4.2.1" Tit1 "TIT1" "Content group" "Content group description"
   , FT "4.2.1" Song "TIT2" "Song" "Title/songname/content description"
   , FT "4.2.1" Tit3 "TIT3" "Subtitle/Description refinement" ""
   , FT "4.2.1" Tkey "TKEY" "Initial key" ""
   , FT "4.2.1" Tlan "TLAN" "Language" ""
   , FT "4.2.1" Tlen "TLEN" "Length [ms]" "Length"
   , FT "4.2.1" Tmed "TMED" "Media type" ""
   , FT "4.2.1" Toal "TOAL" "Original album" "Original album/movie/show title"
   , FT "4.2.1" Tofn "TOFN" "Original filename" ""
   , FT "4.2.1" Toly "TOLY" "Original lyricist" "Original lyricist[s]/text writer[s]"
   , FT "4.2.1" Tope "TOPE" "Original artist" "Original artist[s]/performer[s]"
   , FT "4.2.1" Tory "TORY" "Original release year" ""
   , FT "4.2.1" Town "TOWN" "File owner/licensee" ""
   , FT "4.2.1" Artist "TPE1" "Artist" "Lead performer[s]/Soloist[s]"
   , FT "4.2.1" AlbumArtist "TPE2" "Album Artist" "Band/orchestra/accompaniment"
   , FT "4.2.1" Tpe3 "TPE3" "Conductor/performer refinement" ""
   , FT "4.2.1" Tpe4 "TPE4" "Interpreted, remixed, or otherwise modified by" ""
   , FT "4.2.1" Tpos "TPOS" "Disc" "Part of a set"
   , FT "4.2.1" Tpub "TPUB" "Publisher" ""
   , FT "4.2.1" Track "TRCK" "#" "Track number/Position in set"
   , FT "4.2.1" Trda "TRDA" "Recording dates" ""
   , FT "4.2.1" Trsn "TRSN" "Internet radio station name" ""
   , FT "4.2.1" Trso "TRSO" "Internet radio station owner" ""
   , FT "4.2.1" Tsiz "TSIZ" "Size" ""
   , FT "4.2.1" Tsrc "TSRC" "ISRC" "ISRC [international standard recording code]"
   , FT "4.2.1" Tsse "TSSE" "Settings" "Software/Hardware and settings used for encoding"
   , FT "4.2.1" Year "TYER" "Year" ""
   , FT "4.2.2" Txxx "TXXX" "User text" "User defined text information frame"
   , FT "4.1  " Ufid "UFID" "Unique file identifier" ""
   , FT "4.23 " User "USER" "Terms of use" ""
   , FT "4.9  " Uslt "USLT" "U Lyrics" "Unsychronized lyric/text transcription"
   , FT "4.3.1" Wcom "WCOM" "Commercial information" ""
   , FT "4.3.1" Wcop "WCOP" "Copyright/Legal information" ""
   , FT "4.3.1" Woaf "WOAF" "Official audio file webpage" ""
   , FT "4.3.1" Woar "WOAR" "Official artist/performer webpage" ""
   , FT "4.3.1" Woas "WOAS" "Official audio source webpage" ""
   , FT "4.3.1" Wors "WORS" "Official internet radio station homepage" ""
   , FT "4.3.1" Wpay "WPAY" "Payment" ""
   , FT "4.3.1" Wpub "WPUB" "Publishers official webpage" ""
   , FT "4.3.2" Wxxx "WXXX" "User defined URL link frame" ""
   , -- seen in the wild but not part of the standard
      FT "" Atxt "ATXT" "ATXT" ""
   , FT "" Chap "CHAP" "ID3 Chapter" ""
   , FT "" Ctoc "CTOC" "ID3 Table Of Contents" ""
   , FT "" Rgad "RGAD" "RGAD" ""
   , FT "" Tcmp "TCMP" "Comp" "Set to 1 if the song is part of a compilation"
   , FT "" Tso2 "TSO2" "TSO2" ""
   , FT "" Tsoc "TSOC" "TSOC" ""
   , FT "" Xrva "XRVA" "XRVA" ""
   , FT "" Ntrk "NTRK" "Total number of tracks" ""
   , -- id3v2.4
      FT "4.19 " Aspi "ASPI" "Audio seek point index" ""
   , FT "4.12 " Equ2 "EQU2" "Equalisation" ""
   , FT "4.11 " Rva2 "RVA2" "Relative volume adjustment" ""
   , FT "4.29 " Seek "SEEK" "Seek" ""
   , FT "4.28 " Sign "SIGN" "Signature" ""
   , FT "4.2.5" Tden "TDEN" "Encoding time" ""
   , FT "4.2.5" Tdor "TDOR" "Original release time" ""
   , FT "4.2.5" Tdrc "TDRC" "Recording time" ""
   , FT "4.2.5" Tdrl "TDRL" "Release time" ""
   , FT "4.2.5" Tdtg "TDTG" "Tagging time" ""
   , FT "4.2.2" Tipl "TIPL" "Involved people" "Involved people list"
   , FT "4.2.2" Tmcl "TMCL" "Musician credits list" ""
   , FT "4.2.3" Tmoo "TMOO" "Mood" ""
   , FT "4.2.4" Tpro "TPRO" "Production notice" ""
   , FT "4.2.5" Tsoa "TSOA" "Album sort" "Album sort order"
   , FT "4.2.5" Tsop "TSOP" "Perf sort" "Performer sort order"
   , FT "4.2.5" Tsot "TSOT" "Title sort" "Title sort order"
   , FT "4.2.1" Tsst "TSST" "Set subtitle" ""
   , -- id3v2.2
      FT "4.19 " Buf "BUF" "Recommended buffer size" ""
   , FT "4.17 " Cnt "CNT" "# Plays" ""
   , FT "4.11 " Comm "COM" "Comments" ""
   , FT "4.21 " Aenc "CRA" "Audio encryption" ""
   , FT "4.20 " Crm "CRM" "Encrypted meta frame" ""
   , FT "4.6  " Etco "ETC" "Events" ""
   , FT "4.13 " Equa "EQU" "Equalization" ""
   , FT "4.16 " Geob "GEO" "Object" ""
   , FT "4.4  " Ipls "IPL" "Involved people" ""
   , FT "4.22 " Link "LNK" "Link" "Linked information"
   , FT "4.5  " Mcdi "MCI" "CD ID" "Music CD Identifier"
   , FT "4.7  " Mllt "MLL" "Loc. Grid" "MPEG location lookup Grid"
   , FT "4.15 " Apic "PIC" "Picture" ""
   , FT "4.18 " Popm "POP" "Popularimeter" ""
   , FT "4.14 " Rvrb "REV" "Reverb" ""
   , FT "4.12 " Rvad "RVA" "Relative volume adjustment" ""
   , FT "4.10 " Sylt "SLT" "Synchronized lyric/text" ""
   , FT "4.8  " Album "TAL" "Album" "Album/Movie/Show title"
   , FT "4.2.1" Tbpm "TBP" "BPM" "BPM [Beats Per Minute]"
   , FT "4.2.1" Tcom "TCM" "Composer" ""
   , FT "4.2.1" Genre "TCO" "Genre" "Content type"
   , FT "4.2.1" Tcop "TCR" "Copyright" "Copyright message"
   , FT "4.2.1" Tdat "TDA" "Date" ""
   , FT "4.2.1" Tdly "TDY" "Playlist delay" ""
   , FT "4.2.1" Tenc "TEN" "Encoded by" ""
   , FT "4.2.1" Tflt "TFT" "File type" ""
   , FT "4.2.1" Time "TIM" "Time" ""
   , FT "4.2.1" Tkey "TKE" "Initial key" ""
   , FT "4.2.1" Tlan "TLA" "Language" ""
   , FT "4.2.1" Tlen "TLE" "Length [ms]" "Length"
   , FT "4.2.1" Tmed "TMT" "Media type" ""
   , FT "4.2.1" Tope "TOA" "Original artist[s]/performer[s]" ""
   , FT "4.2.1" Tofn "TOF" "Original filename" ""
   , FT "4.2.1" Toly "TOL" "Original Lyricist[s]/text writer[s]" ""
   , FT "4.2.1" Tory "TOR" "Original release year" ""
   , FT "4.2.1" Toal "TOT" "Original album" "Original album/Movie/Show title"
   , FT "4.2.1" Artist "TP1" "Artist" "Lead artist[s]/Lead performer[s]/Soloist[s]/Performing group"
   , FT "4.2.1" AlbumArtist "TP2" "Album Artist" "Band/Orchestra/Accompaniment"
   , FT "4.2.1" Tpe3 "TP3" "Conductor/Performer refinement" ""
   , FT "4.2.1" Tpe4 "TP4" "Interpreted, remixed, or otherwise modified by" ""
   , FT "4.2.1" Tpos "TPA" "Disc" "Part of a set"
   , FT "4.2.1" Tpub "TPB" "Publisher" ""
   , FT "4.2.1" Tsrc "TRC" "ISRC" "ISRC [International Standard Recording Code]"
   , FT "4.2.1" Trda "TRD" "Recording dates" ""
   , FT "4.2.1" Track "TRK" "#" "Track number/Position in set"
   , FT "4.2.1" Tsiz "TSI" "Size" ""
   , FT "4.2.1" Tsse "TSS" "Settings" "Software/hardware and settings used for encoding"
   , FT "4.2.1" Tit1 "TT1" "Content group description" ""
   , FT "4.2.1" Song "TT2" "Song" "Title/Songname/Content description"
   , FT "4.2.1" Tit3 "TT3" "Subtitle/Description refinement" ""
   , FT "4.2.1" Text "TXT" "Lyrics by" "Lyricist/text writer"
   , FT "4.2.2" Txxx "TXX" "User text" "User defined text information frame"
   , FT "4.2.1" Year "TYE" "Year" ""
   , FT "4.1  " Ufid "UFI" "Unique file identifier" ""
   , FT "4.9  " Uslt "ULT" "U Lyrics" "Unsychronized lyric/text transcription"
   , FT "4.3.1" Woaf "WAF" "Official audio file webpage" ""
   , FT "4.3.1" Woar "WAR" "Official artist/performer webpage" ""
   , FT "4.3.1" Woas "WAS" "Official audio source webpage" ""
   , FT "4.3.1" Wcom "WCM" "Commercial information" ""
   , FT "4.3.1" Wcop "WCP" "Copyright/Legal information" ""
   , FT "4.3.1" Wpub "WPB" "Publishers official webpage" ""
   , FT "4.3.2" Wxxx "WXX" "User defined URL link frame" ""
   ]

