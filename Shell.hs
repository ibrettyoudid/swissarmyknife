-- Copyright 2025 Brett Curtis
{-# LANGUAGE BangPatterns #-}

module Shell where

import System.Directory

-- import System.Time
-- import Text.Regex.Posix

import System.IO
import System.IO.Unsafe
import System.Process

-- import System.FilePath.Windows
-- import qualified System.Win32.File as Win32 --needed for deleteFile

import Control.Exception
import Control.Monad
import Data.IORef
import System.Environment

-- import Data.Maybe
import Control.Applicative
import Data.Array
import Data.ByteString.Char8 qualified as B
import Data.Char
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Word

import Favs
import MyPretty2

-- import Html hiding (cd)

-- deleteFile = Win32.deleteFile
linux = True

ls = do cwd <- getCurrentDirectory; listDirectory cwd

pwd = getCurrentDirectory
cd = setCurrentDirectory

mapRename f = mapM_ (\n -> renameFile n (f n))

mapFiles func = mapxfx (func . readFileU)
mapFilesM func = mapM_ (modifyFile func)

mapDirM func = mapM_ (modifyFile func)

-- apply func to the contents of filename
modifyFile func filename = do
   h <- openBinaryFile filename ReadMode
   contents <- hGetContents h
   print $ length contents
   hClose h
   writeFile filename $ func contents

modifyFile1 func filename newname = do
   contents <- readFileB filename
   let (n, e) = splitExtension filename
   writeFile (n ++ newname ++ e) $ func contents

contains sub str = isJust $ subIndex sub str

{-
search text = case subIndex srch text of
   Nothing -> Nothing
   Just _  -> Just $

search sub text =
   (filter (\(a, b) -> contains sub b) . zip [1..] . lines)
   <$> const text <$> subIndex sub text
-}
search :: [Char] -> [Char] -> [(Int, String)]
search sub text =
   if contains sub text
      then
         let
         l = lines text
         in
         mapxfx (l !!) $
         nubSet $
            concatMap (\a -> [max 0 (a - 1) .. min (a + 1) (length l - 1)]) $
               findIndices (contains sub) l
      else []

insertGaps :: [(Int, String)] -> [(Int, String)]
insertGaps [] = []
insertGaps [a] = [a]
insertGaps ((a, b) : (c, d) : e) =
   if c - a > 1
      then (a, b) : (-1, "") : insertGaps ((c, d) : e)
      else (a, b) : insertGaps ((c, d) : e)

searchTree :: String -> String -> IO ()
searchTree srchStr path = let
   results = filter ((/= []) . snd) $ mapxfx (insertGaps . search srchStr . readFileU) $ fileTree path
   in mapM_
      ( \(fileName, matches) -> do
         putStrLn fileName
         mapM
            ( \(lineNo, line) ->
            if lineNo == -1
               then putStrLn ""
               else putStr $ ((pad 5 $ show lineNo) ++ " " ++ line ++ "\n")
            )
            matches
         putStrLn ""
      )
      results

{-
del n = do deleteFile $ "d:\\code\\python\\stylegan2-master\\dnnlib\\__init__ (" ++ show n ++ ").py"
         del $ n + 1
-}
{-
readFileU1 :: String -> String
readFileU1 f = upIO $ catch (readFile f) handler

handler :: IOError -> IO String
handler e = putStrLn "Whoops, had some trouble"
-}
d0 = "f:/incoming/utorrent complete"

-- unsafe
contents p = map readFileU $ filter (unsafePerformIO . doesFileExist) $ paths p

{-
names1 p = do n <- namesRaw p
            let n1 = sort $ map splitNumAlpha n
            let n2 = reverse n1
            let n3 = map joinNumAlpha n2
            return $ reverse n3
paths1 p = do n <- names1 p
            return (map ((p++"/")++) n)
-}

-- unsafe
upIO = unsafePerformIO
subdir d s = if last d == '/' then d ++ s else d ++ '/' : s
infixr 5 @
d @ s = subdir d s

{-
filterM m (x:xs) = do y <- m x
                  return ((if y then (x:) else id) filterM m xs)
-}
-- unsafe
names p = sort $ unsafePerformIO $ listDirectory p

paths p = map (p @) $ names p

dirNames p = filter (\n -> unsafePerformIO $ doesDirectoryExist (p @ n)) $ names p
fileNames p = filter (\n -> unsafePerformIO $ doesFileExist (p @ n)) $ names p
dirPaths p = filter (unsafePerformIO . doesDirectoryExist) $ paths p
filePaths p = filter (unsafePerformIO . doesFileExist) $ paths p
cdirPaths p = concatMap dirPaths p
cfilePaths p = concatMap filePaths p

dirExists p = unsafePerformIO $ doesDirectoryExist p

interleave xs = concat $ transpose xs

-- get paths and subpaths at p
tree p = paths p ++ (concatMap tree $ dirPaths p)

-- breadth first (i = subdirs interleaved so as to give equal effort to each)
treei p = paths p ++ (concat $ transpose $ map tree $ dirPaths p)

-- same but only returning dirs
dirTree p = dirPaths p ++ (concatMap dirTree $ dirPaths p)
dirTreei p = dirPaths p ++ (concat $ transpose $ map dirTree $ dirPaths p)

-- same but only returning files
fileTree p = filePaths p ++ concatMap fileTree (dirPaths p)

ghciExe = "d:/programs/ghc-6.8.1/bin/ghci.exe"
comExe = "cmd.exe"
wcExe = "d:/programs/cygwin/bin/wc.exe"
sedExe = "d:/programs/cygwin/bin/sed.exe"

run exe = do
   (inp, out, err, pid) <- runInteractiveProcess exe [] Nothing Nothing
   hGetContents out

run1 exe inp = do
   (inph, outh, errh, pid) <- runInteractiveProcess exe [] Nothing Nothing
   hPutStr inph inp
   hGetContents outh

whereisP file paths = catMaybes <$> mapM (\p -> let f = p @ file in justIf f <$> doesFileExist f) paths

whereis file = whereisP file =<< (split ";" <$> getEnv "PATH")

splitPath p = split "/" p
joinPath p = intercalate "/" p
splitExtension p = let
   e = last $ elemIndices '.' p
   s = last $ elemIndices '/' p
   in
   if e > s then splitAt e p else (p, "")

-- remove 2nd last name from path
burst :: FilePath -> FilePath
burst p = let
   (f : d : dd) = reverse $ splitPath p
   in
   joinPath $ reverse (f : dd)

-- as above but with a predicate (String -> String)
bfs pred dir = let
   (pmatch, pfail) = partition pred $ paths dir
   in
   pmatch ++ (concat $ transpose $ map (bfs pred) $ filter dirExists pfail)

bfsub sub = bfs (isJust . subIndex sub)

binsearch f target l r = let
   x = (l + r) `div` 2
   in
   if (r - l) <= 1
      then x
      else
      if f x < target
      then binsearch f target x r
      else binsearch f target l x

-- finds first that is equal or greater (lower bound)
lsearch f target l r = head $ (dropWhile (\x -> f x < target) [l .. r]) ++ [r]

-- finds last  that is equal or less    (upper bound)
rsearch f target l r = head $ (dropWhile (\x -> f x > target) [r, r - 1 .. l]) ++ [l]

-- finds first that is equal or greater (lower bound)
blsearch f target l r = let
   x1 = binsearch f target l r
   x2 = rsearch f target l x1
   x3 = lsearch f target x2 r
   in
   x3

-- finds last  that is equal or less    (upper bound)
brsearch f target l r = let
   x1 = binsearch f target l r
   x2 = lsearch f target x1 r
   x3 = rsearch f target l x2
   in
   x3

-- finds (lower_bound, upper_bound)
blrsearch f target l r = let
   x1 = binsearch f target l r
   x2 = rsearch f target l x1
   x3 = lsearch f target x2 r
   x4 = rsearch f target l x3
   in
   (x3, x4)

data Ordering4 = LT4 | EQ4 | SS4 | GT4 deriving (Eq, Ord, Show)

finda a b = let
   (l, r) = bounds a
   (xl, xr) = blrsearch (a !) b l r
   ll = length $ commonPrefix (a ! xl) b
   lr = length $ commonPrefix (a ! xr) b
   sl = take ll (a ! xl)
   sr = take lr (a ! xr)
   in
   if ll > lr then (ll, sl) else (lr, sr)

lcsub a b = let
   as = listArray (1, length a) $ sort (tails1 a)
   in
   snd $ maximum $ map (finda as) (tails1 b)

tails1 [] = []
tails1 a = a : tails1 (tail a)

testl = ["aback", "abacus", "abatoir", "abet", "apple"]

{-
longest common prefix can be on either side of target:
lcp is less

find abcdef in [abcdaaa, bbbbb]

lcp is more

find abcdef in [aaaaa, abcdzzz]
-}
renumberPrefix pref dir = let
   orig = map (dir ++) $ filter (isPrefixOf pref) $ names dir
   new = map (dir ++) $ map ((pref ++) . (++ ".jpg") . pad0 2 . show) [1 ..]
   in
   sequence $ zipWith renameFile orig new

{-
doZeroPadNums -> splitNumAlpha
            -> joinNumAlpha
            -> mapZPN

-}

doZeroPadNums p = do
   n <- listDirectory p
   let n2 = reverse $ sort $ map splitNumAlpha n
   let n3 = map joinNumAlpha n2
   let n4 = mapZPN n2
   let n5 = map joinNumAlpha n4
   zipWithM_ (renameFile `on` (p ++)) n3 n5
   return (zip n3 n5)

splitNumAlpha [] = []
splitNumAlpha a = let
   (aa, a1) = break isDigit a
   (an, a2) = span isDigit a1
   in
   (map toLower aa, length an, aa, an) : splitNumAlpha a2

joinNumAlpha [] = []
joinNumAlpha ((al, nl, a, n) : as) = a ++ n ++ joinNumAlpha as

mapZPN [a] = [a]
mapZPN (a : b : cs) = let
   (a1, b1) = zeroPadNums a b
   in
   a1 : mapZPN (b1 : cs)

zeroPadNums [] [] = ([], [])
zeroPadNums a [] = (a, [])
zeroPadNums [] b = ([], b)
zeroPadNums
   a@((aal, anl, aa, an) : as)
   b@((bal, bnl, ba, bn) : bs)
      | aal == bal =
         let
         (a1, b1) = zeroPadNums as bs
         l = max anl bnl
         in
         ( (aal, l, aa, pad0 l an) : a1
         , (bal, l, ba, pad0 l bn) : b1
         )
      | otherwise = (a, b)

{-
zeroPadNums [] [] = ([], [])
zeroPadNums  a [] = ( a, [])
zeroPadNums []  b = ([],  b)
zeroPadNums a@((aal, anl, aa, an):as)
         b@((bal, bnl, ba, bn):bs) | aal == bal = let
            (a1, b1) = zeroPadNums as bs
            l        = max anl bnl

            in ((aal, l, aa, pad0 l an):a1,
               (bal, l, ba, pad0 l bn):b1)

                           | otherwise = (a, b)
-}
-- testZeroPadNums = map joinNumAlpha . foldr zeroPadNums [] . sort . map splitNumAlpha

testz = ["ab1a4", "ab2", "ab11", "ab1a17"]

commonPrefix a b = map fst $ takeWhile (uncurry (==)) $ zip a b

commonPrefix2 a b = let l = length $ takeWhile id $ zipWith (==) a b in take l a

compareNumAlpha [] [] = EQ
compareNumAlpha _ [] = GT
compareNumAlpha [] _ = LT
compareNumAlpha (a : as) (b : bs) =
   if isDigit a && isDigit b
      then compareNumAlphaNum (a : as) (b : bs)
      else case compare (toLower a) (toLower b) of
         LT -> LT
         GT -> GT
         EQ -> compareNumAlpha as bs

compareNumAlphaNum a b = let
   (anStr, arest) = span isDigit a
   (bnStr, brest) = span isDigit b
   an = readInt anStr
   bn = readInt bnStr
   in
   case compare an bn of
      LT -> LT
      GT -> GT
      EQ -> compareNumAlpha arest brest

zeroPadNums2 "" "" = ("", "")
zeroPadNums2 a b = let
   (aa, an) = break isDigit a
   (ba, bn) = break isDigit b

   (am, aq) = span isDigit an
   (bm, bq) = span isDigit bn
   la = length am
   lb = length bm
   in
   if map toLower aa == map toLower ba && la > 0 && lb > 0
      then let
         le = max la lb
         ax = replicate (le - la) '0' ++ am
         bx = replicate (le - lb) '0' ++ bm
         (az, bz) = zeroPadNums2 aq bq
         in
         (aa ++ ax ++ az, ba ++ bx ++ bz)
      else 
         (a, b)

readInt0 :: String -> Int
readInt0 "" = 0
readInt0 a = readInt a

-- counts :: (Ord a) => [a] -> M.Map a Integer
-- counts = M.fromListWith (+) . map (\k -> (k, 1))

population = map (\li -> let [_, c, pop] = take 3 $ Favs.splitNZ " " li in (c, readInt pop)) $ lines $ readFileU "populations.txt"

fields = map ((\[year, name, country] -> Award country "Maths" (readInt year)) . split ", ") $ lines $ readFileU "fields.txt"

turings = map ((\[year, name, country] -> Award country "CS" (readInt year)) . split ", ") $ lines $ readFileU "turings.txt"

nobels = nobels1 "" $ lines $ readFileU "nobels.txt"

left `per` pop = reverse $ sort $ map (\(k, (n, p)) -> (fromIntegral n / fromIntegral p, n, p, k)) $ M.toList $ joinMaps left (M.fromList pop)

all1 = const True

{-
countaw f = do aw <- awards; pop <- population; mapM (\(np,n,p,k) -> putStrLn (show np ++ show n ++ show p ++ k)) $
                                       per (counts $ map (\(c,s,y) -> c) $ filter f aw) pop
-}
{-
countaw f = per (M.fromList $ counts $ map country $ filter f awards) population :: [(Double, Int, Int, String)]

countaw2 a = per (counts $ map country a) population :: [(Double, Int, Int, String)]
-}

data Award = Award {country :: String, subject :: String, year :: Int}
awards = fields ++ turings ++ nobels

-- nobels "" <$> readFile "nobels.txt"

nobels1 _ [] = []
nobels1 _ ("" : "[edit]" : country : xs) = nobels1 country xs
nobels1 _ ("" : country : xs) = nobels1 country xs
nobels1 _ ("[edit]" : country : xs) = nobels1 country xs
nobels1 country (x : xs) = case split ", " x of
   [name, subject, year] -> Award country subject (readInt year) : nobels1 country xs
   [name, _, subject, year] -> Award country subject (readInt year) : nobels1 country xs
   [name, _, _, subject, year] -> Award country subject (readInt year) : nobels1 country xs

wordsPath = "d:/code/bcb/anagram/words/"
wordsFile = "english-words.*"

wordsPath1 = "d:\\code\\vc\\codes\\books\\"

ch :: Word8 -> Char
ch = toEnum . fromEnum

w :: Char -> Word8
w = toEnum . fromEnum

{-
toLower8 x = if x >= 'A' && x <= 'Z'
            then x + 32
            else x
-}

engwordsf n f = let 
   lst = split "." f
   in case lst of
      [a, b] -> a == "english-words" && readInt b <= n
      otherwise -> False

readFiles dir filt = do
   files <- listDirectory dir
   let files1 = map (dir ++) $ filter filt files
   contents <- mapM B.readFile files1
   return $ B.concat contents

groupN1 !n !lst = take (B.length lst - n + 1) $ map (B.take n) $ B.tails lst

insertChunks [] _ set = set
insertChunks (chunk : chunks) word set =
   insertChunks chunks word
      ( M.insertWith
         (\(n1, w1) (n2, w2) -> (n1 + n2, w1 ++ w2))
         chunk
         (1 :: Int, [word])
         set
      )

lastN n lst = drop (length lst - n) lst

around g contents = let 
   (bef, aft) = B.breakSubstring g contents
   (bef1, _) = B.breakSubstring (B.pack "\n") $ B.reverse bef
   (aft1, _) = B.breakSubstring (B.pack "\n") aft
   
   in B.reverse bef1 `B.append` aft1

-- around g contents = let at = unjust $ findSubstring g contents

-- find weird words
go = do
   freqF <- readFiles wordsPath1 (const True)
   weirdF <- readFiles wordsPath (engwordsf 99)
   -- weirdF <- B.readFile "d:/code/bcb/anagram/words/twl06.txt"
   go1 freqF weirdF

go1 freqF weirdF = do
   let freqW = splitLow freqF
   -- let weirdF1 = splitLow weirdF
   let freqMid3 = makeSet $ concatMap (groupN1 3) $ freqW
   let freqMid2 = makeSet $ concatMap (groupN1 2 . B.init) $ freqW
   let freqInit2 = makeSet $ map (B.take 2) $ freqW
   let freqMid3a = divSet2 freqMid3 freqMid2
   showSet freqMid3
   showSet freqMid2
   showSet freqMid3a

   let weirdW = splitLow weirdF
   showTups $ sortBy (flip compare) $ map (\w -> (unknowns freqMid3 w, w)) weirdW

{-
table
         $  map (\(g, n, w) -> [show g, show n, show w])
         $  map (\(g, n) -> (g, n, around g freqF))
         $! take 200
--           $! sort
--           $! map (\(x, y) -> (y, x))
         $! sortBy (\(_, a) (_, b) -> compare a b)
         $! M.toList
         freqT3
-}
--           $  map (\(g, n) -> (g, n, B.take 20 $ B.drop ((-8+) $ unjust $ B.findSubstring g contents) contents))

unknowns ma lst = length $ filter (== Nothing) $ map (\k -> M.lookup k ma) $ groupN1 3 lst

divSet s d = M.map (\n -> fromIntegral n / fromIntegral d) s

divSet2 ns ds = M.mapWithKey (\nk nv -> nv / unjust (M.lookup (B.take 2 nk) ds)) ns

makeSet lst = let 
   l = length lst
   s = foldl' (\set word -> M.insertWith (+) word (1 :: Int) set) M.empty lst
   
   in divSet s l

showTups tups = 
   ppTableS $
      map (\(g, n) -> [show g, show n]) $
         take 500 tups

showSet set =
   ppTableS $
      map (\(g, n) -> [show g, show n]) $
         take 20 $
         sortBy ((flip compare) `on` snd) $
         M.toList set

splitLow =
   filter (\s -> B.length s >= 3)
      . B.splitWith (\ch -> ch < 'a' || ch > 'z')
      . B.map toLower

splitSpace = B.splitWith (\ch -> not (ch == '-' || (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')))

splitHyphen s = [a, B.drop 2 b] where (a, b) = B.breakSubstring (B.pack "--") s

-- (\ch -> ch < 'A' || (ch > 'Z' && ch < 'a') || ch > 'z')

-- keyboard = "           qwertyuiopasdfghjklzxcvbnm           "
-- keyboard = "qwerty asdfg zxcvb   "
keyboard = "yuiop hjkl nm  "

kshift1 n c = case elemIndex c keyboard of
   Nothing -> ' '
   Just x -> keyboard !! (x + n)

kshift n w = map (kshift1 n) w

kswords n = map (\w -> (kshift (-n) w, w)) $ Favs.sortOn length $ S.toList $ S.intersection scrabset $ S.map (kshift n) scrabset

scrabwords = words $ map toLower $ readFileU "d:/code/bcb/anagram/words/scrabble.35"

scrabset = S.fromList scrabwords

countLines = sum $ map (length . lines . readFileU) $ filter (\p -> isSuffixOf ".cpp" p || isSuffixOf ".h" p) $ paths "d:/code/bcb/director"

path1 = "d:/pics/1porn"

myFindFile =
   filterM
      ( \f -> do
         h <- openFile f ReadMode
         s <- hFileSize h
         hClose h
         return (s == 54660)
      )
      $ fileTree path1

{-
main = do
   (srchStr:path:cs) <- getArgs
   let results = tf_idf (B.pack srchStr) path
   putTable1 $ transpose $ map (\(score, count, size, fileName) -> [show score, show count, show size, fileName]) results
-}
tf_idf :: B.ByteString -> FilePath -> [(Integer, Integer, Integer, FilePath)]
tf_idf srch path =
   reverse
      $ sort
      $ unsafePerformIO
      $ mapM
         ( \fileName -> do
         handle <- openFile fileName ReadMode
         size <- hFileSize handle
         fileData <- B.hGetContents handle
         let count = countSubstr srch fileData
         return
            ( if contains fileName (B.unpack srch)
               then 2000000 * (count + 1) `div` (size + 1)
               else 1000000 * count `div` (size + 1)
            , count
            , size
            , fileName
            )
         )
      $ fileTree path

tf_idf1 :: B.ByteString -> FilePath -> [String] -> [(Integer, Integer, Integer, FilePath)]
tf_idf1 srch path ignore =
   reverse
      $ sort
      $ unsafePerformIO
      $ mapM
         ( \fileName -> do
         handle <- openFile fileName ReadMode
         size <- hFileSize handle
         fileData <- B.hGetContents handle
         let count = countSubstr srch fileData
         return
            ( if contains fileName (B.unpack srch)
               then 2000000 * (count + 1) `div` (size + 1)
               else 1000000 * count `div` (size + 1)
            , count
            , size
            , fileName
            )
         )
      $ filter (\f -> not $ elem f ignore)
      $ fileTree path

countSubstr :: B.ByteString -> B.ByteString -> Integer
countSubstr srchStr inStr = aux 0 inStr
   where
      breakSS = B.breakSubstring srchStr

      srchLen = B.length srchStr

      aux :: Integer -> B.ByteString -> Integer
      aux count left =
         let
            next = snd $ breakSS left
         in
            if B.length next < srchLen then count else aux (count + 1) $ B.drop srchLen next
