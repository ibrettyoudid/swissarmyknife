-- Copyright 2025 Brett Curtis
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Crossword where

import Favs
import MyPretty2
import Shell

import qualified Data.Set as S
import qualified Data.Map as M
import System.Directory
import System.IO.Unsafe
import Data.Char
import Data.List
import Data.Maybe
import Control.Parallel.Strategies

--path = "d:/code/bcb/anagram/words/"
--path = "/mnt/sol/Code/BCB/Anagram/words/"
path = "/home/brett"

palindromes = filter isPalindrome vocab
{-
>>> palindromes
["aa","aba","abba","acca","aga","aha","aia","aka","ala","alula","ama","ana","anana","anna","araara","ataata","aua","ava","awa","ayaya","bib",
"bob","boob","bub","civic","dad","deed","degged","deified","deked","deled","denned","dered","dewed","did","dod","dud","ecce","ee","eke","eme",
"ene","ere","esse","eve","ewe","eye","gag","gig","goog","hadedah","hah","hajjah","halalah","hallah","heh","hoh","huh","iwi","kaiak","kak",
"kayak","keek","kook","lahal","laval","lemel","level","madam","malam","mallam","mam","marram","mem","mim","minim","mm","mmm","mom","mum",
"naan","nan","non","noon","nun","obo","oho","ono","oo","oppo","otto","oxo","pap","peep","pep","pip","poop","pop","pullup","pup","qajaq",
"radar","redder","refer","reifier","repaper","reviver","rotator","rotavator","rotor","sagas","samas","sedes","sees","seities","seles",
"selles","sememes","semes","senes","seres","serres","sesses","sexes","shahs","simis","siris","sis","slahals","solos","sos","stats","stets",
"stots","succus","sulus","sus","susus","tallat","tat","tenet","terret","tet","tirrit","tit","toot","torot","tot","tut","ulu","umu","utu",
"vav","waw","wow","yay","ziz","zuz","zzz"]
-}
isPalindrome w = w == reverse w

vocab = vocabl

vocabr = vocabrf vocab

-- filter reversible words (argument is a list of words)
vocabrf v = let forwards  = S.fromList v
                backwards = S.fromList $ map reverse v
            in  S.toList $ S.intersection forwards backwards

vocab0 = voc 95

vocab1 = filter (notElem '\'') $ words $ map toLower $ readFileU "d:/code/bcb/anagram/words/english-words.50"

vocabl = words $ map toLower $ readFileU "/home/brett/swissarmyknife/scrabble.txt"

-- list english words up to a particular number
voc n = ("biden":) $ nubSet $ concatMap vocf $ filter (vf n) $ paths path

vf n f = let lst = split "." f in case lst of 
                                         -- [a, b]    -> readInt b <= n
                                         -- [a, b]    -> "english" `isPrefixOf` a && readInt b <= n
                                         [a, b]    -> "words" `isInfixOf` a && readInt b <= n && readInt b > 0
                                         -- [a, b]    -> a == "english-words" && readInt b <= n
                                         _         -> False

vocf fi = nubSet $ words $ map toLower $ readFileU fi

readFiles dir filt = concatMap (\f -> readFileU (dir++f)) 
                   $ filter filt $ filter (\a -> a /= "." && a /= "..") 
                   $ unsafePerformIO $ getDirectoryContents dir

match1 ' ' c = True
match1 a   b = a == b

match m w = length m == length w && and (zipWith match1 m w)

vocm m = filter (match m) vocab

data Clue = Clue { x0      :: Int, 
                   y0      :: Int, 
                   x1      :: Int, 
                   y1      :: Int, 
                   horiz   :: Bool, 
                   len     :: Int, 
                   nans    :: Int, 
                   possibles :: [String], 
                   num     :: Int } deriving (Eq, Ord, Show)

type Grid = [String]

clues1 lin = aux ('#':lin) 0
   where
      aux []  _ = []
      aux (c:cs) n = let l = length $ takeWhile (/='#') cs
                     in (if c == '#' && l > 1 then ((n, l):) else id) $ aux cs (n+1)
      
--clues1 lin = scanl (+) 0 $ map length $ group ('#':lin)
      
--clues1 lin = zipWith (\cs n -> if isPrefixOf "#  " cs then Just (n, length $ takeWhile (==' ') $ tail cs) else Nothing) ('#':lin) [-1..]

clues2 lins = concat $ zipWith (\y str -> map (\(x,l)->(y,x,l)) $ clues1 str) [0..] lins

vhclues lins = (map (\(y,x,l) -> Clue x y (x+l-1) y True  l 0 [] 0) $ clues2 lins) ++ 
               (map (\(x,y,l) -> Clue x y x (y+l-1) False l 0 [] 0) $ clues2 $ transpose lins)

readClueH lins x y l = take l $ drop x $ lins !! y
readClueV lins x y = readClueH (transpose lins) y x
readClue  lins clue = (if horiz clue then readClueH else readClueV) lins (x0 clue) (y0 clue) (len clue)

applyf2 (f1, f2)     x = (f1 x, f2 x)
applyf3 (f1, f2, f3) x = (f1 x, f2 x, f3 x)

compareOn2 fs = compare `on` applyf2 fs
compareOn3 fs = compare `on` applyf3 fs

sortOn1 f  = map snd . sort . map (\x -> (f x, x))
sortOn2 fs = sortOn1 (applyf2 fs)
sortOn3 fs = sortOn1 (applyf3 fs)

newLine = putStrLn ""

updateIndex f n l = take n l ++ [f (l !! n)] ++ drop (n+1) l
setXY val x = updateIndex (updateIndex (const val) x)

putLines = putStr . unlines

fmt [] = return ()
fmt r  = do 
   let x0   = length $ head $ head r
   let x1   = (x0 + 1) * 2 + 3
   let n    = min (length r) $ width `div` x1
   let nrh  = take n r
   let nrv  = map transpose nrh
   let nrht = transpose nrh
   let nrvt = transpose nrv
   putLines $ map concat $ zipWith (zipWith (\h v -> h ++ " " ++ v ++ "    ")) nrht nrvt
   putStrLn ""
   fmt $ drop n r

fmt1 r = mapM_ formatResult r

formatResult res = let
   clues1   = clues res
   nums     = nub $ sort $ map (\c -> (y0 c, x0 c)) clues1
   numbered = map (\c -> c { num = (+1) $ fromJust $ elemIndex (y0 c, x0 c) nums }) clues1
   across   = sortOn1 num $ filter horiz numbered
   down     = sortOn1 num $ filter (not . horiz) numbered
   
   zeros    = map (map (const 0)) res
   filled   = foldl (\l c -> setXY (num c) (x0 c) (y0 c) l) zeros numbered
   csvnums  = unlines $ map (intercalate "," . map (\x -> if x == 0 then "" else show x)) filled
   
   csvgrid  = unlines $ map (intersperse ',') res
   
   in do 
         putLines $ map (\c -> show (num c) ++ "," ++ head (possibles c)) across
         newLine
         putLines $ map (\c -> show (num c) ++ "," ++ head (possibles c)) down
         newLine
         putStr csvnums
         newLine
         putStr csvgrid
         newLine
         
clues = cluesVocab vocab

-- find the clues (empty word slots) in a grid
cluesVocab vocab grid = map (\c -> let done = readClue grid c
                                       a    = filter (match done) vocab
                            
                        in c { possibles = a, nans = length a } ) $ vhclues grid

cluesR size = filter (\clue -> if horiz clue then y0 clue * 2 + 1 >= size else x0 clue * 2 + 1 <= size)

cluesS size = map (\clue -> if y0 clue * 2 + 1 == size then clue { possibles = filter isPalindrome $ possibles clue } else clue) 
               . filter (\clue -> horiz clue && y0 clue * 2 + 1 <= size)

cc (y,x,h,l) = if h then (y,x,y,x+l-1) else (y,x,y+l-1,x)

cl = map (\clue -> clue { possibles = [] })

-- do clues a and b cross?
cluesCross a b = y0 a <= y1 b && y0 b <= y1 a && x0 a <= x1 b && x0 b <= x1 a

-- where should words cross?
wordsCross a b = let
   ac = mapFromList (:) [] $ zip a [0..]
   bc = mapFromList (:) [] $ zip b [0..]

   i  = M.intersectionWith (,) ac bc
   in map snd $ M.toList i

-- where do clues a and b cross? (will not work if they don't!)
crossAt a b = (max (x0 a) (x0 b), max (y0 a) (y0 b))

cwv vocab grid = cw1 grid $ cluesVocab vocab grid

-- this is the main function. type "cw hard4" to solve hard4
-- or type "fmt $ cw hard4"
cw :: Grid -> [Grid]
cw grid = cw1 grid (clues grid)

cw1 :: Grid -> [Clue] -> [Grid]
cw1 grid []    = [grid] -- if there are no more clues to fill, we have succeeded!
cw1 grid clues = let
   nextclue = minimumBy (compare `on` nans) clues
   
   in
   
   --fillWord grid next (head $ possibles next)
   --tryWord grid clues next (head $ possibles next)
   concatMap (tryWord grid clues nextclue) $ possibles nextclue

tryWord :: Grid -> [Clue] -> Clue -> String -> [Grid]
tryWord grid clues clue word = let
   newgrid  = fillGrid clue word grid
   newclues = mapMaybe (fillClue clue word) clues -- fillClue removes the just filled clue
   
   in cw1 newgrid newclues -- loop back to cw1. when the list of newclues reaches [], return the grid
   
tryWordD grid clues clue word = let
   newgrid  = fillGrid clue word grid
   newclues = mapMaybe (fillClue clue word) clues
   
   in (newgrid, newclues)
   
   
-- insert a word into the grid
fillGridH :: String -> Int -> Int -> Grid -> Grid
fillGridH word x y = zipWith
   (\thisy line -> if thisy /= y 
                      then line
                      else take x line ++ word ++ drop (x + length word) line)
   [0..]

fillGridV :: String -> Int -> Int -> Grid -> Grid
fillGridV word x y grid = transpose $ fillGridH word y x (transpose grid)
   
fillGrid :: Clue -> String -> Grid -> Grid
fillGrid clue word = (if horiz clue then fillGridH else fillGridV) word (x0 clue) (y0 clue)

transposec clue = clue { x0 = y0 clue, 
                         y0 = x0 clue,  
                         x1 = y1 clue, 
                         y1 = x1 clue, 
                         horiz = not $ horiz clue }

opposite size clue = clue { x0 = size - 1 - x1 clue, y0 = size - 1 - y1 clue, x1 = size - 1 - x0 clue, y1 = size - 1 - y0 clue }

-- self explanatory
sameClue :: Clue -> Clue -> Bool
sameClue a b = x0 a == x0 b && y0 a == y0 b && horiz a == horiz b

-- return the index of the character at x,y relative to clue c
charOfClue c x y = if horiz c then x - x0 c else y - y0 c



fillClue :: Clue -> String -> Clue -> Maybe Clue
fillClue fillc with otherc
  | sameClue fillc otherc = Nothing -- this removes the clue we've just filled from the list
  | cluesCross fillc otherc = let
              (cx, cy) = crossAt fillc otherc
              cpos     = charOfClue otherc cx cy
              dpos     = charOfClue fillc  cx cy
              newa     = filter (\a -> {-a /= with &&-} a !! cpos == with !! dpos) $ possibles otherc
              
              in Just otherc { possibles = newa, nans = length newa }
              
  | otherwise = let newa = filter (/= with) $ possibles otherc
   
        in  Just otherc { possibles = newa, nans = length newa }

main = fmt $ cwrp $ empty 10

cwrp grid = cwrp1 grid $ clues grid

parMapCs cs f xs = withStrategy (parListChunk (div (length xs) cs) rdeepseq) $ map f xs

cwrp1 :: Grid -> [Clue] -> [Grid]
cwrp1 grid []    = [grid] -- if there are no more clues to fill, we have succeeded!
cwrp1 grid clues = let
   nextclue = minimumBy (compare `on` nans) clues
   poss = possibles nextclue
   n = length poss
   
   in
   
   --map tryWord over the possible words for nextclue
   concat $ parMapCs 28 (tryWordR grid clues nextclue) poss
   --concat $ parMap rseq (tryWordR grid clues nextclue) poss

cwr grid = cwr1 grid $ clues grid

cwr1 :: Grid -> [Clue] -> [Grid]
cwr1 grid []    = [grid] -- if there are no more clues to fill, we have succeeded!
cwr1 grid clues = let
   nextclue = minimumBy (compare `on` nans) clues
   
   in
   
   --map tryWord over the possible words for nextclue
   concatMap (tryWordR grid clues nextclue) $ possibles nextclue

--cwp1 :: Grid -> [Clue] -> [Grid]
--cwpd1 grid []    = [grid] -- if there are no more clues to fill, we have succeeded!
cwrd1 grid clues = let
   nextclue = minimumBy (compare `on` nans) clues
   
   in
   
   --map tryWord over the possible words for nextclue
   map (tryWordRD grid clues nextclue) $ possibles nextclue

-- try word 'word' in position 'clue' and give a list of possible result grids
-- tryWord calls fillGrid and fillClue
-- AND THEN IT CALLS CW1
tryWordR :: Grid -> [Clue] -> Clue -> String -> [Grid]
tryWordR grid oldclues clue word = let
   newgrid  = fillGridR clue word grid
   size     = length grid
   -- newclues = clues newgrid 
   newclues = mapMaybe (fillClue (transposec clue) word) $ mapMaybe (fillClue clue word) oldclues -- fillClue removes the just filled clue
   
   in cwr1 newgrid newclues -- loop back to cw1. when the list of newclues reaches [], return the grid
   
tryWordRD grid clues clue word = let
   newgrid  = fillGridR clue word grid
   size     = length grid
   newclues = catMaybes $ map (fillClue clue word) clues -- fillClue removes the just filled clue
   
   in (newgrid, newclues)
   
fillGridR clue word grid = fillGrid clue word $
                           fillGrid (transposec clue) word grid
                      
fillClueR :: Int -> Clue -> String -> Clue -> Maybe Clue
fillClueR size fillc with otherc = 
   if      sameClue   fillc otherc then Nothing -- this removes the clue we've just filled from the list
   else let
              -- (cx, cy) = crossAt fillc (transposec otherc)
              fpos1    = y0 otherc
              opos1    = y0 fillc
              newa     = filter (\possible -> possible !! opos1 == with !! fpos1) $ possibles otherc
              
              in Just otherc { possibles = newa, nans = length newa }
     
cws grid = cws1 grid $ cluesS (length grid) $ cluesVocab vocabr grid

cws1 :: Grid -> [Clue] -> [Grid]
cws1 grid []    = [grid] -- if there are no more clues to fill, we have succeeded!
cws1 grid clues = let
   nextclue = minimumBy (compare `on` nans) clues
   
   in
   
   --map tryWord over the possible words for nextclue
   concatMap (tryWordS grid clues nextclue) $ possibles nextclue

--cwp1 :: Grid -> [Clue] -> [Grid]
--cwpd1 grid []    = [grid] -- if there are no more clues to fill, we have succeeded!
cwsd1 grid clues = let
   nextclue = minimumBy (compare `on` nans) clues
   
   in
   
   --map tryWord over the possible words for nextclue
   map (tryWordSD grid clues nextclue) $ possibles nextclue

-- try word 'word' in position 'clue' and give a list of possible result grids
-- tryWord calls fillGrid and fillClue
-- AND THEN IT CALLS CW1
tryWordS :: Grid -> [Clue] -> Clue -> String -> [Grid]
tryWordS grid oldclues clue word = let
   newgrid  = fillGridS clue word grid
   size     = length grid
   -- newclues = clues newgrid 
   newclues = mapMaybe (fillClueS size clue word) oldclues -- fillClue removes the just filled clue
   
   in cws1 newgrid newclues -- loop back to cw1. when the list of newclues reaches [], return the grid
   
tryWordSD grid clues clue word = let
   newgrid  = fillGridS clue word grid
   size     = length grid
   newclues = mapMaybe (fillClueS size clue word) clues -- fillClue removes the just filled clue
   
   in (newgrid, newclues)
   
fillGridS clue word grid = fillGrid clue word $
                           fillGrid (transposec clue) word $
                           fillGrid (opposite l clue) (reverse word) $
                           fillGrid (transposec $ opposite l clue) (reverse word) grid
                             where
                               l = length grid
                      
fillClueS :: Int -> Clue -> String -> Clue -> Maybe Clue
fillClueS size fillc with otherc = 
   if      sameClue   fillc otherc then Nothing -- this removes the clue we've just filled from the list
   else let
              -- (cx, cy) = crossAt fillc (transposec otherc)
              fpos1    = y0 otherc
              opos1    = y0 fillc
              fpos2    = size - 1 - fpos1
              opos2    = size - 1 - opos1
              newa     = filter (\possible -> possible !! opos1 == with !! fpos1 && possible !! opos2 == with !! fpos2) $ possibles otherc
              
              in Just otherc { possibles = newa, nans = length newa }
     
{-                 01234
fillc = x0=0 y0=0 0STANG4  1 3
otherc  x0=0 y0=1 1TENON3  0 4
                  2AN NA2
                  3NONET1
                  4GNATS0
                   43210  
crossAt 0,1 and 1,0 and 0,4 and 4,0
-}
biden = ["biden",
         "i e o",
         "demon",
         "e o c",
         "nonce"]

easy = ["ado",
        " # ",
        "   "]

easy1 = ["     ",
         " ### ",
         " ### ",
         " ### ",
         "     "]

easy2 = ["       ",
         " # # # ",
         "       ",
         " # # # ",
         "       ",
         " # # # ",
         "       "]

hard = ["######## ########",
        "######## ########",
        "######## ########",
        "######## ########",
        "######## ########",
        "######## ########",
        "######## ########",
        "######## ########",
        "                 ",
        "######## ########",
        "######## ########",
        "######## ########",
        "######## ########",
        "######## ########",
        "######## ########",
        "######## ########",
        "######## ########"]

hard1 = ["      ## ##      ",
         " # ###     ### # ",
         "     # # # #     ",
         " # # #     # # # ",
         " #   # # # #   # ",
         " ### #     # ### ",
         "#      # #      #",
         "######## ########",
         "                 ",
         "######## ########",
         "#      # #      #",
         " ### #     # ### ",
         " #   # # # #   # ",
         " # # #     # # # ",
         "     # # # #     ",
         " # ###     ### # ",
         "      ## ##      "]

hard2 = ["      ## ##      ",
         " # ###     ### # ",
         " #     # #     # ",
         " # # #     # # # ",
         "     # # # #     ",
         " ### #     # ### ",
         "#      # #      #",
         "# # # ## ## # # #",
         "                 ",
         "# # # ## ## # # #",
         "#      # #      #",
         " ### #     # ### ",
         "     # # # #     ",
         " # # #     # # # ",
         " #     # #     # ",
         " # ###     ### # ",
         "      ## ##      "]

hard3 = ["                 ",
         " # # # # # # # # ",
         "           #     ",
         " # # # # # # # # ",
         "         #       ",
         " ### # # # # # # ",
         "       #         ",
         " # ### # ### # # ",
         "        #        ",
         " # # ### # ### # ",
         "         #       ",
         " # # # # # # ### ",
         "       #         ",
         " # # # # # # # # ",
         "     #           ",
         " # # # # # # # # ",
         "                 "]

empty n = replicate n $ replicate n ' '

{-
leon
ergo
ogre
noel
-}

rotate901 l = fromMaybe '#' $ lookup l $ zip "caduwem3r7jlnzi-ox" "aducem3w7jlrzn-iox"

rotate90 = map rotate901

rotate180 = rotate90 . rotate90 . reverse

vocabrof v = let forwards  = S.fromList v
                 backwards = S.fromList $ map rotate180 v
             in  S.toList $ S.intersection forwards backwards


