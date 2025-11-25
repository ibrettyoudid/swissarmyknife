{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}

module Fods where

import Favs
import HTML
import MyDynamic
import MyPretty2
import NumberParsers

import Data.List

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.LocalTime

import Text.Parsec
import Text.Parsec.String

data Type = M | To

type Amount = Int

ymd = YearMonthDay

data Trans = Trans {day :: Day, amount :: Amount, person :: Person, typ :: Type}

-- go = putGridF =<< readfods

names = ["Tyrone", "Danny", "James"]

trans =
   [ (ymd 2023 6 13, -20, Tyrone, M)
   , (ymd 2023 6 13, -30, Tyrone, M)
   , (ymd 2023 6 13, -20, James, M)
   , (ymd 2023 6 13, -25, James, M)
   , (ymd 2023 6 13, -3, James, To)
   , (ymd 2023 6 13, -20, Danny, M)
   ]

-- counts :: (Ord a, Show a) => [a] -> String
-- counts = show . rsort . mode1
countUnique l = S.size $ S.fromList l

groupBy f = combine (:) [] . mapfxx f

applyL fs x = map ($ x) fs

combine2 fs ts rows =
   let
      frows = map (applyL fs) rows

      fcols = transpose frows
      -- rows2 = map (applyL fs) $ transpose rows
      -- in frows ++ (transpose $ padRWith (String1 "") $ zipWith applyL ts fcols)
      in
      zipWith (++) fcols $ padRWith (String1 "") $ zipWith applyL ts fcols

combine3 g fs ts rows =
   let
      frows = map (applyL fs) rows
      fcols = transpose frows
      gcol = map g rows
      gf = combine (:) [] $ zip gcol fcols
      x g f = [g] : zipWith (++) f (padRWith (String1 "") $ zipWith applyL ts f)
      blah = concatMap (transpose . padRWith (String1 "") . uncurry x) gf
      -- rows2 = map (applyL fs) $ transpose rows
      -- in frows ++ (transpose $ padRWith (String1 "") $ zipWith applyL ts fcols)
      in
      blah

readfods f = do
   Right r <- parseFromFile htmlP f
   return $
      map (convertGrid2 f) $
         findTypes "table" $
            nest $
               map (renameAttribs . renameTag) r

renameTag t = setType t $ case tagType t of
   "table:table" -> "table"
   "table:table-row" -> "tr"
   "table:table-cell" -> "td"
   x -> x

renameAttribs t = setAttribs t $ map renameAttrib $ attribs t

renameAttrib (a, v) =
   ( case a of
         "table:number-columns-repeated" -> "colspan"
         x -> x
   , v
   )

trimGrid1 = take 8

trimGrid1a :: [[String]] -> [[String]]
trimGrid1a = takeWhile (any (/= ""))

trimGrid1b :: [[String]] -> [[String]]
trimGrid1b = reverse . dropWhile (all (== "")) . reverse

trimGrid2 = takeWhile (\x -> x !! 2 /= "")

trimGrid2b :: [[String]] -> [[String]]
trimGrid2b = reverse . dropWhile ((!! 2) $= "") . reverse . drop 2

convertGrid1 f = map2 (parse1 csvcell f)

convertGrid2 f table =
   let
      t = take 11 $ cTextGrid table
      n = tagAttrib "table:name" table
      l = maximum $ map length t
      in
      (n, map2 (parse1 fodscell f) t)

readcsvs = concat <$> mapM readCSVFile names

readCSVFile fn = do
   f <- readFile fn
   case parse csv fn f of
      Left l -> do print l; return []
      Right r -> return $ map (toDyn fn :) r

csv = many csvline

csvline = sepBy csvcell $ char ','

dynGrid f = map2 (parse1 fodscell f)

dynOfCell (String1 s) = toDyn s
dynOfCell (Int1 i) = toDyn i
dynOfCell (Integer1 i) = toDyn i
dynOfCell (Double1 d) = toDyn d
dynOfCell (Date d) = toDyn d

fodscell = try dateExcel <|> try number <|> toDyn <$> many anyChar

csvcell = try dateExcel <|> try number <|> txt

dateExcel = do
   dow <-
      string "Mon"
         <|> try (string "Tue")
         <|> string "Wed"
         <|> string "Thu"
         <|> string "Fri"
         <|> try (string "Sat")
         <|> string "Sun"
   char ' '
   dom <- int
   char '/'
   moy <- int
   char '/'
   year <- integer
   return $ toDyn $ ymd (year + 2000) moy dom

number = toDyn <$> int <|> (toDyn <$> floating)

int = fromInteger <$> integer

txt = toDyn <$> do char '"'; t <- manyTill anyChar $ char '"'; char '"'; return t
