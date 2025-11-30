{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use ((h:_)->h)" #-}
{-# LANGUAGE LambdaCase #-}
-- Copyright 2025 Brett Curtis
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Wiki1 where

import Favs
import HTML
import MHashDynamic2
import MyPretty2
import Table1

import Control.Monad
import Data.Char
import Data.List
import Data.Map qualified as M

-- wikiTable m i u = fromGrid i $ textGrid $ wikiGrid m u

wikiTextGrid m = cTextGrid . wikiTable m
wikiTextGridH m = cTextGridH . wikiTable m
wikiTextGrids m = map cTextGrid . wikiTables m
wikiTextGridsH m = map cTextGridH . wikiTables m

wikiGrid m = cGrid . wikiTable m
wikiGridH m = cGridH . wikiTable m
wikiGrids m = map cGrid . wikiTables m
wikiGridsH m = map cGridH . wikiTables m

wikiTable m = head . wikiTables m
wikiTables m title = findTrees (\t -> tagType t == "table" && classCon "wikitable" t) $ getNested m $ wiki title
wikiTabs = findTrees (\t -> tagType t == "table" && classCon "wikitable" t)

wiki title = "https://en.wikipedia.org/wiki/" ++ spacesToUnders title

wikiHist m p =
   map
      ( \t ->
            let
               d = findClass "mw-changeslist-date" t
            in
               ( tagAttrib "href" d
               , extractText d
               , tagAttrib "href" $ findTree (tagClass $< "mw-userlink") t
               , readInt $ tagAttrib "data-mw-bytes" $ findTree (tagClass $< "history-size") t
               , extractText $ findTree (tagClass $< "comment") t
               )
      )
      $ findTypes "li"
      $ findTree (\t -> tagType t == "section" && tagId t == "pagehistory")
      $ getNested m ("https://en.wikipedia.org/w/index.php?title=" ++ spacesToUnders p ++ "&action=history")

wikiHist1 m p = putGrid $ transpose1 $ map showT $ wikiHist m p

nobels m = findClasses "mw-parser-output" $ getNested m $ wiki "List of Nobel laureates by country"

countries m =
   putMap1 $
      foldUnion $
         map (mapGrid "country" . cTextGridH) $
            wikiTables m "List of countries by past and projected future population"

nato m = M.fromList $ map (\row -> (extractText $ findType "a" $ row !! 2, ())) $ drop 1 $ wikiGridH m "Member states of NATO"

gdp m = M.fromList $ map (\row -> (extractText $ findType "a" $ row !! 0, readInt $ extractText $ row !! 2)) $ drop 3 $ wikiGridH m "List of countries by GDP (nominal)"

cpop m = M.fromList $ map (\row -> (extractText $ findType "a" $ row !! 1, readInt $ extractText $ row !! 2)) $ drop 1 $ wikiGridH m "List of countries and dependencies by population"

area m = M.fromList $ map (\row -> (extractText $ findType "a" $ row !! 1, readInt $ extractText $ row !! 2)) $ drop 1 $ wikiGridH m "List of countries and dependencies by area"

mileq m = fromGridN 0 $ map (\row -> (extractText $ findType "a" $ row !! 0) : (map extractText $ init $ tail row)) $ wikiGridH m "List of countries by level of military equipment"

milper m = fromGridN 0 $ map (\row -> (extractText $ findType "a" $ row !! 1) : (map extractText $ drop 2 row)) $ wikiGridH m "List of countries by number of military and paramilitary personnel"

text1 t =
   let
      as = findTypes "a" t
      in
      if null as then extractText t else extractText $ (\(h : _) -> h) as

-- allies m = fromGridN 0 $ padRWith "" $ map (\row -> (concatMap extractText $ findTypes "a" $ row !! 1) : (map extractText $ findTypes "a" $ row !! 2)) $ concat $ drop 1 $ map convertGridH $ init $ wikiGrids m "List of military alliances"
allies m =
   fromGridT 0 ["name", "years", "country"] $
      padRWith (String1 "") $
         map (\row -> [String1 $ text1 $ row !! 1, String1 $ text1 $ row !! 0, List $ map (String1 . extractText) $ findTypes "a" $ row !! 2]) $
            padRWith (Text "") $
               allies1 m

allies1 m = concat $ init $ wikiGridsH m "List of military alliances"

allies2 m =
   let
      g = padRWith (Text "") $ allies1 m
      in
      TableM (M.fromList $ zip ["country"] [0 ..]) $ M.fromList $ map (\row -> (String1 $ text1 $ row !! 1, map (singleton . String1 . extractText) $ findTypes "a" $ row !! 2)) g

-- allies4 row = (String1 $ text1 $ row !! 1, map
{-}
join m l r =
      sum $
      map (\(c, (l, Just r)) -> r) $
      M.toList $
      joinMapsLeft (l m) (r m)

natogdp m = join m nato gdp

natopop m = join m nato cpop

natoarea m = join m nato area

join2 l r = (sum m, lo)

      where
         li = length l
         lo = length m
         m = mapMaybe (`M.lookup` r) l

join3 l r = (map sum $ transpose m, lo)
      where
         li = length l
         lo = length m
         m = mapMaybe (`M.lookup` r) l

-}
{-
stats cs (g, p, a, me, mp) = let
      j1 = join (? "country") cs g
      j2 = join (? "country") j1 p
      j3 = join (? "country") j2 a
      j4 = join (? "country") j3 me
      j5 = join (? "country") j4 mp
      rows = transpose $ map snd $ M.toList $ records j5
      s = map sum $ transpose rows
      in Table (fields j5) $ M.insert (String1 "Total") s $ records j5

stats1 cs (g, p, a, me, mp) = let
      j1 = join (? "country") cs g
      j2 = join (? "country") j1 p
      j3 = join (? "country") j2 a
      j4 = join (? "country") j3 me
      j5 = join (? "country") j4 mp
      rows = transpose $ map snd $ M.toList $ records j5
      in map sum $ transpose rows
-}
concatT :: [Table] -> Table
concatT ts = Table (fields $ (\(h : _) -> h) ts) $ M.fromList $ concatMap (M.toList . records) ts

{-
allystats m = Table (M.fromList $ zip (["alliance", "countries", "gdp", "population", "area", "mileq", "milper"]) [0..]) $ M.map (\v -> stats1 (v ? "country") gpa) $ records $ allies m

      where
         g = gdp m
         p = cpop m
         a = area m
         me = mileq m
         mp = milper m
         gpa = (g, p, a, me, mp)
-}
{-
allystats m = M.map (\cs -> let (s, o) = stats cs gpa in [show $ length cs] ++ map show s ++ map show o) (allies m)
      where
         g = gdp m
         p = cpop m
         a = area m
         me = mileq m
         gpa = (g, p, a, me)
-}

vulg m = vulg1 m "https://en.wiktionary.org/w/index.php?title=Category:English_vulgarities&from=A"

vulg1 m url = do
   putStrLn url
   let html = getNested m url
   let nexturl = replaceEntities $ tagAttrib "href" $ findTree (\t -> tagType t == "a" && extractText t == "next page") html
   let res1 = map extractText $ findTrees (tagType $= "li") $ findTree (\t -> "mw-category" `isInfixOf` tagClass t) html
   rest <- if null nexturl then return [] else vulg1 m $ relTo url nexturl
   return $ res1 ++ rest

biden = Favs.counts $ words $ map ((\a -> if isAsciiLower a then a else ' ') . toLower) $ extractText $ nestParse $ readFileU "biden.html"

cbr1 m = cTextGrid $ (!! 1) $ findTypes "table" $ getNested m "https://www.ethnicity-facts-figures.service.gov.uk/crime-justice-and-the-law/courts-sentencing-and-tribunals/prosecutions-and-convictions/latest/"

{-
cbr m = zipDims [(map toDyn races !!), (map toDyn ["%", "count"] !!), (map toDyn crimes !!)]
      $ groupN1 2 $ fromList2z 0 $ map (map readInt . drop 2) $ tail $ cbr1 m
-}

-- races = ["Asian", "Black", "Mixed", "White", "Other"]

-- crimes = ["Criminal damage and arson", "Drugs", "Fraud", "Miscellaneous", "Possession of weapons", "Public order", "Robbery", "Sexual", "Theft", "Violence"]
-- >>> 2+3
cbr m =
   let
      a = cbr1 m
      ((_ : crimes) : b) = a
      c = map (!! 1) $ groupN 2 b
      races = map (\(h : _) -> h) c
      t = Data.List.transpose $ zipWith (\p -> map (/ p)) [7, 3, 2, 87.1, 0.9] $ map (map readNum . drop 2) c
      s = map sum t
      in
      (crimes :) $ zipWith (:) races $ map2 (show . round . (/ 0.66)) $ Data.List.transpose t

-- \$ zipWith (\s c -> map (/ s) c) s t

-- rmt m = ph $ getNestedReq m $ addFBCookie $ getRequest "https://www.facebook.com/profile.php?id=100088422993385"

u = "Comparison_of_HTML_editors"

wikiJoin m url = putGrid $ transpose $ joinTLists "" $ map init $ wikiTextGridsH m url

writeCsv file grid = writeFileBinary file $ lbsofs $ unlines $ map (intercalate "," . map quote) grid

huge m = do
   let url = wiki "Lists of sovereign states and dependent territories.html"
   let index = readNestedUrl m url
   let lists = concatMap (findTypes "li") $ findTypes "ul" index
   let names = map extractText lists
   let links = map extractLinks lists
   putGrid $ names : transposez "" links
   let links0 = concat links
   let links1 = map (relTo url) links0
   putStrLn $ show (length links1) ++ " pages to get"
   grids <-
      concat
         <$> mapM
            ( \n -> do
                  putStrLn $ show n
                  g <- readNestedUrl1 m $ links1 !! n
                  let g1 = map cTextGridH $ wikiTabs g
                  mapM (putGrid . transpose) g1
                  let g2 = map (tmofl1 (links0 !! n)) g1
                  return g2
            )
            [0 .. length links1 - 1]
   let grids1 = take 200 grids
   putStrLn $ show (length grids1) ++ " grids to join"
   let grids2 = map (loftm1 ("", "")) $ scanm1 (joinTMapsFuzzy "") grids1
   mapM_ (\n -> putStrLn $ "grid " ++ show n ++ " is " ++ show (length $ (\(h : _) -> h) $ grids2 !! n) ++ "x" ++ show (length $ grids2 !! n)) [0 .. length grids2 - 1]
   let grid = last grids2
   -- writeFileBinary "output.html" $ Https.bsofs $ formatH 1 $ textGridH $ joinTLists "" grids
   writeCsv "countries.csv" grid
   putStrLn "written to countries.csv"

huge1 m = do
   let url = wiki "states0.html"
   index <- readNestedUrl1 m url
   let lists = concatMap (findTypes "li") $ findTypes "ul" index
   let names = map extractText lists
   let links = map extractLinks lists
   putGrid $ names : transposez "" links
   let links0 = reverse $ concat links
   let links1 = map (relTo url) links0
   putStrLn $ show (length links1) ++ " pages to get"
   res <- foldM (cjpage m) ([], M.empty) links0
   writeCsv "countries.csv" $ loftm1 ("", "") res
   return res

cjpage m tm url = do
   h <- readNestedUrl1 m url
   let gs1 = wikiTabs h
   putStrLn $ show (length gs1) ++ " grids"
   let gs2 = mapMaybe (cjgrid1 url) gs1
   let gs3 =
            if not (null gs2) && allEqual (map fst gs2) && allDiff (map (map fst . M.toList . snd) gs2)
               then [(fst $ (\(h : _) -> h) gs2, foldr M.union M.empty $ map snd gs2)]
               else gs2
   foldM cjgrid2 tm gs3

gd g = show (length $ (\(h : _) -> h) g) ++ "x" ++ show (length g)
tmd (f, r) = show (length f) ++ "x" ++ show (M.size r)

cjgrid1 u g =
   let
      tg = cTextGridH g
      in
      ifJust (not $ classCon "mw-collapsed" g) $ tmofl1 (last $ split "/" u) tg

cjgrid2 tm1 g2 = do
   let res = joinTMapsFuzzy "" g2 tm1
   putStrLn $ "grid=" ++ tmd g2 ++ " accum=" ++ tmd res ++ " fields=" ++ show (let f = fst g2 in map fst (take 1 f) ++ map snd f)
   return res

cjgridA1 u g =
   let
      tg = cTextGridH g
      in
      ifJust (not $ classCon "mw-collapsed" g) $ tmofl1 (last $ split "/" u) tg

cjgridA2 tm1 g2 = do
   let res = joinTMapsFuzzy "" g2 tm1
   putStrLn $ "grid=" ++ tmd g2 ++ " accum=" ++ tmd res ++ " fields=" ++ show (let f = fst g2 in map fst (take 1 f) ++ map snd f)
   return res

cross1 (fs, rs) = sort $ concat $ crossWith (\f (i, r) -> ((i, fs !! f), r !! f)) [0 .. length fs - 1] $ M.toList rs
uncross1 ifcs =
   let
      l = M.toList ifcs
      is = nubSet $ map (fst . fst) l
      fs = nubSet $ map (snd . fst) l
      in
      (fs, M.fromList $ map (\i -> (i, map (\f -> fromMaybe "" $ M.lookup (i, f) ifcs) fs)) is)

canMerge a b = isJust $ canMerge1 a b

canMerge1 a@(afs, ars) b@(bfs, brs) =
   let
      ifcs = cross1 b
      afm = M.fromList $ zip afs [0 ..]
      found = map (\((i, f), c) -> (ars M.! i) !! (afm M.! f)) ifcs
      in
      ifJust (not (allDiff $ nubSet $ afs ++ bfs) && all null found) (afm, ifcs)

merge a b = case canMerge1 a b of
   Nothing -> joinTMapsFuzzy "" b a
   Just c -> Wiki1.merge1 c a

merge1 (afm, ifcs) a@(afs, ars) = foldr (set afm) a ifcs

set afm ((i, f), c) (fs, rs) = (fs, M.update (Just . Wiki1.replaceElem (afm M.! f) c) i rs)

replaceElem i e x =
   let
      (a, (_ : b)) = splitAt i x
      in
      a ++ e : b

quote xs = "\"" ++ concatMap quote1 xs ++ "\""

quote1 '"' = "\""
quote1 '\t' = "\\t"
quote1 '\n' = "\\n"
quote1 '\r' = "\\r"
quote1 x = if x < ' ' then "\\0" ++ show (toOctal $ ord x) else [x]

toOctal = unfoldr (\x -> ifJust (x > 0) (let (q, r) = divMod x 8 in (chr (x + ord '0'), r)))

main = do
   m <- nm
   huge1 m
