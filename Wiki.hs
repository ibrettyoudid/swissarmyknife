-- Copyright 2025 Brett Curtis
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use head" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}
{- HLINT ignore "Use map once" -}

module Wiki where

import Favs hiding (readNum, split, replace, ($<))
import HTMLB
import MHashDynamic2 hiding ((?), toList2)
import MyPretty2
import NumberParsersB
import Table hiding (intC, insertWith4)
import ShowTuple
import BString
import Atto
import qualified BString as B
import qualified Tree as T

import Prelude hiding (null, init, tail, head, elem, length, (++), (!!), toLower, split, last, take, drop, notElem, concat, takeWhile, dropWhile, putStrLn, putStr)
import Data.List (singleton, transpose, sort, elemIndex, findIndex)
import Data.Char
import qualified Data.Map as M

import Control.Monad hiding (join)
import Control.Exception hiding (try)
import Control.Applicative

import System.Mem

insertWith4 :: (ByteString, a) -> M.Map ByteString a -> M.Map ByteString a
insertWith4 (k, v) m =
   case M.lookup k m of
      Just j -> let
         k1 = takeWhile (/= convertChar '_') k -- reverse (dropWhile isDigit $ reverse k)
         n1 = readNum $ drop (length k1 + 1) k
         k2 = head $ mapMaybe (\n2 -> let
            k3 = k1 ++ cons (convertChar '_') (c $ show n2)
            in case M.lookup k3 m of
                  Just j2 -> Nothing
                  Nothing -> Just k3) [n1+1..]
         in M.insert k2 v m
      Nothing -> M.insert k v m

-- wikiTable m i u = fromGrid i $ textGrid $ wikiTable m u
instance UniqueList ByteString where
   uniquify = foldl (flip insertWith4) M.empty
   showField x = [c x]



wikiTable   m = cTextTableH   . getWikiTable  m
wikiTableD  m = cTextTableHD  . getWikiTable  m
wikiTables  m = cTextTablesH  . getWikiTables m
wikiTablesD m = cTextTablesHD . getWikiTables m

cTextTable   = fromGrid   . cTextGrid
cTextTableH  = fromGridH  . cTextGridH
cTextTableHD = fromGridHD . map2 c . cTextGridH
cTextTables   = map cTextTable
cTextTablesH  = map cTextTableH
cTextTablesHD = map cTextTableHD

wikiTextGrid   m = cTextGrid  . getWikiTable m
wikiTextGridH  m = cTextGridH . getWikiTable m
wikiTextGrids  m = map cTextGrid  . getWikiTables m
wikiTextGridsH m = map cTextGridH . getWikiTables m

wikiGrid   m = cGrid  . getWikiTable m
wikiGridH  m = cGridH . getWikiTable m
wikiGrids  m = map cGrid  . getWikiTables m
wikiGridsH m = map cGridH . getWikiTables m

getWikiTable m = head . getWikiTables m
getWikiTables m title = filterWikiTables $ readWriteHTML m $ wiki title
filterWikiTables = findTrees (\t -> tagType t == "table" && classCon "wikitable" t)

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
               , parseNoFail 0 intC $ tagAttrib "data-mw-bytes" $ findTree (tagClass $< "history-size") t
               , extractText $ findTree (tagClass $< "comment") t
               )
      )
      $ findTypes "li"
      $ findTree (\t -> tagType t == "section" && tagId t == "pagehistory")
      $ getHTML m ("https://en.wikipedia.org/w/index.php?title=" ++ spacesToUnders p ++ "&action=history")

wikiHist1 m p = putGrid $ transpose1 $ map showT $ wikiHist m p

a -= b = join jInner bzero a b

nobels1 m = findClasses "mw-parser-output" $ getHTML m $ wiki "List of Nobel laureates by country"

nato m = M.fromList $ map (\row -> (extractText $ findType "a" $ row !! 2, ())) $ drop 1 $ cGridH $ getWikiTable m "Member states of NATO"

cont m = by (? "country") $ setFields ["country", "geo", "inter", "continent"] $ wikiTableD m "List of countries and territories by the United Nations geoscheme"

europe m = byl [(? "country")]
   $ fromGridH1 ["country", "member"] $ map (map (toDyn . extractText) . (\row -> findType "a" (head row) : take 1 (tail row))) $ drop 1 $ wikiGridsH m "List of sovereign states and dependent territories by continent" !! 2

gdp  m = by (? "country") $ setFields ["country"] $ wikiTableD m "List of countries by GDP (nominal)"

gdp1 m = by (? "country") $ setFields ["country"] $ wikiTableD m "List of countries by GDP (PPP)"

cpop m = by (? "Location") $ wikiTableD m "List of countries and dependencies by population"

area m = by (? "country") $ setFields ["rank", "country", "area", "land area", "water area", "%water", "blah"] $ wikiTableD m "List of countries and dependencies by area"

readNum = parseNoFail 0 floating

mileq m =
   by (? "country")
      $ fromGridH1 ["country", "budget", "tanks", "carriers", "aws", "cruisers", "destroyers", "frigates", "corvettes", "nuclear subs", "subs", "planes", "helicopters", "nukes", "satellites"]
      $ map
         ( \row ->
               (toDyn $ extractText $ findType "a" $ row !! 0)
                  : (toDyn $ readNum $ concat $ map extractText $ take 2 $ drop 1 row)
                  : (map (toDyn . readNum . extractText) $ init $ drop 3 row)
         )
      $ drop 1
      $ wikiGridH m "List of countries by level of military equipment"

milper m = by (? "Country") $ wikiTableD m "List of countries by number of military and paramilitary personnel"

bzero = toDyn (BString.empty :: ByteString)

join2 xs = foldl1 (joinCollectMisses jLeft bzero) xs

stats m = foldl1 (join jLeft bzero) [cpop m, {-fst $ gdph m, fst $ gdph1 m,-} area m, milper m]

gdph m = foldl1 (joinCollectMisses jLeft bzero) $ map (miss . by (? "country") . setFields ["country"])$ take 7 $ wikiTablesD m "List of countries by past and projected GDP (nominal)"

gdph1 m = foldl1 (joinCollectMisses jLeft bzero) $ map (miss . by (? "country") . setFields ["country"])$ take 5 $ wikiTablesD m "List of countries by past and projected GDP (PPP)"

eustats m = foldSubTable3 sum
   $ byl [(? "isRussia"), (? "country")]
   $ addCalcField "isRussia" (\r -> toDyn (r ? "country" == toDyn ("Russia" :: ByteString)))
   $ join jInner bzero (europe m) (stats m)
-- milper m = fromGridN 0 $ map (\row -> (extractText $ findType "a" $ row !! 1) : (map extractText $ drop 2 row)) $ convertGridH $ wikiTable m "List of countries by number of military and paramilitary personnel"

ip m = join jInner bzero (index $ map toDyn ["India"::ByteString, "Pakistan"]) $ stats m

text1 t =
   let
      as = findTypes "a" t
      in
      if null as then extractText t else extractText $ head as

-- allies m = fromGridN 0 $ padRWith "" $ map (\row -> (concatMap extractText $ findTypes "a" $ row !! 1) : (map extractText $ findTypes "a" $ row !! 2)) $ concat $ drop 1 $ map convertGridH $ init $ wikiGrids m "List of military alliances"
{-
allies m = fromGridT 0 ["name", "years", "country"] $ padRWith (String1 "")
      $ map (\row -> [String1 $ text1 $ row !! 1, String1 $ text1 $ row !! 0, List $ map (String1 . extractText) $ findTypes "a" $ row !! 2])
      $ padRWith (Text "") $ concatMap convertGridH $ init $ wikiGrids m "List of military alliances"
-}
allies1 m = concatMap cGridH $ init $ getWikiTables m "List of military alliances"

{-
allies2 m = let
      g = padRWith (Text "") $ concatMap convertGridH $ init $ wikiGrids m "List of military alliances"

      in TableM (M.fromList $ zip ["country"] [0..]) $ M.fromList $ map (\row -> (String1 $ text1 $ row !! 1, map (singleton . String1 . extractText) $ findTypes "a" $ row !! 2)) g
-}
allies3 m =
   let
      g = padRWith (Text "") $ concatMap cGridH $ init $ getWikiTables m "List of military alliances"
      in
      Table (M.fromList $ zip ["country"] [0 ..]) $
         INode $
            M.fromList $
               map
                  ( \row ->
                        ( toDyn $ text1 $ row !! 1
                        , Recs $ tz $ map (Rec . tz . singleton . toDyn . extractText) $ findTypes "a" $ row !! 2
                        )
                  )
                  g

allies4 m =
   let
      g = padRWith (Text "") $ concatMap cGridH $ init $ getWikiTables m "List of military alliances"
      in
      fromGrid1 ["group", "country"] $
         concatMap
            ( \row ->
                  map (\country -> [toDyn $ text1 $ row !! 1, toDyn $ extractText country]) $
                     findTypes "a" $
                        row !! 2
            )
            g

allies5 m =
   let
      g = padRWith (Text "") $ concatMap cGridH $ init $ getWikiTables m "List of military alliances"
      in
      byl [(? "group"), (? "country")] $
         fromGrid1 ["group", "country"] $
            concatMap (\row -> map (\country -> [toDyn $ text1 $ row !! 1, toDyn $ extractText country]) $ findTypes "a" $ row !! 2) g

tt = fromGridHD $ map2 show $ crossWith (*) [1..10] [1..10]
{-allies6 m = let
      me = mileq m
      a  = allies5 m
      in --mapSubTable (putStr
         mapSubTable (\t -> justTotals $ join t me) 1 a

--addTotals (Table f g) = Table f $ addTotalsG g
--justTotals (Table f g) = Table f $ bleurgh g

bleurgh g = Recs $ T.singleton $ T.fromElems $ totals1 g
--foldTableG f z  (Map m) = foldr f z $ map (f . snd) $ M.toList m
--addTotalsG g@(Map m) = Map $ M.insert (toDyn "~Total") (bleurgh g) m

totals1 t@(Table flds g) = foldSubTable ((mode . (?"group")) : (const $ toDyn "Total") : map (\x -> (sum . (? x)) :: Record [Dynamic] -> Dynamic) (fieldsUT t)) t
-}

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
{-
concatT :: [Table] -> Table
concatT ts = Table (fields $ head ts) $ M.fromList $ concatMap (M.toList . records) ts
-}
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
   B.putStr url
   let html = getHTML m url
   let nexturl = replaceEntities $ tagAttrib "href" $ findTree (\t -> tagType t == "a" && extractText t == "next page") html
   let res1 = map extractText $ findTrees (tagType $= "li") $ findTree (\t -> "mw-category" `isInfixOf` tagClass t) html
   rest <- if null nexturl then return [] else vulg1 m $ relTo url nexturl
   return $ res1 ++ rest

--biden = Favs.counts $ words $ map ((\a -> if isAsciiLower a then a else ' ') . toLower) $ extractText $ nestParse $ readFileU "biden.html"

cbr1 m = cTextGrid $ (!! 1) $ findTypes "table" $ getHTML m "https://www.ethnicity-facts-figures.service.gov.uk/crime-justice-and-the-law/courts-sentencing-and-tribunals/prosecutions-and-convictions/latest/"

{-
cbr m = zipDims [(map toDyn races !!), (map toDyn ["%", "count"] !!), (map toDyn crimes !!)]
      $ groupN1 2 $ fromList2z 0 $ map (map readInt . drop 2) $ tail $ cbr1 m
-}

-- races = ["Asian", "Black", "Mixed", "White", "Other"]

-- crimes = ["Criminal damage and arson", "Drugs", "Fraud", "Miscellaneous", "Possession of weapons", "Public order", "Robbery", "Sexual", "Theft", "Violence"]

cbr m =
   let
      a = cbr1 m
      crimes = tail $ head a
      b = tail a
      --((_ : crimes) : b) = a
      d = map (!! 1) $ groupN 2 b
      races = map head d
      t = Data.List.transpose $ zipWith (\p -> map (/ p)) [7, 3, 2, 87.1, 0.9] $ map (map readNum . drop 2) d
      s = map sum t

   in cons crimes $ zipWith cons races $ map2 (c . show . round . (/ 0.66)) $ Data.List.transpose t

-- \$ zipWith (\s c -> map (/ s) c) s t

mapOfGrid = M.fromList . map (\[k, v] -> (extractText k, v))

wikiInfo m url = mapOfGrid $ cGridH $ findTree (classCon "infobox") $ getHTML m url

wikiInfo1 cat m url = fromMaybe "Unknown" $ fmap extractText $ M.lookup cat $ wikiInfo m url

nationality = wikiInfo1 "Nationality"

birthplace = wikiInfo1 "Born"

nobels m =
   let
      l = wikiGrid m "List of Nobel laureates"
      (h : t) = l
      in
      map (counts . concatMap (map (nationality m . wiki) . extractLinks)) $ drop 1 t

-- rmt m = ph $ getNestedReq m $ addFBCookie $ getRequest "https://www.facebook.com/profile.php?id=100088422993385"

u = "Comparison_of_HTML_editors"

--wikiJoin m url = putGrid $ transpose $ joinTLists "" $ map init $ wikiTextGridsH m url

writeCsv file grid = writeFileBinary file $ c $ unlines $ map (intercalate "," . map show) grid
{-
huge m = do
   let url = wiki "Lists of sovereign states and dependent territories.html"
   let index = readNestedUrl m url
   let lists = concatMap (findTypes "li") $ findTypes "ul" index
   let names = map extractText lists
   let links = map extractLinks lists
   putGrid $ map2 c $ names : transposez "" links
   let links0 = concat links
   let links1 = map (relTo url) links0
   putStrLn $ show (length links1) ++ " pages to get"
   grids <-
      concat
         <$> mapM
            ( \n -> do
                  print n
                  g <- readNestedUrl1 m $ links1 !! n
                  let g1 = map cTextGridH $ filterWikiTables g
                  mapM_ (putGrid . map2 c . transpose) g1
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
   putGrid $ map2 c $ names : transposez "" links
   let links0 = reverse $ concat links
   let links1 = map (relTo url) links0
   putStrLn $ show (length links1) ++ " pages to get"

   res <- foldM (cjpage m) ([], M.empty) links0
   writeCsv "countries.csv" $ loftm1 ("", "") res
   return res


cjpage m tm url = do
   h <- readNestedUrl1 m url
   let gs1 = filterWikiTables h
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
-}
wc m = do
   let url = wiki "states0c.html"
   index <- readWriteHTML1 m url
   let lists = map (findTypes "li") $ findTypes "ul" index
   let names = map2 extractText lists
   let links = map2 extractLinks lists
   mapM_ (putGrid . map2 c) $ zipWith (\n l -> n : transposez "" l) names links
   let links0 = map2 concat links
   let links1 = map2 (relTo url) links0
   putStrLn $ show (length links1) ++ " pages to get"
   let statsm = stats m
   master <- cjpageA m $ wiki "List of countries and dependencies by population"
   putStrLn $ "MASTER=" ++ showTableMeta master
   --let master = Table (uniquify $ zip (zipWith (\fn n -> ("stats", 1, fn, n)) (fields statsm) [1..]) [0..])
   --(res, missing) <- foldMB (cjpageB m jLeft) (master, []) links0
   mapM (\url -> do
      a <- mapM (cjpageB m jLeft master) url
      let b = fold3 (joinFuzzy jOuter 3 bzero) a
      return b) links0
   --writeCsv "countries.csv" $ loftm1 ("", "") res
   {-}
   putStrLn ("writing countriesbig.html" :: ByteString)
   writeHTML "countriesbig.html" $ toHTML res
   putStrLn ("writing countriesbig.csv" :: ByteString)
   writeFile "countriesbig.csv" $ toCsv res
   putStrLn $ showTableMeta res
   -}
   --putStrLn "writing misses"
   --mapM_ (putStrLn . showTableMeta) miss
   --writeFile "misses.csv" $ concatMap toCsv miss

foldMB f z [] = return z
foldMB f z (x:xs) = do
   xr <- f z x
   performMajorGC
   foldMB f xr xs

fold2 f [] = []
fold2 f [a] = [a]
fold2 f [a, b, c] = let
   abr = f a b
   abcr = f abr c
   
   in [abcr]

fold2 f (a:b:xs) = let
   abr = f a b
   xr  = fold2 f xs
   
   in abr : xr

fold3 :: (b -> b -> b) -> [b] -> b
fold3 f xs = let
   xr = fold2 f xs
   
   in case xr of
      [] -> error "must be more than 0"
      [a] -> a
      other -> fold3 f other

foldM2 f [] = return []
foldM2 f [a] = return [a]
foldM2 f [a, b, c] = do
   abr <- f a b
   abcr <- f abr c
   return [abr]
foldM2 f (a:b:xs) = do
   abr <- f a b
   xr  <- foldM2 f xs
   return (abr : xr)

foldM3 f xs = do
   xr <- foldM2 f xs
   case xr of
      [] -> error "must be more than 0"
      [a] -> return a
      other -> foldM3 f xs

foldM1 f [] = error "must be more than 0"
foldM1 f [a] = return a
foldM1 f (a:b:xs) = do
   xr <- f a b
   foldM1 f (xr:xs)

cjpageA m url = do
   h <- readWriteHTML1 m url
   --print h
   let gs1 = filterWikiTables h
   putStrLn $ show (length gs1) ++ " grids"
   return $ fromJust $ cjgridA1 url 0 $ head gs1

cjgridA1 u n g =
   let
      tg = cTextGridH g
      tb = fromGridHD4 u n $ map2 c tg
      tb1 = by (? (wiki "List of countries and dependencies by population", n::Int, 2::Int, "Location")) tb
   in
      ifJust (not $ classCon "mw-collapsed" g) tb1

cjpageB m joinType master url = do
   h <- readWriteHTML1 m url
   let gs1 = filterWikiTables h
   putStrLn $ show (length gs1) ++ " grids"
   gs2 <- zipWithM (cjgridB1 master url) [1..] gs1
   let gs3 = catMaybes gs2
   --res <- foldM (cjgridB2 joinType) (master, missing) $ map miss $ catMaybes gs2
   g4 <- foldM1 (cjgridB2 joinType) gs3
   writeHTML (urlToFileName url) $ toHTML g4
   return g4


cjpageBMissing m joinType (master, missing) url = do
   h <- readWriteHTML1 m url
   let gs1 = filterWikiTables h
   putStrLn $ show (length gs1) ++ " grids"
   gs2 <- zipWithM (cjgridB1 master url) [1..] gs1
   let gs3 = catMaybes gs2
   res <- foldM (cjgridB2Missing joinType) (master, missing) $ map miss gs3
   putStrLn ("writing countriesbig.html" :: ByteString)
   writeHTML "countriesbig.html" $ toHTML $ fst res
   return res


cjgridB1 master u n g = do
   let
      tg = cTextGridH g
      tb = fromGridHD4 (replace "_" " " $ last $ split "/" u) n $ map2 c tg
      ix = findIndexField master tb 3
   putStrLn $ "grid "++show n++"="++showTableMeta tb
   case ix of
      Just ixj -> do
         let tb1 = by (? ixj) tb
         
         putStrLn $ "grid "++show n++"="++showTableMeta tb1
         print tb1
   --print $ Table.lookup (toDyn "France") tb1
         return $
            ifJust (not $ classCon "mw-collapsed" g) tb1

      Nothing -> do
         putStrLn $ b "IIIIII NN   NN DDDDDD  EEEEEEE XX    XX  "
         putStrLn $ b "  II   NNN  NN DD  DDD EE       XX  XX   "
         putStrLn $ b "  II   NNNN NN DD   DD EE        XXXX    "
         putStrLn $ b "  II   NN NNNN DD   DD EEEEEEE    XX     "
         putStrLn $ b "  II   NN  NNN DD   DD EE        XXXX    "
         putStrLn $ b "  II   NN   NN DD  DDD EE       XX  XX   "
         putStrLn $ b "IIIIII NN   NN DDDDDD  EEEEEEE XX    XX NOT FOUND "
         putStrLn $ b ""
         putStrLn $ "grid "++show n++"="++showTableMeta tb
         return Nothing


cjgridB2 joinType g1 g2 = do
   let res = joinFuzzy joinType 3 bzero g1 g2
   print $ showTableMeta2 res
   --putStrLn $ "miss=" ++ showTableMeta (last miss)
   return res

cjgridB2Missing joinType (g1, miss1) (g2, miss2) = do
   let res = joinFuzzy joinType 3 bzero g1 g2
   print $ showTableMeta2 res
   --putStrLn $ "miss=" ++ showTableMeta (last miss)
   return (res, [])

toElems (fs, rs) = sort $ concat $ crossWith (\f (i, r) -> ((i, fs !! f), r !! f)) [0 .. length fs - 1] $ M.toList rs
fromElems ifcs =
   let
      l = M.toList ifcs
      is = nubSet $ map (fst . fst) l
      fs = nubSet $ map (snd . fst) l
   in
      (fs, M.fromList $ map (\i -> (i, map (\f -> fromMaybe "" $ M.lookup (i, f) ifcs) fs)) is)

canMerge a b = isJust $ canMerge1 a b

canMerge1 a@(afs, ars) b@(bfs, brs) =
   let
      ifcs = toElems b
      afm = M.fromList $ zip afs [0 ..]
      found = map (\((i, f), c) -> (ars M.! i) !! (afm M.! f)) ifcs
   in
      ifJust (not (allDiff $ nubSet $ afs ++ bfs) && all null found) (afm, ifcs)

merge a b = case canMerge1 a b of
   Nothing -> joinTMapsFuzzy "" b a
   Just c -> Wiki.merge1 c a

merge1 (afm, ifcs) a@(afs, ars) = foldr (set afm) a ifcs

set afm ((i, f), c) (fs, rs) =
   if M.member i rs
      then (fs, M.update (Just . replaceIndex (afm M.! f) c) i rs)
      else (fs, M.insert i (replicate (afm M.! f) "" ++ [c]) rs)

quote xs = "\"" ++ concatMap quote1 xs ++ "\""

quote1 '"' = "\""
quote1 '\t' = "\\t"
quote1 '\n' = "\\n"
quote1 '\r' = "\\r"
quote1 x = if x < ' ' then "\\" ++ show (ord x) else [x]

--toOctal = unfoldr (\x -> ifJust (x > 0) (let (q, r) = divMod x 8 in (chr (x + ord '0'), r)))

findIndexField1 master tab = let
   INode xs = tgroup master
   list = toList2 $ tgroup tab
   fs = map (\(fieldname, fieldnum) -> (,fieldname) $ M.size $ M.intersection xs $ M.fromList $ map ((, fieldnum) . fromJust . T.lookup fieldnum) list) $ M.toList $ fields tab

   in snd $ maximum fs

findIndexField master tab maxDist =
   snd <$> (
      find ((> fromIntegral (size tab) / 4) . fromIntegral . fst)
         $ map (\f -> (\r -> (size r, f))
            $ joinFuzzy jInner maxDist bzero master
            $ by (? f) tab) 
         $ map fst
         $ M.toList
         $ fields tab)



   --in concatMap T.toList $ toList2 $ tgroup tab
{-
main = do
      m <- nm
      huge1 m
-}
toHTML t = let
   header  = map (Tag "tr" [] . map (Tag "th" [] . singleton . Text)) $ transposez ""  (map (map c . showField) $ fieldsUT t)
   records = map (Tag "tr" [] . map (Tag "td" [] . singleton . Text)) $ transposez "" $ map (map c . showColD) $ transposez (toDyn (""::String)) $ ungroup $ tgroup t

   in html [Text "Countries"] [Tag "table" [] (header ++ records)]

