-- Copyright 2025 Brett Curtis
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant $" #-}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant multi-way if" -}

module HTMLAny where

import Favs hiding (split, trim, trimtrailing, replace)
import MyPretty2
import NumberParsersAtto
--import NumberParsers hiding (parse, try)
import ShowTuple
import BString
import AttoAny
import AttoT
import AttoB
import qualified Data.Attoparsec.ByteString as AttoB
import qualified AttoAny as AP

import Prelude hiding (null, tail, head, elem, length, (++), (!!), toLower, split, last, take, drop, notElem, concat, takeWhile, dropWhile, putStr, putStrLn, splitAt)
import Data.List (transpose, sort, elemIndex, findIndex)
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.IO as T
--import Data.Text.Lazy.Builder hiding (singleton)
import Data.List (singleton, partition)
import Data.String (IsString)
import Data.Monoid
import Data.Word (Word8)

import Control.Monad
import Control.Applicative hiding (empty, many)
import Data.Char hiding (toLower)
import Data.IORef
import System.IO (openBinaryFile, hClose, IOMode(..))
import System.IO.Unsafe

-- import System.Win32.Process

import Data.Map qualified as M
import Data.Set qualified as S
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.URI
import System.Directory

class Parser p h c => HTMLString p h c where
   getHTTP       :: Manager -> h -> h
   getHTTPIO     :: Manager -> h -> IO h
   getHTTPReq    :: Manager -> Request -> IO h
   getReadHTTP   :: Manager -> h -> h
   getReadHTTPIO :: Manager -> h -> IO h
   getHTML       :: Manager -> h -> HTML h
   getHTMLIO     :: Manager -> h -> IO (HTML h)
   getHTMLReq    :: Manager -> Request -> HTML h
   getReadHTML   :: Manager -> h -> HTML h
   getReadHTMLIO :: Manager -> h -> IO (HTML h)
   readHTML      :: h -> HTML h
   readHTMLIO    :: h -> IO (HTML h)
   nestParse     :: h -> h -> HTML h
--import Text.Parsec
--import Text.Parsec.Combinator
data HTML str
   = Tag      { tagType_ :: str, tagAttribs :: [(str, str)], inner :: [HTML str] }
   | EmptyTag { tagType_ :: str, tagAttribs :: [(str, str)] }
   | EndTag   { tagType_ :: str }
   | Text     { text     :: str }
   deriving (Eq, Ord)

instance ConvertString str String => Show (HTML str) where
   --show html = formatHTML html

-- instance Read HTML where
--    read htmlstring = parseHTML "Read" htmlstring
a m = getHTML m

putLines l = mapM_ putStrLn l

ukulele m = filter (not . null) $ extractLinks $ findTree (\t -> tagType t == "table" && tagAttrib "width" t == "407") $ getHTML m "https://wpu3a.org.uk/Groups/ukulele-for-beginners"

uku m = map (relTo "https://wpu3a.org.uk") $ L.tail $ (ukulele m)

nm :: IO Manager
nm = newTlsManager

cTextGrid  = map2 extractText . cGrid
cTextGridH = map2 extractText . cGridH

cTextGridHBF = map3t (map2 extractText) . cGridHBF
cTextGridHBFH = map3t (map2 extractText) . cGridHBFH

html h b = Tag "html" [] [Tag "head" [] h, Tag "body" [] b]

textGridH = gridH . map2 (singleton . Text)
textGrid = textGridH . transpose

putTextGrid = putGrid . map2 c . cTextGrid

putGridFile :: Text -> IO ()
putGridFile = putTextGrid . readHTML

-- convertGridH tag = map (getTags ["td", "th"] . subTags) $ filter (("tr" ==) . tagType) $ subTags tag
extractText1 = trim . squash . clean . concat . map text . findTrees isText
extractTexts htmls = trim $ squash $ concat $ map tagText htmls

firstText = (\case { [] -> empty; (a:_) -> a }) . mapMaybe (ifPred (not . null) . trim . clean . tagText) . findTrees isText

extractLink = L.head . extractLinks
extractLinks t = map (tagAttrib "href") $ findTypes "a" t

extractTitles t = map (tagAttrib "title") $ findTypes "a" t

extractLinks1 = map (tagAttrib "href") . filter (\t -> typeIs "a" t && hasAttrib "href" t)

extractPics = mapMaybe (\t -> ifJust (tagType t == "img" && hasAttrib "src" t) (tagAttrib "src" t))

-- count rows in each of thead, tbody, tfoot
countHBF tag = (L.length $ map (subTagsType "tr") $ subTagsType "thead" tag,
                L.length ( map (subTagsType "tr") $ subTagsType "tbody" tag) + 
                L.length (                          subTagsType "tr"    tag),
                L.length $ map (subTagsType "tr") $ subTagsType "tfoot" tag)

cGridHBFH tag = let
   caption = extractText $ wrap $ subTagsType "caption" tag
   rows = findTypes "tr" tag
   h = filter        headerrow  rows
   b = filter (not . headerrow) rows
   f = concatMap (subTagsType "tr") $ subTagsType "tfoot" tag
   in map3t (cGridH . wrap) (caption, h, b, f)

headerrow (Tag "tr" attr subs) = most (tagType $= "th") subs

most pred xs = let
   (p, f) = Data.List.partition pred xs
   in L.length p >= L.length f

--headerrow (Tag "tr" attr subs) = unsafePerformIO $ do
--   mapM_ print subs
--   return $ all (tagType $= "th") subs
{-
cGridHBFH tag = let
   hbf = cGridH tag
   (h, b, f) = countHBF tag
   (h1, bf1) = splitAt h hbf
   (b1, f1)  = splitAt (b-h) bf1
   in (h1, b1, f1)
-}
map3t fn (c, h, b, f) = (c, fn h, fn b, fn f)

cGridHBF tag = map3t transpose $ cGridHBFH tag

cGridH tag = padRWith (Text "") $ expandV4 $ map (expand . findTrees (\t -> tagType t `L.elem` ["td", "th"])) (findTypes ("tr") tag)
cGrid = transpose . cGridH
-----------------------------------------------------------------------------------------------------------------------------------
gridH = Tag "table" [] . map (Tag "tr" [] . map (Tag "td" []))
grid = gridH . transpose

--cellA ats (Tag "td" atsin inin) = Tag "td" (mergeAttribs1 ats atsin) inin
cellA ats inner = Tag "td" ats inner
cell = cellA []

olist = Tag "ol" [] . map (Tag "li" [] . singleton)

h1 = Tag "h1" [] . singleton
p = Tag "p" []
s = singleton
(&) = singleton

($<) :: (BString s, Eq c, BStringC c s) => (t -> s) -> s -> t -> Bool
f $< b = \x -> b `isPrefixOf` f x


link url = Tag "a" [("href", url)]
img url = EmptyTag "img" [("src", url)]
textLink url txt = link url $ [Text txt]
imgLink url thumb = link url $ [img thumb]

isSpace2 c = c <= convertChar ' ' || c > convertChar '|'

trim :: (Ord c, ConvertChar Char c, BStringC c s) => s -> s
trim = trimtrailing . dropWhile isSpace2

trimtrailing :: (Ord c, ConvertChar Char c, BStringC c s) => s -> s
trimtrailing xs
   | null xs = empty
   | otherwise = let xst = trimtrailing (tail xs) in if isSpace2 (head xs) && null xst then empty else cons (head xs) xst

clean = smap (\c -> if (c >= convertChar ' ' && c <= convertChar '~') || (c > convertChar '\160' && c <= convertChar '\255') then c else convertChar ' ')

--------------------------------------------------------------------------------
{-
HHH     HHH TTTTTTTTTT TTTTTTTTTT PPPPPPPP
HHH     HHH     TT         TT     PPP    PPP
HHH     HHH     TT         TT     PPP    PPP
HHHHHHHHHHH     TT         TT     PPPPPPPP
HHH     HHH     TT         TT     PPP
HHH     HHH     TT         TT     PPP
HHH     HHH     TT         TT     PPP
-}


simpleHTTP m req = httpLbs req m

getRequest = addUserAgent . unsafePerformIO . parseRequest . c


-------------------------------------------------------------------------------
data P p = P p

instance Functor P where

instance Applicative P where

instance Alternative P where

instance Monad P where

instance Parser P String Char where



instance Parser P String Char => HTMLString P String Char where

instance (BString h, 
   BStringIO h, 
   Ord h, 
   IsString h, 
   ConvertString h String, 
   ConvertString ByteString h, 
   ConvertString LB.ByteString h,
   ConvertChar c Char,
   ConvertChar Word8 c,
   Parser p h c,
   ParserC c,
   MonadFail p,
   BChar c)
      => HTMLString p h c where --getHTML :: (NumberParser parser b Char Integer, MonadFail parser, Ord b,  IsString b, ConvertString b String,  ConvertString LB.ByteString b) => Manager -> b -> HTML b
   getHTTP m url = unsafePerformIO $ getHTTPIO m url
   getHTTPIO m url = do
      let req = getRequest url
      -- print req
      resp <- httpLbs req m
      -- print resp
      return $ c $ responseBody resp
   getReadHTTP m url = unsafePerformIO $ getReadHTTPIO m url
   getReadHTTPIO m url = do
      let fn = c $ urlToFilename url
      str1 <- readBinaryFile fn
      str <- if null str1
               then getHTTPIO m url
               else return str1
      writeBinaryFile fn str
      return str

   getHTTPReq m req = do
      let url = reqPath req
      putStrLn ("getting " ++ show url)
      nsr <- httpLbs req m
      return $ c $ responseBody nsr
   getHTML m url = nestParse url $ getHTTP m url
   getHTMLIO m url = nestParse url <$> getHTTPIO m url
   getHTMLReq m req = nestParse (c $ reqPath req) $ unsafePerformIO $ getHTTPReq m req
   getReadHTML m url = unsafePerformIO $ getReadHTMLIO m url
   getReadHTMLIO m url = nestParse url <$> getReadHTTPIO m url
   nestParse f = nest . parseHTML f
   readHTML f = unsafePerformIO $ readHTMLIO f
   readHTMLIO f = nestParse f <$> readBinaryFile (c f)


mytrace x = unsafePerformIO (do print x; return x)

-- $ (\weird -> if null weird then xs else error weird) $ mapMaybe (ifPred isWeird) xs

-- getHTTPIO with refering page as first parameter
--lbsofs = LT.pack . map (fromIntegral . ord)

--soflbs = map chr . filter (\x -> x == 10 || x == 13 || x >= 32 && x <= 127 || x >= 160 && x <= 255) . map fromIntegral . LT.unpack

getHTTPRefr m ref url = getHTTPReq m (getRequestRefr (convertString ref) url)

reqPath = Network.HTTP.Client.path

reqURI = fromJust . parseURI . c . reqPath


urlToFilename = unsafePerformIO . urlToFilenameIO

urlToFilenameIO :: (ConvertString a String, ConvertString ByteString a) => a -> IO a
urlToFilenameIO url = do
   r <- parseRequest $ convertString url
   let
      p = reqPath r
      s = split "/" $ replace "%20" " " p
   mapM_ (createDirectory . c . intercalate "/") $ L.init $ L.inits s
   return $ c p
   -- let
   --    s = split "/" $ replace "%20" " " url -- (if ".html" `isSuffixOf` url then id else (++ ".html")) $
   -- in c $ ".cache/"++last s

readHTTP m url = unsafePerformIO $ getHTTPIO m url

--------------------------------------------------------------------------------------------
nest = nest1 []

nest1 context [] = case until ((< 2) . L.length) pop context of
   []  -> Tag "html" [] [Tag "head" [] [], Tag "body" [] []] 
   [a] -> a
nest1 context (t : tags)
   | tagType t == "!doctype" = nest1 context tags
   | isEnd t context && L.length context >= 2 =
         let 
            ty = tagType t
            ix = fromMaybe (-1) $ L.elemIndex ty $ map tagType context
         in nest1 (iterate pop context L.!! (ix + 1)) tags
   | isCont t = nest1 (t : context) tags
   | otherwise =
         case context of
            [] -> nest1 context tags
            (c : cs) -> nest1 (insertLastSubTag (convertEmpty t) c : cs) tags

isCont (Tag name atts _) = let n = smap toLower name in n /= "p" && n /= "br" && (n /= "script" || not (any ((=="src") . fst) atts))-- S.member (map toLower name) tagSet
isCont x = False

isEnd (EndTag n) c = n `L.elem` map tagType c
isEnd x c = False

isEmpty (EmptyTag _ _) = True
isEmpty x = False

pop (c1 : c2 : cs) = insertLastSubTag c1 c2 : cs

insertLastSubTag tag (Tag name attribs tags) = Tag name attribs (tags L.++ [tag])

data Property whole part = Property {getP :: whole -> part, putP :: part -> whole -> whole}

rqHeadersP = Property requestHeaders (\p w -> w{requestHeaders = p})

getRequestRefr ref url =
   let
      r = getRequest url
      h = requestHeaders r
   
   in r{requestHeaders = h L.++ [(hReferer, ref)]}

addUserAgent req = req{requestHeaders = (hUserAgent, "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36") : requestHeaders req}

{-
urlToFileName url = let
      s = split "/" url

      in if last s == "" then last (init s) ++ ".html" else last s
-}


isPicName url = any (`isSuffixOf` url) [".jpg", ".jpeg", ".bmp", ".png", ".gif"]

{-
getHdrLocation1 (Header HdrLocation l) = Just l
getHdrLocation1 _                      = Nothing

getHdrLocation h = head $ catMaybes $ map getHdrLocation1 h
-}

wget m url recP saveP levels = do
   done <- newIORef S.empty
   let
      wget1 0 url = return ()
      wget1 level url = do
         d <- readIORef done
         if S.member url d
            then return ()
            else do
               modifyIORef done (S.insert url)
               putStr url
               rbody <- getHTTPIO m url
               writeBinaryFile (c $ urlToFilename url) rbody
               let tags = parseHTML url rbody
               let links1 = map (relTo url) $ extractLinks1 tags
               let linksp = filter isPicName links1
               let pics1 = map (relTo url) $ extractPics tags
               let links = filter recP links1
               let pics = filter saveP (linksp L.++ pics1)
               mapM_ putStr pics
               --mapM_ (writeHTTPRefr m url) pics
               when (level > 1) $ mapM_ (wget1 (level - 1)) links

   wget1 levels url

zeropad n l = let str = show n in replicate (l - length str) '0' ++ str

formatSpaces :: (BStringC c str, Eq c, Eq str) => str -> str
formatSpaces = replace "%20" " "
parseSpaces :: (BStringC c str, Eq c, Eq str) => str -> str
parseSpaces = replace " " "%20"
parseURI1 u = parseURI $ c $ parseSpaces u

relTo1 sa sr = do
   ua <- parseURI $ c $ parseSpaces sa
   ur <- parseURIReference $ c $ parseSpaces sr
   
   return $ relativeTo ur ua

relTo :: (BStringC c s, Eq c, Eq s, ConvertString URI s) => s -> s -> s
relTo sa sr = c $ fromMaybe (error $ "joining paths failed: "++c sa++" won't join with"++c sr) $ relTo1 sa sr

relate base (Tag "a" a s) = Tag "a" (map (\case 
   ("href", url) -> ("href", relTo base url)
   (x, y) -> (x, y)) a) $ map (relate base) s
relate base (Tag "img" a s) = Tag "img" (map (\case 
   ("src", url) -> ("url", relTo base url)
   (x, y) -> (x, y)) a) $ map (relate base) s
-------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------
integerHTML str = parseNoFail 0 integer str

getRowSpan = fromInteger . maybe 1 integerHTML . mbAttrib "rowspan"

setRowSpan r t
   | r > 0 = setAttrib "rowspan" (c $ show r) t
   | otherwise = delAttrib "rowspan" t

expand1 t = let colspan = fromIntegral $ maybe 1 (parseNoFail 1 integer) $ mbAttrib "colspan" t in replicate colspan t

expand = concatMap expand1

insertL1 x e l = L.take x l L.++ (e : L.drop x l)

insertL _ 0 _ rows = rows
insertL x rowspan cell rows = map (insertL1 x cell) (L.take rowspan rows) L.++ L.drop rowspan rows

expandV1 _ [] rs = rs
expandV1 x (cell : cells) rs = expandV1 (x + 1) cells $ insertL x (getRowSpan cell - 1) (delAttrib "rowspan" cell) rs

expandV2 [] = []
expandV2 (r : rs) = r : expandV2 (expandV1 0 r rs)

-- expandV $ [[Tag "TD" [("rowspan","4")] []],[],[Tag "td" [("rowspan","3")] []],[],[],[]]

expandV3 [] bs = bs
expandV3 (a : as) bs =
   let
      r = getRowSpan a
      in
      if r > 1
         then setRowSpan (r - 1) a : expandV3 as bs
         else case bs of
            [] -> Text "" : expandV3 as []
            (c : cs) -> c : expandV3 as cs

expandV4 (a : b : rest) = a : expandV4 (expandV3 a b : rest)
expandV4 other = other

isTag (Tag _ _ _) = True
isTag (Text _) = False
isTag (EndTag _) = False

isText (Text _) = True
isText _ = False

tagType (Tag n _ _) = n
tagType (EmptyTag n _) = n
tagType (EndTag n) = n
tagType (Text _) = ""

setType (Tag n a c) n1 = Tag n1 a c
setType (EmptyTag n a) n1 = EmptyTag n1 a
setType (EndTag n) n1 = EndTag n1
setType x n1 = x

convertEmpty (EmptyTag n a) = Tag n a []
convertEmpty x = x

concatText (tag1 : tag2 : tags) =
   case (tag1, tag2) of
      (Text t1, Text t2) -> concatText (Text (t1 ++ t2) : tags)
      _ -> tag1 : concatText (tag2 : tags)
concatText rest = rest

matchTags ts t = L.elem (tagType t) ts
removeTags ts = L.filter (not . matchTags ts)
getTags ts = L.filter (matchTags ts)

testNest = nest $ concatText $ L.filter (\x -> tagType x `L.notElem` ["a", "i"]) testParse

subText = tagText . subTag
subTagN n t = subTags t L.!! n
subTag = L.head . subTags
subTagsType ty = filter (typeIs ty) . subTags
subTags (Tag _ _ x) = x
subTags (EndTag _) = []
subTags (Text _) = []
firstSub t = L.head $ subTags t

addTags add (Tag t a s) = Tag t a (s L.++ add)

attribs (Tag _ a _) = a
attribs (EmptyTag _ a) = a
attribs (EndTag _) = []
attribs (Text _) = []

setAttribs (Tag      n a s) a1 = Tag n a1 s
setAttribs (EmptyTag n a) a1 = EmptyTag n a1
setAttribs (EndTag   n) a1 = EndTag n
setAttribs (Text t) a1 = Text t

with a = mergeAttribs a
with1 a = setAttrib a
mergeAttribs ats tag = setAttribs tag $ mergeAttribs1 ats $ attribs tag
mergeAttribs1 ats atsin = foldr setAttrib1 atsin ats

setAttrib1 (a, v) t = ((a, v) :) $ filter ((a /=) . fst) t
setAttrib a v t = setAttribs t $ setAttrib1 (a, v) $ attribs t
delAttrib a t = setAttribs t $ L.filter ((a /=) . fst) $ attribs t
mbAttrib a t = lookup a (attribs t)
getAttrib a t = fromJust $ lookup a (attribs t)
hasAttrib a t = isJust $ mbAttrib a t
-- tagAttrib a t = fromJust $ mbAttrib a t
tagAttrib a t = fromMaybe "" $ mbAttrib a t
tagId = tagAttrib "id"
tagClass = tagAttrib "class"
tagText (Text t) = t
tagText _ = empty

typeIs = (tagType $=)
classIs = (tagClass $=)
classCon c t = c `isInfixOf` tagClass t
idIs = (tagId $=)
attribIs a = (tagAttrib a $=)

{-
findTree pred tag = let
      r = concatMap (findTree pred) (subTags tag)
      in
      if pred tag
         then tag:r
         else r
-}

wrap = Tag "results" []

setSubTags (Tag typ attribs _) subtags = Tag typ attribs subtags

filterType = filter . typeIs
filterId = filter . idIs
filterClass = filter . classIs

-- all findTree type functions changed to findTreeR
-- all findTree1 functions changed to findTree
findTreeR pred tag = setSubTags tag $ findTrees pred tag
findTrees pred tag = if pred tag then [tag] else concatMap (findTrees pred) (subTags tag)
findTree pred tag = case findTrees pred tag of
   [] -> Text ""
   (r : _) -> r

extractText = trim . squash . clean . mapTree aux map tagText
   where
      
      aux tag subtexts  | tagType tag == "ul" = concat $ map (++ "\n") subtexts
                        | tagType tag == "td" = intercalate " " subtexts
                        | tagType tag == "br" = "\n"
                        | tagType tag == "p"  = "\n" ++ intercalate " " subtexts ++ "\n"
                        | otherwise  = concat subtexts

k = T.intersperse

filterTree pred tag@(Tag typ atts subs) = ifPred pred $ Tag typ atts $ mapMaybe (filterTree pred) subs
filterTree pred tag = ifPred pred tag

mapTree f g h tag@(Tag {}) = f tag (g (mapTree f g h) $ subTags tag)
mapTree f g h tag = h tag

mapMaybeTree :: (HTML str -> Maybe a) -> HTML str -> Maybe a
mapMaybeTree f tag@(Tag typ atts subs) = f tag <|> listToMaybe (mapMaybe (mapMaybeTree f) subs)
mapMaybeTree f tag = f tag

mapMaybeTree1 f tag@(Tag typ atts subs) = f $ Tag typ atts $ mapMaybe (mapMaybeTree1 f) subs
mapMaybeTree1 f tag = f tag

mapMMaybeTree f tag@(Tag typ atts subs) = f $ Tag typ atts $ mapMaybe (mapMaybeTree f) subs
mapMMaybeTree f tag = f tag

isSrcScript (Tag "script" a c) = any ((== "src") . fst) a
isSrcScript other = False

removeScripts = filterTree 

removeScripts1 = mapMaybeTree (\case Tag "script" p c -> Just $ Tag "p" p c; other -> Just other)

fetchScripts m html = do
   let scripttags = findTrees isSrcScript html
   let scripturls = nubSet $ mapMaybe (mbAttrib "src") scripttags
   scriptreqs <- mapM parseRequest $ map c scripturls
   scriptresp <- mapM (`responseOpen` m) scriptreqs
   let scriptrdrs = map responseBody scriptresp
   let x = mapM brRead scriptrdrs
   let y = replicate (L.length scripturls) empty
   scripts <- fetcher x y
   let env = zip scripturls scripts
   return $ mapTree (\t u -> if isSrcScript t
               then t { inner = [Text $ fromJust $ lookup (getAttrib "src" t) env] }
               else t) map id html

   --fetcher (mapM brRead scriptbods) $ replicate (length scripturls) LT.empty

fetcher rs ss = do
   es <- rs
   if all null es then return ss else fetcher rs $ zipWith (++) ss $ map c es

emptyTag (Tag t _ []) = t == "span"
emptyTag (Text t) = trim (squash t) == empty
emptyTag x = False

findType = findTree . typeIs
findId = findTree . idIs
findClass = findTree . classIs
findClassCon = findTree . classCon
findAttrib a = findTree . attribIs a

findTypes = findTrees . typeIs
findIds = findTrees . idIs
findClasses = findTrees . classIs
findClassesCon = findTrees . classCon
findAttribs a = findTrees . attribIs a

-- HH   HH TTTTTTT MMM MMM LL
-- HH   HH    T    MMMMMMM LL
-- HHHHHHH    T    MM M MM LL
-- HH   HH    T    MM   MM LL
-- HH   HH    T    MM   MM LLLLLLL

#define ONLY
parseHTML sn xs = 
#ifdef ONLY
   case parseOnly htmlP xs of
      Left  l -> error l
      Right r -> r
#else
   case parse htmlP xs of
      Done i r        -> r
      Partial cont    -> error $ c $ "partial input in "++sn
      Fail i ctxt msg -> error $ "context = "++concat ctxt++" message="++msg++" input="++c i
#endif

htmlP = manyTill (choice [tag, textP, comment, Text <$> rest]) endOfInput

comment = (do
   string "<!--"
   commentMid "") <?> "comment"

commentMid x = (do
   com <- AP.takeWhile1 (notInClass "-")
   choice [commentMid (x++com), commentEnd (x++com)]) <?> "commentMid"

commentEnd x = (do
   string "-->"
   return $ Tag "comment" [("comment", x)] []) <?> "commentEnd"

textP = Text <$> charEnts "<&"

charEntList = [("quot", "\""), ("amp", "&"), ("lt", "<"), ("gt", ">"), ("nbsp", " "), ("apos", "'"), ("cent", "c"), ("pound", "£"), ("yen", "Y"), ("euro", "E"), ("copy", "(c)"), ("reg", "reg"), ("trade", "TM"), 
               ("circ", "^"), ("tilde", "~"), ("ensp", " "), ("emsp", "  "), ("thinsp", " "),
               ("zwnj", " "), ("zwj", ""), ("lrm", ""), ("rlm", ""), ("ndash", "-"), ("mdash", "--"), ("lsquo", "'"), ("rsquo", "'"), ("sbquo", "'"), ("ldquo", "\""), ("rdquo", "\""), ("bdquo", "\""), ("lsaquo", "<"), ("rsaquo", ">"), 
               ("bull", "*"), ("hellip", "..."), ("permil", "/000"), ("prime", "'"), ("Prime", "\"")]

charEntMap = M.fromList charEntList

charEntList2 = [("quot", "\""), ("amp", "&"), ("lt", "<"), ("gt", ">"), ("nbsp", " "), ("apos", "'")]

charEntMap2 = M.fromList $ map tflip charEntList2

charEnts endChars = concat <$> many1 (charEnts1 endChars) <?> "charEnts"

charEnts1 :: (Parser m str char, Ord str, MonadFail m, ConvertChar char Char, ConvertChar Word8 char, ParserC char) => [Char] -> m str
charEnts1 endChars = do
   clear <- AP.takeWhile (notInClass endChars)
   lchar <- AP.lookAhead anyChar
   if | convertChar lchar == '&' -> do
         qchar <- option "" $ do
            char '&'
            choice  [do ent <- AP.takeWhile (inClass alpha);                        char ';'; return $ fromMaybe "UCE" $ M.lookup ent charEntMap,
                     do string "#" ; i <- fromIntegral <$> integer;                 char ';'; return $ cons (convertChar (i :: Word8)) empty,
                     do string "#x"; i <- fromIntegral <$> baseInteger 16 hexDigit; char ';'; return $ cons (convertChar (i :: Word8)) empty]
         return $ clear ++ qchar
      | null clear -> fail "no text"
      | otherwise  -> return clear

alpha = ['A' .. 'Z'] ++ ['a'..'z']

tagignore = do whiteSpace; char '<'; manyTill anyChar (char '>'); whiteSpace

tag :: forall parser str char. (IsString str, IsString str, BStringC char str, BChar char, Ord str, Parser parser str char, ParserC char, ConvertChar char Char, ConvertChar Word8 char, MonadFail parser) => parser (HTML str)
tag = (do
         char '<'
         whiteSpace
         end <- option False (do char '/'; whiteSpace; return True)
         name1 <- AP.takeWhile1 (notInClass " />\t\n\r")
         let name = smap toLower name1
         whiteSpace
         attribs <- many attrib
         emptyTag <- option False (do char '/'; return True)
         char '>'

         if | name == "script" && not end -> do
               script <- manyTill anyChar (try $ string "</script>")
               return $ EmptyTag name (attribs L.++ [("script", fromList script)]) 
            | name == "style" && not end -> do
               style <- manyTill anyChar (try $ string "</style>")
               return $ EmptyTag name (attribs L.++ [("style", fromList style)])
            | end ->
               return $ EndTag name
            | emptyTag ->
               return $ EmptyTag name attribs
            | otherwise ->
               return $ Tag name attribs []
   
      ) <?> "Tag"

attrib = (do
   i <- AP.takeWhile1 (notInClass " =/>\t\n\r")
   whiteSpace
   v <- option "" (do char '='; whiteSpace; value)
   whiteSpace
   return (c i, c v)) <?> "attrib"

value = quotedValue <|> quotedValue2 <|> rawValue

quotedValue = (do
   char '"'
   res <- charEnts "\"&"
   char '"'
   return res) <?> "quoteVal"

quotedValue2 = (do
   char '\''
   res <- charEnts "'&"
   char '\''
   return res) <?> "quoteVal2"

rawValue = charEnts " />" <?> "rawVal"

rawVal2 = AP.takeWhile (notInClass " />'\"") <?> "rawVal2"

escapeValue = do
   list <- many $ do
      clear <- AP.takeWhile1 (notInClass "&<'\"")
      escape <- choice [do char '&' ; return "&amp;" ,
                        do char '<' ; return "&lt;"  ,
                        do char '"' ; return "&quot;",
                        do char '\''; return "&#39;" ]
      return $ clear ++ escape
   return $ concat list

indentStep = 3

ph = BString.putStr . HTMLAny.format

c x = convertString x
{-
--formatLBS = lbsofs . formatH 1
formatBS1 :: HTML -> LByteString
formatBS1 (EndTag typ) = "[/" ++ c typ ++ "]"
formatBS1 (EmptyTag typ attribs) = "<" ++ c typ ++ c (formatAttribs attribs) ++ "/>\n"
formatBS1 (Text text) = c $ trim $ squash text
formatBS1 (Tag typ attribs terms) =
   let
      open = "<" ++ c typ ++ c (formatAttribs attribs) ++ ">"
      close = "</" ++ c typ ++ ">"
      fterms = map formatLBS terms
      res = open ++ concat fterms ++ close
      in
      if length res <= width
         then c res
         else open ++ "\n" ++ c (indent indentStep $ intercalate "\n" $ map c fterms) ++ "\n" ++ close

formatAttrib (name, val) = string7 " " <> fromText name <> string7 "=\"" <> fromText val <> string7 "\""
formatAttribs a = mconcat $ map formatAttrib a
-}
format html = toText $ formatLT 0 html

formatHTML html = HTMLAny.format html

formatLT i (EndTag   typ        ) = mempty --string7 (replicate i ' ') <> "[/" <> fromText typ <> "]\n"
formatLT i (EmptyTag typ attribs) = fromString (replicate i ' ') <> "<" <> fromText typ <> formatAttribs attribs <> "/>\n"
formatLT i (Text            text) = let
   t = squash $ trim text
   
   in if null t || t == " " then mempty else fromString (replicate i ' ') <> fromText (trim $ squash text) <> "\n"

formatLT i t@(Tag typ attribs1 terms) = let
   open  = "<"  <> fromText typ <> formatAttribs (attribs $ delAttrib "script" t) <> ">"
   close = "</" <> fromText typ <> ">"
   fterms = map (formatLT (i+3)) terms
   ind = fromString $ replicate i ' '
   --res = open <> mconcat fterms <> close
   
   in if typ == "script" && hasAttrib "script" t
         then 
            ind <> open <> "\n" <> fromText (getAttrib "script" t) <> ind <> close <> "\n"
         else
            ind <> open <> "\n" <> mconcat fterms <> ind <> close <> "\n"

formatAttrib (name, val) = " " <> fromText name <> "=\"" <> fromText val <> "\""
formatAttribs a = mconcat $ map formatAttrib a


{-
else if allEqual clens
            then
            then open ++ indent1 (length open) (intercalate (sep ++ "\n") fterms) ++ close
-}
{-
if a subitem can't fit into the width,
must trigger the outer to not fit
      it will as long as it's not shorter when spread across lines, don't see how it could be shorter
must also redo all the sub items
or could format them originally for spreading as the spread indent is <= than the open+close tag
-}

squash :: (Ord c, ConvertChar Char c, BStringC c str) => str -> str
squash x 
   | null   x      = x
   | length x == 1 = if isSpace2 (head x) then " " else x
   | length x >  1 = if
      | isSpace2 $ head x -> if
         | isSpace2 $ x !! 1 -> squash $ c " " ++ drop 2 x
         | otherwise -> c " " ++ squash (drop 1 x)
      | otherwise -> cons (head x) $ squash $ drop 1 x

tag2 = do
   char '<'
   whiteSpace
   r <- many (lexeme tagstuff)
   char '>'
   whiteSpace
   return r

tagstuff = choice [rawVal2, quotedValue, quotedValue2]

maybeP p = option Nothing (Just <$> p)

lexeme p = do r <- p; whiteSpace; return r

whiteSpace = AP.takeWhile (inClass " \t\n\r")

ws = many ws1

ws1 = oneOf " \t\n\r"

-- nws1 = noneOf " \t\n\r"

-- followedBy p = choice [choice [(do p; return True), return False]

file = readBinaryFile "britfilms2010.htm"

testParse = parseHTML T.empty $ unsafePerformIO file

file1 = "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" dir=\"ltr\">\n<head>"

file2 = "<blah><grid><tr>blah</tr><tr>hello</tr></grid>"

tagList =
   [ "a"
   , "abbr"
   , "acronym"
   , "address"
   , "applet"
   , "b"
   , "bdo"
   , "big"
   , "blockquote"
   , "body"
   , "button"
   , "caption"
   , "center"
   , "cite"
   , "colgroup"
   , "dd"
   , "del"
   , "dfn"
   , "dir"
   , "div"
   , "dl"
   , "dt"
   , "em"
   , "fieldset"
   , "font"
   , "form"
   , "frameset"
   , "h1"
   , "h2"
   , "h3"
   , "h4"
   , "h5"
   , "h6"
   , "head"
   , "hr"
   , "html"
   , "i"
   , "iframe"
   , "ins"
   , "isindex"
   , "kbd"
   , "label"
   , "legend"
   , "li"
   , "map"
   , "menu"
   , "noframes"
   , "noscript"
   , "object"
   , "ol"
   , "optgroup"
   , "option"
   , "p"
   , "param"
   , "pre"
   , "q"
   , "s"
   , "samp"
   , "script"
   , "select"
   , "small"
   , "span"
   , "strike"
   , "strong"
   , "style"
   , "sub"
   , "sup"
   , "table"
   , "tbody"
   , "td"
   , "textarea"
   , "tfoot"
   , "th"
   , "thead"
   , "title"
   , "tr"
   , "tt"
   , "u"
   , "ul"
   , "var"
   , "section"
   ]

tagSet = S.fromList tagList

{-
lists:
DIR = directory list
DL  = definition list
OL  = ordered list
UL  = unordered list
-}

testNest2 = subTag $ findTree (\t -> tagType t == "table" && mbAttrib "class" t == Just "wikigrid") testNest

level 0 (Tag n a t) = Tag n a []
level 0 (EndTag n) = EndTag n
level 0 (Text t) = Text t
level x (Tag n a t) = Tag n a (map (level (x - 1)) t)
level x (EndTag n) = EndTag n
level x (Text t) = Text t

convertRowH tag = getTags ["td", "th"] $ subTags tag
convertRow tag = getTags ["td"] $ subTags tag

testGrid = cGrid testNest2
testExtract = map2 extractText testGrid

-- test5 = pr $ getCols [0,3] test4

getCol col tbl = map (!! col) tbl

-- getCols cols tbl = transpose $ crossWith nthB tbl cols

{-
pmAll :: [Integer] -> [(Integer, String)]
pmAll m = map (\u -> (pmReview m u, u)) . concatMap (pmIndexPage m . ("http://www.popmatters.com/pm/reviews/recent/section/dvds/P" ++) . show)
-}
-- http://s3.amazonaws.com/data.tumblr.com/tumblr_lnd2c7MAK31qldyuto1_1280.jpg?AWSAccessKeyId=AKIAJ6IHWSU3BX3X7X3Q&Expires=1318390589&Signature=ojbE2oqAD6wQBbLfC9xOvVQsxHo%3D
spacesToPluses :: (Eq c, Eq str, BStringC c str) => str -> str
spacesToPluses x = replace " " "+" x
spacesToUnders x = replace " " "_" x

undersToSpaces x = replace "_" " " x

discSrch m typ name =
   map (tagAttrib "href") $
      subTags $
         findTree ((== "search_result_title") . tagClass) $
                  getHTML m ("https://www.discogs.com/search/?q=" ++ spacesToPluses name ++ "&type=" ++ typ)

discTracks m name =
   map
      ( \tr ->
            ( tagAttrib "data-track-position" tr
            , subText $ subTag $ subTagN 2 tr
            , subText $ subTagN 3 tr
            )
      )
      $ subTags
      $ findType "tr"
      $ findTree (("tracklist" `isPrefixOf`) . tagClass)
      $ (\url -> getHTML m ("https://www.discogs.com" ++ url))
      $ L.head
      $ discSrch m "master" name

discImage m name =
   tagAttrib "src" $
      subTag $
         findType "img" $
            findClass "link_1ctor link_33If6" $
                     (\url -> getHTML m ("https://www.discogs.com" ++ url)) $
                        L.head $
                           discSrch m "master" name

{-
<div class="image_3rzgk bezel_2NSgk">
<picture>
<img src="https://i.discogs.com/vxAuOwF92iyWcStjK3UUqdylXK4JcLJjF4hZRQdxhe0/rs:fit/g:sm/q:40/h:300/w:300/czM6Ly9kaXNjb2dz/LWRhdGFiYXNlLWlt/YWdlcy9SLTc1OTA2/MS0xMjkzMzYwNzk4/LmpwZWc.jpeg" alt="Paradise Lost - Paradise Lost album cover" width="150" height="146"></picture></div>
-}
upIO = unsafePerformIO

gnusrch m typ name = getHTML m ("http://gnudb.org/" ++ typ ++ "/" ++ spacesToPluses name)

gov = "List of British governments"

findSubs sub txt = pp 416 $ map (\n -> take 300 $ drop n txt) $ subIndices sub txt

deloitte500 = putGridFile "deloitte500.html"

padMapRWith w with = M.map (\row -> row ++ replicate (w - length row) with)

mapGrid1 indexcol tab = M.fromList $ map (\row -> (trim $ row !! indexcol, take indexcol row ++ drop (indexcol + 1) row)) tab

findCol indexname tab = fromJust $ findIndex (\fieldname -> indexname `isInfixOf` map toLower fieldname) $ head tab

--mapGrid indexname tab = mapGrid1 (findCol indexname tab) $ tail tab

putMap m = putGrid $ transpose $ map showT $ M.toList m

putMap1 = putGrid . transposez "" . sortOn (readInt . (L.!! 2)) . map (uncurry (:)) . M.toList

putMap2 = putGrid . transposez "" . sortOn (readInt . (L.!! 2)) . map (\(a, b :: [Int]) -> a : map show b) . M.toList

lego :: [HTML Text]
lego = concatMap (findTrees (attribIs "data-test" "pab-item")) $ concatMap (\p -> findTrees (attribIs "data-test" "pab-search-results-list") $ readHTML $ "lego"++c (show p)++".html") [1..5]

dropBut n l = drop (length l - n) l

lego1 = sort $ map (\item -> let
   title = extractText $ findTree (attribIs "data-test" "pab-item-title") item
   img = dropBut 11 $ tagAttrib "src" $ findType "img" $ findTree (attribIs "data-test" "pab-item-image") item
   in (title, take 7 img, img)) lego

writeHTML file html = do
   h <- openBinaryFile file WriteMode
   hPutStr h $ toText $ formatLT 0 html
   hClose h

writeHTMLT file htmls = writeBinaryFile file $ toText $ mconcat $ map (formatLT 0) htmls

--lego2 = writeHTML "lego.html" $ Tag "table" [] $ map (\(title, n, img) -> Tag "tr" [] [Tag "td" [] [Text n], Tag "td" [] [Text title], Tag "td" [] [Tag "image" [("src", img)] []]]) lego1
