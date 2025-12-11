-- Copyright 2025 Brett Curtis
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant $" #-}
{- HLINT ignore "Redundant multi-way if" -}

module HTMLB where

import Favs hiding (split, trim, trimtrailing, replace)
import MyPretty2
import NumberParsersB
--import NumberParsers hiding (parse, try)
import ShowTuple
import BString
import Atto

import Prelude hiding (null, tail, head, elem, length, (++), (!!), toLower, split, last, take, drop, notElem, concat, takeWhile, dropWhile, putStrLn)
import Data.List (singleton, transpose, sort, elemIndex, findIndex)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Builder
import Data.Monoid
import Data.Word (Word8)

import Control.Monad
import Control.Applicative hiding (empty, many)
import Data.Char hiding (toLower)
import Data.IORef
import System.IO hiding (putStrLn)
import System.IO.Unsafe

-- import System.Win32.Process

import Data.Map qualified as M
import Data.Set qualified as S
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.URI
import System.Directory

import Data.Attoparsec.ByteString hiding (take, takeWhile, dropWhile)
import qualified Data.Attoparsec.Combinator as AP
import qualified Data.Attoparsec.ByteString as AP

--import Text.Parsec
--import Text.Parsec.Combinator

data HTML
   = Tag      { tagType_ :: B.ByteString, tagAttribs :: [(B.ByteString, B.ByteString)], inner :: [HTML] }
   | EmptyTag { tagType_ :: B.ByteString, tagAttribs :: [(B.ByteString, B.ByteString)] }
   | EndTag   { tagType_ :: B.ByteString }
   | Text     { text     :: B.ByteString }
   deriving (Show, Eq, Ord)

putLines l = mapM_ putStrLn l

ukulele m = filter (not . null) $ extractLinks $ findTree (\t -> tagType t == "table" && tagAttrib "width" t == "407") $ getHTML m "https://wpu3a.org.uk/Groups/ukulele-for-beginners"

uku m = mapM (writeHTTP m . relTo "https://wpu3a.org.uk") $ tail $ ukulele m

nm :: IO Manager
nm = newTlsManager

cTextGrid  = map2 extractText . cGrid
cTextGridH = map2 extractText . cGridH

html h b = Tag "html" [] [Tag "head" [] h, Tag "body" [] b]

textGridH = gridH . map2 (singleton . Text)
textGrid = textGridH . transpose

putTextGrid = putGrid . map2 c . cTextGrid

putTextFile = putTextGrid . readHTML

-- convertGridH tag = map (getTags ["td", "th"] . subTags) $ filter (("tr" ==) . tagType) $ subTags tag
extractText = trim . squash . clean . concat . map text . findTrees isText
extractTexts htmls = trim $ squash $ concat $ map tagText htmls

firstText = (\case { [] -> empty; (a:_) -> a }) . mapMaybe (ifPred (not . null) . trim . clean . tagText) . findTrees isText

extractLink = head . extractLinks
extractLinks t = map (tagAttrib "href") $ findTypes "a" t

extractLinks1 = map (tagAttrib "href") . filter (\t -> typeIs "a" t && hasAttrib "href" t)

extractPics = mapMaybe (\t -> ifJust (tagType t == "img" && hasAttrib "src" t) (tagAttrib "src" t))

cGridH tag = padRWith (Text "") $ expandV4 $ map (expand . findTrees (\t -> tagType t `elem` ["td", "th"])) (findTypes "tr" tag)
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

($<) :: BStringC c s => (t -> s) -> s -> t -> Bool
f $< b = \x -> b `isPrefixOf` f x


link url = Tag "a" [("href", url)]
img url = EmptyTag "img" [("src", url)]
textLink url txt = link url $ [Text txt]
imgLink url thumb = link url $ [img thumb]

isSpace2 x = x <= 32 || x > 126

trim :: B.ByteString -> B.ByteString
trim = trimtrailing . dropWhile isSpace2

trimtrailing :: B.ByteString -> B.ByteString
trimtrailing xs
   | null xs = empty
   | otherwise = let xst = trimtrailing (tail xs) in if isSpace2 (head xs) && null xst then empty else cons (head xs) xst

clean = smap (\c -> if (c >= 32 && c <= 126) || (c > 160 && c <= 255) then c else 32)

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

getRequest :: B.ByteString -> Request
getRequest = addUserAgent . parseRequest_ . c
--------------------------------------------------------------------------------
getHTTP :: Manager -> B.ByteString -> B.ByteString
getHTTP m url = c $ unsafePerformIO $ getHTTPIO m url

--------------------------------------------------------------------------------
getHTTPIO :: Manager -> B.ByteString -> IO B.ByteString
getHTTPIO m url = do
   let req = getRequest url
   -- print req
   resp <- simpleHTTP m req
   -- print resp
   return $ c $ responseBody resp

--------------------------------------------------------------------------------

-- getHTTPIO -> parse html "" -> nest -> findTree (\t -> mbAttrib "id" t == "main")
getHTML m url = nestParse url $ getHTTP m url
getHTMLIO m url = nestParse url <$> getHTTPIO m url
getHTMLReq m req = nestParse (show $ reqPath req) $ c $ unsafePerformIO $ getHTTPReq m req
nestParse f = nest . parseHTML f

readHTML f = nestParse f $ readFileBinary f

readWriteHTTP m url = unsafePerformIO $ readWriteHTTP1 m url
readWriteHTML m url = unsafePerformIO $ readWriteHTML1 m url

readWriteHTTP1 m url = do
   let file = urlToFileName url
   flag <- doesFileExist file
   if flag
      then do
         let h = readFileBinary file
         putStrLn $ "read " ++ file
         return h
      else do
         putStrLn $ "fetching " ++ file
         dat <- getHTTPIO m url
         writeFileBinary file dat
         putStrLn $ "written " ++ file
         return dat

readWriteHTML1 m url = do
   let file = urlToFileName url
   flag <- doesFileExist file
   if flag
      then do
         let h = readHTML file
         putStrLn $ "read " ++ file
         return h
      else do
         putStrLn $ "fetching " ++ file
         html <- getHTMLIO m url
         Data.ByteString.Builder.writeFile file $ formatB 0 html
         putStrLn $ "written " ++ file
         return html


mytrace x = unsafePerformIO (do print x; return x)

-- $ (\weird -> if null weird then xs else error weird) $ mapMaybe (ifPred isWeird) xs

-- getHTTPIO with refering page as first parameter
--lbsofs = LB.pack . map (fromIntegral . ord)

--soflbs = map chr . filter (\x -> x == 10 || x == 13 || x >= 32 && x <= 127 || x >= 160 && x <= 255) . map fromIntegral . LB.unpack

getHTTPRefr :: Manager -> B.ByteString -> B.ByteString -> IO B.ByteString
getHTTPRefr m ref url = do
   nsr <- simpleHTTP m (getRequestRefr ref url)
   return $ c $ responseBody nsr

getHTTPReq :: Manager -> Request -> IO B.ByteString
getHTTPReq m req = do
   let url = reqPath req
   putStrLn ("getting " ++ show url)
   nsr <- simpleHTTP m req
   return $ c $ responseBody nsr

reqPath = Network.HTTP.Client.path

reqURI = fromJust . parseURI . c . reqPath


urlToFileName :: B.ByteString -> String
urlToFileName url =
   let
      s = split "/" $ replace "%20" " " url -- (if ".html" `isSuffixOf` url then id else (++ ".html")) $
   
   in c $ ".cache/"++last s

writeHTTP m url = do
   let fileName = urlToFileName url
   rbody <- getHTTPIO m url
   writeFileBinary fileName rbody

writeHTTPRefr m ref url = getHTTPRefr m ref url >>= writeFileBinary (urlToFileName url)
writeHTTPReq m req = getHTTPReq m req >>= writeFileBinary (urlToFileName $ reqPath req)

writeHTTPAs m file req = do
   rbody <- getHTTPReq m req
   putStrLn ("saving " ++ file)
   writeFileBinary file rbody

readFileBinary name = unsafePerformIO $ do
   handle <- openBinaryFile name ReadMode
   B.hGetContents handle

writeFileBinary name dat = do
   handle <- openBinaryFile name WriteMode
   B.hPut handle dat
   hClose handle

readHTTP m url = do
   nsr <- simpleHTTP m (getRequest url)
   return $ responseBody nsr


--------------------------------------------------------------------------------------------

nest = nest1 []

nest1 context [] = case until ((< 2) . length) pop context of
   []  -> Tag "html" [] [Tag "head" [] [], Tag "body" [] []] 
   [a] -> a
nest1 context (t : tags)
   | tagType t == "!doctype" = nest1 context tags
   | isEnd t context && length context >= 2 =
         let 
            ty = tagType t
            ix = fromMaybe (-1) $ elemIndex ty $ map tagType context
         in nest1 (iterate pop context !! (ix + 1)) tags
   | isCont t = nest1 (t : context) tags
   | otherwise =
         case context of
            [] -> nest1 context tags
            (c : cs) -> nest1 (insertLastSubTag (convertEmpty t) c : cs) tags

isCont (Tag name atts _) = let n = smap toLower name in n /= "p" && n /= "br" && (n /= "script" || not (any ((=="src") . fst) atts))-- S.member (map toLower name) tagSet
isCont x = False

isEnd (EndTag n) c = n `elem` map tagType c
isEnd x c = False

isEmpty (EmptyTag _ _) = True
isEmpty x = False

pop (c1 : c2 : cs) = insertLastSubTag c1 c2 : cs

insertLastSubTag tag (Tag name attribs tags) = Tag name attribs (tags ++ [tag])

data Property whole part = Property {getP :: whole -> part, putP :: part -> whole -> whole}

rqHeadersP = Property requestHeaders (\p w -> w{requestHeaders = p})

getRequestRefr ref url =
   let
      r = getRequest url
      h = requestHeaders r
   
   in r{requestHeaders = h ++ [(hReferer, ref)]}

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

wget :: Manager -> B.ByteString -> (B.ByteString -> Bool) -> (B.ByteString -> Bool) -> Int -> IO ()
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
               B.putStr url
               rbody <- getHTTPIO m url
               writeFileBinary (urlToFileName url) rbody
               let tags = parseHTML url $ c rbody
               let links1 = map (relTo url) $ extractLinks1 tags
               let linksp = filter isPicName links1
               let pics1 = map (relTo url) $ extractPics tags
               let links = filter recP links1
               let pics = filter saveP (linksp ++ pics1)
               mapM_ B.putStr pics
               mapM_ (writeHTTPRefr m url) pics
               when (level > 1) $ mapM_ (wget1 (level - 1)) links

   wget1 levels url

zeropad n l = let str = show n in replicate (l - length str) '0' ++ str

formatSpaces :: B.ByteString -> B.ByteString
formatSpaces = replace "%20" " "
parseSpaces :: B.ByteString -> B.ByteString
parseSpaces = replace " " "%20"
parseURI1 u = parseURI $ c $ parseSpaces u

-- relTo1 :: [Char] -> [Char] -> URI
relTo1 sa sr =
   let
      ua = fromJust $ c $ parseURI $ c $ parseSpaces sa
      ur = fromJust $ c $ parseURIReference $ c $ parseSpaces sr
      in
      relativeTo ur ua

relTo :: B.ByteString -> B.ByteString -> B.ByteString
relTo sa sr = c $ show $ relTo1 sa sr

-------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------
getRowSpan = fromIntegral . maybe 1 (parseNoFail 0 integer) . mbAttrib "rowspan"

setRowSpan r t
   | r > 0 = setAttrib "rowspan" (c $ show r) t
   | otherwise = delAttrib "rowspan" t

expand1 t = let colspan = fromIntegral $ maybe 1 (parseNoFail 1 integer) $ mbAttrib "colspan" t in replicate colspan t

expand :: [HTML] -> [HTML]
expand = concatMap expand1

insertL1 x e l = take x l ++ (e : drop x l)

insertL _ 0 _ rows = rows
insertL x rowspan cell rows = map (insertL1 x cell) (take rowspan rows) ++ drop rowspan rows

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

matchTags ts t = elem (tagType t) ts
removeTags ts = filter (not . matchTags ts)
getTags ts = filter (matchTags ts)

testNest = nest $ concatText $ filter (\x -> tagType x `notElem` ["a", "i"]) testParse

subText = tagText . subTag
subTagN n t = subTags t !! n
subTag = head . subTags
subTags (Tag _ _ x) = x
subTags (EndTag _) = []
subTags (Text _) = []
firstSub t = head $ subTags t

addTags add (Tag t a s) = Tag t a (s ++ add)

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
delAttrib a t = setAttribs t $ filter ((a /=) . fst) $ attribs t
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

filterTree pred tag@(Tag typ atts subs) = ifPred pred $ Tag typ atts $ mapMaybe (filterTree pred) subs
filterTree pred tag = ifPred pred tag

mapTree f g h tag@(Tag {}) = f tag (g (mapTree f g h) $ subTags tag)
mapTree f g h tag = h tag

mapMaybeTree :: (HTML -> Maybe a) -> HTML -> Maybe a
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
   let y = replicate (length scripturls) empty
   scripts <- fetcher x y
   let env = zip scripturls scripts
   return $ mapTree (\t u -> if isSrcScript t
               then t { inner = [Text $ c $ fromJust $ lookup (getAttrib "src" t) env] }
               else t) map id html

   --fetcher (mapM brRead scriptbods) $ replicate (length scripturls) LB.empty

fetcher :: (Monad m) => m [B.ByteString] -> [B.ByteString] -> m [B.ByteString]
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

htmlP = manyTill (choice [tag, textP, comment, Text <$> takeByteString]) endOfInput

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

charEnts endChars = concat <$> many1 (charEnts1 endChars)

charEnts1 endChars = do
   clear <- AP.takeWhile (notInClass endChars)
   lchar <- AP.lookAhead anyChar1
   if | lchar == '&' -> do
         qchar <- option "" $ do
            char '&'
            choice  [do string "amp;"  ; return "&",
                     do string "lt;"   ; return "<",
                     do string "gt;"   ; return ">",
                     do string "nbsp;" ; return " ",
                     do string "apos;" ; return "'",
                     do string "quot;" ; return "\"",
                     do string "pound;"; return "£",
                     do string "#" ; i <- fromIntegral <$> integer;                 return $ cons i empty,
                     do string "#x"; i <- fromIntegral <$> baseInteger 16 hexDigit; return $ cons i empty]
         return $ clear ++ qchar
      | null clear -> fail "no text"
      | otherwise  -> return clear
      
tagignore = do whiteSpace; char '<'; manyTill anyChar (char '>'); whiteSpace

tag = (do
         char '<'
         whiteSpace
         end <- option False (do char '/'; whiteSpace; return True)
         name1 <- AP.takeWhile1 (notInClass " />\t\n\r")
         let name = smap toLower name1 :: B.ByteString
         whiteSpace
         attribs <- many attrib
         emptyTag <- option False (do char '/'; return True)
         char '>'

         if | name == "script" && not end -> do
               script <- manyTill anyChar (try $ string "</script>")
               return $ EmptyTag name (attribs ++ [("script", c script)]) 
            | name == "style" && not end -> do
               style <- manyTill anyChar (try $ string "</style>")
               return $ EmptyTag name (attribs ++ [("style", c style)])
            | end ->
               return $ EndTag name
            | emptyTag ->
               return $ EmptyTag name attribs
            | otherwise ->
               return $ Tag name attribs []
   
      ) <?> "Tag"

attrib :: Parser (B.ByteString, B.ByteString)
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

--entityList = [("&quot;", "\""), ("&amp;", "&"), ("&lt;", "<"), ("&gt;", ">"), ("&nbsp;", " "), ("&#160;", " "), ("&apos;", "'"), ("&cent;", "c"), ("&pound;", "£"), ("&yen;", "Y"), ("&euro;", "E"), ("&copy;", "(c)"), ("&reg;", "reg"), ("&trade;", "TM")]

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

ph = LB.putStr . formatLBS 

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

formatAttrib (name, val) = string7 " " <> byteString name <> string7 "=\"" <> byteString val <> string7 "\""
formatAttribs a = mconcat $ map formatAttrib a
-}
formatLBS html = toLazyByteString $ formatB 0 html

formatB :: Int -> HTML -> Builder
formatB i (EndTag   typ        ) = mempty --string7 (replicate i ' ') <> "[/" <> byteString typ <> "]\n"
formatB i (EmptyTag typ attribs) = string7 (replicate i ' ') <> "<" <> byteString typ <> formatAttribs attribs <> "/>\n"
formatB i (Text            text) = let
   t = squash $ trim text
   
   in if t == "" || t == " " then mempty else string7 (replicate i ' ') <> byteString (trim $ squash text) <> "\n"

formatB i t@(Tag typ attribs1 terms) = let
   open  = "<"  <> byteString typ <> formatAttribs (attribs $ delAttrib "script" t) <> ">"
   close = "</" <> byteString typ <> ">"
   fterms = map (formatB (i+3)) terms
   ind = string7 $ replicate i ' '
   --res = open <> mconcat fterms <> close
   
   in if typ == "script" && hasAttrib "script" t
         then 
            ind <> open <> "\n" <> byteString (getAttrib "script" t) <> ind <> close <> "\n"
         else
            ind <> open <> "\n" <> mconcat fterms <> ind <> close <> "\n"

formatAttrib (name, val) = " " <> byteString name <> "=\"" <> byteString val <> "\""
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

squash :: (BStringC Word8 s, ConvertString String s) => s -> s
squash x 
   | null   x      = x
   | length x == 1 = if isSpace2 (head x :: Word8) then c " " else x
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

file = readFileBinary "britfilms2010.htm"

testParse = parseHTML (b "") file

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
spacesToPluses :: B.ByteString -> B.ByteString
spacesToPluses x = replace " " "+" x
spacesToUnders x = replace " " "_" x

discSrch m typ name =
   map (tagAttrib "href") $
      subTags $
         findTree ((== "search_result_title") . tagClass) $
            getHTMLReq m $
               addUserAgent $
                  getRequest ("https://www.discogs.com/search/?q=" ++ spacesToPluses name ++ "&type=" ++ typ)

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
      $ getHTMLReq m
      $ addUserAgent
      $ (\url -> getRequest ("https://www.discogs.com" ++ url))
      $ head
      $ discSrch m "master" name

discImage m name =
   tagAttrib "src" $
      subTag $
         findType "img" $
            findClass "link_1ctor link_33If6" $
               getHTMLReq m $
                  addUserAgent $
                     (\url -> getRequest ("https://www.discogs.com" ++ url)) $ c $
                        head $
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

deloitte500 = putTextFile "deloitte500.html"

padMapRWith w with = M.map (\row -> row ++ replicate (w - length row) with)

listUnion a b =
   let
      wa = map (length . snd) $ M.toList a
      wb = map (length . snd) $ M.toList b
      maxa = maximum wa
      maxb = maximum wb
      mina = minimum wa
      minb = minimum wb
      a1 = if maxa > mina then padMapRWith maxa "" a else a
      b1 = if maxb > minb then padMapRWith maxb "" b else b
      i = M.intersectionWith (++) a1 b1
      l = M.map (++ replicate maxb "") $ M.difference a1 b1
      r = M.map (replicate maxa "" ++) $ M.difference b1 a1
      in
      i `M.union` l `M.union` r

foldUnion :: (Ord k) => [M.Map k [String]] -> M.Map k [String]
foldUnion = foldr1 listUnion

mapGrid1 indexcol tab = M.fromList $ map (\row -> (trim $ row !! indexcol, take indexcol row ++ drop (indexcol + 1) row)) tab

findCol indexname tab = fromJust $ findIndex (\fieldname -> indexname `isInfixOf` map toLower fieldname) $ head tab

--mapGrid indexname tab = mapGrid1 (findCol indexname tab) $ tail tab

putMap m = putGrid $ transpose $ map showT $ M.toList m

putMap1 = putGrid . transposez "" . sortOn (readInt . (!! 2)) . map (uncurry (:)) . M.toList

putMap2 = putGrid . transposez "" . sortOn (readInt . (!! 2)) . map (\(a, b :: [Int]) -> a : map show b) . M.toList

lego = concatMap (findTrees (attribIs "data-test" "pab-item")) $ concatMap (\p -> findTrees (attribIs "data-test" "pab-search-results-list") $ readHTML $ "lego"++show p++".html") [1..5]

dropBut n l = drop (length l - n) l

lego1 = sort $ map (\item -> let
   title = extractText $ findTree (attribIs "data-test" "pab-item-title") item
   img = dropBut 11 $ tagAttrib "src" $ findType "img" $ findTree (attribIs "data-test" "pab-item-image") item
   in (title, take 7 img, img)) lego

writeHTML file html = Data.ByteString.Builder.writeFile file $ formatB 0 html

writeHTMLT file htmls = Data.ByteString.Builder.writeFile file $ mconcat $ map (formatB 0) htmls

lego2 = writeHTML "lego.html" $ Tag "table" [] $ map (\(title, n, img) -> Tag "tr" [] [Tag "td" [] [Text n], Tag "td" [] [Text title], Tag "td" [] [Tag "image" [("src", img)] []]]) lego1
