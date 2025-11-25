module HTTP where

import Parser6 hiding (crlf)

import HTTPTypes
import Favs
import Iso
import NewTuple

import Data.List
import Data.Char

req = Eith requestConnect request

emptyUrl = Url "" "" (Just 0) [] Nothing ""

emptyMessage = Message "" emptyUrl "" 0 "" [] 0 ""

requestConnect = Build emptyMessage (
   MethodK <-- Alt [String "CONNECT"] :/
   Many (Token ' ') :/
   UrlK <-- hostPort :/
   Token ' ' :/
   HTTPVersionK <-- AnyTill crlf :/
   crlf)

request = Build emptyMessage (
   MethodK <-- Alt [String "GET", String "get"] :/
   Many (Token ' ') :/
   UrlK <-- urlP :/
   HeadersK <-- ManyTill header crlf :/
   crlf)
   --SetM [Headers1, ContentLength] $ Apply (ilookup "Content-Length") $ Get Headers]
   --Body <-- Count (Get ContentLength) AnyToken]

response = Build emptyMessage (
   HTTPVersionK <-- AnyTill (Token ' ') :/
   Token ' ' :/
   StatusCodeK <-- int :/
   Token ' ' :/
   ReasonPhraseK <-- AnyTill crlf :/
   crlf :/
   HeadersK <-- ManyTill header crlf :/
   crlf)

headers :: Rule String Char Message [[Char] :- [Char]]
headers = 
   HeadersK <-- ManyTill header crlf ://
   crlf 
   --SetM (Headers1K, (ContentLengthK, ())) (Apply (ilookup "Content-Length") $ Get HeadersK) :/
   --BodyK <-- Count (Get ContentLengthK) AnyToken

urlP = Build emptyUrl (
   ProtocolK <-- Alt [String "http:", String "https:", String "ftp:", String "file:"] :/
   String "//" :/ HostK <-- AnyTill (Alt [Token ':', Token '/', Token '?', Token ' ']) :/ PortK <-- Option (Token ':' :/ int) :/
   AbsPathK  <-- Apply (total (split "/") (intercalate "/")) (AnyTill (Alt [Token ' ', Token '?'])) :/ 
   QueryK    <-- Option (Token '?' :/ AnyTill (Token ' ')) :/
   Token ' ' :/
   HTTPVersionK <-- AnyTill crlf :/
   crlf)

hostPort = Build emptyUrl (
   HostK <-- AnyTill (Alt [Token ':', Token '/', Token '?', Token ' ']) :/
   PortK <-- Option (Token ':' :/ int))

header = 
   AnyTill (Token ':') :+
   Token      ':' :/ Many (Token ' ') :/
   AnyTill crlf ://
   crlf

lws = crlf :/ many1 (Alt [Token '\9', Token ' '])

crlf = Alt [String "\r\n", String "\n\r", String "\n", String "\r"]

{-
ifind1 k = totald 
   (\xs -> [toDyn $ lookup k xs, toDyn (filter ((k /=) . fst) xs)]) 
   (\[vd, xsd] -> let 
      v = fromDyn1 vd
      xs = fromDyn1 xsd 
      in (k, v):filter ((k /=) . fst) xs)
-}