module HTTP where

import Parser4
import MHashDynamic hiding (Method, Apply, Value)

import HTTPTypes

import Data.List
import Data.Char

req = Alt [requestLine, request]

requestLine = Build $ Seq [
   Method <-- Alt [String "CONNECT"],
   Many (Token ' '),
   Url <-- hostPort,
   Token ' ',
   HTTPVersion <-- AnyTill crlf,
   crlf]

request = Build $ Seq [
   Method <-- Alt [String "GET", String "get"],
   Many (Token ' '),
   Url <-- url,
   Headers <-- ManyTill header crlf,
   crlf]
   --SetM [Headers1, ContentLength] $ Apply (ilookup "Content-Length") $ Get Headers]
   --Body <-- Count (Get ContentLength) AnyToken]

response = Build $ Seq [
   HTTPVersion <-- Many AnyToken,
   Token ' ',
   StatusCode <-- int,
   Token ' ',
   ReasonPhrase <-- Many AnyToken,
   crlf,
   Headers <-- Many header,
   crlf]

message = Seq [
   Headers <-- Many header,
   crlf,
   SetM [Headers1, ContentLength] $ Apply (ilookup "Content-Length") $ Get Headers,
   Body <-- Count (Get ContentLength) AnyToken]

url = Seq [
   Protocol <-- Alt [String "http:", String "https:", String "ftp:", String "file:"],
   Alt [
      Seq [
         String   "//", 
         Host     <-- AnyTill (Alt [Token ':', Token '/', Token '?', Token ' ']), 
         Port     <-- Alt [Seq [Ignore $ Token ':', int], Seq []]], 
      Seq []],
   AbsPath  <-- AnyTill (Alt [Token ' ', Token '?']), 
   Query    <-- Alt [Seq [Token '?', AnyTill (Token ' ')], Seq []],
   Token ' ',
   HTTPVersion <-- AnyTill crlf,
   crlf]

hostPort = Seq [
   Host     <-- AnyTill (Alt [Token ':', Token '/', Token '?', Token ' ']),
   Alt [Seq [Token ':', Port <-- int], Seq []]]

header = Build $ Seq [
   HeaderName <-- AnyTill (Token ':'),
   Token      ':', --Many (Token ' '),
   Value      <-- AnyTill crlf,
   crlf]

many1 p = Seq [p, Many p]

lws = Seq [crlf, many1 $ Alt [Token '\9', Token ' ']]

crlf = Alt [String "\r\n", String "\n\r", Token '\n', Token '\r']

int = Apply istr $ Many $ Range '0' '9'

alpha = Alt [Range 'a' 'z', Range 'A' 'Z']

low = map toLower

ilookup k = isod 
   (\xs -> do
      v <- find ((low k ==) . low . head) xs
      let xs1 = filter ((low k /=) . low . head) xs
      return [toDyn v, toDyn xs1])
   (\[vd, xsd] -> let 
      v = fromDyn1 vd
      xs = fromDyn1 xsd 
      in Just $ [k, v]:filter ((low k /=) . low . head) xs)
{-
ifind1 k = totald 
   (\xs -> [toDyn $ lookup k xs, toDyn (filter ((k /=) . fst) xs)]) 
   (\[vd, xsd] -> let 
      v = fromDyn1 vd
      xs = fromDyn1 xsd 
      in (k, v):filter ((k /=) . fst) xs)
-}