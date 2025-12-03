{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LexicalNegation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

module Server where

import Favs hiding (split, filename, ext)
import HTMLB hiding (sofbs)

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Network.Socket
import Network.HTTP.Client

import Control.Exception

import BString 
import Prelude hiding (null, tail, head, elem, length, (++), (!!), toLower, split, last, take, drop, notElem, concat, takeWhile, dropWhile, putStrLn, putStr, (/))
import Data.List (singleton, transpose, sort, elemIndex, findIndex)
import Data.Char

import System.IO hiding (putStr, putStrLn)
import System.Directory

mypath = "/home/brett/Music"

sep = "/" :: ByteString

d / f = d ++ if isSuffixOf (c sep) d then f else sep ++ f

app m cj req respond = bracket_
   (putStr $ concat [rawPathInfo req, "\n"])
   (putStrLn ("Cleaning up"::ByteString))
   (do
      let 
         fs = formatSpaces $ rawPathInfo req
         user = 
            case remoteHost req of
               SockAddrInet port ip -> 
                  case hostAddressToTuple ip of
                     (127,   0,   0,   1) -> "brett" -- Intel X299
                     (192, 168,  68,  51) -> "brett" -- Intel X299
                     (192, 168,  68,  54) -> "brett" -- Google Pixel 8
                     (192, 168,  68,  55) -> "jamesg" -- ~James
                     _                    -> "unknown"
         fs1 = 
            case stripPrefix "/player" fs of
               Just player -> "/home/brett/code/html/playlist2" / player
               Nothing -> case user of
                              "brett" -> fs
                              _       -> mypath / fs
      case stripPrefix "/spotify" fs of
         Just spotify -> do
            let req = getRequest $ "https://open.spotify.com" / spotify
            let req1 = req { cookieJar = Just cj }
            resp <- httpLbs req1 m
            let body = responseBody resp
            let html = nestParse $ c body
            respond $ responseLBS status200 [] $ c $ formatLBS html
         Nothing -> do
            isDir  <- doesDirectoryExist $ c fs1
            isFile <- doesFileExist      $ c fs1
            let type1 = mimetype fs1
            if | isDir -> do
                  objects <- getDirectoryContents $ c fs1
                  let html1 = map (\n -> thumbhtml (fs1 / n) (fs / n) n) $ map c objects
                  let html2 = map snd (sort (filter fst html1) ++ sort (filter (not . fst) html1))
                  respond $ responseLBS status200 [] $ c $ formatLBS $ html [] [grid [html2]]
               | isFile -> do
                  file <- readBinaryFile $ c fs1
                  respond $ responseLBS status200 [(hContentType, type1)] file
               | True -> do
                  respond $ responseLBS status404 [] "file not found")

dropTo c list = drop (length list - 1 - fromMaybe 0 (elemIndex '/' (reverse list))) list

mimetype fs1 = let
   e = ext $ filename fs1
   in case e of
         ".mp3" -> "audio/mpeg"
         ".mp4" -> "audio/mpeg"
         ".m4a" -> "audio/mpeg"
         ".ogg" -> "audio/ogg"
         ".jpg" -> "image/jpeg"
         ""     -> "text/html"
         _      -> "text/plain"

thumbhtml :: ByteString -> ByteString -> ByteString -> (Bool, [HTML])
thumbhtml fs1 fs n = let
   --fs1 is the actual path
   --fs  is what we're referring to it as
   type1 = mimetype fs1
   in case head $ split "/" type1 of
            "audio" -> (False, audio n fs type1)
            "image" -> (False, image fs)
            "text"  -> (False, link1 fs n)
            other   -> (True , link1 fs n)

link1 fs n = [link fs [Text n]]

audio name url filetype = [Text name, Tag "audio" [("controls", "")] [EmptyTag "source" [("src", parseSpaces url), ("type", filetype)]]]

image url = [EmptyTag "image" [("src", parseSpaces url)]]

imagelink url = [link (parseSpaces url) [EmptyTag "image" [("src", parseSpaces url), ("width","128"), ("height", "128")]]]


readBinaryFile f = do
   h <- openBinaryFile f ReadMode
   c <$> hGetContents h

main = do
   m <- nm
   let cj = createCookieJar []
   run 8888 $ app m cj
