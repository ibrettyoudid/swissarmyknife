{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LexicalNegation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

module Server where

import Favs
import HTML hiding (sofbs)

import Prelude hiding ((/))

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Network.Socket

import Control.Exception

import Data.ByteString qualified as B
import Data.List
import Data.Char

import System.IO
import System.Directory

path = "/home/brett/Documents/Music"

d / f = d ++ if isSuffixOf "/" d then f else "/" ++ f

app req respond = bracket_
   (B.putStr $ B.concat [rawPathInfo req, "\n"])
   (putStrLn "Cleaning up")
   (do
      let fs = formatSpaces $ sofbs $ rawPathInfo req
      let 
         user = 
            case remoteHost req of
               SockAddrInet port ip -> 
                  case hostAddressToTuple ip of
                     (127,   0,   0,   1) -> "brett" -- Intel X299
                     (192, 168,   0, 125) -> "brett" -- Intel X299
                     (192, 168,   0,  60) -> "brett" -- Google Pixel 8
                     (192, 168,   0, 197) -> "brett" -- Google Pixel 8
                     (192, 168,   0, 200) -> "brett" -- ASUS laptop
                     (192, 168,   0,  43) -> "adamw" -- Adam W
                     (192, 168,   0, 115) -> "jamesg" -- ~James
                     (192, 168,   0, 190) -> "adamw" -- Adam B / Samsung S21
                     _                  -> "unknown"
         path1 = 
            case user of
               "brett" -> ""
               _       -> path
      let fs1 = path1 / fs
      isDir <- doesDirectoryExist fs1
      isFile <- doesFileExist fs1
      let type1 = mimetype fs1
      if | isDir -> do
            objects <- getDirectoryContents fs1
            let html1 = map (\n -> object (fs1 / n) (fs / n) fs n) objects
            let html2 = map snd (sort (filter fst html1) ++ sort (filter (not . fst) html1))
            respond $ responseLBS status200 [] $ formatLBS $ html [] [grid [html2]]
         | isFile -> do
            file <- readBinaryFile fs1
            respond $ responseLBS status200 [(hContentType, type1)] file
         | True -> do
            respond $ responseLBS status404 [] "file not found")

dropTo c list = drop (length list - 1 - fromMaybe 0 (elemIndex '/' (reverse list))) list
filename list = drop (length list - 1 - fromMaybe (length list - 1) (elemIndex '/' (reverse list))) list
ext list = drop (length list - 1 - fromMaybe -1 (elemIndex '.' (reverse list))) list

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

object fs1 fs d n = let
   type1 = mimetype fs1
   in case head $ split "/" type1 of
            "audio" -> (False, audio n fs type1)
            "image" -> (False, image fs)
            "text"  -> (False, link1 fs n)
            other   -> (True, link1 fs n)

link1 fs n = [link fs [Text n]]

audio name url filetype = [Text name, Tag "audio" [("controls", "")] [EmptyTag "source" [("src", url), ("type", filetype)]]]

image url = [EmptyTag "image" [("src", url)]]

readBinaryFile f = do
   h <- openBinaryFile f ReadMode
   lbsofs <$> hGetContents h

main = run 8000 app