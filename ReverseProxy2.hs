{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module ReverseProxy2 where

import Prelude hiding ((/))

import HTML
import HTTPTypes
import HTTP
import Parser4
import Favs
import MHashDynamic

import Control.Monad
import Control.Concurrent

import Data.Maybe
import Data.List

import qualified Data.Map as M

import qualified Data.ByteString as S

import Network.HTTP.Client

import Network.HTTP.Types

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.ReverseProxy

import Network.Socket

import Network.Socket.ByteString

import Network.Run.TCP

import System.IO

d / f = d ++ if last d == '/' then f else '/' : f

runProxy proxy = runTCPServer Nothing "8888" talk
   where
      talk browser = loop ""
         where
            loop buf = do
               msg <- recv browser 1024
               let buf1 = S.append buf msg
               if S.null msg
                  then return ()
                  else do
                     print "recv"
                     print buf1
                     case parse req $ sofbs buf1 of
                        Done rd _ rest -> do
                           putStrLn "done"
                           case (do
                              r     <- fromDynamic rd
                              hostd <- M.lookup Host r
                              host  <- fromDynamic hostd
                              portd <- M.lookup Port r
                              port  <- fromDynamic portd
                              return (host, port)) of 
                                 Just (host, port) -> do
                                    print "connecting..."
                                    runTCPClient host port (connection3 proxy host port buf1 browser)
                                 Nothing -> do
                                    print rd
                                    return ()
                        Fail em fs i -> do
                           print "fail"
                           putStrLn em
                           print i
                           print fs
                           loop buf1
                        other -> putStrLn "other"

type Proxy = Dynamic -> (Dynamic, Dynamic -> Dynamic)

straightThrough a = (a, id)

connection3 proxy host port requestIn browser server = do
   print (host, port)
   {-
   more <- recvAll browser
   print more
   let requestIn = S.append rest more
   -}
   case parse request $ sofbs requestIn of
      Done requestInD _ rest -> let
         fileName = case (do
            r     <- fromDynamic requestInD
            urld  <- M.lookup Url r
            url   <- fromDynamic urld
            pathd <- M.lookup AbsPath url
            fromDynamic pathd) of
               Just  j -> "/home/brett/mycache" / (intercalate "/" $ reverse $ split "." host) / j
               Nothing -> ""

         (requestOutD, responseFunc) = proxy requestInD -- Important line

         requestOut = case format request requestOutD of
            FDone ts _ -> bsofs ts
            FFail {}   -> requestIn
         in do
            print fileName
            sendAll server requestOut
            responseIn <- recvAll server
            case parse response $ sofbs responseIn of
               Done responseInD _ rest -> let

                  responseOutD = responseFunc responseInD -- Important line

                  responseOut  = case format response responseOutD of
                     FDone ts _ -> bsofs ts
                     FFail {}   -> responseIn
                  in do
                     sendAll browser responseOut
                     sendAll browser $ bsofs rest
                     writeBinaryFile (fileName ++ ".req.in") requestIn
                     writeBinaryFile (fileName ++ ".req.out") requestOut
                     writeBinaryFile (fileName ++ ".resp.in") responseIn
                     writeBinaryFile (fileName ++ ".resp.out") responseOut
                     writeBinaryFile fileName $ bsofs rest

writeBinaryFile fileName byteString = do
   h <- openBinaryFile fileName WriteMode
   S.hPut h byteString
   hClose h

recvAll s = loop ""
   where
      loop buf = do
         msg <- recv s 1024
         if S.null msg
            then return buf
            else loop $ S.append buf msg


proxy2 = runTCPServer Nothing "8888" talk
   where
      talk s = do
         (buf, msg) <- readLine "" s
         case S.elemIndex 32 msg of
            Just  j -> let url = sofbs $ S.drop (j+1) msg in runTCPClient url "80" (connection url s)
            Nothing -> return ()
      readLine buf s = do
         msg <- recv s 1024
         if S.null msg
            then return (buf, "")
            else do
               let buf1 = S.append buf msg
               case S.elemIndex 13 buf1 of
                  Just  j -> let
                     msg1 = S.take j buf1
                     buf2 = S.drop (j+1) buf1
                     in do
                        putStrLn $ ('<':) $ sofbs msg1
                        return (buf2, msg1)
                  Nothing -> readLine buf1 s

fileNameFromUrl url =
  let
    s = replace "%20" " " url -- (if ".html" `isSuffixOf` url then id else (++ ".html")) $
   in
    "/home/brett/mycache/"++s

--connection :: Socket -> Socket -> IO ()
connection url browser server = do
   h <- openBinaryFile (fileNameFromUrl url ++ ".hdr") ReadMode
   passFromTo browser server h
   passFromTo server browser h
   hClose h

passFromTo browser server h = loop
   where
      loop = do
         msg <- recv browser 1024
         if S.null msg
            then return ()
            else do
               S.hPut h msg
               send server msg
               loop

browser2server browser server = do
   msg <- recv browser 1024
   if S.null msg
      then return ()
      else do
         send server msg
         browser2server browser server

server2browser browser server = do
   msg <- recv server 1024
   if S.null msg
      then return ()
      else do
         send browser msg
         server2browser browser server

{-
proxy1 = do
   s <- socket AF_INET Stream 8888
   bind s $ SockAddrInet 8888 $ tupleToHostAddress (127, 0, 0, 1)
   listen s 5
   go
      where
         go = do
            (news, addr) <- accept s
-}
proxy = do
   manager <- newManager defaultManagerSettings
   let app = waiProxyTo convertReq onErr manager
   run 8888 app


convertReq r = return $ WPRProxyDest (ProxyDest (fromMaybe "www.google.co.uk" (requestHeaderHost r)) 443)

onErr e req respond = respond $ responseLBS status200 [] $ lbsofs $ show e