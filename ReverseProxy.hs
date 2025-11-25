{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ReverseProxy where

import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.Run.TCP (runTCPClient) -- network-run
import Control.Concurrent.Async
import qualified Control.Exception as E

import Network.HTTP2.Client hiding (run)
import qualified Network.HTTP2.Client as C
--start server
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Exception as E
import Data.ByteString.Builder (byteString)
import Network.HTTP.Types (ok200)
import Network.Run.TCP (runTCPServer) -- network-run

import Network.HTTP2.Server hiding (run)
import qualified Network.HTTP2.Server as S
{-
main :: IO ()
main = runTCPServer Nothing "80" runHTTP2Server
      where
            runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
                                                                                                   freeSimpleConfig
                                                                                                   (\config -> S.run defaultServerConfig config server)
            server _req _aux sendResponse = sendResponse response []
                  where
                        response = responseBuilder ok200 header body
                        header = [("Content-Type", "text/plain")]
                        body = byteString "Hello, world!\n"
--end server

convHeaders req = g

serverName :: String
serverName = "127.0.0.1"

mainc :: IO ()
mainc = runTCPClient serverName "80" $ runHTTP2Client serverName
      where
            cliconf host = defaultClientConfig { authority = host }
            runHTTP2Client host s = E.bracket (allocSimpleConfig s 4096)
                                                                                                                  freeSimpleConfig
                                                                                                                  (\conf -> C.run (cliconf host) conf client)
            client :: Request -> Client ()
            client sendRequest _aux = do
                        let req0 = requestNoBody methodGet "/" []
                                    client0 = sendRequest req0 $ \rsp -> do
                                                print rsp
                                                getResponseBodyChunk rsp >>= C8.putStrLn
                                    req1 = requestNoBody methodGet "/foo" []
                                    client1 = sendRequest req1 $ \rsp -> do
                                                print rsp
                                                getResponseBodyChunk rsp >>= C8.putStrLn
                        ex <- E.try $ concurrently_ client0 client1
                        case ex of
                              Left  e  -> print (e :: HTTP2Error)
                              Right () -> putStrLn "OK"
-}
