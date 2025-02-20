{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Redundant multi-way if" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use map once" #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Eta reduce" #-}

import BString
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.ByteString as B
import Data.Char
import qualified Data.HashTable as H
import Data.List
import Favs hiding (split)
import HTML hiding (bsofs, cache, sofbs)
import MyPretty2
import Network.Socket
import Network.Socket.ByteString
import Prob
import ShowTuple
import System.Directory
import System.IO

import qualified Data.Map as M
import qualified Data.Set as S

import qualified Data.Array.IArray as A

allEqual a = and $ zipWith (==) a (tail a)

groupR a = group $ rsort a

groupByR f = groupBy (\l r -> f r l == EQ) . sortBy (flip f)

rsortBy f = sortBy (flip f)

deleteMulti as bs = foldr delete bs as

insSort xs = foldl (flip insert) [] xs

convHexDigit x = elemIndex x "0123456789abcdef"

convHexDigit1 x = if x == ' ' then Just 0 else elemIndex x "0123456789abcdef"

convHex xs = sum . zipWith (*) (iterate (* 16) 1) . reverse <$> mapM convHexDigit xs

convHex1 xs = sum . zipWith (*) (iterate (* 16) 1) . reverse <$> mapM convHexDigit1 xs

a <.> b = (\x -> a <$> b x)

convBytes xs = mapM (\x -> mychr <$> (convHex1 $ take 2 x)) $ groupN 3 xs

mychr x =
  if
    | x == 10 -> '\n'
    | x < 32 || x >= 127 -> '.'
    | otherwise -> chr x

convLine xs = do
  let (xs1, xs2) = splitAt 4 xs
  addr <- convHex xs1
  let xs3 = take 48 $ drop 2 xs2
  convBytes xs3

{-
Not sure how I'm gonna make the browsers accept this as a proxy on https
Connect to my local proxy by http, redirect urls in html returned to http
Connect to the real server on https so it doesn't redirect you

Quadral multiplication table (or is it Tetral?)
0  1  2  3
1  1  2  3
2  2 10 12
3  3 12 21

Octal Multiplication table
0  1  2  3  4  5  6  7
1  1  2  3  4  5  6  7
2  2  4  6 10 12 14 16
3  3  6 11 14 17 22 25
4  4 10 14 20 24 30 34
5  5 12 17 24 31 36 43
6  6 14 22 30 36 44 52
7  7 16 25 34 43 52 61

It's like I have to stare at a grid to get organised
-}

draw c = do cl <- lift get; lift $ put $ delete c cl

draw1 c = lift $ do cl <- get; put $ delete c cl

{-
pickCard :: (MonadIO m) => Int -> StateT Int m Int
pickCard cardsLeft = do
   cn <- lift $ uniformRM (0::Int, 59) globalStdGen
   if shift 1 cn .&. cardsLeft == 0
      then pickCard cardsLeft
      else return cn
-}
data Var = Var

breaks for inside = (a, B.drop (B.length for) b) where (a, b) = B.breakSubstring for inside

recvUpto :: B.ByteString -> StateT (Socket, B.ByteString) IO B.ByteString
recvUpto delim = do
  (s :: Socket, buf :: B.ByteString) <- get
  loop s buf
 where
  loop s buf = do
    dat <- lift $ recv s 1024
    let buf1 = B.append buf dat
    let (b, a) = B.breakSubstring delim buf1
    if B.null a
      then do
        loop s buf1
      else do
        put (s, B.drop (B.length delim) a)
        return b

bconcat ss = foldr B.append "" ss

bput s = B.putStr (B.append s "\n")

cache = "~/.bcache"

proxyserver = do
  (s, _) <- get
  line1 <- recvUpto "\n"
  let (method : url : ver : _) = B.split 32 line1
  lift $ bput $ bconcat ["method: ", method, " url: ", url, " ver: ", ver]
  let (proto : gap : host1 : ufile) = split "/" url
  let host = split "." host1
  let rhost = reverse host
  let subdir = rhost ++ ufile
  let filename = sofbs $ cache & sintercalate "/" (rhost ++ ufile)
  flag <- lift $ doesFileExist $ filename ++ ".hdr"
  if flag
    then lift $ do
      hdr <- B.readFile $ filename ++ ".hdr"
      sendAll s hdr
      dat <- B.readFile filename
      sendAll s dat
    else do
      -- let http = method & " " & "https://" & host1 & bconcat ufile & " " & ver
      -- lift $ runTCPClient (sofbs host1) "443" (proxyclient http filename s)
      lift $ runTCPClient (sofbs host1) "80" (proxyclient line1 filename s)

      return ()

proxyclient line1 filename ourclient = do
  (s, _) <- get
  lift $ sendAll s line1
  hdr <- recvUpto "\n\n"
  lift $ sendAll ourclient hdr
  lift $ B.writeFile (filename ++ ".hdr") hdr
  h <- lift $ openBinaryFile filename WriteMode
  loop s h
 where
  loop s h = do
    chunk <- lift $ recv s 65536
    lift $ sendAll ourclient chunk
    lift $ B.hPutStr h chunk
    loop s h

minel xs = findel minimum xs

-- maxel = findel maximum

findel f xs = fromJust $ elemIndex (f xs) xs

-- l = x mean $ x minimum k

main = main1

main1 :: IO ()
main1 = do
  runTCPServer Nothing "8888" proxyserver

runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close run
 where
  resolve = do
    let hints = defaultHints{addrSocketType = Stream}
    head <$> getAddrInfo (Just hints) (Just host) (Just port)
  open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    connect sock $ addrAddress addr
    return sock
  run sock = evalStateT client (sock, "")

runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
 where
  resolve = do
    let hints =
          defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
    head <$> getAddrInfo (Just hints) mhost (Just port)
  open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    listen sock 128
    return sock
  loop sock = forever $
    E.bracketOnError (accept sock) (close . fst) $
      \(conn, _peer) ->
        void $
          -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
          -- but 'E.bracketOnError' above will be necessary if some
          -- non-atomic setups (e.g. spawning a subprocess to handle
          -- @conn@) before proper cleanup of @conn@ is your case
          forkFinally (evalStateT server (conn, "")) (const $ gracefulClose conn 5000)
