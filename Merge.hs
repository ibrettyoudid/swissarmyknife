-- Distributed version control system
-- spreads as needed throughout the file system
module Merge where

import HTML

import System.Directory
import System.Process

import Prelude hiding ((/))

diff = callProcess "diff" ["--help"]

a / b = if last a == '/' then a ++ b else a ++ '/' : b

initRepo dir = do
         createDirectory $ dir / ".diff"

addFile file = do
         copyFile file $ ".diff" / file

   
