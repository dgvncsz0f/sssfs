-- Copyright (c) 2012, Diego Souza
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--   * Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright notice,
--     this list of conditions and the following disclaimer in the documentation
--     and/or other materials provided with the distribution.
--   * Neither the name of the <ORGANIZATION> nor the names of its contributors
--     may be used to endorse or promote products derived from this software
--     without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module Main where

import           Data.Char
import           System.Console.GetOpt
import           System.Environment
import           SSSFS.Fuse
import           SSSFS.Fuse.DebugFuse
import           SSSFS.Filesystem.LocalStorage
import           SSSFS.Storage.DebugStorage
import           SSSFS.Storage.S3Storage

data Options = Options { rootdir     :: String
                       , mountpt     :: String
                       , optDebug    :: Bool
                       , optFuseOpts :: [String]
                       } deriving Show

options :: [OptDescr (Options -> Options)]
options = [ Option ['d'] ["debug"]
            (NoArg (\opts -> opts { optDebug = True }))
            "print debug information on stdout/stderr"
          , Option ['o'] ["fuse-opt"]
            (ReqArg (\s opts -> opts { optFuseOpts = optFuseOpts opts ++ makeOpt s })
                    "OPT")
            "Fuse mount option (may be used multiple times)"
          ]
  where makeOpt s = let (a, b) = break isSpace s
                    in filter (not . null) [a, drop 1 b]

defaultOptions :: Options
defaultOptions = Options { rootdir     = ""
                         , mountpt     = ""
                         , optDebug    = False
                         , optFuseOpts = []
                         }

sssfsOptions :: String -> [String] -> Either String Options
sssfsOptions prg argv = case (getOpt Permute options argv) 
                        of (o,[a,b],[])
                             -> Right $ (foldl (flip id) defaultOptions o) { rootdir = a, mountpt = b}
                           (_,_,errs)
                             -> Left $ concat errs ++ usageInfo header options
  where header = "Usage: "++ prg ++" [OPTIONS...] rootdir mountpoint"

main :: IO ()
main = do { prg   <- getProgName
          ; mopts <- fmap (sssfsOptions prg) getArgs
          ; case mopts
            of Left err
                 -> error err
               Right opts
                 -> do { s <- new $ rootdir opts
                       ; withArgs (fuseOpts opts) $ exec (storage s opts) (fuseDbg opts)
                       }
          }
  where storage s opts
           | optDebug opts = Left $ DebugStorage s
           | otherwise     = Right $ s
        
        fuseDbg opts
          | optDebug opts = debugFuse
          | otherwise     = id

        fuseOpts opts = optFuseOpts opts ++ [mountpt opts]
        
        exec (Left s)  = sssfsMain s
        exec (Right s) = sssfsMain s
