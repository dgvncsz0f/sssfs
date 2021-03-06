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

import           System.IO
import           System.Environment
import           Control.Exception
import qualified Data.ByteString as B
import           Data.Maybe
import           SSSFS.Storage (showKeyS)
import           SSSFS.Filesystem.Types

main :: IO ()
main = do { f        <- fmap head getArgs
          ; contents <- myGetContents f
          ; case (eulav contents)
            of Left msg -> putStrLn "Error decoding unit" >> putStrLn msg
               Right u  -> putStrLn f >> render (decodeUnit u)
          }
  where myGetContents "-" = B.getContents
        myGetContents f   = bracket (openBinaryFile f ReadMode) hClose (B.hGetContents)

        render []         = return ()
        render ((a,b):xs) = putStrLn (a ++ ": " ++ b) >> render xs
    
        decodeUnit (DataBlockUnit o ix _) = [ ("type", "datablock")
                                            , ("oid", show o)
                                            , ("index", show ix)
                                            ]
        decodeUnit (DirEntUnit n o)    = [ ("type", "dirent")
                                         , ("name", n)
                                         , ("oid", showKeyS $ iFromOID o)
                                         ]
        decodeUnit u@(INodeUnit _ _ _) = [ ("type", "inode")
                                         , ("inode", showKeyS $ iFromOID $ inode inum)
                                         , ("itype", show (itype inum))
                                         , ("atime", show (atime inum))
                                         , ("mtime", show (mtime inum))
                                         , ("ctime", show (ctime inum))
                                         , ("blksz", show (blksz inum))
                                         , ("size", show (size inum))
                                         , ("blocks", show (blocks inum))
                                         ]
          where inum = fromJust (unitToINode u)
                
