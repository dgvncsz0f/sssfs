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


module SSSFS.Filesystem.Directory
       ( mkdir
       , rmdir
       , rename
       , readDir
       ) where

import Control.Exception
import System.FilePath
import SSSFS.Storage
import SSSFS.Except
import SSSFS.Filesystem.Core
import SSSFS.Filesystem.Types
import SSSFS.Filesystem.Path

mkdir :: (StorageHashLike s) => s -> FilePath -> IO INode
mkdir s path = mknod s 4096 path Directory

rmdir :: (StorageHashLike s, StorageEnumLike s) => s -> FilePath -> IO ()
rmdir s path = do { empty <- fmap not (enumDir s path)
                  ; if (empty)
                    then unlink s path
                    else throw (NotEmpty path)
                  }

rename :: (StorageHashLike s) => s -> FilePath -> FilePath -> IO ()
rename s opath npath = do { ninum <- mStat s npath
                          ; if (nothingOrFile ninum)
                            then renameTo npath
                            else renameTo (npath </> (basename opath))
                          }
  where nothingOrFile (Just x) = isFile x
        nothingOrFile Nothing  = True
        
        renameTo path = do { okey <- fmap (flip iFromDirEnt (basename opath)) (stat s (dirname opath))
                           ; nkey <- fmap (flip iFromDirEnt (basename path)) (stat s (dirname path))
                           ; val  <- get s okey
                           ; put s nkey val
                           ; del s okey
                           }

enumDir :: (StorageContext s r) => s -> FilePath -> IO r
enumDir s path = do { inum <- fmap (ensureDirectory path) (stat s path)
                    ; enum s (iFromINode inum)
                    }

-- | Returns the contents of a given directory
readDir :: (StorageHashLike s, StorageEnumLike s) => s -> FilePath -> IO [(FilePath, INode)]
readDir s path = enumDir s path >>= mapM (statDEnt . showRefS)
  where statDEnt dent = do { inum <- stat s dpath
                           ; return (dent, inum)
                           }
          where dpath = path </> dent
