module SSSFS.Filesystem.Directory
       ( mkdir
       , dirContents
       ) where

import SSSFS.Storage
import SSSFS.Filesystem.Core
import SSSFS.Filesystem.Types

mkdir :: (Storage s) => s -> FilePath -> IO INode
mkdir s path = mknod s path Directory

-- | Returns the contents of a given directory
dirContents :: (Storage s) => s -> FilePath -> IO [FilePath]
dirContents s path = do { inum <- fmap (ensureDirectory path) (stat s path)
                        ; fmap (map showRefS) (enum s (fromOID $ inode inum))
                        }
