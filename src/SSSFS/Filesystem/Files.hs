module SSSFS.Filesystem.Files
       ( creat
       ) where

import SSSFS.Storage
import SSSFS.Filesystem.Types
import SSSFS.Filesystem.Core

-- | Creates a new empty file
creat :: (Storage s) => s -> FilePath -> IO INode
creat s path = mknod s path File
