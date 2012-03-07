{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module SSSFS.Filesystem 
       ( mkfs
       ) where

import           SSSFS.Config
import           SSSFS.Storage
import           SSSFS.Filesystem.Types

putUnit :: (Storage s) => s -> StorageUnit -> IO Loc
putUnit s u = put s (key u) (value u) >> return (key u)

putUnit_ :: (Storage s) => s -> StorageUnit -> IO ()
putUnit_ s u = put s (key u) (value u)

getUnit :: (Storage s) => s -> Loc -> IO StorageUnit
getUnit s l = get s l >>= eulavM

mkfs :: (Storage s) => s -> IO ()
mkfs s = do { inum <- fmap mkINode now
            ; l    <- putUnit s (inodeToINodeUnit inum)
            ; putUnit_ s (inodeToINodePtrUnit inum l)
            }
  where mkINode time = INode { inode  = oidOne
                             , atime  = time
                             , ctime  = time
                             , mtime  = time
                             , meta   = []
                             , size   = 0
                             , blksz  = blockSize
                             , blocks = []
                             }

statRef :: (Storage s) => s -> (INode,String) -> IO INode
statRef s par name = do { tmpFile <- return "/tmp/foobar.cdb" -- enumINode |. tmpfileI
                        ; 
                        }

-- stat :: (Throws SysExcept e, Throws UsrExcept e, Storage s) => s -> Loc -> EMT e IO INode
-- stat s (_:path) = getUnit s locOne >>= statWith
--   where statWith parent (p:ps) = 