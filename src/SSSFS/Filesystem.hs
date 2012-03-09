{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module SSSFS.Filesystem 
       ( mkfs
       , stat
       ) where

import Control.Exception
import System.FilePath
import SSSFS.Config
import SSSFS.Storage
import SSSFS.Except
import SSSFS.Filesystem.Types

type MakeKey = Either Key (OID -> Key) -> Key

makeKey :: (Maybe OID) -> MakeKey
makeKey _ (Left k)         = k
makeKey Nothing (Right _)  = error "cant create key without oid"
makeKey (Just o) (Right f) = f o

putUnit :: (Storage s) => s -> StorageUnit -> MakeKey -> IO Key
putUnit s u f = let k = f $ fromStorageUnit u
                in put s k (value u) >> return k

putUnit_ :: (Storage s) => s -> StorageUnit -> MakeKey -> IO ()
putUnit_ s u f = put s (f $ fromStorageUnit u) (value u)

getUnit :: (Storage s) => s -> Key -> IO StorageUnit
getUnit s l = get s l >>= eulavM

follow :: (Storage s) => s -> Key -> IO INode
follow s l = getUnit s l >>= followUM
  where followU (INodePtrUnit _ l2) = fmap inodeUnitToINode (getUnit s l2)
        followU (DirEntUnit _ l2)   = fmap inodeUnitToINode (getUnit s (fromOID l2))
        followU _                   = return Nothing
        
        followUM u = do { mv <- followU u
                        ; case mv 
                          of Nothing
                               -> throw NotFound
                             Just v
                               -> return v
                        }

-- | Initializes a new filesystem. This is a very fast operation. It
-- creates a new inode with no contents and changes the root inode
-- ptr, which will point to the newly created inode. Therefore old
-- data will still be preserved but from now on is unreachable.
mkfs :: (Storage s) => s -> IO ()
mkfs s = do { inum <- fmap mkINode now
            ; l    <- putUnit s (inodeToINodeUnit inum) (makeKey Nothing)
            ; putUnit_ s (inodeToINodePtrUnit inum l) (makeKey Nothing)
            }
  where mkINode time = INode { inode  = oidOne
                             , itype  = Directory
                             , atime  = time
                             , ctime  = time
                             , mtime  = time
                             , meta   = []
                             , size   = 0
                             , blksz  = blockSize
                             , blocks = []
                             }

-- | This operation is only defined for absolute paths. The behavior
-- is undefined if you provide a relative path.
stat :: (Storage s) => s -> FilePath -> IO INode
stat s p = do { root <- follow s keyOne
              ; stat_ root (tail $ map dropTrailingPathSeparator (splitPath p))
              }
  where stat_ inum []        = return inum
        stat_ inum (x:xs) 
          | isDirectory inum = follow s (fromLinkName (inode inum) x) >>= flip stat_ xs
          | otherwise        = throw NotFound
        
