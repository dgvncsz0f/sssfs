{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module SSSFS.Filesystem 
       ( mkfs
       , stat
       , mkdir
       , creat
       , mknod
       , dirContents
       ) where

import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Maybe
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

makeKey_ :: MakeKey
makeKey_ = makeKey Nothing

makeKey1 :: OID -> MakeKey
makeKey1 = makeKey . Just

putUnit :: (Storage s) => s -> StorageUnit -> MakeKey -> IO Key
putUnit s u f = let k = f $ fromStorageUnit u
                in put s k (value u) >> return k

putUnit_ :: (Storage s) => s -> StorageUnit -> MakeKey -> IO ()
putUnit_ s u f = let k = f $ fromStorageUnit u
                 in put s k (value u)

getUnit :: (Storage s) => s -> Key -> IO StorageUnit
getUnit s l = get s l >>= eulavM

follow :: (Storage s) => s -> Key -> IO INode
follow s l = getUnit s l >>= followUM
  where followU (INodePtrUnit _ l2) = fmap inodeUnitToINode (getUnit s l2)
        followU (DirEntUnit _ o)    = getUnit s (fromOID o) >>= followU
        followU _                   = return Nothing
        
        followUM u = do { mv <- followU u
                        ; case mv 
                          of Nothing
                               -> throw (ParseExcept $ "INodePtrUnit or DirEntUnit expected ["++ showKeyS l ++"]")
                             Just v
                               -> return v
                        }

mkINode :: Maybe OID -> IType -> IO INode
mkINode moid ftype = do { time  <- now
                        ; moid2 <- fmap pure newid
                        ; return $ INode { inode  = fromJust (moid <|> moid2)
                                         , itype  = ftype
                                         , atime  = time
                                         , ctime  = time
                                         , mtime  = time
                                         , meta   = []
                                         , size   = 0
                                         , blksz  = blockSize
                                         , blocks = []
                                         }
                       }

-- | Initializes a new filesystem. This is a very fast operation. It
-- creates a new inode with no contents and changes the root inode
-- ptr, which will point to the newly created inode. Therefore old
-- data will still be preserved but from now on is unreachable.
mkfs :: (Storage s) => s -> IO ()
mkfs s = do { inum <- mkINode (Just oidOne) Directory
            ; l    <- putUnit s (inodeToINodeUnit inum) makeKey_
            ; putUnit_ s (inodeToINodePtrUnit inum l) makeKey_
            }

mkdir :: (Storage s) => s -> FilePath -> IO INode
mkdir s path = mknod s path Directory

creat :: (Storage s) => s -> FilePath -> IO INode
creat s path = mknod s path File

-- | Creates a new entry on a given directory. The dirname of this
-- path must already be defined and must be a directory already.
-- 
-- This function does not check if there is a link already. It will
-- get overwritten with no mercy. Make sure you do this before calling
-- mknod.
mknod :: (Storage s) => s -> FilePath -> IType -> IO INode
mknod s path ftype = do { p_inum <- fmap (ensureDirectory dir) (stat s dir)
                        ; inum   <- mkINode Nothing ftype
                        ; l      <- putUnit s (inodeToINodeUnit inum) makeKey_
                        ; putUnit_ s (inodeToINodePtrUnit inum l) makeKey_
                        ; putUnit_ s (inodeToDirEntUnit (basename path) inum) (makeKey1 (inode p_inum))
                        ; return inum
                        }
  where dir = dirname path

-- | Returns the contents of a given directory
dirContents :: (Storage s) => s -> FilePath -> IO [FilePath]
dirContents s path = do { inum <- fmap (ensureDirectory path) (stat s path)
                        ; fmap (map showRefS) (enum s (fromOID $ inode inum))
                        }

-- | This operation is only defined for absolute paths. The behavior
-- is undefined if you provide a relative path.
stat :: (Storage s) => s -> FilePath -> IO INode
stat s p = do { root <- follow s keyOne
              ; stat_ root (tail $ safeSplitPath p)
              }
  where stat_ inum []        = return inum
        stat_ inum (x:xs)
          | isDirectory inum = follow s (fromLinkName (inode inum) x) >>= flip stat_ xs
          | otherwise        = throw (NotADirectory p)

ensureDirectory :: FilePath -> INode -> INode
ensureDirectory path inum
  | isDirectory inum = inum 
  | otherwise        = throw $ NotADirectory path

safeSplitPath :: FilePath -> [FilePath]
safeSplitPath = map dropTrailingPathSeparator . splitPath

dirname :: FilePath -> FilePath
dirname = joinPath . init . safeSplitPath

basename :: FilePath -> FilePath
basename = last . safeSplitPath