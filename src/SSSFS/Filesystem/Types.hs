{-# LANGUAGE OverloadedStrings     #-}

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


module SSSFS.Filesystem.Types
       ( -- | Types
         OID
       , Timestamp
       , Metadata
       , DataBlock(..)
       , INode(..)
       , StorageUnit(..)
       , IType(..)
       , Block
       , BlockSeek
       , BlockIx
       , Seek
       , Size
         -- | INode functions
       , mkINode
       , ensureDirectory
       , ensureFile
       , isFile
       , isDirectory
       , getBlock
       , putBlock
       , truncateI
         -- | Metadata String Functions
       , encode
       , decode
       , encodePair
       , decodePair
       , singleton
       , fromList
       , toList
         -- | Unique IDs
       , newid
       , oidOne
       , keyOne
       , keyZero
         -- | Date/Time Functions
       , now
         -- | StorageUnit Functions
       , emptyBlock
       , fromOID
       , fromStorageUnit
       , fromLinkName
       , value
       , eulav
       , eulavM
       , inodeToINodeUnit
       , inodeToINodePtrUnit
       , inodeToDirEntUnit
       , inodeUnitToINode
         -- | IO
       , enumINode
       ) where

import           Control.Exception
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.UUID
import           Data.UUID.V1
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Map as M
import           Data.Text.Encoding
import           Data.IterIO
import qualified Data.Serialize as S
import           System.Posix.Clock
import           SSSFS.Config
import           SSSFS.Except
import           SSSFS.Storage

type OID = B.ByteString

type Block = B.ByteString

type Timestamp = (Int,Int)

type Seek = Integer

type BlockSeek = Int

type Size = Int

type BlockIx = Int

-- | Arbirtrary information held as a key-value pair
type Metadata = [(B.ByteString, B.ByteString)]

-- | A data block is a pointer to a chunk of data.
-- 
data DataBlock = Direct { addr :: Key 
                        , sz   :: Int
                        }
               deriving (Eq,Show)

data IType = File
           | Directory
           deriving (Eq,Show)

-- | The information about an object in the filesystem. This mimics
-- the tradicional UNIX inode data structure.
data INode = INode { inode  :: OID                 -- ^ Unique id of this inode
                   , itype  :: IType               -- ^ The type of this file (e.g. File, Directory)
                   , atime  :: Timestamp           -- ^ Access time
                   , ctime  :: Timestamp           -- ^ Creation time
                   , mtime  :: Timestamp           -- ^ Modification time
                   , meta   :: Metadata            -- ^ Arbitrary metadata information
                   , blksz  :: Int                 -- ^ Fixed size of each datablock
                   , size   :: Integer             -- ^ Size of this file in bytes
                   , blocks :: M.Map Int DataBlock -- ^ The actual file contents
                   }
           deriving (Eq,Show)

data StorageUnit = INodeUnit Metadata Metadata
                 | DataBlockUnit B.ByteString
                 | INodePtrUnit OID Key
                 | DirEntUnit String OID

newid :: IO OID
newid = fmap (B.concat . L.toChunks . toByteString . fromJust) nextUUID

oidOne :: OID
oidOne = B.pack [1]

keyOne :: Key
keyOne = fromOID oidOne

emptyBlock :: StorageUnit
emptyBlock = DataBlockUnit B.empty

getBlock :: INode -> BlockIx -> Maybe DataBlock
getBlock inum ix = M.lookup ix (blocks inum)

putBlock :: INode -> BlockIx -> DataBlock -> INode
putBlock inum ix blk
  | sz blk == 0 = inum 
  | otherwise   = updateSize $ inum { blocks = uBlocks }
    where uBlocks = M.insert ix blk (blocks inum)
        
truncateI :: INode -> BlockIx -> INode
truncateI inum newSz = updateSize $ inum { blocks = uBlocks }
  where uBlocks = M.filterWithKey (\k _ -> k<newSz) (blocks inum)
                                                
updateSize :: INode -> INode
updateSize inum = inum { size = M.fold (\a b -> szI a + b) 0 (blocks inum) }
  where szI = fromIntegral . sz

keyZero :: Key
keyZero = v
  where Left v = fromStorageUnit emptyBlock

now :: IO Timestamp
now = do { t <- getTime Realtime
         ; return (sec t, nsec t)
         }

isFile :: INode -> Bool
isFile = (==File) . itype

isDirectory :: INode -> Bool
isDirectory = (==Directory) . itype

encode :: T.Text -> B.ByteString
encode = encodeUtf8

decode :: B.ByteString -> T.Text
decode = decodeUtf8

encodePair :: (T.Text, T.Text) -> (B.ByteString, B.ByteString)
encodePair (a,b) = (encode a, encode b)

decodePair :: (B.ByteString, B.ByteString) -> (T.Text, T.Text)
decodePair (a,b) = (decode a, decode b)

singleton :: (T.Text, T.Text) -> Metadata
singleton v = [encodePair v]

fromList :: [(T.Text, T.Text)] -> Metadata
fromList = map encodePair

toList :: Metadata -> [(T.Text, T.Text)]
toList = map decodePair

fromOID :: OID -> Key
fromOID o = fromStr "i" ++ (fromStr $ computeHash o)

fromLinkName :: OID -> String -> Key
fromLinkName o n = fromOID o ++ fromStr n

fromStorageUnit :: StorageUnit -> Either Key (OID -> Key)
fromStorageUnit u = case u
        of INodeUnit _ _
             -> Left $ fromStr "d" ++ hashKey
           DataBlockUnit _
             -> Left $ fromStr "d" ++ hashKey
           INodePtrUnit o _
             -> Left $ fromOID o
           DirEntUnit n _
             -> Right $ \o -> fromLinkName o n
  where v  = value u

        hashValue = computeHash v

        hashKey = map ref [take 2 hashValue, take 3 $ drop 2 hashValue, hashValue]

value :: (S.Serialize a) => a -> B.ByteString
value = S.encode

eulav :: (S.Serialize a) => B.ByteString -> Either String a
eulav = S.decode

eulavM :: (S.Serialize a, Monad m) => B.ByteString -> m a
eulavM raw = case (eulav raw)
             of Left msg
                  -> throw (DataCorruptionExcept msg)
                Right u
                  -> return u

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
                                         , blocks = M.empty
                                         }
                       }

inodeToDirEntUnit :: String -> INode -> StorageUnit
inodeToDirEntUnit n i = DirEntUnit n (inode i)

inodeToINodeUnit :: INode -> StorageUnit
inodeToINodeUnit i = let core = [ (encode "inode", inode i)
                                , (encode "itype", value (itype i))
                                , (encode "atime", value (atime i))
                                , (encode "ctime", value (ctime i))
                                , (encode "mtime", value (mtime i))
                                , (encode "blksz", value (blksz i))
                                , (encode "size", value (size i))
                                , (encode "blocks", value (M.toList $ blocks i))
                                ]
                         user = meta i
                     in INodeUnit core user

inodeUnitToINode :: StorageUnit -> Maybe INode
inodeUnitToINode (INodeUnit c u) = INode <$> (lookup "inode" core)
                                         <*> (lookup "itype" core >>= eulavM)
                                         <*> (lookup "atime" core >>= eulavM)
                                         <*> (lookup "ctime" core >>= eulavM)
                                         <*> (lookup "mtime" core >>= eulavM)
                                         <*> (Just u)
                                         <*> (lookup "blksz" core >>= eulavM)
                                         <*> (lookup "size" core >>= eulavM)
                                         <*> (lookup "blocks" core >>= fmap M.fromList . eulavM)
  where core = map (\(k,v) -> (decode k, v)) c
inodeUnitToINode _               = Nothing

inodeToINodePtrUnit :: INode -> Key -> StorageUnit
inodeToINodePtrUnit i l = INodePtrUnit (inode i) l

enumBlocks :: (MonadIO m, StorageHashLike s) => s -> [DataBlock] -> Onum B.ByteString m ()
enumBlocks _ []     = enumPure B.empty
enumBlocks s [b]    = enumKey s (addr b)
enumBlocks s (b:bs) = enumKey s (addr b) `lcat` (enumBlocks s bs)

enumINode :: (MonadIO m, StorageHashLike s) => s -> INode -> Onum B.ByteString m ()
enumINode s = enumBlocks s . M.elems . blocks

ensureDirectory :: FilePath -> INode -> INode
ensureDirectory path inum
  | isDirectory inum = inum 
  | otherwise        = throw $ NotADir path

ensureFile :: FilePath -> INode -> INode
ensureFile path inum
  | isFile inum = inum 
  | otherwise   = throw $ IsDir path

instance S.Serialize IType where
  put File      = S.putWord8 0
  put Directory = S.putWord8 1
  
  get = do { opcode <- S.getWord8
           ; case opcode
             of 0 -> return File
                1 -> return Directory
                _ -> fail "unknow opcode"
           }

instance S.Serialize DataBlock where
  put (Direct k s) = S.putWord8 0 >> S.put k >> S.put s
  
  get = do { opcode <- S.getWord8
           ; case opcode
             of 0 -> liftA2 Direct S.get S.get
                _ -> fail "unknown opcode"
           }

instance S.Serialize StorageUnit where
  put (INodeUnit c u) = 
    do { S.putWord8 0
       ; S.put c
       ; S.put u
       }
  put (DataBlockUnit raw) =
    do { S.putWord8 1
       ; S.put raw
       }
  put (INodePtrUnit oid l) =
    do { S.putWord8 2
       ; S.put oid
       ; S.put l
       }
  put (DirEntUnit name oid) =
    do { S.putWord8 3
       ; S.put name
       ; S.put oid
       }
  
  get = 
    do { opcode <- S.getWord8
       ; case opcode 
         of 0 -> liftA2 INodeUnit S.get S.get
            1 -> liftA DataBlockUnit S.get
            2 -> liftA2 INodePtrUnit S.get S.get
            3 -> liftA2 DirEntUnit S.get S.get
            _ -> fail "unknown opcode"
       }
