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
       , Blocks
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
       , oidZero
       , keyZero
       , oidOne
       , keyOne
         -- | Date/Time Functions
       , now
         -- | StorageUnit Functions
       , dFromOID
       , iFromOID
       , iFromINode
       , fromStorageUnit
       , fromLinkName
       , value
       , eulav
       , eulavM
       , inodeToUnit
       , inodeToDirEntUnit
       , unitToINode
       ) where

import           Control.Exception
import           Control.Applicative
import           Data.UUID
import           Data.UUID.V1
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Map as M
import           Data.Text.Encoding
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

type Blocks = M.Map Int DataBlock

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
                   , blocks :: Blocks              -- ^ The actual file contents
                   }
           deriving (Eq,Show)

data StorageUnit = INodeUnit OID Metadata Metadata
                 | DataBlockUnit OID B.ByteString
                 | DirEntUnit String OID

newid :: IO OID
newid = fmap (B8.pack . toString . fromJust) nextUUID

oidOne :: OID
oidOne = B8.pack "1"

oidZero :: OID
oidZero = B8.pack "0"

keyOne :: Key
keyOne = fromStr "i" ++ fromOID oidOne

keyZero :: Key
keyZero = fromStr "d" ++ fromOID oidZero

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
fromOID o = fromStr (B8.unpack o)

dFromOID :: OID -> Key
dFromOID = (fromStr "d" ++) . fromOID

iFromOID :: OID -> Key
iFromOID = (fromStr "i" ++) . fromOID

iFromINode :: INode -> Key
iFromINode = (fromStr "i" ++) . fromOID . inode

fromLinkName :: OID -> String -> Key
fromLinkName o n = iFromOID o ++ fromStr n

fromStorageUnit :: StorageUnit -> Either Key (OID -> Key)
fromStorageUnit u = case u
        of INodeUnit o _ _
             -> Left $ iFromOID o
           DataBlockUnit o _
             -> Left $ dFromOID o
           DirEntUnit n _
             -> Right $ \o -> fromLinkName o n

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

inodeToUnit :: INode -> StorageUnit
inodeToUnit i = let core = [ (encode "inode", inode i)
                           , (encode "itype", value (itype i))
                           , (encode "atime", value (atime i))
                           , (encode "ctime", value (ctime i))
                           , (encode "mtime", value (mtime i))
                           , (encode "blksz", value (blksz i))
                           , (encode "size", value (size i))
                           , (encode "blocks", value (M.toList $ blocks i))
                           ]
                    user = meta i
                in INodeUnit (inode i) core user

inodeToDirEntUnit :: String -> INode -> StorageUnit
inodeToDirEntUnit n i = DirEntUnit n (inode i)

unitToINode :: StorageUnit -> Maybe INode
unitToINode (INodeUnit _ c u) = INode <$> (lookup "inode" core)
                                      <*> (lookup "itype" core >>= eulavM)
                                      <*> (lookup "atime" core >>= eulavM)
                                      <*> (lookup "ctime" core >>= eulavM)
                                      <*> (lookup "mtime" core >>= eulavM)
                                      <*> (Just u)
                                      <*> (lookup "blksz" core >>= eulavM)
                                      <*> (lookup "size" core >>= eulavM)
                                      <*> (lookup "blocks" core >>= fmap M.fromList . eulavM)
  where core = map (\(k,v) -> (decode k, v)) c
unitToINode _               = Nothing

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
  put (INodeUnit o c u) = 
    do { S.putWord8 0
       ; S.put o
       ; S.put c
       ; S.put u
       }
  put (DataBlockUnit o raw) =
    do { S.putWord8 1
       ; S.put o
       ; S.put raw
       }
  put (DirEntUnit name oid) =
    do { S.putWord8 2
       ; S.put name
       ; S.put oid
       }
  
  get = 
    do { opcode <- S.getWord8
       ; case opcode 
         of 0 -> liftA3 INodeUnit S.get S.get S.get
            1 -> liftA2 DataBlockUnit S.get S.get
            2 -> liftA2 DirEntUnit S.get S.get
            _ -> fail "unknown opcode"
       }
