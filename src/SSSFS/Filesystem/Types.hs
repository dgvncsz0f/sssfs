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
       , INode(..)
       , StorageUnit(..)
       , IType(..)
       , Block
       , BlockSeek
       , BlockSize
       , BlockIx
       , Seek
         -- | INode functions
       , mkINode
       , ensureDirectory
       , ensureFile
       , isFile
       , isDirectory
       , putBlock
       , truncateI
       , address
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
         -- | Date/Time Functions
       , now
         -- | StorageUnit Functions
       , dFromOID
       , iFromOID
       , dFromINode
       , iFromINode
       , fromDirEnt
       , iFromDirEnt
       , fromStorageUnit
       , isDirEnt
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
import           Data.Text.Encoding
import qualified Data.Serialize as S
import           System.Posix.Clock
import           SSSFS.Except
import           SSSFS.Storage

type OID = B.ByteString

type Block = B.ByteString

type Timestamp = (Int,Int)

type Seek = Integer

type BlockSeek = Int

type BlockSize = Int

type BlockIx = Integer

type Blocks = (OID, Integer)

-- | Arbirtrary information held as a key-value pair
type Metadata = [(B.ByteString, B.ByteString)]

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
                   , blksz  :: BlockSize           -- ^ Fixed size of each datablock
                   , size   :: Seek                -- ^ Size of this file in bytes
                   , blocks :: Blocks              -- ^ The actual file contents
                   }
           deriving (Eq,Show)

data StorageUnit = INodeUnit OID Metadata Metadata
                 | DataBlockUnit OID BlockIx B.ByteString
                 | DirEntUnit String OID

newid :: IO OID
newid = fmap (B8.pack . toString . fromJust) nextUUID

oidOne :: OID
oidOne = B8.pack "1"

keyOne :: Key
keyOne = iFromOID oidOne

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

putBlock :: INode -> BlockIx -> Block -> INode
putBlock inum ix bytes
  | ix+1 >= nblocks = inum { size   = uSize
                           , blocks = (oblocks, ix+1)
                           }
  | otherwise       = inum
    where (oblocks, nblocks) = blocks inum
          
          uSize = (fromIntegral $ blksz inum) * ix + (fromIntegral $ B.length bytes)

address :: BlockSize -> Seek -> (BlockIx, BlockSeek)
address bsz offset = let (blk, inOffset) = offset `divMod` (fromIntegral bsz)
                     in (fromIntegral blk, fromIntegral inOffset)

truncateI :: INode -> Seek -> INode
truncateI inum offset = inum { blocks = (oblocks, blkIx)
                             , size   = offset
                             }
  where (oblocks, _) = blocks inum
    
        (blkIx, _) = address (blksz inum) offset

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

dFromOID :: OID -> BlockIx -> Key
dFromOID o ix = fromStr "d" ++ fromOID o ++ fromStr (show ix)

dFromINode :: INode -> BlockIx -> Key
dFromINode i = dFromOID (fst (blocks i))

iFromOID :: OID -> Key
iFromOID = (fromStr "i" ++) . fromOID

iFromINode :: INode -> Key
iFromINode = (fromStr "i" ++) . fromOID . inode

isDirEnt :: Key -> Bool
isDirEnt ["i",_,_] = True
isDirEnt _         = False

fromDirEnt :: OID -> String -> Key
fromDirEnt o n = iFromOID o ++ fromStr n

iFromDirEnt :: INode -> String -> Key
iFromDirEnt i n = fromDirEnt (inode i) n

fromStorageUnit :: StorageUnit -> Either Key (OID -> Key)
fromStorageUnit u = case u
        of INodeUnit o _ _
             -> Left $ iFromOID o
           DataBlockUnit o ix _
             -> Left $ dFromOID o ix
           DirEntUnit n _
             -> Right $ \o -> fromDirEnt o n

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

mkINode :: Maybe OID -> IType -> BlockSize -> IO INode
mkINode moid ftype bsz = do { time  <- now
                            ; ioid  <- fmap pure newid
                            ; boid  <- newid
                            ; return $ INode { inode  = fromJust (moid <|> ioid)
                                             , itype  = ftype
                                             , atime  = time
                                             , ctime  = time
                                             , mtime  = time
                                             , meta   = []
                                             , size   = 0
                                             , blksz  = bsz
                                             , blocks = (boid, 0)
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
                           , (encode "blocks", value (blocks i))
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
                                      <*> (lookup "blocks" core >>= eulavM)
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

instance S.Serialize StorageUnit where
  put (INodeUnit o c u) = 
    do { S.putWord8 0
       ; S.put o
       ; S.put c
       ; S.put u
       }
  put (DataBlockUnit o ix raw) =
    do { S.putWord8 1
       ; S.put o
       ; S.put ix
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
            1 -> liftA3 DataBlockUnit S.get S.get S.get
            2 -> liftA2 DirEntUnit S.get S.get
            _ -> fail "unknown opcode"
       }
