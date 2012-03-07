{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SSSFS.Filesystem.Types
       ( -- | Types
         OID
       , Timestamp
       , Metadata
       , DataBlock(..)
       , INode(..)
       , StorageUnit(..)
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
       , locOne
         -- | Date/Time Functions
       , now
         -- | StorageUnit Functions
       , key
       , value
       , eulav
       , eulavM
       , inodeToINodeUnit
       , inodeToINodePtrUnit
         -- | Iteratees
       , enumINode
       ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.UUID
import           Data.UUID.V1
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.IterIO
import qualified Data.Serialize as S
import           System.Posix.Clock
import           SSSFS.Except
import           SSSFS.Storage

type OID = B.ByteString

type Timestamp = (Int,Int)

-- | Arbirtrary information held as a key-value pair
type Metadata = [(B.ByteString, B.ByteString)]

-- | A data block is a pointer to a chunk of data.
-- 
data DataBlock = Direct { addr :: Loc }

-- | The information about an object in the filesystem. This mimics
-- the tradicional UNIX inode data structure.
data INode = INode { inode  :: OID         -- ^ Unique id of this inode
                   , atime  :: Timestamp   -- ^ Access time
                   , ctime  :: Timestamp   -- ^ Creation time
                   , mtime  :: Timestamp   -- ^ Modification time
                   , meta   :: Metadata    -- ^ Arbitrary metadata information
                   , blksz  :: Int         -- ^ Fixed size of each datablock
                   , size   :: Integer     -- ^ Size of this file in bytes
                   , blocks :: [DataBlock] -- ^ The actual file contents
                   }

data StorageUnit = INodeUnit Metadata Metadata
                 | DataBlockUnit B.ByteString
                 | INodePtrUnit OID Loc

newid :: IO OID
newid = fmap (B.concat . L.toChunks . toByteString . fromJust) nextUUID

oidOne :: OID
oidOne = B.pack [1]

locOne :: Loc
locOne = computeHash oidOne

now :: IO Timestamp
now = do { t <- getTime Realtime
         ; return (sec t, nsec t)
         }

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

key :: StorageUnit -> Loc
key u = case u
        of INodeUnit _ _
             -> loc "d" ++ computeHash v
           DataBlockUnit _
             -> loc "d" ++ computeHash v
           INodePtrUnit o _
             -> loc "i" ++ computeHash o
  where v = value u

value :: StorageUnit -> B.ByteString
value = S.encode

eulav :: B.ByteString -> Either String StorageUnit
eulav = S.decode

eulavM :: B.ByteString -> IO StorageUnit
eulavM raw = case (eulav raw)
             of Left _
                  -> throw ParseExcept
                Right u
                  -> return u

inodeToINodeUnit :: INode -> StorageUnit
inodeToINodeUnit i = let core = [ (encode "inode", inode i)
                                , (encode "atime", B.empty)
                                , (encode "ctime", B.empty)
                                , (encode "mtime", B.empty)
                                , (encode "blksz", B.empty)
                                , (encode "size", B.empty)
                                , (encode "blocks", B.empty)
                                ]
                         user = meta i
                     in INodeUnit core user

inodeToINodePtrUnit :: INode -> Loc -> StorageUnit
inodeToINodePtrUnit i l = INodePtrUnit (inode i) l

enumBlocks :: (MonadIO m, Storage s) => s -> [DataBlock] -> Onum B.ByteString m ()
enumBlcoks s []     = enumPure B.empty
enumBlocks s [b]    = enumKey s (addr b)
enumBlocks s (b:bs) = enumKey s (addr b) `lcat` (enumBlocks s bs)

enumINode :: (MonadIO m, Storage s) => s -> INode -> Onum B.ByteString m ()
enumINode s = enumBlocks s . blocks

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
  
  get = 
    do { opcode <- S.getWord8
       ; case opcode 
         of 0 -> getINodeUnit
            1 -> getDataBlockUnit
            2 -> getINodePtrUnit
            _ -> fail "unknown opcode"
       }
    where 
          getINodeUnit     = do { core <- S.get
                                ; user <- S.get
                                ; return (INodeUnit core user)
                                }
          
          getDataBlockUnit = fmap DataBlockUnit S.get
          
          getINodePtrUnit  = do { o <- S.get
                                ; l <- S.get
                                ; return (INodePtrUnit o l)
                                }