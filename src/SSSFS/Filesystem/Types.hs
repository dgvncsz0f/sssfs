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
       , keyOne
         -- | Date/Time Functions
       , now
         -- | StorageUnit Functions
       , fromOID
       , fromStorageUnit
       , fromLinkName
       , value
       , eulav
       , eulavM
       , inodeToINodeUnit
       , inodeToINodePtrUnit
       , inodeUnitToINode
         -- | Iteratees
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
data DataBlock = Direct { addr :: Key }

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
                 | INodePtrUnit OID Key
                 | DirEntUnit String OID

newid :: IO OID
newid = fmap (B.concat . L.toChunks . toByteString . fromJust) nextUUID

oidOne :: OID
oidOne = B.pack [1]

keyOne :: Key
keyOne = fromOID oidOne

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

fromOID :: OID -> Key
fromOID o = fromStr "i" ++ computeHash o

fromLinkName :: OID -> String -> Key
fromLinkName o n = fromOID o ++ fromStr n

fromStorageUnit :: StorageUnit -> Either Key (OID -> Key)
fromStorageUnit u = case u
        of INodeUnit _ _
             -> Left $ fromStr "d" ++ computeHash v
           DataBlockUnit _
             -> Left $ fromStr "d" ++ computeHash v
           INodePtrUnit o _
             -> Left $ fromOID o
           DirEntUnit n _
             -> Right $ \o -> fromLinkName o n
  where v = value u

value :: (S.Serialize a) => a -> B.ByteString
value = S.encode

eulav :: (S.Serialize a) => B.ByteString -> Either String a
eulav = S.decode

eulavM :: (S.Serialize a, Monad m) => B.ByteString -> m a
eulavM raw = case (eulav raw)
             of Left msg
                  -> throw (ParseExcept msg)
                Right u
                  -> return u

inodeToINodeUnit :: INode -> StorageUnit
inodeToINodeUnit i = let core = [ (encode "inode", inode i)
                                , (encode "atime", S.encode (atime i))
                                , (encode "ctime", S.encode (ctime i))
                                , (encode "mtime", S.encode (mtime i))
                                , (encode "blksz", S.encode (blksz i))
                                , (encode "size", S.encode (size i))
                                -- , (encode "blocks", S.encode [])
                                ]
                         user = meta i
                     in INodeUnit core user

inodeUnitToINode :: StorageUnit -> Maybe INode
inodeUnitToINode (INodeUnit c u) = INode <$> (lookup "inode" core)
                                         <*> (lookup "atime" core >>= eulavM)
                                         <*> (lookup "ctime" core >>= eulavM)
                                         <*> (lookup "mtime" core >>= eulavM)
                                         <*> (Just u)
                                         <*> (lookup "blksz" core >>= eulavM)
                                         <*> (lookup "size" core >>= eulavM)
                                         <*> (Just [])
  where core = map (\(k,v) -> (decode k, v)) c
inodeUnitToINode _               = Nothing

inodeToINodePtrUnit :: INode -> Key -> StorageUnit
inodeToINodePtrUnit i l = INodePtrUnit (inode i) l

enumBlocks :: (MonadIO m, Storage s) => s -> [DataBlock] -> Onum B.ByteString m ()
enumBlocks _ []     = enumPure B.empty
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
  put (DirEntUnit name oid) =
    do { S.putWord8 3
       ; S.put name
       ; S.put oid
       }
  
  get = 
    do { opcode <- S.getWord8
       ; case opcode 
         of 0 -> getINodeUnit
            1 -> getDataBlockUnit
            2 -> getINodePtrUnit
            3 -> getDirEntUnit
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
          
          getDirEntUnit = do { n <- S.get
                             ; o <- S.get
                             ; return (DirEntUnit n o)
                             }