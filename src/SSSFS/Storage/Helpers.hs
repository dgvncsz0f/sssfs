{-# LANGUAGE FlexibleContexts #-}

module SSSFS.Storage.Helpers
       ( transfer
       , transferIf
       , copy
       , copyIf
       ) where

import qualified Data.ByteString as B
import           SSSFS.Storage

transfer :: (Storage src, Storage dst) => src -> dst -> Key -> Key -> IO ()
transfer = transferIf (const True)

copy :: (Storage s) => s -> Key -> Key -> IO ()
copy s srcL dstL = transfer s s srcL dstL

transferIf :: (Storage src, Storage dst) => (B.ByteString -> Bool) -> src -> dst -> Key -> Key -> IO ()
transferIf p src dst srcL dstL = do { bytes <- get src srcL
                                    ; if (p bytes)
                                      then put dst dstL bytes
                                      else return ()
                                    }

copyIf :: (Storage s) => (B.ByteString -> Bool) -> s -> Key -> Key -> IO ()
copyIf p s srcL dstL = transferIf p s s srcL dstL


