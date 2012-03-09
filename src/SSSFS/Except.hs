{-# LANGUAGE DeriveDataTypeable    #-}

module SSSFS.Except
       where

import           Data.Typeable
import           Control.Exception

-- | The mother of all exceptions
data CFSExcept = CFSExcept 
               deriving (Show, Typeable)

data SysExcept = SysExcept String
               | ParseExcept String
               | ObjectNotFound String
               deriving (Show, Typeable)

data IOExcept = NotFound String
              | NotADirectory String
               deriving (Show, Typeable)

instance Exception CFSExcept
instance Exception SysExcept
instance Exception IOExcept
