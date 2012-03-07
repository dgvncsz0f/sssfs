{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SSSFS.Except
       where

import           Data.Typeable
import           Control.Exception

-- | The mother of all exceptions
data CFSExcept = CFSExcept 
               deriving (Show, Typeable)

-- | Uncatchable exceptions
data SysExcept = SysExcept String
               | ParseExcept
               deriving (Show, Typeable)

data UsrExcept = NotFound
               deriving (Show, Typeable)

instance Exception CFSExcept
instance Exception SysExcept
instance Exception UsrExcept
