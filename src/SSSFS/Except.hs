{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}

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


module SSSFS.Except
       where

import           Data.Typeable
import           Control.Exception

-- | The mother of all exceptions
data CFSExcept = forall e. Exception e => CFSExcept e
               deriving (Typeable)

data SysExcept = DataCorruptionExcept String
               deriving (Show, Typeable)

data IOExcept = NotFound String
              | NotADir String
              | NotEmpty String
               deriving (Show, Typeable)

cfsExceptToException :: Exception e => e -> SomeException
cfsExceptToException = toException . CFSExcept

cfsExceptFromException :: Exception e => SomeException -> Maybe e
cfsExceptFromException x = do { CFSExcept e <- fromException x
                              ; cast e
                              }

instance Show CFSExcept where
  show (CFSExcept e) = show e

instance Exception CFSExcept

instance Exception SysExcept where
  toException   = cfsExceptToException
  fromException = cfsExceptFromException
  
instance Exception IOExcept where
  toException   = cfsExceptToException
  fromException = cfsExceptFromException
