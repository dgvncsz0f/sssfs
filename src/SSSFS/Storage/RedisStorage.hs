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


module SSSFS.Storage.RedisStorage
       ( RedisStorage(..)
       , new
       ) where

import           Control.Exception
import           Database.Redis.Redis as R
import           SSSFS.Except
import           SSSFS.Storage

data RedisStorage = RedisStorage { conn :: Redis }

new :: String -> String -> Int -> IO RedisStorage
new host port db = do { redis <- connect host port 
                      ; _     <- select redis db
                      ; return (RedisStorage redis)
                      }

expectOk :: Reply () -> ()
expectOk ROk = ()
expectOk e   = throw (SysExcept $ show e)

expectBulk :: Reply s -> s
expectBulk (RBulk (Just s)) = s
expectBulk (RBulk Nothing)  = throw (NotFound "not found")
expectBulk _                = throw (SysExcept $ "redis error")

expectMult :: Reply s -> [s]
expectMult (RBulk (Just s))        = [s]
expectMult (RMulti (Just replies)) = concatMap expectMult replies
expectMult (RBulk Nothing)         = []
expectMult (RMulti Nothing)        = []
expectMult _                       = throw (SysExcept $ "redis error")

instance Storage RedisStorage

instance StorageHashLike RedisStorage where
  
  put s k v = fmap expectOk (set (conn s) (showKeyS k) v)

  get s k = fmap expectBulk (R.get (conn s) (showKeyS k))
  
  head s k = fmap (== (RInt 1)) (exists (conn s) (showKeyS k))
  
  del s k = R.del (conn s) (showKeyS k) >> return ()

instance StorageEnumLike RedisStorage where
  
  enumKeys s k = fmap (map makeRef . expectMult) (keys (conn s) keyPrefix)
    where keyPrefix = showKeyS k ++ "/*"
          
          makeRef = ref . drop (length keyPrefix - 1)