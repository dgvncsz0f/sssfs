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


module SSSFS.Storage.DebugStorage
       ( DebugStorage(..)
       ) where

import qualified Data.ByteString as B
import           SSSFS.Storage as S

data DebugStorage s = DebugStorage String s

debug :: String -> String -> IO ()
debug s m = putStrLn ("[debug.storage:"++ s ++"] " ++ m)

debugValue :: B.ByteString -> String
debugValue bytes  = show (B.length bytes) ++ " bytes"

debugValues :: Show a => [a] -> String
debugValues xs  = show (length xs) ++ " elements"

instance (Storage s) => Storage (DebugStorage s) where
  
  shutdown (DebugStorage n s) = shutdown s >> debug n "shutdown"

instance (StorageHashLike s) => StorageHashLike (DebugStorage s) where
  
  put (DebugStorage n s) k v =
    do { debug n ("put " ++ showKeyS k ++ " - " ++ debugValue v)
       ; put s k v
       }
  
  get (DebugStorage n s) k =
    do { v <- get s k
       ; debug n ("get " ++ showKeyS k ++ " - " ++ debugValue v)
       ; return v
       }
  
  del (DebugStorage n s) k =
    do { debug n ("del " ++ showKeyS k)
       ; del s k
       }
  
  head (DebugStorage n s) k =
    do { v <- S.head s k
       ; debug n ("head " ++ showKeyS k ++ " - " ++ show v)
       ; return v
       }

instance (StorageEnumLike s) => StorageEnumLike (DebugStorage s) where
  
  enumKeys (DebugStorage n s) k =
    do { vs <- enumKeys s k
       ; debug n ("enumKeys " ++ showKeyS k ++ " - " ++ debugValues vs)
       ; return vs
       }
  
  enumTest (DebugStorage n s) k =
    do { v <- enumTest s k
       ; debug n ("enumTest " ++ showKeyS k ++ " - " ++ show v)
       ; return v
       }
  
  enumCount (DebugStorage n s) k =
    do { v <- enumCount s k
       ; debug n ("enumCount " ++ showKeyS k ++ " - " ++ show v)
       ; return v
       }
