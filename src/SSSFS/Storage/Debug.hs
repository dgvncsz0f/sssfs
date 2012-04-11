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


module SSSFS.Storage.Debug
       ( new
       ) where

import qualified Data.ByteString as B
import           SSSFS.Storage as S

newtype DebugStorage s = DebugStorage s

new :: s -> DebugStorage s
new = DebugStorage

debug :: String -> IO ()
debug m = putStrLn ("[debug] " ++ m)

instance (StorageHashLike s) => StorageHashLike (DebugStorage s) where
  
  put (DebugStorage s) k v = do { debug ("put "
                                         ++ showKeyS k
                                         ++ " - "
                                         ++ show (B.length v)
                                         ++ " bytes")
                                ; put s k v
                                }
  
  get (DebugStorage s) k = do { v <- get s k
                              ; debug ("get "
                                       ++ showKeyS k
                                       ++ " - "
                                       ++ show (B.length v)
                                       ++ " bytes")
                              ; return v
                              }
  
  del (DebugStorage s) k = do { debug ("del "
                                       ++ showKeyS k)
                              ; del s k
                              }
  
  head (DebugStorage s) k = do { v <- S.head s k
                               ; debug ("head "
                                        ++ showKeyS k
                                        ++ " - "
                                        ++ show v)
                               ; return v
                               }

instance (StorageEnumLike s) => StorageEnumLike (DebugStorage s) where
  
  enumKeys (DebugStorage s) k = do { vs <- enumKeys s k
                                   ; debug ("enumKeys " 
                                            ++ showKeyS k 
                                            ++ " - " 
                                            ++ show (length vs) ++ " elements")
                                   ; return vs
                                   }
  
  enumTest (DebugStorage s) k = do { v <- enumTest s k
                                   ; debug ("enumTest "
                                            ++ showKeyS k
                                            ++ " - "
                                            ++ show v)
                                   ; return v
                                   }
  
  enumCount (DebugStorage s) k = do { v <- enumCount s k
                                    ; debug ("enumCount "
                                             ++ showKeyS k
                                             ++ " - "
                                             ++ show v)
                                    ; return v
                                    }