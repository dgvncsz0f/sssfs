{-# LANGUAGE ScopedTypeVariables #-}

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

module SSSFS.Fuse.Debug where

import           Foreign.C.Error
import           System.Fuse as F
import           System.Posix.Types
import qualified Data.ByteString as B

debugEither :: String -> Either a b -> IO ()
debugEither msg (Right _) = putStrLn $ "[debug.fuse    ] " ++ msg ++ " - ok"
debugEither msg (Left _)  = putStrLn $ "[debug.fuse    ] " ++ msg ++ " - fail"

debugEither1 :: String -> Either a ByteCount -> IO ()
debugEither1 msg (Right b) = putStrLn $ "[debug.fuse   ] " ++ msg ++ " - ok : " ++ show b
debugEither1 msg (Left _)  = putStrLn $ "[debug.fuse   ] " ++ msg ++ " - fail"

debugEither2 :: String -> Either a [(FilePath, FileStat)] -> IO ()
debugEither2 msg (Right b) = putStrLn $ "[debug.fuse   ] " ++ msg ++ " - ok : " ++ show (map fst b)
debugEither2 msg (Left _)  = putStrLn $ "[debug.fuse   ] " ++ msg ++ " - fail"

debugE :: String -> Errno -> IO ()
debugE msg e
  | e == eOK  = putStrLn $ "[debug.fuse   ] " ++ msg ++ " - ok"
  | otherwise = putStrLn $ "[debug.fuse   ] " ++ msg ++ " - fail"

debug :: String -> IO ()
debug msg = putStrLn $ "[debug.fuse   ] " ++ msg

debugger :: FuseOperations a -> FuseOperations a
debugger backend = FuseOperations { fuseGetFileStat =
                                        \f -> do { result <- fuseGetFileStat backend f
                                                 ; debugEither ("stat " ++ f) result
                                                 ; return result
                                                 }
                                  , fuseReadSymbolicLink =
                                        \f -> do { result <- fuseReadSymbolicLink backend f
                                                 ; debugEither ("readlink " ++ f) result
                                                 ; return result
                                                 }
                                  , fuseCreateDevice =
                                        \f a b c -> do { result <- fuseCreateDevice backend f a b c
                                                       ; debugE ("mknod " ++ f) result
                                                       ; return result
                                                       }
                                  , fuseCreateDirectory      =
                                        \f a -> do { result <- fuseCreateDirectory backend f a
                                                   ; debugE ("mkdir " ++ f) result
                                                   ; return result
                                                   }
                                  , fuseRemoveLink =
                                        \f -> do { result <- fuseRemoveLink backend f
                                                 ; debugE ("unlink " ++ f) result
                                                 ; return result
                                                 }
                                  , fuseRemoveDirectory =
                                        \f -> do { result <- fuseRemoveDirectory backend f
                                                 ; debugE ("rmdir " ++ f) result
                                                 ; return result
                                                 }
                                  , fuseCreateSymbolicLink =
                                        \f1 f2 -> do { result <- fuseCreateSymbolicLink backend f1 f2
                                                     ; debugE ("symlink " ++ f1 ++ " " ++ f2) result
                                                     ; return result
                                                     }
                                  , fuseRename =
                                        \f1 f2 -> do { result <- fuseRename backend f1 f2
                                                     ; debugE ("rename " ++ f1 ++ " " ++ f2) result
                                                     ; return result
                                                     }
                                  , fuseCreateLink =
                                        \f1 f2 -> do { result <- fuseCreateLink backend f1 f2
                                                     ; debugE ("link " ++ f1 ++ " " ++ f2) result
                                                     ; return result
                                                     }
                                  , fuseSetFileMode =
                                        \f a -> do { result <- fuseSetFileMode backend f a
                                                   ; debugE ("chmod " ++ f ++ " " ++ show a) result
                                                   ; return result
                                                   }
                                  , fuseSetOwnerAndGroup =
                                        \f a b -> do { result <- fuseSetOwnerAndGroup backend f a b
                                                     ; debugE ("chown " ++ f ++ " " ++ show a ++ " " ++ show b) result
                                                     ; return result
                                                     }
                                  , fuseSetFileSize =
                                        \f a -> do { result <- fuseSetFileSize backend f a
                                                   ; debugE ("truncate " ++ f ++ " " ++ show a) result
                                                   ; return result
                                                   }
                                  , fuseSetFileTimes =
                                        \f a b -> do { result <- fuseSetFileTimes backend f a b
                                                     ; debugE ("utime " ++ show a ++ " " ++ show b) result
                                                     ; return result
                                                     }
                                  , fuseOpen =
                                        \f a b -> do { result <- fuseOpen backend f a b
                                                     ; debugEither ("open " ++ f) result
                                                     ; return result
                                                     }
                                  , fuseRead =
                                        \f a b c -> do { result <- fuseRead backend f a b c
                                                       ; debugEither ("read " ++ f ++ " " ++ show b ++ " " ++ show c) result
                                                       ; return result
                                                       }
                                  , fuseWrite =
                                        \f a b c -> do { result <- fuseWrite backend f a b c
                                                       ; debugEither1 ("write " ++ f ++ " " ++ show (B.length b) ++ " " ++ show c) result
                                                       ; return result
                                                       }
                                  , fuseGetFileSystemStats =
                                        \s -> do { result <- fuseGetFileSystemStats backend s
                                                 ; debugEither ("statfs " ++ s) result
                                                 ; return result
                                                 }
                                  , fuseFlush =
                                        \f a -> do { result <- fuseFlush backend f a
                                                   ; debugE ("flush " ++ f) result
                                                   ; return result
                                                   }
                                  , fuseRelease =
                                        \f a -> do { fuseRelease backend f a
                                                   ; debug ("release " ++ f)
                                                   }
                                  , fuseSynchronizeFile =
                                        \f a -> do { result <- fuseSynchronizeFile backend f a
                                                   ; debugE ("fsync " ++ f) result
                                                   ; return result
                                                   }
                                  , fuseOpenDirectory =
                                        \f -> do { result <- fuseOpenDirectory backend f
                                                 ; debugE ("opendir " ++ f) result
                                                 ; return result
                                                 }
                                  , fuseReadDirectory =
                                        \f -> do { result <- fuseReadDirectory backend f
                                                 ; debugEither2 ("readdir " ++ f) result
                                                 ; return result
                                                 }
                                  , fuseReleaseDirectory =
                                        \f -> do { result <- fuseReleaseDirectory backend f
                                                 ; debugE ("releasedir " ++ f) result
                                                 ; return result
                                                 }
                                  , fuseSynchronizeDirectory =           
                                        \f a -> do { result <- fuseSynchronizeDirectory backend f a
                                                   ; debugE ("fsyncdir " ++ f) result
                                                   ; return result
                                                   }
                                  , fuseAccess =
                                        \f a -> do { result <- fuseAccess backend f a
                                                   ; debugE ("access " ++ f ++ " " ++ show a) result
                                                   ; return result
                                                   }
                                  , fuseInit = fuseInit backend >> debug "init"
                                  , fuseDestroy = fuseDestroy backend >> debug "destroy"
                                  }
