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

instance (Storage s) => Storage (DebugStorage s) where
  
  put (DebugStorage s) k v = do { debug ("put " ++ showKeyS k ++ " - " ++ show (B.length v) ++ " bytes")
                                ; put s k v
                                }
  
  get (DebugStorage s) k = do { v <- get s k
                              ; debug ("get " ++ showKeyS k ++ " - " ++ show (B.length v) ++ " bytes")
                              ; return v
                              }
  
  head (DebugStorage s) k = do { v <- S.head s k
                               ; debug ("head " ++ showKeyS k ++ " - " ++ show v)
                               ; return v
                               }

  enum (DebugStorage s) k = do { vs <- enum s k
                               ; debug ("enum " ++ showKeyS k ++ " - " ++ show (length vs) ++ " elements")
                               ; return vs
                               }