module Main where

import SSSFS.Filesystem
import SSSFS.Filesystem.Types
import SSSFS.Storage.Local

main :: IO ()
main = let s = new "/home/dsouza/tmp/sssfs"
       in do { mkfs s
             ; stat s "/" >>= print . inode
             }