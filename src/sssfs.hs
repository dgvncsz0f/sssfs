module Main where

import SSSFS.Filesystem
import SSSFS.Storage.Local

main :: IO ()
main = mkfs $ new "/home/dsouza/tmp/cloudfs"