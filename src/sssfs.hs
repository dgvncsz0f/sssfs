module Main where

import SSSFS.Filesystem
import SSSFS.Storage.Local as L
import SSSFS.Storage.Debug as D

main :: IO ()
main = let s = D.new (L.new "/home/dsouza/tmp/sssfs")
       in do { mkfs s
             ; _ <- mkdir s "/foobar"
             ; stat s "/foobar" >>= print
             }