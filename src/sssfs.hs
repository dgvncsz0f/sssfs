module Main where

import SSSFS.Filesystem
import SSSFS.Storage.Local as L
import SSSFS.Storage.Debug as D

main :: IO ()
main = let s = D.new (L.new "/home/dsouza/tmp/sssfs")
       in do { mkfs s
             ; _ <- mkdir s "/foo"
             ; _ <- mkdir s "/foo/bar"
             ; _ <- mkdir s "/foo/bar/foobar"
             ; _ <- creat s "/foo/bar/foobar/file"
             ; stat s "/foo/bar/foobar" >>= print
             }