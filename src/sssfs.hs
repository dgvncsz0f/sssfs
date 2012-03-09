module Main where

import SSSFS.Filesystem.Core
import SSSFS.Filesystem.Files
import SSSFS.Filesystem.Directory
import SSSFS.Storage.Local as L
import SSSFS.Storage.Debug as D

main :: IO ()
main = let s = D.new (L.new "/home/dsouza/tmp/sssfs")
       in do { mkfs s
             ; _ <- mkdir s "/foo"
             ; _ <- mkdir s "/foo/bar"
             ; _ <- mkdir s "/foo/bar/foobar"
             ; _ <- creat s "/foo/bar/foobar/a"
             ; _ <- creat s "/foo/bar/foobar/b"
             ; _ <- creat s "/foo/bar/foobar/c"
             ; _ <- creat s "/foo/bar/foobar/d"
             ; dirContents s "/foo/bar/foobar" >>= mapM_ (\n -> stat s ("/foo/bar/foobar/" ++ n) >>= print)
             }