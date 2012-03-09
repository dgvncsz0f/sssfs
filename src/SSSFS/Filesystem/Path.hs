module SSSFS.Filesystem.Path
       ( dirname
       , basename
       , safeSplitPath
       ) where

import System.FilePath

safeSplitPath :: FilePath -> [FilePath]
safeSplitPath = map dropTrailingPathSeparator . splitPath

dirname :: FilePath -> FilePath
dirname = joinPath . init . safeSplitPath

basename :: FilePath -> FilePath
basename = last . safeSplitPath