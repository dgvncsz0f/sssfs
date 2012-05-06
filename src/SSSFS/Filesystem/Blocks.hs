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

module SSSFS.Filesystem.Blocks
       ( writeB
       , truncateB
       , toChunks
       , slice
       , calc
       ) where

import qualified Data.ByteString as B
import           SSSFS.Filesystem.Types

writeB :: BlockSeek -> Block -> Block -> Block
writeB offset old new = let (prefix, maybeSuffix) = B.splitAt offset old
                            padding               = B.replicate (offset - B.length prefix) 0
                            suffix                = B.drop (B.length new) maybeSuffix
                        in B.concat [prefix, padding, new, suffix]

truncateB :: BlockSeek -> Block -> Block
truncateB = B.take

toChunks :: Block -> Size -> [Block]
toChunks bytes s 
  | B.null bytes = []
  | otherwise    = let (a,b) = B.splitAt s bytes
                   in a : toChunks b s

slice :: Size -> BlockSeek -> Block -> Block
slice bsz offset = B.take bsz . B.drop offset

calc :: Size -> Seek -> (BlockIx, BlockSeek)
calc bsz offset = let (block, inOffset) = offset `divMod` (fromIntegral bsz)
                  in (fromIntegral block, fromIntegral inOffset)