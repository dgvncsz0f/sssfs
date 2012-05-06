#!/usr/bin/python
# -*- coding: utf-8; -*-

# Copyright (c) 2012, Diego Souza
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
#   * Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the following disclaimer.
#   * Redistributions in binary form must reproduce the above copyright notice,
#     this list of conditions and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#   * Neither the name of the <ORGANIZATION> nor the names of its contributors
#     may be used to endorse or promote products derived from this software
#     without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import base
import os
import time
from exceptions       import OSError
from nose.tools       import *
from test_directories import filepath

def test_rmdir_should_remove_directory():
    d = filepath()
    os.mkdir(d)
    os.rmdir(d)
    assert_false(os.path.exists(d), "exists(%s)" % d)

def test_rmdir_should_only_remove_empty_dirs():
    d = filepath()
    os.mkdir(d)
    os.mkdir(filepath(d, "1"))
    assert_raises(OSError, os.rmdir, d)

def test_rmdir_twice_should_fail():
    d = filepath()
    os.mkdir(d)
    os.rmdir(d)
    assert_raises(OSError, os.rmdir, d)

@base.skip_on_fail
def test_rmdir_should_update_ctime_and_mtime():
    d = filepath()
    os.mkdir(d)
    os.mkdir(filepath(d, "1"))
    s0 = os.stat(d)
    time.sleep(1)
    os.rmdir(filepath(d, "1"))
    s1 = os.stat(d)
    assert_true(s0.st_mtime < s1.st_mtime, u"s0.st_mtime ≮ s1.st_mtime")
    assert_true(s0.st_ctime < s1.st_ctime, u"s0.st_ctime ≮ s1.st_ctime")
