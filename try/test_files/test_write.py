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

import os
import time
import base
import random
from exceptions import OSError
from nose.tools import *
from test_files import filepath

def test_write():
    f = filepath()
    with open(f, "w") as fh:
        fh.write("foobar")
    with base.posix_open(f, os.O_RDONLY) as fd:
        assert_equals("foobar", os.read(fd, 6))

def test_write_updates_size_field():
    f = filepath()
    with base.posix_open(f, os.O_RDWR | os.O_CREAT) as fd:
        s = os.fstat(fd)
        assert_equal(0, s.st_size)
        os.write(fd, "foobar")
        os.fsync(fd) # TODO:fix fstat
        s = os.fstat(fd)
        assert_equal(6, s.st_size)

def test_write_update_mtime():
    f = filepath()
    with base.posix_open(f, os.O_RDWR | os.O_CREAT) as fd:
        s0 = os.fstat(fd)
        time.sleep(1)
        os.write(fd, "foobar")
        os.fsync(fd) # TODO:fix fstat
        s1 = os.fstat(fd)
        assert_less(s0.st_mtime, s1.st_mtime)
