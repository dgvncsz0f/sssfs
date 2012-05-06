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
from test_files import truncate
from test_files import touch

def test_read():
    f = filepath()
    with open(f, "w") as fh:
        fh.write("foobar")
    with base.posix_open(f, os.O_RDONLY) as fd:
        assert_equals("foobar", os.read(fd, 6))

def test_read_returns_eof_corretly():
    f = filepath()
    with open(f, "w") as fh:
        fh.write("foobar")
    with base.posix_open(f, os.O_RDONLY) as fd:
        os.read(fd, 6)
        assert_equals("", os.read(fd, 6))

def test_read_greater_than_pagesize():
    f = filepath()
    s = os.sysconf("SC_PAGE_SIZE") * 2 + 1
    with base.posix_open(f, os.O_RDWR | os.O_CREAT) as fd:
        base.random_data(fd, s)
    with base.posix_open(f, os.O_RDONLY) as fd:
        assert_equals(s, len(os.read(fd, s)))

def test_read_with_buffersize_greater_than_file():
    f = filepath()
    with open(f, "w") as fh:
        fh.write("foobar")
    with base.posix_open(f, os.O_RDONLY) as fd:
        assert_equals("foobar", os.read(fd, 1024))

def test_read_and_stat_size_field():
    f = filepath()
    with base.posix_open(f, os.O_RDWR | os.O_CREAT) as fd:
        written = base.random_data(fd, random.randint(1024, 8192))
    with base.posix_open(f, os.O_RDONLY) as fd:
        data = os.read(fd, 8192)
        stat = os.fstat(fd)
        assert_equals(written, len(data))
        assert_equals(written, stat.st_size)

