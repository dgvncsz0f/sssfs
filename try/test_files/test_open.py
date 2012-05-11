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
from exceptions import OSError
from nose.tools import *
from test_files import filepath

def test_open_rdonly_should_raise_exception_when_file_does_not_exist():
    f = filepath()
    assert_raises(OSError, lambda: os.close(os.open(f, os.O_RDONLY)))

def test_open_rdonly_should_not_allow_writing():
    f = filepath()
    base.touch(f)
    with base.posix_open(f, os.O_RDONLY) as fd:
        assert_raises(OSError, os.write, fd, "foobar")

def test_open_wronly_should_raise_exception_when_file_does_not_exist():
    f = filepath()
    assert_raises(OSError, lambda: os.close(os.open(f, os.O_WRONLY)))

def test_open_truncate_set_size_to_zero():
    f = filepath()
    with open(f, "w") as fh:
        fh.write("foobar")
    assert_greater(os.stat(f).st_size, 0)
    with base.posix_open(f, os.O_RDWR | os.O_TRUNC):
        pass
    assert_equals(os.stat(f).st_size, 0)

def test_open_update_atime():
    f  = filepath()
    s0 = base.touch(f)
    time.sleep(1)
    s1 = base.touch(f)
    assert_less(s0.st_atime, s1.st_atime)

@base.skip_on_fail
def test_open_update_mtime_of_its_parent_dir():
    d = filepath()
    os.mkdir(d)
    s0 = os.stat(d)
    time.sleep(1)
    base.touch(filepath(d, "1"))
    s1 = os.stat(d)
    assert_less(s0.st_mtime, s1.st_mtime)
