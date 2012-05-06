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

def test_mkdir_should_create_directory():
    d = filepath()
    os.mkdir(d)
    assert_true(os.path.isdir(d), u"¬ isdir(%s)" % d)

def test_mkdir_should_create_empty_directory():
    d = filepath()
    os.mkdir(d)
    assert_equals(os.listdir(d), [])

def test_mkdir_should_work_recursively():
    d = filepath()
    os.mkdir(d)
    os.mkdir(filepath(d, "1"))
    assert_equals(os.listdir(d), ["1"])

def test_mkdir_twice_should_fail():
    d = filepath()
    os.mkdir(d)
    assert_raises(OSError, os.mkdir, d)

@base.skip_on_fail
def test_mkdir_should_update_ctime_and_mtime():
    d = filepath()
    os.mkdir(d)
    s0 = os.stat(d)
    time.sleep(1)
    os.mkdir(filepath(d, "1"))
    s1 = os.stat(d)
    assert_true(s0.st_mtime < s1.st_mtime, u"s0.st_mtime ≮ s1.st_mtime")
    assert_true(s0.st_ctime < s1.st_ctime, u"s0.st_ctime ≮ s1.st_ctime")

@base.skip_on_fail
def test_open_should_update_atime():
    d = filepath()
    os.mkdir(d)
    s0 = os.stat(d)
    time.sleep(1)
    os.listdir(d)
    s1 = os.stat(d)
    assert_true(s0.st_atime < s1.st_atime, u"s0.st_atime ≮ s1.st_atime")
