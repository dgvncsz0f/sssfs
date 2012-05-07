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

import sys
import os
import popen2
import tempfile
import shutil
import time
import random
from exceptions        import AssertionError
from nose.plugins.skip import SkipTest
from contextlib        import contextmanager

def cfg(key, default=None):
    return(os.environ.get("_%s" % key, default))

def skip_on_fail(f):
    def proxy_f(*args, **kwargs):
        try:
            return(f(*args, **kwargs))
        except AssertionError:
            if (cfg("noskip") is None):
                raise(SkipTest())
            else:
                raise
    proxy_f.__name__ = f.__name__
    return(proxy_f)

def debug(msg):
    print("[debug] %s" % msg)

def system(cmd):
    p = popen2.Popen3(cmd, capturestderr=True)
    return(p.wait() == 0)

def which(prg):
    path = { "make"      : os.environ.get("make_bin", "/usr/bin/make"),
             "fusermount": os.environ.get("fusermount_bin", "/bin/fusermount")
           }
    return(path[prg])

def project_filepath(f):
    this = os.path.dirname(os.path.realpath(__file__))
    return(os.path.realpath(os.path.join(this, "..", f)))

def mountpoint(handle):
    return("%s/fuse" % handle)

def mount():
    tmp  = tempfile.mkdtemp()
    root = "%s/root" % tmp
    fuse = mountpoint(tmp)
    map(os.mkdir, (root, fuse))
    cmd  = (project_filepath("dist/build/sssfs/sssfs"), root, fuse)
    rc = system(cmd)
    if (rc):
        debug("mount %s" % tmp)
        return(tmp)
    else:
        raise(RuntimeError("error mounting sssfs: %s" % " ".join(cmd)))

def umount(handle):
    debug("umount %s" % handle)
    system((which("fusermount"), "-u", mountpoint(handle)))
    shutil.rmtree(handle, ignore_errors=True)

def silently(f, *args, **kwargs):
    try:
        f(*args, **kwargs)
        return(True)
    except:
        return(False)

def generic_setup(mountpt):
    silently(os.unlink, mountpt)
    if (cfg("extmount") is None):
        handle = mount()
        os.symlink(mountpoint(handle), mountpt)
    else:
        handle = None
        os.symlink(cfg("extmount"), mountpt)
    debug("setup %s [%s]" % (handle, mountpt))
    return((handle, mountpt))

def generic_teardown(handle):
    debug("teardown %s [%s]" % handle)
    silently(os.unlink, handle[1])
    if (cfg("extmount") is None):
        umount(handle[0])

def generic_filepath(mountpt):
    def f(*args):
        r = None
        if (len(args) > 0):
            r = os.path.join(mountpt, *args)
        else:
            r = tempfile.mktemp(dir=mountpt)
        debug("filepath %s" % r)
        return(r)
    return(f)

def random_data(fd, size):
    written  = 0
    pagesize = os.sysconf("SC_PAGE_SIZE")
    with open(project_filepath("try/fixtures/lorem"), "r") as fh:
        words = fh.read().replace("\n", "").replace(".", "").replace(",", "").split()
    while (size > 0):
        random.shuffle(words)
        page     = " ".join(words)[:pagesize]
        written += os.write(fd, page[:size])
        size    -= len(page)
    return(written)

def touch(f):
    with posix_open(f, os.O_WRONLY | os.O_CREAT):
        pass
    os.utime(f, None)
    return(os.stat(f))

def truncate(f):
    with posix_open(f, os.O_WRONLY | os.O_TRUNC | os.O_CREAT) as fd:
        return(os.fstat(fd))

@contextmanager
def sssfs():
    handle = mount()
    yield(mountpoint(handle))
    umount(handle)

@contextmanager
def posix_open(*args, **kwargs):
    fd = os.open(*args, **kwargs)
    yield(fd)
    os.close(fd)

