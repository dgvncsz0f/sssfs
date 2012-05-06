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
import os.path
import popen2
import tempfile
import shutil
import time
from contextlib import contextmanager

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

def rootdir():
    this = os.path.dirname(os.path.realpath(__file__))
    return(os.path.realpath(os.path.join(this, "..")))

def mountpoint(handle):
    return("%s/fuse" % handle)

def mount():
    tmp  = tempfile.mkdtemp()
    root = "%s/root" % tmp
    fuse = mountpoint(tmp)
    map(os.mkdir, (root, fuse))
    cmd  = ("%s/src/sssfs" % rootdir(), root, fuse)
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
        return(f(*args, **kwargs))
    except:
        pass

def generic_setup(mountpt):
    handle = mount()
    silently(os.unlink, mountpt)
    os.symlink(mountpoint(handle), mountpt)
    debug("setup %s [%s]" % (mountpt, handle))
    return((handle, mountpt))

def generic_teardown(handle):
    debug("teardown %s [%s]" % handle)
    umount(handle[0])
    silently(os.unlink, handle[1])

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

@contextmanager
def sssfs():
    handle = mount()
    yield(mountpoint(handle))
    umount(handle)