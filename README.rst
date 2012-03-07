==============================
 sssfs: Amazon S3 File System
==============================

Introduction
============

This project implements a network filesystem backed by Amazon
S3. Although there are quite a number of such implementations, none
seems to address the problem of multiple simultaneous clients. The
goals of this project are, therefore:

a. implement a filesystem that allows simultaneous clients, and;
b. create a full featured filesystem, which would allow you to
   use as any other local filesystem, except maybe by the fact that it
   is not \[and will never be\] a POSIX compliance filesystem;

This short document describes the planning to achieve this and the challenges
to overcome.

N.B.: This is mostly a brainstorm, something to remind me about the
problems/solutions during the implementation. Therefore, it is likely
outdated and likely contains lots of nonsense stuff. Consider yourself
warned.

Design Decisions
================

Inspired by UNIX filesystem design, a file is represented by a
structure similar to an inode. This structure, heavily inspired by the
stat structure, contains:

* id;
* file type;
* size in bytes;
* data block size;
* pointer to the data blocks;
* time of last access;
* time of last modification;
* time of last status change;
* user defined meta-data in the form of a list of 2-tuple;

The /inode/ structure is stored as two objects in S3. The /inode/
itself it just a pointer, which we will refer as /inode ptr/ from now
on. It contains the key of the object that actually contains the
information. This second object is immutable, its key is the hash of
its contents. As a matter effect the only object that is subject to
modification is the /inode ptr/. Everything else is read only::
  
                              read only
                   +------------------------------+
                   |                              |
        +-------+      +---------+   +------+
        | inode |      | inode   |   |+------+
        |  ptr  +------> payload |   ||+------+
        |       |      |         |   +||+------+
        +-------+      +----+----+    +||+------+
                            |          +|| data |
                            |           +|blocks|
                            |            +---^--+
                            |                |
                            +----------------+

The reason every object is immutable is that it simplifies the
/copy-on-write/ implementation. Copy-on-write is desirable as it may
reduce storage costs and some operations, like COPY, become very
cheap/fast. On the other hand, DELETE operation may leave dangling
blocks, blocks that no /inode ptr/ is referring to. These are hard to
detect and reclaim as the current state of s3 one is able to see might
not reflect the latest updates. In fact, this is an open issue and for
now there is no good solution.

Directories
-----------

Directories are an interesting problem. If the directory contents are
stored as /data blocks/, we may have race conditions and the current s3
data model makes it really hard to implement a reasonable
solution. There has been a couple of ideas, each one with its
pros/cons. I'll try to explain the problems and the reason behind the
decision.

The first idea was to represent the directories using some data
structure like a /BTree/ or a /Hash/. Conflicts would be solved using
a merge algorithm, and if the conflict couldn't be solved by the
merging algorithm, say two updates on the file, the latest transaction
would win.

The problem with this approach is that s3 makes no guarantees when an
object will be available. Then we might miss the conflict, leaving the
resolution for the s3 itself. Thus, great potential for huge data
losses.

The other ideas were all some variation of this and do not deserve to
be mentioned.

The second idea is to use s3 itself, using suffix keys, to store the
directory contents. In this case the directory have no /data blocks/,
instead, each name would be a suffix of the actual /inode ptr/ key.

As a real example, consider the file `/foo/bar/foobar` the following
objects in s3 should be created (hope this will clarify the directory
layout)::

   |-------------+-------+--------------------------------------------------|
   | key         | value | description                                      |
   |-------------+-------+--------------------------------------------------|
   | /i/1        | /d/1  | The root inode ptr (the / dir)                   |
   | /i/1/foo    | /i/2  | The name foo that links to the /i/2 inode        |
   | /d/1        |       | The actual data of the /i/1 inode                |
   |-------------+-------+--------------------------------------------------|
   | /i/2        | /d/2  | The inode ptr (the foo dir)                      |
   | /i/2/bar    | /i/3  | The name bar that links to the /i/3 inode        |
   | /d/2        |       | The actual data of the /i/2 inode                |
   |-------------+-------+--------------------------------------------------|
   | /i/3        | /d/3  | The inode ptr (the bar dir)                      |
   | /i/3/foobar | /i/4  | The name foobar that links to the /i/4 inode     |
   | /d/3        |       | The actual data of the /i/3 inode                |
   |-------------+-------+--------------------------------------------------|

Changing directory contents is relatively cheap, but we loose
/copy-on-write/ feature for directories. There are other drawbacks
too, but in the end of the day this seems to be the best
representation so far.

TODOS
=====

* TODO:hard_links_are_hard
* TODO:garbage_collection
* TODO:consistency_model
* TODO:conflict_resolution
* TODO:stop_procrastination_and_finishing_writing_up
