==============================
 sssfs: Amazon S3 File System
===============================

Introduction
============

This project implements a network filesystem backed by Amazon
S3. Although there are quite a number of such implementations, none
seems to address the problem of multiple simultaneous clients. The
goals of this project are, therefore:

a. implement a filesystem that allows simultaneous clients, and;
b. create a full featured filesystem, which would allow you to
   use as any other local filesystem, except maybe by the fact that it
   is not \[and will never be\] a POSIX filesystem;

This short document describes the planning to achieve this and the challenges
to overcome.

Design Decisions
================

Inspired by UNIX filesystem design, a file is represented by a
structure similar to an inode. This structure, heavily inspired by the
stat structurem, contains:

* id;
* file type;
* size in bytes;
* data block size;
* pointer to the data blocks;
* time of last access;
* time of last modification;
* time of last status change;
* user defined meta-data in the form of a list of 2-tuple;

The inode structure is stored as two objects in S3. The inode itself
it just a pointer. It contains the address of the object that actually
contains the information. This second object is immutable, its key is
the hash of its contents. As a matter effect the only object that is
subject to modification is the inode pointer. Everything else is read
only::
  
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

TODO:pros/cons of immutable objects
TODO:directory representation/issues
TODO:hard links are hard
TODO:garbage collection
TODO:consistency model
TODO:conflict resolution
TODO:stop procrastination and start writing up
