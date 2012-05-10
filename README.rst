==============================
 sssfs: Amazon S3 File System
==============================

::

                         +------+
                         |+------+
                         ||+------+
                         +||+------+
                          +||+------+
        +-------+          +|| data |
        | inode |           +|blocks|
        +-------+            +---^--+
            |                    |
            +--------------------+


This project implements a network filesystem backed by Amazon
S3. Although there are quite a number of such implementations, none
seems to address the problem of multiple simultaneous clients. The
goals of this project are, therefore:

a. implement a filesystem that allows simultaneous clients, and;
b. create a full featured filesystem, which would allow you to
   use as any other local filesystem, except maybe by the fact that it
   is not \[and will never be\] a POSIX compliance filesystem;

Author
======

* Diego Souza <dsouza+sssfs@bitforest.org>

License
=======

* GPLv3

