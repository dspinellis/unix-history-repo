.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)2.t	5.1 (Berkeley) %G%
.\"
.ds RH Old file system
.NH
Old File System
.PP
In the old file system developed at Bell Laboratories
each disk drive contains one or more file systems.\(dg
.FS
\(dg A file system always resides on a single drive.
.FE
A file system is described by its super-block,
which contains the basic parameters of the file system.
These include the number of data blocks in the file system,
a count of the maximum number of files,
and a pointer to a list of free blocks.
All the free blocks in the system are chained together in
a linked list.
Within the file system are files.
Certain files are distinguished as directories and contain
pointers to files that may themselves be directories.
Every file has a descriptor associated with it called an
.I "inode".
The inode contains information describing ownership of the file,
time stamps marking last modification and access times for the file,
and an array of indices that point to the data blocks for the file.
For the purposes of this section, we assume that the first 8 blocks
of the file are directly referenced by values stored
in the inode structure itself*.
.FS
* The actual number may vary from system to system, but is usually in
the range 5-13.
.FE
The inode structure may also contain references to indirect blocks
containing further data block indices.
In a file system with a 512 byte block size, a singly indirect
block contains 128 further block addresses,
a doubly indirect block contains 128 addresses of further single indirect
blocks,
and a triply indirect block contains 128 addresses of further doubly indirect
blocks.
.PP
A traditional 150 megabyte UNIX file system consists
of 4 megabytes of inodes followed by 146 megabytes of data.
This organization segregates the inode information from the data;
thus accessing a file normally incurs a long seek from its inode to its data.
Files in a single directory are not typically allocated
slots in consecutive locations in the 4 megabytes of inodes,
causing many non-consecutive blocks to be accessed when executing
operations on all the files in a directory.
.PP
The allocation of data blocks to files is also suboptimum.
The traditional
file system never transfers more than 512 bytes per disk transaction
and often finds that the next sequential data block is not on the same
cylinder, forcing seeks between 512 byte transfers.
The combination of the small block size,
limited read-ahead in the system,
and many seeks severely limits file system throughput.
.PP
The first work at Berkeley on the UNIX file system attempted to improve both
reliability and throughput.
The reliability was improved by changing the file system so that
all modifications of critical information were staged so that they could
either be completed or repaired cleanly
by a program after a crash [Kowalski78].
The file system performance was improved by a factor of more than two by
changing the basic block size from 512 to 1024 bytes.
The increase was because of two factors;
each disk transfer accessed twice as much data, 
and most files could be described without need to access through
any indirect blocks since the direct blocks contained twice as much data.
The file system with these changes will henceforth be referred to as the
.I "old file system."
.PP
This performance improvement gave a strong indication that
increasing the block size was a good method for improving
throughput.
Although the throughput had doubled, 
the old file system was still using only about
four percent of the disk bandwidth.
The main problem was that although the free list was initially
ordered for optimal access,
it quickly became scrambled as files were created and removed.
Eventually the free list became entirely random
causing files to have their blocks allocated randomly over the disk.
This forced the disk to seek before every block access.
Although old file systems provided transfer rates of up
to 175 kilobytes per second when they were first created,
this rate deteriorated to 30 kilobytes per second after a
few weeks of moderate use because of randomization of their free block list.
There was no way of restoring the performance an old file system
except to dump, rebuild, and restore the file system.
Another possibility would be to have a process that periodically
reorganized the data on the disk to restore locality as
suggested by [Maruyama76].
.ds RH New file system
.bp
