.\" Copyright (c) 1986 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" Redistribution and use in source and binary forms are permitted
.\" provided that the above copyright notice and this paragraph are
.\" duplicated in all such forms and that any documentation,
.\" advertising materials, and other materials related to such
.\" distribution and use acknowledge that the software was developed
.\" by the University of California, Berkeley.  The name of the
.\" University may not be used to endorse or promote products derived
.\" from this software without specific prior written permission.
.\" THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
.\" IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
.\" WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
.\"
.\"	@(#)2.t	6.2 (Berkeley) %G%
.\"
.ds RH Old file system
.NH
Old File System
.PP
In the file system developed at Bell Laboratories
(the ``traditional'' file system),
each disk drive is divided into one or more
partitions.  Each of these disk partitions may contain
one file system.  A file system never spans multiple
partitions.\(dg
.FS
\(dg By ``partition'' here we refer to the subdivision of
physical space on a disk drive.  In the traditional file
system, as in the new file system, file systems are really
located in logical disk partitions that may overlap.  This
overlapping is made available, for example,
to allow programs to copy entire disk drives containing multiple
file systems.
.FE
A file system is described by its super-block,
which contains the basic parameters of the file system.
These include the number of data blocks in the file system,
a count of the maximum number of files,
and a pointer to the \fIfree list\fP, a linked
list of all the free blocks in the file system.
.PP
Within the file system are files.
Certain files are distinguished as directories and contain
pointers to files that may themselves be directories.
Every file has a descriptor associated with it called an
.I "inode".
An inode contains information describing ownership of the file,
time stamps marking last modification and access times for the file,
and an array of indices that point to the data blocks for the file.
For the purposes of this section, we assume that the first 8 blocks
of the file are directly referenced by values stored
in an inode itself*.
.FS
* The actual number may vary from system to system, but is usually in
the range 5-13.
.FE
An inode may also contain references to indirect blocks
containing further data block indices.
In a file system with a 512 byte block size, a singly indirect
block contains 128 further block addresses,
a doubly indirect block contains 128 addresses of further singly indirect
blocks,
and a triply indirect block contains 128 addresses of further doubly indirect
blocks.
.PP
A 150 megabyte traditional UNIX file system consists
of 4 megabytes of inodes followed by 146 megabytes of data.
This organization segregates the inode information from the data;
thus accessing a file normally incurs a long seek from the
file's inode to its data.
Files in a single directory are not typically allocated
consecutive slots in the 4 megabytes of inodes,
causing many non-consecutive blocks of inodes
to be accessed when executing
operations on the inodes of several files in a directory.
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
The reliability was improved by staging modifications
to critical file system information so that they could
either be completed or repaired cleanly by a program
after a crash [Kowalski78].
The file system performance was improved by a factor of more than two by
changing the basic block size from 512 to 1024 bytes.
The increase was because of two factors:
each disk transfer accessed twice as much data, 
and most files could be described without need to access
indirect blocks since the direct blocks contained twice as much data.
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
Eventually the free list became entirely random,
causing files to have their blocks allocated randomly over the disk.
This forced a seek before every block access.
Although old file systems provided transfer rates of up
to 175 kilobytes per second when they were first created,
this rate deteriorated to 30 kilobytes per second after a
few weeks of moderate use because of this
randomization of data block placement.
There was no way of restoring the performance of an old file system
except to dump, rebuild, and restore the file system.
Another possibility, as suggested by [Maruyama76],
would be to have a process that periodically
reorganized the data on the disk to restore locality.
.ds RH New file system
.sp 2
.ne 1i
