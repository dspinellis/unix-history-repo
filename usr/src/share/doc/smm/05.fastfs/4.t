.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)4.t	5.1 (Berkeley) %G%
.\"
.ds RH Performance
.NH 
Performance
.PP
Ultimately, the proof of the effectiveness of the
algorithms described in the previous section
is the long term performance of the new file system.
.PP
Our empiric studies have shown that the inode layout policy has
been effective.
When running the ``list directory'' command on a large directory
that itself contains many directories,
the number of disk accesses for inodes is cut by a factor of two.
The improvements are even more dramatic for large directories
containing only files,
disk accesses for inodes being cut by a factor of eight.
This is most encouraging for programs such as spooling daemons that
access many small files,
since these programs tend to flood the
disk request queue on the old file system.
.PP
Table 2 summarizes the measured throughput of the new file system.
Several comments need to be made about the conditions under which these
tests were run.
The test programs measure the rate that user programs can transfer
data to or from a file without performing any processing on it.
These programs must write enough data to insure that buffering in the
operating system does not affect the results.
They should also be run at least three times in succession;
the first to get the system into a known state
and the second two to insure that the 
experiment has stabilized and is repeatable.
The methodology and test results are
discussed in detail in [Kridle83]\(dg.
.FS
\(dg A UNIX command that is similar to the reading test that we used is,
``cp file /dev/null'', where ``file'' is eight Megabytes long.
.FE
The systems were running multi-user but were otherwise quiescent.
There was no contention for either the cpu or the disk arm.
The only difference between the UNIBUS and MASSBUS tests
was the controller.
All tests used an Ampex Capricorn 330 Megabyte Winchester disk.
As Table 2 shows, all file system test runs were on a VAX 11/750.
All file systems had been in production use for at least
a month before being measured.
.KF
.DS B
.TS
box;
c c|c s s
c c|c c c.
Type of	Processor and	Read
File System	Bus Measured	Speed	Bandwidth	% CPU
_
old 1024	750/UNIBUS	29 Kbytes/sec	29/1100 3%	11%
new 4096/1024	750/UNIBUS	221 Kbytes/sec	221/1100 20%	43%
new 8192/1024	750/UNIBUS	233 Kbytes/sec	233/1100 21%	29%
new 4096/1024	750/MASSBUS	466 Kbytes/sec	466/1200 39%	73%
new 8192/1024	750/MASSBUS	466 Kbytes/sec	466/1200 39%	54%
.TE
.ce 1
Table 2a \- Reading rates of the old and new UNIX file systems.
.TS
box;
c c|c s s
c c|c c c.
Type of	Processor and	Write
File System	Bus Measured	Speed	Bandwidth	% CPU
_
old 1024	750/UNIBUS	48 Kbytes/sec	48/1100 4%	29%
new 4096/1024	750/UNIBUS	142 Kbytes/sec	142/1100 13%	43%
new 8192/1024	750/UNIBUS	215 Kbytes/sec	215/1100 19%	46%
new 4096/1024	750/MASSBUS	323 Kbytes/sec	323/1200 27%	94%
new 8192/1024	750/MASSBUS	466 Kbytes/sec	466/1200 39%	95%
.TE
.ce 1
Table 2b \- Writing rates of the old and new UNIX file systems.
.DE
.KE
.PP
Unlike the old file system,
the transfer rates for the new file system do not
appear to change over time.
The throughput rate is tied much more strongly to the
amount of free space that is maintained.
The measurements in Table 2 were based on a file system run
with 10% free space.
Synthetic work loads suggest the performance deteriorates
to about half the throughput rates given in Table 2 when no
free space is maintained.
.PP
The percentage of bandwidth given in Table 2 is a measure
of the effective utilization of the disk by the file system.
An upper bound on the transfer rate from the disk is measured
by doing 65536* byte reads from contiguous tracks on the disk.
.FS
* This number, 65536, is the maximal I/O size supported by the
VAX hardware; it is a remnant of the system's PDP-11 ancestry.
.FE
The bandwidth is calculated by comparing the data rates
the file system is able to achieve as a percentage of this rate.
Using this metric, the old file system is only
able to use about 3-4% of the disk bandwidth,
while the new file system uses up to 39%
of the bandwidth.
.PP
In the new file system, the reading rate is always at least
as fast as the writing rate.
This is to be expected since the kernel must do more work when
allocating blocks than when simply reading them.
Note that the write rates are about the same 
as the read rates in the 8192 byte block file system;
the write rates are slower than the read rates in the 4096 byte block
file system.
The slower write rates occur because
the kernel has to do twice as many disk allocations per second,
and the processor is unable to keep up with the disk transfer rate.
.PP
In contrast the old file system is about 50%
faster at writing files than reading them.
This is because the \fIwrite\fR system call is asynchronous and
the kernel can generate disk transfer
requests much faster than they can be serviced,
hence disk transfers build up in the disk buffer cache.
Because the disk buffer cache is sorted by minimum seek order,
the average seek between the scheduled disk writes is much
less than they would be if the data blocks are written out
in the order in which they are generated.
However when the file is read,
the \fIread\fR system call is processed synchronously so
the disk blocks must be retrieved from the disk in the
order in which they are allocated.
This forces the disk scheduler to do long
seeks resulting in a lower throughput rate.
.PP
The performance of the new file system is currently
limited by a memory to memory copy operation
because it transfers data from the disk into buffers
in the kernel address space and then spends 40% of the processor
cycles copying these buffers to user address space.
If the buffers in both address spaces are properly aligned, 
this transfer can be affected without copying by
using the VAX virtual memory management hardware.
This is especially desirable when large amounts of data
are to be transferred.
We did not implement this because it would change the semantics
of the file system in two major ways;
user programs would be required to allocate buffers on page boundaries, 
and data would disappear from buffers after being written.
.PP
Greater disk throughput could be achieved by rewriting the disk drivers
to chain together kernel buffers.
This would allow files to be allocated to
contiguous disk blocks that could be read
in a single disk transaction.
Most disks contain either 32 or 48 512 byte sectors per track.
The inability to use contiguous disk blocks effectively limits the performance
on these disks to less than fifty percent of the available bandwidth.
Since each track has a multiple of sixteen sectors
it holds exactly two or three 8192 byte file system blocks,
or four or six 4096 byte file system blocks.
If the the next block for a file cannot be laid out contiguously,
then the minimum spacing to the next allocatable
block on any platter is between a sixth and a half a revolution.
The implication of this is that the best possible layout without
contiguous blocks uses only half of the bandwidth of any given track.
If each track contains an odd number of sectors, 
then it is possible to resolve the rotational delay to any number of sectors
by finding a block that begins at the desired 
rotational position on another track.
The reason that block chaining has not been implemented is because it
would require rewriting all the disk drivers in the system,
and the current throughput rates are already limited by the
speed of the available processors.
.PP
Currently only one block is allocated to a file at a time.
A technique used by the DEMOS file system
when it finds that a file is growing rapidly,
is to preallocate several blocks at once,
releasing them when the file is closed if they remain unused.
By batching up the allocation the system can reduce the
overhead of allocating at each write,
and it can cut down on the number of disk writes needed to
keep the block pointers on the disk
synchronized with the block allocation [Powell79].
.ds RH Functional enhancements
.bp
