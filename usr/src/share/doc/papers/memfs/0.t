.\"	@(#)0.t	1.1	(Copyright 1990 M. K. McKusick)	90/02/07
.rm CM
.nr PO 1.25i
.ds CH "
.ds CF "%
.nr Fn 0 1
.ds b3 4.3\s-1BSD\s+1
.de KI
.ds Lb "Fig. \\n+(Fn
.KF
.ce 1
Figure \\n(Fn - \\$1.
..
.de SM
\\s-1\\$1\\s+1\\$2
..
.de NM
\&\fI\\$1\fP\\$2
..
.de RN
\&\fI\\$1\fP\^(\^)\\$2
..
.de PN
\&\fB\\$1\fP\\$2
..
.TL
A Pageable Memory Based File System
.AU
Marshall Kirk McKusick
.AU
Michael J. Karels
.AU
Keith Bostic
.AI
Computer Systems Research Group
Computer Science Division
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California  94720
.sp
email: mckusick@Berkeley.EDU
telephone: 415-642-4948
.AB
This paper describes the motivation for memory based filesystems.
It compares the techniques used to implement them and
describes the drawbacks of using dedicated memory to
support such filesystems.
To avoid the drawbacks of using dedicated memory,
it tells how to build a simple memory based
filesystem in pageable memory.
It details the performance characteristics of this filesystem
and concludes with areas for future work.
.AE
.LP
.SH
Introduction
.PP
This paper describes the motivation for and implementation of
a memory based filesystem.
Memory based filesystems have existed for a long time;
they have generally been marketed as RAM-disks or sometimes
as software packages that use the general purpose memory on
a machine.
.[
white
.]
Because the contents of memory based filesystems are lost
across reboots and power failures their usefulness is limited
to transient or easily recreated information such as would
be found in
.PN /tmp .
Their primary benefit is that they have higher throughput
than disk based filesystems.
.[
smith
.]
.PP
Using dedicated memory to exclusively support a filesystem
is a poor use of resources.
The memory is better used in the buffer cache because of faster access
(one copy to the user process instead of a copy from RAM-disk
to the buffer cache, then another copy to the user process).
Additionally the memory can be used for other purposes
such as supporting virtual address space when necessary.
The new work being presented in this paper is to build the
filesystem in pageable memory instead of dedicated memory.
By building the filesystem in pageable memory,
it competes with other processes for the available memory.
When memory runs short, the paging system pushes its
least recently used pages to backing store.
Being pageable also allows the filesystem to be much larger than
would be practical if it were limited by the amount of physical
memory that could be dedicated to that purpose.
We typically operate our
.PN /tmp
with 30 to 60 megabytes of space
which is larger than the amount of memory on the machine.
This configuration allows small files to be accessed quickly,
while still allowing
.PN /tmp
to be used for big files albeit
at the speed more typical of normal disk based filesystems.
.PP
An alternative to building a memory based filesystem would be to have
a filesystem that never did operations synchronously and never
flushed its dirty buffers to disk.
However, such a filesystem would either use a disproportionately large
percentage of the buffer cache space to the detriment of other
disk based filesystems,
or would require the equivalent of the
paging system to push out its dirty pages.
Waiting for other filesystems to push dirty pages
subjects them to delays while waiting for the pages to be written.
.SH
Implementation
.PP
The implementation took less time to write than this paper.
It consists of 560 lines of kernel code (1.7K text + data)
and some minor modifications to the program that builds
disk based filesystems, ``newfs''.
A filesystem is created by invoking ``newfs'' with an option
telling it to create a memory based filesystem.
It allocates a piece of virtual address space of the requested
size and builds a filesystem in it instead of on a disk partition.
When built, it does a ``mount'' system call of type
.SM MFS
giving a pointer to the base of the memory in which it has built
the filesystem.
All operations on the filesystem are handled by the disk based
filesystem code to the point where the filesystem passes
a buffer with an I/O request to a disk driver.
.PP
The new code in the kernel to support the memory based
filesystem consists of a pseudo device driver to service
I/O requests from the disk based filesystem.
The request is serviced by doing a context-switch to the ``newfs''
process and doing a copyin from its address space to the buffer
for a read request or doing a copyout from the buffer to its
address space for a write request.
.PP
The paging of the filesystem does not require any additional
code beyond that already in the kernel to support virtual memory.
The ``newfs'' process competes with other processes on an equal basis
for the available memory on the machine.
If memory is plentiful, the entire contents of the filesystem
will remain memory resident.
When memory runs short the oldest pages of ``newfs'' will
be pushed to backing store.
These usually consist of files that have been left unused
in the memory based filesystem.
.[
leffler
.]
.SH
Performance
.PP
The performance of the memory based filesystem is determined by
the memory-to-memory copy speed of the processor.
Empirically we find that the throughput is about 45% of this
memory-to-memory copy speed.
The basic set of steps for each block written is:
.IP 1)
memory-to-memory copy from the user process doing the write to a kernel buffer
.IP 2)
context-switch to the ``newfs'' process
.IP 3)
memory-to-memory copy from the kernel buffer to the ``newfs'' address space
.IP 4)
context switch back to the writing process
.LP
Thus each write requires at least two memory-to-memory copies
accounting for about 90% of the
.SM CPU
time.
The remaining 10% is consumed in the context switches and
the filesystem allocation and block location code.
The actual context switch count is really only about half
of the worst case outlined above because
read-ahead and write-behind allow multiple blocks
to be handled with each context switch.
.SH
Future Work
.PP
Ideally part of the kernel's address space could reside in pageable memory.
Once such a facility is available it would be more efficient to
build a memory based filesystem within the kernel.
Such a filesystem would eliminate at least one of the memory-to-memory
copies in the present system, and would eliminate all the context switches.
.PP
The current system uses the existing local filesystem code to manage 
the virtual memory comprising the memory based filesystem.
It would be interesting to explore other filesystem implementations
which would be less expensive to execute and that would make better
use of the space.
.PP
It would be interesting to build a filesystem that never
did operations synchronously and never
flushed its dirty buffers to disk
to see how well it performed and how badly it affected
the other filesystems on the machine.
