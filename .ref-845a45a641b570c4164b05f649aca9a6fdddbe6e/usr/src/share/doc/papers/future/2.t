.\" Copyright (c) 1986 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.man%
.\"
.\"	@(#)2.t	5.1 (Berkeley) %G%
.\"
.NH
The Future of UNIX at Berkeley
.PP
The release of 4.3BSD in April of 1986 addressed many of the 
performance problems and unfinished interfaces
present in 4.2BSD [Leffler84] [McKusick85].
Berkeley has now embarked on a new development phase to likewise
update other old parts of the system.
There are three main areas of work.
The first is to rewrite the virtual memory system to take
advantage of current technology and to provide new capabilities
such as mapped files and shared memory.
The second is to provide a standard interface to file systems
so that multiple local and remote file systems can be supported
much as multiple networking protocols are by 4.3BSD.
Finally, there is a need to provide more internal flexibility in a
way similar to the System V Streams paradigm.
.NH 2
A New Virtual Memory Implementation
.PP
With the cost per byte of memory approaching that of the cost per byte
for disks, and with file systems increasingly removed from host
machines, a new approach to the implementation of virtual memory is
necessary. In 4.3BSD the swap space is preallocated;
this limits the maximum virtual memory that can be
supported to the size of the swap area [Babaoglu79] [Someren84].
The new system should support virtual memory space at least as great as
the sum of sizes of physical memory plus swap space
(a system may run with no swap space if it has no local disk).
For systems that have a local swap
disk, but utilize remote file systems,
using some memory to keep track of the contents of swap space
may be useful to avoid multiple fetches
of the same data from the file system.
.PP
The new implementation should also add new functionality.  Processes
should be allowed to have large sparse address spaces, to map files
into their address spaces, to map device memory into their address
spaces, and to share memory with other processes. The shared address
space may either be obtained by mapping a file into (possibly
different) parts of the address space, or by arranging for processes to
share ``anonymous memory'' (that is, memory that is zero-fill on demand, and
whose contents are lost when the last process unmaps the memory).
This latter approach was the one adopted by the developers of System V.
.PP
One possible use of shared memory is to provide a high-speed
Inter-Process Communication (IPC) mechanism between two or more
cooperating processes. To insure the integrity of data structures
in a shared region, processes must be able to use semaphores to
coordinate their access to these shared structures. In System V,
semaphores are provided as a set of system calls. Unfortunately,
the use of system calls reduces the throughput of the shared memory
IPC to that of existing IPC mechanisms.
To avoid this bottleneck,
we expect that the next release of BSD will incorporate a scheme
that places the semaphores in the shared memory segment, so that
machines with a test-and-set instruction will be able to handle the usual
uncontested ``lock'' and ``unlock'' without doing two system calls.
Only in the unusual case of trying to lock an already-locked lock or when
a desired lock is being released will a system call be required.  The
interface will allow a user-level implementation of the System V semaphore
interface on most machines with a much lower runtime cost [McKusick86].
.NH 2
Toward a Compatible File System Interface
.PP
As network or remote file systems have been implemented for UNIX,
several stylized interfaces between the file system implementation
and the rest of the kernel have been developed.
Among these are Sun Microsystems' Virtual File System interface (VFS)
using \fBvnodes\fP [Sandburg85] [Kleiman86],
Digital Equipment's Generic File System (GFS) architecture [Rodriguez86],
AT&T's File System Switch (FSS) [Rifkin86],
the LOCUS distributed file system [Walker85],
and Masscomp's extended file system [Cole85].
Other remote file systems have been implemented in research or
university groups for internal use \-
notably the network file system in the Eighth Edition UNIX
system [Weinberger84] and two different file systems used at Carnegie Mellon
University [Satyanarayanan85].
Numerous other remote file access methods have been devised for use
within individual UNIX processes,
many of them by modifications to the C I/O library
similar to those in the Newcastle Connection [Brownbridge82].
.PP
Each design attempts to isolate file system-dependent details
below a generic interface and to provide a framework within which
new file systems may be incorporated.
However, each of these interfaces is different from
and is incompatible with the others.
Each addresses somewhat different design goals,
having been based on a different starting version of UNIX,
having targeted a different set of file systems with varying characteristics,
and having selected a different set of file system primitive operations.
.PP
We have studied the various file system interfaces to determine
their generality, completeness, robustness, efficiency, and aesthetics.
Based on this study, we have developed a proposal for a new
file system interface that we believe includes the best features of
each of the existing implementations.
Briefly, the proposal adopts the 4.3BSD calling convention for name lookup,
but otherwise is closely related to Sun's VFS.
A prototype implementation now is being developed.
This proposal and the rationale underlying its development
have been presented to major software vendors as an early step
toward convergence on a compatible file system interface [Karels86].
.NH 2
Changes to the Protocol Layering Interface
.PP
The original work on restructuring the UNIX character I/O system
to allow flexible configuration of the internal modules by user
processes was done at Bell Laboratories [Ritchie84].
Known as stackable line disciplines, these interfaces allowed a user
process to open a raw terminal port and then push on appropriate
processing modules (such as one to do line editing).
This model allowed terminal processing modules to be used with
virtual-circuit network modules to create ``network virtual terminals''
by stacking a terminal processing module on top of a
networking protocol.
.PP
The design of the networking facilities for 4.2BSD took
a different approach based on the \fBsocket\fP interface.
This design allows a single system to support multiple sets of networking
protocols with stream, datagram, and other types of access.
Protocol modules may deal with multiplexing of data from different connections
onto a single transport medium.
.PP
A problem with stackable line disciplines though, is that they
are inherently linear in nature.
Thus, they do not adequately model the fan-in and fan-out
associated with multiplexing.
The simple and elegant stackable line discipline implementation
of Eighth Edition UNIX was converted to the full production implementation
of Streams in System V Release 3.
In doing the conversion, many pragmatic issues were addressed,
including the handling of
multiplexed connections and commercially important protocols.
Unfortunately, the implementation complexity increased enormously.
.PP
Because AT&T will not allow others to include Streams unless they
also change their interface to comply with the System V Interface Definition
base and Networking Extension,
we cannot use the Release 3 implementation of Streams in the Berkeley system.
Given that compatibility thus will be difficult,
we feel we will have complete freedom to make our
choices based solely on technical merits.
As a result, our implementation will appear far more like the simpler stackable
line disciplines than the more complex Release 3 Streams [Chandler86].
A socket interface will be used rather than a character device interface,
and demultiplexing will be handled internally by the protocols in the kernel.
However, like Streams, the interfaces between kernel
protocol modules will follow a uniform convention.
