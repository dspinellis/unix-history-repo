.\"	@(#)2.t	1.1	(Copyright 1986 M. K. McKusick)	86/11/25
.SH
The Future of UNIX at Berkeley
.PP
The release of 4.3BSD in April of 1986 addressed many of the 
performance problems and unfinished interfaces that were
present in 4.2BSD [Leffler84] [McKusick85].
We are now embarking on a new development phase to update older
parts of the system to the current state of the art.
There are three main areas of work.
The first is to rewrite the virtual memory system to take
advantage of current technology and to provide new capabilities
such as mapped files and shared memory.
The second is to provide a standard interface to file systems
to allow multiple local and remote file systems much the way
multiple networking protocols are supported in 4.3BSD.
Finally we want to provide more internal flexibility in a
way similar to the streams paradigm in System V.
.SH 2
A New Virtual Memory Implementation
.PP
With the cost per byte of memory approaching that of the cost per byte
for disks, and with file systems increasingly distant from the host
machines, a new approach to the implementation of virtual memory is
necessary. In 4.3BSD the swap space is preallocated which limits the
maximum virtual memory that can be supported to the size of the swap
area [Babaoglu79] [Someren84].
The new system should support virtual memory up to the sum of the
sizes of physical memory plus swap space (a system may run with no swap
space if it has no local disk). For systems with a local swap
disk, but remote file systems, it may be useful to use some of the memory
to keep track of the contents of the swap space to avoid multiple fetches
of the same data from the file system.
.PP
The new implementation should also add new functionality.  Processes
should be allowed to have large sparse address spaces, to map files
into their address spaces, to map device memory into their address
spaces, and to share memory with other processes. The shared address
space may either be obtained by mapping a file into (possibly
different) parts of their address space, or by arranging to share
``anonymous memory'' (that is, memory that is zero fill on demand, and
whose contents are lost when the last process unmaps the memory) with
another process as is done in System V.
.PP
One use of shared memory is to provide a high-speed
Inter-Process Communication (IPC) mechanism between two or more
cooperating processes. To insure the integrity of data structures
in a shared region, processes must be able to use semaphores to
coordinate their access to these shared structures. In System V,
these semaphores are provided as a set of system calls. Unfortunately,
the use of system calls reduces the throughput of the shared memory
IPC to that of existing IPC mechanisms.  We are proposing a scheme
that places the semaphores in the shared memory segment, so that
machines that have a test-and-set instruction can handle the usual
uncontested lock and unlock without doing a system call. Only in
the unusual case of trying to lock an already-locked lock or in
releasing a wanted lock will a system call be required.  The
interface will allow a user-level implementation of the System V semaphore
interface on most machines with a much lower runtime cost [McKusick86].
.SH 2
Toward a Compatible Filesystem Interface
.PP
As network or remote filesystems have been implemented for UNIX,
several stylized interfaces between the filesystem implementation
and the rest of the kernel have been developed.
Among these are Sun Microsystems' Virtual Filesystem interface (VFS)
using vnodes [Sandburg85] [Kleiman86],
Digital Equipment's Generic File System (GFS) architecture [Rodriguez86],
AT&T's File System Switch (FSS) [Rifkin86],
the LOCUS distributed filesystem [Walker85],
and Masscomp's extended filesystem [Cole85].
Other remote filesystems have been implemented in research or
university groups for internal use,
notably the network filesystem in the Eighth Edition UNIX
system [Weinberger84] and two different filesystems used at Carnegie-Mellon
University [Satyanarayanan85].
Numerous other remote file access methods have been devised for use
within individual UNIX processes,
many of them by modifications to the C I/O library
similar to those in the Newcastle Connection [Brownbridge82].
.PP
Each design attempts to isolate filesystem-dependent details
below a generic interface and to provide a framework within which
new filesystems may be incorporated.
However, each of these interfaces is different from
and incompatible with the others.
Each of them addresses somewhat different design goals.
Each was based on a different starting version of UNIX,
targeted a different set of filesystems with varying characteristics,
and uses a different set of primitive operations provided by the filesystem.
.PP
We have studied the various filesystem interfaces to determine
their generality, completeness, robustness, efficiency and aesthetics.
As a result of this study, we have developed a proposal for a new
filesystem interface that includes the best features of
the existing implementations.
The proposal adopts the calling convention for name lookup introduced
in 4.3BSD, but is otherwise closely related to Sun's VFS.
A prototype implementation is now being developed at Berkeley.
This proposal and the rationale underlying its development
have been presented to major software vendors as an early step
toward convergence on a compatible filesystem interface [Karels86].
.SH 2
Changes to the Protocol Layering Interface
.PP
The original work on restructuring the internal kernel interfaces
to allow flexible configuration of the internal modules by user
processes was done at Bell Laboratories [Ritchie84].
Known as stackable line disciplines, these interfaces allowed a user
process to open a raw terminal port, then push on desired processing modules
such as one to do line editing.
With the advent of networking, an obvious extension was to provide
processing modules that implemented various protocols such as
the internet protocols TCP and IP or the Xerox protocols SPP and IDP.
By stacking a terminal processing module on top of a stream based
networking protocol, a user could build a network virtual terminal
without all the delay and expense of using ``telnet'' or ``rlogin''.
.PP
A problem with stackable line disciplines is that they
are inherently linear in nature.
Thus they do not adequately model the fan-in and fan-out
associated with multiplexing.
The simple and elegant stackable line discipline implementation was
converted to the full production implementation
of streams in System V Release 3.
In doing the conversion, many of the pragmatic issues such as handling of
multiplexed connections were handled.
Unfortunately the complexity of the implementation increased as well.
.PP
Because AT&T will not allow the inclusion of streams without also
changing the interface to comply with the System V interface definition,
we cannot use their implementation of streams in the Berkeley system.
Given that compatibility is impossible, we have complete freedom
to make our choices based solely on technical merits.
Our implementation will appear far more like the simpler
stackable line disciplines than the more complex streams.
