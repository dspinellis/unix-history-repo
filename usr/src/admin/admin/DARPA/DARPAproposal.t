.\"	@(#)DARPAproposal.t	1.7	87/06/08
.\" *troff -ms
.rm CM
.sp 2
.ce 100
\fB\s+2Proposal for Continued UNIX-based
Operating Systems Research at Berkeley\s-2\fP
.ie 1 .ds DT 
.el     .ds DT \n(mo/\n(dy/\n(yr
\fBDRAFT of \*(DT\fP
.ce 0
.sp 2
.nf
.ce 4
Marshall Kirk McKusick
Michael J Karels
Susan L. Graham
Domenico Ferrari
.fi
.sp 2
.ce 1
\fISummary\fP
.PP
The release of 4.3BSD in April of 1986 addressed many of the 
performance problems and unfinished interfaces
present in 4.2BSD [Leffler84] [McKusick85].
The Computer Systems Research Group at Berkeley
has now embarked on a new development phase to
update other old parts of the system.
There are three main areas of work.
The first is to provide a standard interface to file systems
so that multiple local and remote file systems can be supported,
much as multiple networking protocols are supported by 4.3BSD.
The second is to rewrite the virtual memory system to take
advantage of current technology and to provide new capabilities
such as mapped files and shared memory.
Finally, there is a need to refine the internal layering of the network
and terminal protocols to provide more internal flexibility.
.sp 2
.NH
Accomplishments Under the Current Contract
.NH 2
4.3BSD
.PP
This section summarizes the work done at Berkeley between the
September 1983 4.2BSD distribution of
.UX
for the VAX\(dd
.FS
\(dd \s-2DEC\s0, \s-2VAX\s0, \s-2PDP\s0, \s-2MASSBUS\s0, \s-2UNIBUS\s0,
\s-2Q-bus\s0 and \s-2ULTRIX\s0
are trademarks of Digital Equipment Corporation.
.FE
and the March 1986 4.3BSD release.
Most of the changes between 4.2BSD and 4.3BSD fall into one
of several categories.
These are:
.RS
.IP \(bu 3
bug fixes,
.IP \(bu 3
performance improvements,
.IP \(bu 3
completion of skeletal facilities,
.IP \(bu 3
generalizations of the framework to accommodate
new hardware and software systems,
or to remove hardware- or protocol-specific code
from common facilities, and
.IP \(bu 3
new protocol and hardware support.
.RE
.LP
The major changes to the kernel are:
.RS
.IP \(bu 3
the use of caching to decrease the overhead of file system name translation,
.IP \(bu 3
a new interface to the \fInamei\fP name lookup function
that encapsulates the arguments, return information and side effects
of this call,
.IP \(bu 3
removal of most of the Internet dependencies from common parts of the network,
and greater allowance for the use of multiple address families on the same
network hardware,
.IP \(bu 3
support for the Xerox NS network protocols,
.IP \(bu 3
support for the VAX 8600 and 8650 processors (with UNIBUS and MASSBUS
peripherals, but not with CI bus or HSC50 disk controllers),
.IP \(bu 3
new drivers for the DHU11 and DMZ32 terminal multiplexors,
the TU81 and other TMSCP tape drives,
the VS100 display,
the DEUNA, Excelan 204, and Interlan NP100 Ethernet* interfaces, and
.FS
* Ethernet is a trademark of Xerox Corporation.
.FE
the ACC HDH and DDN X.25 IMP interfaces, and
.IP \(bu 3
full support for the MS780-E memory controller on the VAX 11/780 and 11/785,
using 64K and 256K memory chips.
.RE
.sp
.PP
The performance of the system has been improved to be at least as
good as that of 4.1BSD, and in many instances is better.
In addition to improving the performance of kernel operations,
heavily used utilities were optimized and
many user level programs were improved by
rewriting C library routines for efficiency.
.IP \s+1\(bu\s0
Many programs were rewritten to do I/O in optimal blocks for the file system.
Most of these programs were doing their own I/O and not using the standard
I/O library.
.IP \s+1\(bu\s0
The system now supports the Xerox Network System
network communication protocols.
Most of the remaining Internet dependencies in shared common code
have been removed or generalized.
.IP \s+1\(bu\s0
The signal mechanism has been extended
to allow selected signals to interrupt pending system calls.
.IP \s+1\(bu\s0
The C and Fortran 77 compilers have been modified so that they
can generate single precision floating point operations.
.IP \s+1\(bu\s0
The Fortran 77 compiler and associated I/O library have undergone
extensive changes to improve reliability and performance.  Compilation may,
optionally, include optimization phases to improve code density and
decrease execution time.
Many minor bugs in the C compiler have been fixed.
.IP \s+1\(bu\s0
The math library has been completely rewritten
by a group of numerical analysts
to improve both its speed and accuracy.
.IP \s+1\(bu\s0
Password lookup functions now use a hashed database rather than linear
search of the password file.
.IP \s+1\(bu\s0
C library string routines and several standard I/O functions
were recoded in VAX assembler for greater speed.
The C versions are available for portability.
Standard error is now buffered within a single call to do output.
.IP \s+1\(bu\s0
The symbolic debugger, \fIdbx\fP, has been dramatically improved.
\fIDbx\fP works on C, Pascal and Fortran 77 programs and allows users
to set break points and trace execution by source code line numbers,
references to memory locations, procedure entry, etc.  \fIDbx\fP allows
users to reference structured and local variables using
the program's programming language syntax.
.IP \s+1\(bu\s0
A new internet name domain server has been added to allow sites to
administer their name space locally and export it to the rest of the Internet.
Sites not using the name server may use a static host table with a hashed
lookup mechanism.
.IP \s+1\(bu\s0
A new time synchronization server has been added to allow a set of machines to
keep their clocks within tens of milliseconds of each other.
.NH 2
Work since the release of 4.3BSD
.PP
There have been several changes in the system since the release of 4.3BSD.
The largest change has been the incorporation of support for the first
non-VAX processor, the CCI Power 6/32.
The Power 6 version of 4.3BSD is based upon the compilers and
device drivers done for CCI's 4.2BSD UNIX,
and is otherwise similar to the VAX release of 4.3.
The kernel source tree and the sources for all user-level software
have been merged using a structure that will accommodate addition
of other processor families.
The 4.3BSD release for the CCI Power 6 (and for OEM versions
sold by Harris and Sperry) is now in beta test.
.PP
In the course of the work on the CCI machine, it was finally
resolved that disk geometry and filesystem layout information
must be stored on each disk in a pack label.
This was implemented for the CCI disks and for the most common
types of disk controllers on the VAX.
A utility was written to create and maintain the disk information,
and other user-level programs that use such information now obtain
it from the disk label.
.PP
The Internet and the Berkeley collection of local-area networks
have both grown at high rates in the last year.
The Bay Area Regional Research Network (BARRNet)
connecting several UC campuses, Stanford and NASA-Ames
has recently become operational, increasing the complexity
of the network connectivity.
Both Internet and local routing algorithms are showing the strain
of continued growth.
We have made several changes in the local routing algorithm
to keep accommodate the current topology,
and are participating in the development of new routing algorithms
and protocols.
.br
.ne 10
.sp 2
.NH
Current UNIX Research at Berkeley
.PP
Since the release of 4.3BSD in mid 1986,
we have begun work on three major new areas of research.
The decision on what our next areas of investigation should be were
drawn from discussions at the last steering committee meeting held
in the summer of 1985, and from later discussions held at
the annual Berkeley UNIX workshops.
Our goal is to apply leading edge research ideas into a stable
and reliable implementation that solves current problems in
operating systems research.
.NH 2
Toward a Compatible File System Interface
.PP
The most critical shortcoming of our current UNIX system is in the
area of distributed file systems.
As with networking protocols,
there is no single distributed file system
that provides enough speed and functionality for all problems.
It is frequently necessary to support several different distributed
file system protocols, just as it is necessary to run several 
different network protocols.
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
Our work is aimed at providing a common framework to
support these different distributed file systems simultaneously rather than to
simply implement yet another protocol.
This requires a detailed study of the existing protocols, 
and discussion with their implementors to determine whether
they can modify their implementation to fit within our proposed framework.
We have studied the various file system interfaces to determine
their generality, completeness, robustness, efficiency, and aesthetics.
Based on this study, we have developed a proposal for a new
file system interface that we believe includes the best features of
each of the existing implementations.
This proposal and the rationale underlying its development
have been presented to major software vendors as an early step
toward convergence on a compatible file system interface.
Briefly, the proposal adopts the 4.3BSD calling convention for name lookup,
but otherwise is closely related to Sun's VFS [Karels86].
.PP
A prototype implementation is now being developed.
We expect that this work will be finished in time for a release at the
end of the current DARPA contract if that is deemed desirable.
.NH 2
A New Virtual Memory Implementation
.PP
With the cost per byte of memory approaching that of the cost per byte
for disks, and with file systems increasingly removed from host
machines, a new approach to the implementation of virtual memory is
necessary.
In 4.3BSD the swap space is preallocated;
this limits the maximum virtual memory that can be
supported to the size of the swap area [Babaoglu79] [Someren84].
The new system should support virtual memory space at least as great as
the sum of sizes of physical memory plus swap space
(a system may run with no swap space if it has no local disk).
For systems that have a local swap
disk, but use remote file systems,
using some memory to keep track of the contents of swap space
may be useful to avoid multiple fetches
of the same data from the file system.
.PP
The new implementation should also add new functionality.  Processes
should be allowed to have large sparse address spaces, to map files
into their address spaces, to map device memory into their address
spaces, and to share memory with other processes.
The shared address
space may either be obtained by mapping a file into (possibly
different) parts of the address space, or by arranging for processes to
share ``anonymous memory'' (that is, memory that is zero-fill on demand, and
whose contents are lost when the last process unmaps the memory).
This latter approach was the one adopted by the developers of System V.
Unrelated processes should be able to share memory for reading and writing
if they are running on the same system.
Rendezvous for such uses of shared memory will use filesystem names.
In order to avoid the overhead of filesystem storage allocation
when only shared memory is desired, processes will be able to use
a new virtual-memory-resident filesystem.
.PP
One possible use of shared memory is to provide a high-speed
Inter-Process Communication (IPC) mechanism between two or more
cooperating processes.
To insure the integrity of data structures
in a shared region, processes must be able to use semaphores to
coordinate their access to these shared structures.
In System V,
semaphores are provided as a set of system calls.
Unfortunately,
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
interface on most machines with a much lower run time cost [McKusick86].
.PP
We have maintained an active mailing list to discuss
the issues of the user interface to the virtual memory system\(dg.
.FS
\(dgParticipants in the mailing list include:
Mike Karels and Kirk McKusick (CSRG),
Avadis Tevanian (Carnegie-Mellon Univ, MACH Project),
Dennis Ritchie (AT&T Bell Labs),
Robert Elz (Univ of Melbourne),
Michael L. Powell (DEC Western Research),
Bill Shannon, Rob Gingell, Dan Walsh, and Joe Moran (Sun Microsystems),
Tom Watson and Jim Mankovich (Convex),
Gregory Depp (DEC Ultrix Engineering),
Ron Gomes (AT&T Information Systems),
David C. Stewart (Sequent),
Jack A. Test and Herb Jacobs (Alliant),
Steve Gaede (NBI),
Jim Lipkis (New York Univ),
Stephen J. Hartley (Univ of Vermont),
Hermann Haertig and Alan Sexton (European Computer-Industry
Research Center, Germany),
Jukka Virtanen (Helsinki Univ of Technology, Finland)
.FE
Within the last few months, the specification of the interface has been
agreed upon.
A preliminary design has been done, and implementation is scheduled
to start this summer.
There are several groups that have recently done
virtual memory implementations, including several major UNIX vendors
as well as groups in academic environments.
We intend to take advantage of the experience gained by these groups.
The academic work is most interesting to us because the source
code is unencumbered by licensing restrictions making it readily
available for our direct incorporation.
The most promising of this work is that done as part of the MACH
project since it can most likely be extended to provide the
services described for our user interface [Accetta86].
.PP
A preliminary version of the new virtual memory system is expected
to be available for testing near the end of the current contract.
The virtual memory work uses many new and untested ideas that will
require extensive experience to insure that they work well in a wide range
of environments.
.NH
Future Projects
.PP
The stream protocol research is a longer term project
that we would expect would be done as part of our next contract
or extension.
It is our expectation that this work would be ready for release toward
the end of the follow-on contract.
.NH 2
Changes to the Protocol Layering Interface
.PP
The original work on restructuring the UNIX character I/O system
to allow flexible configuration of the internal processing modules
was done at Bell Laboratories [Ritchie84].
Known as the stream I/O system, the new system allowed a user
process to open a raw terminal port and then insert appropriate
processing modules (such as one to do normal terminal line editing).
This model allowed terminal processing modules to be used with
virtual-circuit networks to create ``network virtual terminals''
by stacking a terminal processing module on top of a
networking protocol.
.PP
A problem with stream line disciplines, though, is that they
are inherently linear in nature.
Thus, they do not adequately handle the fan-in and fan-out
associated with multiplexing in datagram-based networks.
The simple and elegant stream I/O system
of Eighth Edition UNIX was converted to the production implementation
of Streams in System V Release 3.
In doing the conversion, many pragmatic issues were addressed,
including the handling of
multiplexed connections and commercially important protocols.
Unfortunately, the implementation complexity increased enormously.
.PP
The design of the networking facilities for 4.2BSD took
a different approach, based on the \fBsocket\fP interface
and a flexible multi-layer network architecture.
This design allows a single system to support multiple sets of networking
protocols with stream, datagram, and other types of access.
Protocol modules may deal with multiplexing of data from different connections
onto a single transport medium, and demultiplexing of data for different
protocols and connections received from each network device.
.PP
Because AT&T will not allow others to include Streams unless they
also change their interface to comply with the System V Interface Definition
base and Networking Extension,
we cannot use the Release 3 implementation of Streams in the Berkeley system.
Given that complete compatibility thus will be difficult,
we feel we will have complete freedom to make our
choices based solely on technical merits.
We intend to combine the best features of the original Bell Labs
version of streams with the network-level structure used in 4.2BSD.
As a result, our implementation will appear far more like the original
Bell Labs stream implementation
than the more complex Release 3 Streams [Chandler86].
A socket interface will be used rather than a character device interface,
and demultiplexing will be handled internally by the protocols in the kernel.
However, like Streams, the interfaces between kernel
protocol modules above the multiplexed layers
will follow a uniform convention.
This convention will allow incorporation of terminal processing
modules into a network stream, producing efficient network virtual
terminal connections.
It will also allow kernel support for remote procedure
protocols based on standard transport protocols.
Finally, this interface will provide a mechanism to extend the kernel
protocol framework into user processes to allow prototyping
of new protocols and to do network monitoring functions.
.PP
We have recently requested a distribution of Eighth Edition UNIX
with permission to incorporate parts of the stream I/O system
into future Berkeley releases.
It will be substantially modified if we use it as a part of the new
protocol layering scheme.
However, adoption of a consistent set of message types
and certain structuring techniques will increase
the similarity of user interfaces between the Berkeley
implementation and the Bell Labs research system.
.NH 2
Process debugging interface
.PP
As a part of our request to Bell Labs for Eighth Edition UNIX,
we have also requested permission to incorporate the \fI/proc\fP
process debugging interface for future release.
\fI/proc\fP will be heavily modified to fit the Berkeley system environment,
as it depends heavily on both the filesystem
interface and the virtual memory system.
However, use of the interface from the research version of UNIX
would increase compatibility between the two systems.
.NH 2
ISO protocol development
.PP
The DoD has committed publicly to adoption of the ISO standard networking
protocols as they are specified and implemented.
We were recently approached by Mike Corrigan of the Office
of the Secretary of Defense to see whether we are interested
in developing and/or integrating ISO standard protocols for Berkeley UNIX.
This area has been receiving much attention in the Internet research
community, but few implementations have been available for testing
or experimentation.
The network architecture of 4.2BSD was designed to accommodate
multiple network protocol families and address formats,
and an ISO implementation should fit this framework without
difficulty.
This subject is still under consideration.
.NH
References
.sp
.IP Accetta86 \w'Satyanarayanan85\0\0'u
Accetta, M., R. Baron, W. Bolosky, R. Rashid, A. Tevanian, M. Young,
``MACH: A New Foundation for UNIX Development''
Computer Science Department, Carnegie-Mellon University, Pittsburg, PA 15213,
April 1986.
.sp
.IP Babaoglu79
Babaoglu, O., W. Joy,
``Data Structures Added in the Berkeley Virtual Memory Extensions
to the UNIX Operating System''
Computer Systems Research Group, Dept of EECS, University of California,
Berkeley, CA 94720, USA, November 1979.
.sp
.IP Brownbridge82
Brownbridge, D.R., L.F. Marshall, B. Randell,
``The Newcastle Connection, or UNIXes of the World Unite!,''
\fISoftware\- Practice and Experience\fP, Vol. 12, pp. 1147-1162, 1982.
.sp
.IP Chandler86
Chandler, D.,
``The Monthly Report \- Up the Streams Without a Standard'',
\fIUNIX Review\fP, Vol. 4, No. 9, pp. 6-14, September 1986.
.sp
.IP Cole85
Cole, C.T., P.B. Flinn, A.B. Atlas,
``An Implementation of an Extended File System for UNIX,''
\fIUsenix Conference Proceedings\fP,
pp. 131-150, June, 1985.
.sp
.IP Karels86
Karels, M., M. McKusick,
``Towards a Compatible File System Interface,''
\fIProceedings of the European UNIX Users Group Meeting\fP,
Manchester, England, pp. 481-496, September 1986.
.sp
.IP Kleiman86
Kleiman, S.,
``Vnodes: An Architecture for Multiple File System Types in Sun UNIX,''
\fIUsenix Conference Proceedings\fP,
pp. 238-247, June, 1986.
.sp
.IP Leffler84
Leffler, S., M.K. McKusick, M. Karels,
``Measuring and Improving the Performance of 4.2BSD,''
\fIUsenix Conference Proceedings\fP, pp. 237-252, June, 1984.
.sp
.IP McKusick85
McKusick, M.K., M. Karels, S. Leffler,
``Performance Improvements and Functional Enhancements in 4.3BSD,''
\fIUsenix Conference Proceedings\fP, pp. 519-531, June, 1985.
.sp
.IP McKusick86
McKusick, M., M. Karels,
``A New Virtual Memory Implementation for Berkeley UNIX,''
\fIProceedings of the European UNIX Users Group Meeting\fP,
Manchester, England, pp. 451-460, September 1986.
.sp
.IP Rifkin86
Rifkin, A.P., M.P. Forbes, R.L. Hamilton, M. Sabrio, S. Shah, K. Yueh,
``RFS Architectural Overview,'' \fIUsenix Conference Proceedings\fP,
pp. 248-259, June, 1986.
.sp
.IP Ritchie74
Ritchie, D.M., K. Thompson,
``The Unix Time-Sharing System,''
\fICommunications of the ACM\fP, Vol. 17, pp. 365-375, July, 1974.
.sp
.IP Rodriguez86
Rodriguez, R., M. Koehler, R. Hyde,
``The Generic File System,''
\fIUsenix Conference Proceedings\fP,
pp. 260-269, June, 1986.
.sp
.IP Sandberg85
Sandberg, R., D. Goldberg, S. Kleiman, D. Walsh, B. Lyon,
``Design and Implementation of the Sun Network File System,''
\fIUsenix Conference Proceedings\fP,
pp. 119-130, June, 1985.
.sp
.IP Satyanarayanan85
Satyanarayanan, M., \fIet al.\fP,
``The ITC Distributed File System: Principles and Design,''
\fIProc. 10th Symposium on Operating Systems Principles\fP, pp. 35-50,
ACM, December, 1985.
.sp
.IP Someren84
Someren, J. van,
``Paging in Berkeley UNIX,''
Laboratorium voor schakeltechniek en techneik v.d. 
informatieverwerkende machines,
Codenummer 051560-44(1984)01, February 1984.
.sp
.IP Walker85
Walker, B.J. and S.H. Kiser, ``The LOCUS Distributed File System,''
\fIThe LOCUS Distributed System Architecture\fP,
G.J. Popek and B.J. Walker, ed., The MIT Press, Cambridge, MA, 1985.
.sp
.IP Weinberger84
Weinberger, P.J., ``The Version 8 Network File System,''
\fIUsenix Conference presentation\fP,
June, 1984.
