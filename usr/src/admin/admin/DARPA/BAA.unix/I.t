.\"     @(#)I.t	1.1     89/02/23
.LP
\fB\s+4I. Accomplishments of the Berkeley UNIX Project\fP\s-4
.PP
The last major release of Berkeley UNIX,
in June, 1986, was 4.3BSD [CSRG86].
The release of 4.3BSD in April of 1986 addressed many of the 
performance problems and unfinished interfaces
present in 4.2BSD [Leffler84] [McKusick85].
Projects completed since that time have been made available
in a July, 1988 test release known as the 4.3BSD Tahoe release,
named for the CCI Power 6/32 processor for which support was added.
.sp
.LP
\fB\s+2I.1 4.3BSD\fP\s-2
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
new facilities,
.IP \(bu 3
new protocol and hardware support,
.IP \(bu 3
completion of skeletal facilities,
.IP \(bu 3
generalizations of the framework to accommodate
new hardware and software systems,
or to remove hardware- or protocol-specific code
from common facilities,
.IP \(bu 3
bug fixes, and
.IP \(bu 3
performance improvements.
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
The system is much better adapted to the larger physical memories
of current computers.
In addition to improving the performance of kernel operations,
heavily used utilities were optimized and
many user level programs were improved by
rewriting C library routines for efficiency.
.IP \s+1\(bu\s0
A new internet name domain server has been added to allow sites to
administer their name space locally and export it to the rest of the Internet.
Sites not using the name server may use a static host table with a hashed
lookup mechanism.
.IP \s+1\(bu\s0
A new time synchronization server has been added to allow a set of machines to
keep their clocks within tens of milliseconds of each other.
.IP \s+1\(bu\s0
The system now supports the Xerox Network System
network communication protocols.
Most of the remaining Internet dependencies in shared common code
have been removed or generalized.
.IP \s+1\(bu\s0
The math library has been completely rewritten
by a group of numerical analysts
to improve both its speed and accuracy.
.IP \s+1\(bu\s0
The symbolic debugger, \fIdbx\fP, has been dramatically improved.
\fIDbx\fP works on C, Pascal and Fortran 77 programs and allows users
to set break points and trace execution by source code line numbers,
references to memory locations, procedure entry, etc.  \fIDbx\fP allows
users to reference structured and local variables using
the program's programming language syntax.
.IP \s+1\(bu\s0
Many programs were rewritten to do I/O in optimal blocks for the file system.
Most of these programs were doing their own I/O and not using the standard
I/O library.
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
Password lookup functions now use a hashed database rather than linear
search of the password file.
.IP \s+1\(bu\s0
C library string routines and several standard I/O functions
were recoded in VAX assembler for greater speed.
The C versions are available for portability.
Standard error is now buffered within a single call to do output.
.sp
.LP
\fB\s+2I.2 Work since the release of 4.3BSD\fP\s-2
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
to keep accommodating the current topology,
and are participating in the development of new routing algorithms
and protocols.
.bp
