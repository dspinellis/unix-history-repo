.\"	@(#)NBSproposal.t	1.2	88/02/26
.\" *troff -ms
.rm CM
.sp 2
.ce 100
\fB\s+2Proposal for a Prototype OSI Network
Implementation for Berkeley UNIX\s-2\fP
.ds DT "
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
The first is to develop an OSI network protocol suite and to integrate
existing ISO applications into Berkeley UNIX.
The second is to refine the TCP/IP networking to improve
its performance and limit congestion on slow and/or lossy networks.
The third is to provide a standard interface to file systems
so that multiple local and remote file systems can be supported,
much as multiple networking protocols are supported by 4.3BSD.
.PP
We are planning to implement prototypes for each of these
three main areas of work over the period of this proposal.
We plan to do an informal release at the end of the proposal period
to interested developers.
After incorporating feedback and refinements from the testers,
the work will appear in the next full Berkeley release that typically
is made about a year after the test release.
.br
.ne 10
.sp 2
.NH
Current UNIX Research at Berkeley
.PP
Since the release of 4.3BSD in mid 1986,
we have begun work on three major new areas of research.
Our goal is to apply leading edge research ideas into a stable
and reliable implementation that solves current problems in
operating systems research.
.NH 2
OSI network protocol development
.PP
The network architecture of 4.2BSD was designed to accommodate
multiple network protocol families and address formats,
and an implementation of the ISO OSI network protocols
should fit this framework without much difficulty.
The outline of the proposal is to
implement the OSI connectionless internet protocol (CLNP),
and device drivers for X.25, 802.3, and possibly 802.5 interfaces, and
to integrate these with an OSI transport class 4 (TP-4) implementation.
We will receive an updated ISO Development Environment (ISODE)
from Northrop (or any other party)
featuring International Standard (IS) versions of utilities, and
incorporate these into the Berkeley Software Distribution.
ISODE implements the session and presentation layers of the OSI protocol suite,
and will include an implementation of the file transfer protocol FTAM.
If possible, an X.400 implementation now being done at University College,
London and the University of Nottingham will also be available for testing
and distribution.
This work will include participation in
interoperability tests with vendors and users on OSINET.
.LP
The tasks to be done include: 
.IP 1)
We will incorporate an updated University of
Wisconsin TP-4 to match GOSIP requirements.
The University of Wisconsin developed a transport class 4
implementation for the 4.2BSD kernel under contract to Mitre.
This implementation must be updated to reflect NBS
workshop agreements, GOSIP, and 4.3BSD requirements.
We expect that the University of Wisconsin will do this job.
We will make this TP-4 operate with an OSI IP,
as the original implementation was built to run over the DoD IP.
.IP 2)
A kernel version of the OSI IP and ES-IS protocols must be produced.
We will implement the kernel version of these protocols.
.IP 3)
The required device drivers need to be integrated into BSD kernel.
4.3BSD has existing device drivers for many ethernet devices, and
has, or will soon have, an X.25 device driver.
These device drivers must be integrated
into the kernel OSI protocol implementations.
If other device drivers are required for token ring networks,
then additional work will be required.
.IP 4)
A POSIX (IEEE standard) network interface needs to be specified.
We will define a network interface between the kernel level
network services and the user-level programs (e.g., ISODE).
A proposal will be made to the IEEE P1003 committee to establish
a group to develop a POSIX network interface.
The UCB-defined interface will be submitted as a proposed standard.
The exact makeup of the committee is not yet agreed,
but representatives from the NBS,
Northrop, DoD, UCB, and Mitre are candidates.
Also, the work of drafting the proposed standard has not been assigned.
.IP 5)
The existing OSINET interoperability test network is available so 
that the interoperability of the ISODE and BSD kernel protocols
can be established through tests with several vendors.
Testing is crucial because an openly available version of GOSIP protocols
that does not interoperate with DEC, IBM, SUN, ICL, HIS, and other
major vendors would be embarrassing.
To allow testing of the integrated pieces the most desirable
approach is to provide access to OSINET at UCB.
A second approach is to do the interoperability testing at
the site of an existing OSINET member, such as the NBS.
UCB can join the OSINET directly by installing
ACCUNET X.25 circuits or an OSI IP level router can be
provided between the ARPANET and the OSINET.
.IP 6)
We will accept responsibility for integrating the UNIX kernel code,
the POSIX mapping library, and any related code.
We will also incorporate ISODE into the Berkeley Software Distribution
as user-contributed software.
The release cycle for 4BSD is about once every 24 months.
We will target a test release version of the system one year after the 
project is started.
.NH 2
Improvements to the TCP/IP Networking Protocols
.PP
The Internet and the Berkeley collection of local-area networks
have both grown at high rates in the last year.
The Bay Area Regional Research Network (BARRNet),
connecting several UC campuses, Stanford and NASA-Ames
has recently become operational, increasing the complexity
of the network connectivity.
Both Internet and local routing algorithms are showing the strain
of continued growth.
We have made several changes in the local routing algorithm
to keep accommodating the current topology,
and are participating in the development of new routing algorithms
and protocols.
.PP
Recent work in collaboration with Van Jacobson of the Lawrence Berkeley
Laboratory has led to the design and implementation of several new algorithms
for TCP that improve throughput on both local and long-haul networks
while reducing unnecessary retransmission.
The improvement is especially striking when connections must traverse
slow and/or lossy networks.
The new algorithms include ``slow-start,''
a technique for opening the TCP flow control window slowly
and using the returning stream of acknowledgements as a clock
to drive the connection at the highest speed tolerated by the intervening
network.
A modification of this technique allows the sender to dynamically modify
the send window size to adjust to changing network conditions.
In addition, the round-trip timer has been modified to estimate the variance
in round-trip time, thus allowing earlier retransmission of lost packets
with less spurious retransmission due to increasing network delay.
Along with a scheme proposed by Phil Karn of Bellcore,
these changes reduce unnecessary retransmission over difficult paths
such as Satnet by nearly two orders of magnitude
while improving throughput dramatically.
.PP
The current TCP implementation is now being readied
for more widespread distribution via the network.
We will continue to refine the TCP and IP implementations during the term
covered by this proposal, using the ARPANET, BARRNet, the NSF network
and local campus nets as testbeds, in preparation for the next 4BSD release.
In addition, we will incorporate applicable algorithms from this work
into the TP-4 protocol implementation.
.NH 2
Toward a Compatible File System Interface
.PP
The most critical shortcoming of our current UNIX system is in the
area of distributed file systems.
As with networking protocols,
there is no single distributed file system
that provides enough speed and functionality for all problems.
It is frequently necessary to support several different remote
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
university groups for internal use\-
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
Briefly, the proposal adopts the 4.3BSD calling convention for file name lookup
but otherwise is closely related to Sun's VFS [Karels86].
.PP
A prototype implementation is now being developed.
We expect that this work will be finished in time for a release at the
end of the period covered by this proposal.
.NH 2
Work since the release of 4.3BSD
.PP
There have been several changes in the system since the release of 4.3BSD
that have already been completed and would be part of any future release.
The largest change has been the incorporation of support for the first
non-VAX processor, the CCI Power 6/32.
The Power 6 version of 4.3BSD is based on the compilers and
device drivers done for CCI's 4.2BSD UNIX,
and is otherwise similar to the VAX release of 4.3.
The kernel source tree and the sources for all user-level software
have been merged using a structure that will accommodate addition
of other processor families.
The 4.3BSD release for the CCI Power 6 (and for OEM versions
sold by Harris and Sperry) is now in beta test.
.PP
During the work on the CCI machine,
we resolved that disk geometry and filesystem layout information
must be stored on each disk in a pack label.
Disk labels were implemented for the CCI disks and for the most common
types of disk controllers on the VAX.
A utility was written to create and maintain the disk information,
and other user-level programs that use such information now obtain
it from the disk label.
The use of this facility has allowed improvements in the file system's
knowledge of irregular disk geometries such as track-to-track skew.
.PP
In recent months we have been heavily involved in the development
the IEEE P1003.1 POSIX standard for portable operating system interfaces.
We have participated actively in the incorporation and improvement
of the job control and signal facilities first developed in Berkeley UNIX
systems.
Part of the current work has involved developing prototypes of the new
interfaces to get feedback on their strengths and weaknesses.
We expect that we will be able to provide POSIX compliant
interfaces in Berkeley UNIX shortly after the standard is finalized.
The major areas that we have prototyped include the POSIX terminal driver, 
POSIX job control, and POSIX group and user identifier semantics.
.br
.ne 10
.sp 2
.NH
References
.sp
.IP Brownbridge82
Brownbridge, D.R., L.F. Marshall, B. Randell,
``The Newcastle Connection, or UNIXes of the World Unite!,''
\fISoftware\- Practice and Experience\fP, Vol. 12, pp. 1147-1162, 1982.
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
.IP Walker85
Walker, B.J. and S.H. Kiser, ``The LOCUS Distributed File System,''
\fIThe LOCUS Distributed System Architecture\fP,
G.J. Popek and B.J. Walker, ed., The MIT Press, Cambridge, MA, 1985.
.sp
.IP Weinberger84
Weinberger, P.J., ``The Version 8 Network File System,''
\fIUsenix Conference presentation\fP,
June, 1984.
