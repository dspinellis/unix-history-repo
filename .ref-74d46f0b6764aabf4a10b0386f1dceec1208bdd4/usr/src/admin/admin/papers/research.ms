.\" @(#)research.ms	1.3 %G%
.\" *troff -ms
.rm CM
.sp 2
.ce 100
\fB\s+2Current Research by
The Computer Systems Research Group
of Berkeley\s-2\fP
.ds DT "
\fB\*(DT\fP
.sp 2
.nf
Marshall Kirk McKusick
Michael J Karels
Keith Sklower
Kevin Fall
Marc Teitelbaum
Keith Bostic
.fi
.sp 2
.ce 1
\fISummary\fP
.ce 0
.PP
The release of 4.3BSD in April of 1986 addressed many of the 
performance problems and unfinished interfaces
present in 4.2BSD [Leffler84] [McKusick85].
The Computer Systems Research Group at Berkeley
has embarked on a new development phase to
update other major components of the system, as well as to offer
new functionality.
There are seven major ongoing projects.
.PP
The first five projects are near completion and will soon be sent to beta
test sites.
The first is to develop an OSI network protocol suite and to integrate
existing ISO applications into Berkeley UNIX.
The second is to develop and support an interface compliant with the
P1003.1 POSIX standard recently approved by the IEEE.
The third is to refine the TCP/IP networking to improve
its performance and limit congestion on slow and/or lossy networks.
The fourth is to provide a standard interface to file systems
so that multiple local and remote file systems can be supported,
much as multiple networking protocols are supported by 4.3BSD.
The fifth is to evaluate alternate access control mechanisms and
audit the existing security features of the system, particularly
with respect to network services.
.PP
The remaining two projects will not be in the next beta release,
but are expected to be complete by the release of 4.4BSD. 
The first is to replace the 4.3BSD virtual memory system
in order to support mapped files, larger memories, copy-on-write on fork
and I/O, and better portability.
The other project is a revised internal network
architecture incorporating the best features of both the existing
socket support and the ``streams'' layering of the Ninth Edition
research version of \s-1UNIX\s0.
.PP
Other areas of work include multi-architecture support, 
a general purpose kernel memory allocator, disk labels, and
extensions to the 4.2BSD fast filesystem.
.PP
Several of these projects are nearing completion,
while others are in the prototype stage.
We are planning to provide an informal test release sometime this summer
to interested developers containing most of these changes.
After incorporating feedback and refinements from the testers,
they will appear in the next full Berkeley release, which is typically
made about a year after the test release.
.br
.ne 10
.sp 2
.NH
Recently Completed Projects
.PP
There have been several changes in the system that were included
in the 4.3BSD Tahoe release.
.NH 2
Multi-architecture support
.PP
Support has been added for the DEC VAX 8600/8650, VAX 8200/8250, 
MicroVAXII and MicroVAXIII.
.PP
The largest change has been the incorporation of support for the first
non-VAX processor, the CCI Power 6/32 and 6/32SX.  (This addition also
supports the
Harris HCX-7 and HCX-9, as well as the Sperry 7000/40 and ICL machines.)
The Power 6 version of 4.3BSD is largely based on the compilers and
device drivers done for CCI's 4.2BSD UNIX,
and is otherwise similar to the VAX release of 4.3BSD.
The entire source tree, including all kernel and user-level sources,
has been merged using a structure that will easily accommodate the addition
of other processor families.  A MIPS R2000 has been donated to us,
making the MIPS architecture a likely candidate for inclusion into a future
BSD release.
.NH 2
Kernel Memory Allocator
.PP
The 4.3BSD UNIX kernel used 10 different memory allocation mechanisms,
each designed for the particular needs of the utilizing subsystem.
These mechanisms have been replaced by a general purpose dynamic
memory allocator that can be used by all of the kernel subsystems.
The design of this allocator takes advantage of known memory usage
patterns in the UNIX kernel and a hybrid strategy that is time-efficient
for small allocations and space-efficient for large allocations.
This allocator replaces the multiple memory allocation interfaces 
with a single easy-to-program interface,
results in more efficient use of global memory by eliminating
partitioned and specialized memory pools,
and is quick enough (approximately 15 VAX instructions) that no
performance loss is observed relative to the current implementations.
[McKusick88].
.NH 2
Disk Labels
.PP
During the work on the CCI machine,
it became obvious that disk geometry and filesystem layout information
must be stored on each disk in a pack label.
Disk labels were implemented for the CCI disks and for the most common
types of disk controllers on the VAX.
A utility was written to create and maintain the disk information,
and other user-level programs that use such information now obtain
it from the disk label.
The use of this facility has allowed improvements in the file system's
knowledge of irregular disk geometries such as track-to-track skew.
.NH 2
Fat Fast File System
.PP
The 4.2 fast file sytem [McKusick84]
contained several statically sized structures,
imposing limits on the number of cylinders per cylinder group,
inodes per cylinder group,
and number of distinguished rotational positions.
The new ``fat'' filesystem allows these limits to be set at filesystem
creation time.
Old kernels will treat the new filesystems as read-only,
and new kernels
will accomodate both formats.
The filesystem check facility, \fCfsck\fP, has also been modified to check
either type.
.br
.ne 10
.sp 2
.NH
Current UNIX Research at Berkeley
.PP
Since the release of 4.3BSD in mid 1986,
we have begun work on several new major areas of research.
Our goal is to apply leading edge research ideas into a stable
and reliable implementation that solves current problems in
operating systems development.
.NH 2
OSI network protocol development
.PP
The network architecture of 4.2BSD was designed to accommodate
multiple network protocol families and address formats,
and an implementation of the ISO OSI network protocols
should enter into this framework without much difficulty.
The current Berkeley system includes
implementations of the OSI Connectionless Network Protocol (CLNP),
the End System\-Intermediate System protocol (ES-IS),
and OSI transport class 4 (TP-4)
done by the University of Wisconsin with support from IBM.
Device drivers for 802.3 are now included as well,
and device drivers for X.25 and possibly 802.5 interfaces
are expected to be added.
We will also incorporate into the Berkeley Software Distribution an
updated ISO Development Environment (ISODE)
featuring International Standard (IS) versions of OSI applications.
ISODE implements the session and presentation layers of the OSI protocol suite,
and includes an implementation of the file transfer protocol (FTAM)
and the OSI Virtual Terminal protocol (VT).
An X.400 implementation now being done at
University College, London and the University of Nottingham
will be available for testing and distribution.  
.LP
This integration work is comprised of four areas.
.IP 1)
We are updating the University of
Wisconsin TP-4 to match GOSIP and COS requirements.
The University of Wisconsin developed a transport class 4
implementation for the 4.2BSD kernel under contract to IBM.
This implementation must be updated to reflect the National Institute
of Standards and Technology (NIST, formerly NBS) workshop agreements,
GOSIP, and COS requirements.
.IP 2)
There is an ongoing collaboration with resarchers at the University
of Wisconsin who are developing an IS-IS routing daemon, which may require
architectural changes at the routing and link level.  We believe we
have provided adequate faciilities but requirements may change with time.
.IP 3)
The required device drivers need to be integrated into a BSD kernel.
4.3BSD has existing device drivers for many Ethernet devices;
these are being upgraded to support 802.3.
Future BSD versions should support X.25 devices and may support token ring
networks.
A major reworking of the architecture is required to support
connection-oriented networks like X.25 (see Protocol Layering below).
With that layering, both kernel (for OSI and IP datagrams)
and user level services (such as TP-0 over
the Connection-Oriented Network Service, CONS,
or traditional X.25 PAD services) can be supported over a variety
of X.25 devices, from intelligent front ends to devices capable only of HDLC
framing.
.IP 4)
The existing OSINET interoperability test network is available so 
that the interoperability of the ISODE and BSD kernel protocols
can be established through tests with several vendors.
Testing is crucial because an openly available version of GOSIP protocols
that does not interoperate with DEC, IBM, SUN, ICL, HIS, and other
major vendors would be useless.
Berkeley has acquired OSINET (in reality ACCUNET) service; further
funding is required to continue our connection until the X.25 implementation
is completed.
.NH 2
Compliance with POSIX 1003
.PP
Berkeley has been involved in the development
of the IEEE POSIX standards since the last draft of the P1003.1
system interface standard.
Since then, we have been parcipating in the working groups
of P1003.2 (shell and application utility interface),
P1003.4 (real-time and other system extensions),
P1003.6 (security), P1003.7 (system administration),
P1003.8 (transparent remote file access) and
P1003.12 (process-to-process network interface).
.PP
The IEEE published the POSIX P1003.1 standard in late 1988.
POSIX-related changes to the BSD system have included a new terminal
driver, support for POSIX sessions and job control, expanded signal
functionality, restructured directory access routines, and new set-user
and set-group id facilities.
We have an implementation of the POSIX driver with extensions
similar to those in the old Berkeley terminal driver.
A compatibility module provides binary compatibility with
applications developed for the old Berkeley terminal driver.
We also have an implementation of the 4.2BSD-based POSIX
job control facility.
.PP
The P1003.2 draft is currently being voted on by the IEEE
P1003.2 balloting group.
Berkeley is particularly interested in the results of this standard,
as it will profoundly influence the user environment.
The other groups are in comparatively early phases, with drafts
coming to ballot sometime in the 90's.
Berkeley will continue to participate in these groups, and
move in the near future toward a P1003.1 and P1003.2 compliant
system.
We have many of the utilities outlined in the current P1003.2 draft
already implemented, and have other parties willing to contribute
additional implementations.
.NH 2
Improvements to the TCP/IP Networking Protocols
.PP
The Internet and the Berkeley collection of local-area networks
have both grown at high rates in the last several years.
The Bay Area Regional Research Network (BARRNet),
connecting several UC campuses, Stanford and NASA-Ames
has recently become operational, increasing the complexity
of the network connectivity.
Both Internet and local routing algorithms are showing the strain
of continued growth.
We have made several changes in the local routing algorithm
to keep accommodating the current topology,
and are participating in the development of new routing algorithms
and standard protocols.
.PP
Work in collaboration with Van Jacobson of the Lawrence Berkeley
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
The current TCP implementation is now available
via the network and as a 
standard Berkeley distribution unencumbered by any commercial licensing.
We are continuing to refine the TCP and IP implementations
using the the NSF network, BARRNet, ARPANET/Milnet,
and local campus nets as testbeds.
In addition, we are incorporating applicable algorithms from this work
into the TP-4 protocol implementation.
.NH 2
Toward a Compatible File System Interface
.PP
The most critical shortcoming of the 4.3BSD UNIX system was in the
area of distributed file systems.
As with networking protocols,
there is no single distributed file system
that provides sufficient speed and functionality for all problems.
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
university groups for internal use,
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
and incompatible with the others.
Each addresses somewhat different design goals,
having been based on a different version of UNIX,
having targeted a different set of file systems with varying characteristics,
and having selected a different set of file system primitive operations.
.PP
Our effort in this area is aimed at providing a common framework to
support these different distributed file systems simultaneously rather than to
simply implement yet another protocol.
This requires a detailed study of the existing protocols, 
and discussion with their implementors to determine whether
they could modify their implementation to fit within our proposed
framework.  We have studied the various file system interfaces to determine
their generality, completeness, robustness, efficiency, and aesthetics.
We have implemeneted a file system interface
that we believe includes the best features of
each of the previous implementations.
This work and the rationale underlying its development
have been presented to major software vendors as an early step
toward convergence on a standard compatible file system interface.
Briefly, the proposal adopts the 4.3BSD calling convention for file
name lookup but otherwise is closely related to Sun's VFS
and DEC's GFS. [Karels86].
.PP
Non-propietary client and server implementations of Sun's NFS protocol have been
produced in this new framework at the University of Guelph.
This NFS version has been tested successfully
at the most recent ``Connectathon,''
and both the filesystem interface and NFS will be included
in the Open Software Foundation's first system release, OSF/1.
.PP
One additional file system has been added under the new interface,
a virtual-memory-resident file system (MFS).
This file system is used for temporary file storage in /tmp,
and greatly speeds ``I/O'' for creation and use of temporary files.
.NH 2
System Security
.PP
The invasion of the DARPA Internet by a quickly reproducing ``worm''
highlighted the need for a thorough review of the access
safeguards built into the system.
Until now, we have taken a passive approach to dealing with
weaknesses in the system access mechanisms, rather than actively
searching for possible weaknesses.
When we are notified of a problem or loophole in a system utility
by one of our users,
we have a well defined procedure for fixing the problem and 
expeditiously disseminating the fix to the BSD mailing list.
This procedure has proven itself to be effective in
solving known problems as they arise
(witness its success in handling the recent worm).
However, we feel that it would be useful to take a more active
role in identifying problems before they are reported (or exploited).
We will make a complete audit of the system
utilities and network servers to find unintended system access mechanisms.
.PP
As a part of the work to make the system more resistant to attack
from local users or via the network, it will be necessary to produce
additional documentation on the configuration and operation of the system.
This documentation will cover such topics as file and directory ownership
and access, network and server configuration,
and control of privileged operations such as file system backups.
.PP
We are investigating the addition of access control lists (ACLs) for
filesystem objects.
ACLs provide a much finer granularity of control over file access permissions
than the current
discretionary access control mechanism (mode bits).
Furthermore, they are necessary
in environments where C2 level security or better, as defined in the DoD
TCSEC [DoD83], is required.
The POSIX P1003.6 security group has made notable progress in determining
how an ACL mechanism should work, and several vendors have implemented
ACLs for their commercial systems.
Berkeley will investigate the existing implementations and determine
how to best integrate ACLs with the existing mechanism.
.PP
A major shortcoming of the present system is that authentication
over the network is based solely on the privileged port mechanism
between trusting hosts and users.
Although privileged ports can only be created by processes running as root
on a UNIX system,
such processes are easy for a workstation user to obtain;
they simply reboot their workstation in single user mode.
Thus, a better authentication mechanism is needed.
At present, we believe that the MIT Kerberos authentication
server [Steiner88] provides the best solution to this problem.
We have integrated the current version of Kerberos into our current system
and are beginning installation in a larger part of the Berkeley Computer
Science Division.
This integration includes the addition of the
authentication mechanism in utilities such as
telnet, login, remote shell, etc., as well as conversion aids.
We plan to add support for telnet (eventually replacing rlogin),
the X window system, and the mail system within an authentication
domain (a Kerberos \fIrealm\fP).
We hope to replace the existing password authentication completely
with the network authentication system.
.NH 2
Virtual Memory
.PP
The 4.3BSD virtual memory system is a direct descendent of the original
Berkeley virtual memory system for 3BSD, released in 1979.
The state of the art in virtual memory hardware and software has
changed tremendously in that time.
There are several problems with the current system:
.IP 1.
Other than shared code regions, shared memory is not supported.
.IP 2.
Memory-mapped files and devices are not supported.
.IP 3.
Backing store is fully allocated for each process section,
preventing support of sparse address spaces and requiring excessive
backing store for large-memory systems.
.IP 4.
The implementation is tightly coupled to the VAX page table format.
.LP
Most of these problems have been addressed in the virtual memory system
in the Mach system from Carnegie-Mellon University.
That virtual memory system has been ported to 4.3BSD at the University
of Utah, and work has begun to convert to the BSD
.I mmap\^ ()
interface for mapped files and shared memory.
That virtual memory system is expected to be the basis for the 4.4BSD
system, and its integration and development at Berkeley will begin
in the next few months.
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
This architecture was substantially modified by the time of the 
production implementation of \s-1STREAMS\s0 in System V Release 3.
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
We intend to combine the best features of the original Bell Labs
version of streams with the network-level structure used in 4.2BSD.
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
.NH 2
Other projects
.PP
There are numerous other projects in progress within CSRG and in other
groups that are targetted for inclusion in 4.4BSD.
.PP
In addition to the system interface changes made for POSIX compliance,
the system call interface is being updated in several other areas.
Minor extensions to the socket interface facilitate use of OSI-specific
features.
Other calls are being consolidated and updated.
Full compatibility with the older interfaces will be retained for one
release cycle.
.PP
Support for the Hewlett-Packard 9000/300 series of workstations
and servers has been added by the University of Utah.
Another group has done a port to AT-style i386 systems
and is adding support for various peripherals.
Ports to the line of servers from MIPS
and to the MIPS-based DEC workstations are in progress as well.
.NH
References
.sp
.IP Brownbridge82
Brownbridge, D.R., L.F. Marshall, B. Randell,
``The Newcastle Connection, or UNIXes of the World Unite!,''
\fISoftware\- Practice and Experience\fP, Vol. 12, pp. 1147-1162, 1982.
.sp
.IP Cole85
.br
Cole, C.T., P.B. Flinn, A.B. Atlas,
``An Implementation of an Extended File System for UNIX,''
\fIUsenix Conference Proceedings\fP,
pp. 131-150, June, 1985.
.sp
.IP DoD83
.br
Department of Defense,
``Trusted Computer System Evaluation Criteria,''
\fICSC-STD-001-83\fP,
DoD Computer Security Center, August, 1983.
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
.IP McKusick84
McKusick, M.K., W. Joy, S. Leffler, R. Fabry,
``A Fast File System for UNIX'',
\fIACM Transactions on Computer Systems 2\fP, 3.
pp 181-197, August 1984.
.sp
.IP McKusick85
McKusick, M.K., M. Karels, S. Leffler,
``Performance Improvements and Functional Enhancements in 4.3BSD,''
\fIUsenix Conference Proceedings\fP, pp. 519-531, June, 1985.
.sp
.IP McKusick86
McKusick, M.K., M. Karels,
``A New Virtual Memory Implementation for Berkeley UNIX,''
\fIProceedings of the European UNIX Users Group Meeting\fP,
Manchester, England, pp. 451-460, September 1986.
.sp
.IP McKusick88
McKusick, M.K., M. Karels,
``Design of a General Purpose Memory Allocator for the 4.3BSD UNIX Kernel,''
\fIUsenix Conference Proceedings\fP,
pp. 295-303, June, 1988.
.sp
.IP Rifkin86
Rifkin, A.P., M.P. Forbes, R.L. Hamilton, M. Sabrio, S. Shah, K. Yueh,
``RFS Architectural Overview,'' \fIUsenix Conference Proceedings\fP,
pp. 248-259, June, 1986.
.sp
.IP Ritchie84
Ritchie, D.M., ``A Stream Input-Output System,''
\fIAT&T Bell Laboratories Technical Journal\fP,
pp. 1897-1910, October, 1984.
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
.IP Steiner88
Steiner, J., C. Newman, J. Schiller,
``\fIKerberos:\fP An Authentication Service for Open Network Systems,''
\fIUsenix Conference Proceedings\fP, pp. 191-202, February, 1988.
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
