.\"     @(#)H.t	1.1     89/02/23
.LP
\fB\s+4H. Technical Rationale of the Proposed Research\fP\s-4
.PP
The following sections provide
an expanded description of each of these projects
described in the previous section.
.sp
.LP
\fB\s+2H.1 OSI Network Protocol Development\fP\s-2
.PP
The network architecture of 4.2BSD was designed to accommodate
multiple network protocol families and address formats.
The current prototype implementation of the ISO OSI network protocols
makes use of all the existing facilities, but will be aided by
some architectural changes.
.PP
The architectural changes that have already been completed include
improved network buffer layouts,
changes to allow variable length network addresses,
and the construction of a uniform
hierarchical routing lookup algorithm.
We have made alterations to the socket interface to allow
transmission of record boundaries,
distinction between dequeuing connection requests and explicit
confirmation, and passage of ancillary data such as user connection-
or disconnection-request data.
.PP
At the beginning of the period covered by this proposal,
we expect to have initial versions of
the OSI connectionless internet protocol (CLNP),
an OSI transport class 4 (TP-4) implementation,
device drivers for 802.2/802.3,
and the End System to Intermediate System (ES-IS) protocol.
We will add support for X.25 interfaces using CLNP
and will then develop a software layer allowing connection-oriented access
to X.25 as well as packet-switching service.
We will investigate a more general framework for 802.2-based link-level
interfaces with more graceful support of ES-IS and related protocols.
If hardware is available, we will incorporate support for 802.5 and/or
ProNET-80 token rings.
We will receive an updated ISO Development Environment (ISODE)
and incorporate this into the Berkeley Software Distribution.
ISODE implements the session and presentation layers of the OSI protocol suite,
and will include an implementation of the file transfer protocol FTAM.
If possible, an X.400 implementation now being done at University College,
London and the University of Nottingham will also be available for testing
and distribution.
This work will include participation in
interoperability tests with vendors and users on OSINET.
.PP
We are designing modifications to the current network architecture
(analogous to the ``Streams''
concept of Bell Laboratories 9th Edition UNIX or System V.3)
to improve communication between protocol layers,
to allow more flexible layering,
to incorporate terminal and asynchronous serial I/O modules into the same
processing framework,
and to provide a more natural framework for
devices already providing virtual circuit services
(such as X.25 devices).
Such layering will provide a natural setting
for kernel-resident virtual terminal services,
and will allow direct access to communications-layer devices
for network monitoring or prototyping of new protocols.
This structure will also make it easier to tunnel one protocol through another.
Additional work may be necessary for IP
(in particular for the address resolution protocol, ARP) to allow such
revisions.
.PP
We have become members of the IEEE 1003.8 group working on a POSIX networking
interface.
We intend to introduce a proposal for a high-level protocol-independent
interface for network services suitable for distributed applications.
This interface will be based on the UNI interface proposed by Marshall Rose
[Rose88].
.sp
.LP
\fB\s+2H.2 Compliance with POSIX 1003\fP\s-2
.PP
Bringing the Berkeley UNIX kernel into compliance
with the P1003.1 POSIX interface recently approved by the IEEE
requires two major projects and many smaller ones.
The first major project is the development
of a completely new terminal driver.
The new terminal driver must have a binary-compatibility interface
to allow a transition path for programs using the old Berkeley
terminal driver.
The other major project is the development of a
POSIX session and job control implementation.
Those system utilities that create sessions and manipulate jobs must
be converted to use the new facilities.
These two projects are nearly finished, and we are completing
the conversion of the standard utility programs that are affected
by this change.
These facilities will be tested and made available to other groups
during the period covered by this proposal.
.PP
The smaller POSIX related changes that must be made
include expanded signal functionality,
restructured directory access routines,
and new set-user-identifier functionality.
Many of these interfaces will be developed as a superset
of both the POSIX and existing BSD facilities.
This work will involve coordination with other groups
intending to support both POSIX and BSD interfaces.
.PP
We intend to remain involved with the IEEE P1003.1 committee
that will work on corrections and extensions to the existing standard.
We will also continue to work actively with the 1003.2 committee
developing a shell and utilities POSIX standard.
.sp
.LP
\fB\s+2H.3 Improvements to the Networking Architecture and Protocols\fP\s-2
.PP
The networking architecture introduced in 4.2BSD
provided a framework for multiple networking protocols
using a single consistent user interface.
This framework includes three distinct layers: socket,
transport and network protocols, and network link layer.
This framework has been used for implementations of TCP/IP, Xerox NS,
the OSI protocols, and local interprocess communication.
Our experience with this framework and in performance analysis
of the existing implementations suggest some refinements to the framework
and its implementation.
One refinement is the use of structuring techniques similar to those
of the Stream I/O system, described in section 2.1,
unifying two existing interfaces.
In addition to providing additional flexibility, careful structuring
will allow improved pipelining of the network protocols using upcalls
[Clark85].
Such pipelining has been prototyped by Van Jacobson of the Lawrence
Berkeley Laboratory, along with several caching strategies
that will also be incorporated.
With this work, it will be necessary to improve
communication among the levels of the system to improve performance.
In particular, the transport protocols must influence the way in which
data are buffered by higher layers for optimal performance.
We will investigate layering techniques that are both modular
and efficient, two conflicting goals.
At the same time, we will modify the current buffer management
to be more portable and to allow efficient use on machines with different
I/O architectures.
.PP
The communication mechanisms between the layers must also be extended
for robustness of the network: the transport level must be able to notify
the network and link levels of failures detected by timeouts;
the link level must be able to inform the network level of failures
that it detects, and the routing layer needs to use all this information.
.PP
The Internet and the Berkeley collection of local-area networks
have both grown at high rates in the last year.
The Bay Area Regional Research Network (BARRNet)
connects several UC campuses, Stanford and NASA-Ames with the NSF network,
increasing the complexity
of the network connectivity.
Both Internet and local routing algorithms are showing the strain
of continued growth.
We have made several changes in the local routing algorithm
to keep up with the current topology,
and are participating in the development of new routing algorithms
and protocols.
In the period covered by this proposal,
we expect to merge the current version of the Berkeley routing daemon
\fIrouted\fP with the multi-protocol daemon \fIgated\fP (which currently
supports the RIP and EGP protocols), and to update the RIP implementation
for full conformance with the recent specification [Hedrick88].
We also expect to collaborate on an implementation of a new EGP protocol,
version 3 [Lepp88]
in a \fIgated\fP framework,
and may also incorporate or collaborate on an implementation
of the SPF-based IGP protocol being developed within the Internet Engineering
Task Force.
We are continuing our involvement with that group in the evolution of standards
for the DARPA Internet.
.PP
We will review the VMTP and IP multicast support
done by David Cheriton and Steve Deering at Stanford,
evaluate the necessary changes to the system layering,
and then incorporate these modifications into the system
if the changes are found to be compatible with our goals.
.sp
.LP
\fB\s+2H.4 Toward a Compatible File System Interface\fP\s-2
.PP
The most critical shortcoming of our current UNIX system is in the
area of remote file access.
As with networking protocols,
there is no single remote file system
that provides enough speed and functionality for all problems.
It is frequently necessary to support several different remote
file system protocols, just as it is necessary to run several 
different network protocols.
The proposal currently under development
adopts the 4.3BSD calling convention for file name lookup
and allows stateful file systems,
but otherwise is closely related to Sun's virtual file system interface.
This interface has been advanced (to Sun, DEC, AT&T and others)
as a potential standard;
when the implementation is complete, we intend to explore
consensus in this area, so that file system implementations could
be as portable as device drivers.
We do not intend to develop our own remote file system protocol;
instead we will support the full semantics of existing file systems,
including the local UNIX file system
and Sun's network file system (NFS).
We are currently negotiating with several groups that have
developed their own implementations of NFS to convince them to
contribute their code to Berkeley;
we will not include NFS support unless it may be distributed with
no new licensing requirement.
We currently expect receipt of an NFS implementation from Apollo
that uses a different system interface; if feasible, we will adapt it
to our file system interface. 
.bp
