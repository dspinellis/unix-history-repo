.\"	@(#)NBSproposal.t	1.2	88/02/26
.\" *troff -ms
.nr SP 6
.rm CM
.ds DT "
.ll 6i
.sp 3
.ps 12
.vs 14
.ft B
Project Title: Berkeley UNIX
.ps 10
.vs 12
.sp
Principal Investigator:
.in +0.5i
.ta +3.5i
.nf
.ft R
Domenico Ferrari
.in +0.6i
Computer Science Division, EECS Department
University of California, Berkeley, CA 94720
.in -0.6i
ferrari@berkeley.edu
(415)642-3806
.sp
.in -0.5i
.ft B
Summary:
.fi
.sp \*(SPp
.ft R
The goal of the Computer Systems Research Group at Berkeley
is to apply leading edge research ideas into a stable
and reliable implementation that solves current problems in
operating systems research.
Since the release of 4.3BSD in mid 1986,
we have been working on four major new areas of research.
.sp \*(SPp
1)  Develop an OSI network protocol suite and to integrate
existing ISO applications into Berkeley UNIX.
The network architecture of 4.2BSD was designed to accommodate
multiple network protocol families and address formats,
and an implementation of the ISO OSI network protocols
should fit this framework without much difficulty.
The outline of the proposal is to
implement the OSI connectionless internet protocol (CLNP),
and device drivers for X.25, 802.3, and possibly 802.5 interfaces, and
to integrate these with an OSI transport class 4 (TP-4) implementation.
We will receive an updated ISO Development Environment (ISODE)
and incorporate these into the Berkeley Software Distribution.
ISODE implements the session and presentation layers of the OSI protocol suite,
and will include an implementation of the file transfer protocol FTAM.
If possible, an X.400 implementation now being done at University College,
London and the University of Nottingham will also be available for testing
and distribution.
This work will include participation in
interoperability tests with vendors and users on OSINET.
.sp \*(SPp
2)  Bring the Berkeley UNIX kernel into compliance
with the P1003.1 POSIX interface recently approved by the IEEE.
This work includes the development of a completely new terminal driver.
The new terminal driver must have a binary compatibility interface
to allow a transition path for programs using the old Berkeley
terminal driver.
A POSIX session and job control implementation must be developed.
Those system utilities that create sessions and manipulate jobs must
be converted to use the new facilities.
Numerous other smaller POSIX related changes must be made
including expanded signal functionality,
restructured directory access routines,
and new set-user-identifier functionality.
.sp \*(SPp
3)  Refine the TCP/IP networking to improve
its performance and limit congestion on slow and/or lossy networks.
The version of TCP with the new ``slow-start,'' round-trip time estimation,
and congestion control
algorithms has been tested extensively, and is included in the current
Berkeley ``Tahoe'' test release.
It has been adopted by many vendors of 4.2BSD- and 4.3BSD-derived
networking software.
Additional performance experiments have been done by
Van Jacobson of the Lawrence Berkeley Laboratory,
and several additional performance improvements are ready
to be merged into the Berkeley TCP.
.sp \*(SPp
4)  Provide a standard interface to file systems
so that multiple local and remote file systems can be supported,
much as multiple networking protocols are supported by 4.3BSD.
The most critical shortcoming of our current UNIX system is in the
area of distributed file systems.
As with networking protocols,
there is no single distributed file system
that provides enough speed and functionality for all problems.
It is frequently necessary to support several different remote
file system protocols, just as it is necessary to run several 
different network protocols.
Our work is aimed at providing a common framework to
support these different distributed file systems simultaneously rather than to
simply implement yet another protocol.
Briefly, the proposal adopts the 4.3BSD calling convention for file name lookup
but otherwise is closely related to Sun's VFS.
A prototype implementation is now being developed.
