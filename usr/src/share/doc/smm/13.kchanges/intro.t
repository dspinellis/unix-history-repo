.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)intro.t	1.11 (Berkeley) 4/16/86
.\"
.EH 'SMM:13-%''Changes to the Kernel in 4.3BSD'
.OH 'Changes to the Kernel in 4.3BSD''SMM:13-%'
.TL
Changes to the Kernel in 4.3BSD
.sp
.de D?
.ie \\n(.$>1 \\$1 \\$2 \\$3
.el DRAFT of \n(mo/\n(dy/\n(yr
..
.D? April 16, 1986
.AU
Michael J. Karels
.AI
Computer Systems Research Group
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California 94720
.PP
This document summarizes the changes to the kernel between the
September 1983 4.2BSD distribution of
.UX
for the VAX\(dd
.FS
\(dd \s-2DEC\s0, \s-2VAX\s0, \s-2PDP\s0, \s-2MASSBUS\s0, \s-2UNIBUS\s0,
\s-2Q-bus\s0 and \s-2ULTRIX\s0
are trademarks of Digital Equipment Corporation.
.FE
and the March 1986 4.3BSD release.
It is intended to provide sufficient information
that those who maintain the kernel,
have local modifications to install,
or who have versions of 4.2BSD modified to run on other hardware
should be able to determine how to integrate this version
of the system into their environment.
As always, the source code is the final source of information,
and this document is intended primarily to point out those areas
that have changed.
.LP
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
the use of caching to decrease the overhead of filesystem name translation,
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
.PP
This document is not intended to be an introduction to the kernel,
but assumes familiarity with prior versions of the kernel.
Other documents may be consulted for more complete discussions of the kernel
and its other subsystems.
For more complete information on the internal structure
and interfaces of the network subsystem,
refer to ``4.3BSD Networking Implementation Notes.''
.PP
The author gratefully acknowledges the contributions
of the other members of the Computer Systems Research Group
at Berkeley and the other contributors to the work described
here.
Major contributors include Kirk McKusick, Sam Leffler, Jim Bloom,
Keith Sklower, Robert Elz, and Jay Lepreau.
Sam Leffler and Anne Hughes made numerous suggestions and corrections
during the preparation of the manuscript.
