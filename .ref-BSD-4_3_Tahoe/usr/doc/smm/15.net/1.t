.\" Copyright (c) 1983,1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)1.t	6.3 (Berkeley) 6/5/86
.\"
.\".ds RH Introduction
.br
.ne 2i
.NH
\s+2Introduction\s0
.PP
This report describes the internal structure of
facilities added to the
4.2BSD version of the UNIX operating system for
the VAX,
as modified in the 4.3BSD release.
The system facilities provide
a uniform user interface to networking
within UNIX.  In addition, the implementation 
introduces a structure for network communications which may be
used by system implementors in adding new networking
facilities.  The internal structure is not visible
to the user, rather it is intended to aid implementors
of communication protocols and network services by
providing a framework which
promotes code sharing and minimizes implementation effort.
.PP
The reader is expected to be familiar with the C programming
language and system interface, as described in the
\fIBerkeley Software Architecture Manual, 4.3BSD Edition\fP [Joy86].
Basic understanding of network
communication concepts is assumed; where required
any additional ideas are introduced.
.PP
The remainder of this document
provides a description of the system internals,
avoiding, when possible, those portions which are utilized only
by the interprocess communication facilities.
