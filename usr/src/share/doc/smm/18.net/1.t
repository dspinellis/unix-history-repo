.\" Copyright (c) 1983 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)1.t	6.1 (Berkeley) %G%
.\"
.ds RH Introduction
.NH
\s+2Introduction\s0
.PP
This report describes the internal structure of
facilities added to the
4.2BSD version of the UNIX operating system for
the VAX.  The system facilities provide
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
language and system interface, as described in the \fI4.2BSD
System Manual\fP [Joy82a].  Basic understanding of network
communication concepts is assumed; where required
any additional ideas are introduced.
.PP
The remainder of this document
provides a description of the system internals,
avoiding, when possible, those portions which are utilized only
by the interprocess communication facilities.
.ds RH Overview
.bp
