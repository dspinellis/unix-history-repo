.\" Copyright (c) 1983,1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)3.t	6.3 (Berkeley) 6/5/86
.\"
.nr H2 1
.\".ds RH Goals
.br
.ne 2i
.NH
\s+2Goals\s0
.PP
The networking system was designed with the goal of supporting
multiple \fIprotocol families\fP and addressing styles.  This required
information to be ``hidden'' in common data structures which
could be manipulated by all the pieces of the system, but which
required interpretation only by the protocols which ``controlled''
it.  The system described here attempts to minimize
the use of shared data structures to those kept by a suite of
protocols (a \fIprotocol family\fP), and those used for rendezvous
between ``synchronous'' and ``asynchronous'' portions of the
system (e.g. queues of data packets are filled at interrupt
time and emptied based on user requests).
.PP
A major goal of the system was to provide a framework within
which new protocols and hardware could be easily be supported.
To this end, a great deal of effort has been extended to
create utility routines which hide many of the more
complex and/or hardware dependent chores of networking.
Later sections describe the utility routines and the underlying
data structures they manipulate.
