.\" Copyright (c) 1983, 1986 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" Redistribution and use in source and binary forms are permitted
.\" provided that the above copyright notice and this paragraph are
.\" duplicated in all such forms and that any documentation,
.\" advertising materials, and other materials related to such
.\" distribution and use acknowledge that the software was developed
.\" by the University of California, Berkeley.  The name of the
.\" University may not be used to endorse or promote products derived
.\" from this software without specific prior written permission.
.\" THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
.\" IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
.\" WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
.\"
.\"	@(#)3.t	6.4 (Berkeley) 3/7/89
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
