.TL
Changes to the Kernel in 4.2BSD
.sp
July 25, 1983
.AU
Samuel J. Leffler
.AI
Computer Systems Research Group
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California 94720
(415) 642-7780
.PP
This document summarizes the changes to the kernel
between the September 1981 4.1BSD release and the
July 1983 4.2BSD distribution.  The information is presented
in both overall terms (e.g. organizational changes), and as
specific comments about individual files.  See
the source code itself for more details.
.PP
The system has undergone too many changes to detail everything.
Instead the major areas of change will pointed out, followed by
a brief description of the contents of files present in the
4.1BSD release.  Where important changes and/or bug fixes were
applied they are described.  The networking support is not discussed
in this document, refer to ``4.2BSD Networking Implementation
Notes'' for a discussion of the internal structure of the network
facilities.
.PP
Major changes include:
.IP \(bu 3
organizational changes to isolate VAX specific portions
of the system
.IP \(bu 3
changes to support the new file system organization
.IP \(bu 3
changes to support the new interprocess communication
facilities
.IP \(bu 3
changes for the new networking support; in particular,
the DARPA standard Internet protocols TCP, UDP, IP, and ICMP,
and the \fInetwork interface drivers\fP which provide
hardware support
.IP \(bu 3
changes for the new signal facilities
.IP \(bu 3
changes for the new time and interval timer facilities
.IP \(bu 3
changes to eliminate references to global variables;
in particular, the global
variables \fIu.u_base\fP, \fIu.u_offset\fP, \fIu.u_segflg\fP,
and \fIu.u_count\fP have been almost completely replaced
by \fIuio\fP structures which are passed by reference;
the \fIu.u_error\fP variable
has not been completely purged from low level portions of the
system, but is in many places now returned as a function
value;  the \fIuio\fP changes were necessitated by the
new scatter-gather i/o facilities
.IP \(bu 3
changes for the new disk quota facilities
.IP \(bu 3
changes for more flexible configuration of the disk
space used for paging and swapping
