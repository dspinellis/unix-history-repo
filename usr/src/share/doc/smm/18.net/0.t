.\" Copyright (c) 1983 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)0.t	5.1 (Berkeley) %G%
.\"
.if n .ND
.TL
4.2BSD Networking Implementation Notes
.sp
Revised July, 1983
.AU
Samuel J. Leffler, William N. Joy, Robert S. Fabry
.AI
Computer Systems Research Group
Computer Science Division
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, CA  94720

(415) 642-7780
.AB
.FS
* UNIX is a trademark of Bell Laboratories.
.FE
This report describes the internal structure of the
networking facilities developed for the 4.2BSD version
of the UNIX* operating system
for the VAX\(dg.  These facilities
.FS
\(dg DEC, VAX, DECnet, and UNIBUS are trademarks of
Digital Equipment Corporation.
.FE
are based on several central abstractions which
structure the external (user) view of network communication
as well as the internal (system) implementation.
.PP
The report documents the internal structure of the
networking system.  The
``4.2BSD System Manual'' provides a
description of the user interface to the networking
facilities.
.AE
.LP
.de PT
.lt \\n(LLu
.pc %
.nr PN \\n%
.tl '\\*(LH'\\*(CH'\\*(RH'
.lt \\n(.lu
..
.af PN i
.ds LH Networking Implementation
.ds RH Contents
.bp 1
.if t .ds LF CSRG TR/6
.if t .ds RF Leffler, et. al.
.ce
.B "TABLE OF CONTENTS"
.LP
.sp 1
.nf
.B "1.  Introduction"
.LP
.sp .5v
.nf
.B "2.  Overview"
.LP
.sp .5v
.nf
.B "3.  Goals
.LP
.sp .5v
.nf
.B "4.  Internal address representation"
.LP
.sp .5v
.nf
.B "5.  Memory management"
.LP
.sp .5v
.nf
.B "6.  Internal layering
\0.1.    Socket layer
\0.1.1.    Socket state
\0.1.2.    Socket data queues
\0.1.3.    Socket connection queueing
\0.2.    Protocol layer(s)
\0.3.    Network-interface layer
\0.3.1.    UNIBUS interfaces
.LP
.sp .5v
.nf
.B "7.  Socket/protocol interface"
.LP
.sp .5v
.nf
.B "8.  Protocol/protocol interface"
\0.1.     pr_output
\0.2.     pr_input
\0.3.     pr_ctlinput
\0.4.     pr_ctloutput
.LP
.sp .5v
.nf
.B "9.  Protocol/network-interface interface"
\0.1.     Packet transmission
\0.2.     Packet reception
.LP
.sp .5v
.nf
.B "10. Gateways and routing issues
\0.1.     Routing tables
\0.2.     Routing table interface
\0.3.     User level routing policies
.LP
.sp .5v
.nf
.B "11. Raw sockets"
\0.1.     Control blocks
\0.2.     Input processing
\0.3.     Output processing
.LP
.sp .5v
.nf
.B "12. Buffering and congestion control"
\0.1.     Memory management
\0.2.     Protocol buffering policies
\0.3.     Queue limiting
\0.4.     Packet forwarding
.LP
.sp .5v
.nf
.B "13. Out of band data"
.LP
.sp .5v
.nf
.B "14. Trailer protocols"
.LP
.sp .5v
.nf
.B Acknowledgements
.LP
.sp .5v
.nf
.B References
.ds RH Introduction
.af PN 1
.bp 1
.de _d
.if t .ta .6i 2.1i 2.6i
.\" 2.94 went to 2.6, 3.64 to 3.30
.if n .ta .84i 2.6i 3.30i
..
.de _f
.if t .ta .5i 1.25i 2.5i
.\" 3.5i went to 3.8i
.if n .ta .7i 1.75i 3.8i
..
