.\" Copyright (c) 1980, 1986, 1988 Regents of the University of California.
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
.\"	@(#)0.t	6.4 (Berkeley) 3/7/89
.\"
.EH 'SMM:1-%''Installing and Operating 4.3BSD-tahoe UNIX on the VAX'
.OH 'Installing and Operating 4.3BSD-tahoe UNIX on the VAX''SMM:1-%'
.ds 4B 4.3BSD-tahoe
.nr Vx 1		\" VAX version
.ds Mc VAX
.ds mC vax
.ds Dk hp
.ds Dn RM80
.ds Pa g
.ds Ps 4.3BSD
.bd S B 3
.TL
Installing and Operating \*(4B UNIX* on the VAX\(dg
.br
July 14, 1988
.AU
Michael J. Karels
.AU
Chris Torek
.AU
James M. Bloom
.AU
Marshall Kirk McKusick
.AU
Samuel J. Leffler
.AU
William N. Joy
.AI
Computer Systems Research Group
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California  94720
(415) 642-7780
.de IR
\\fI\\$1\|\\fP\\$2
..
.de UX
UNIX\\$1
..
.AB
.PP
.FS
*\s-2UNIX\s0 is a register trademark of AT&T in the USA and other countries.
.FE
.FS
\(dgDEC, VAX, IDC, SBI, UNIBUS and MASSBUS are trademarks of
Digital Equipment Corporation.
.FE
This document contains instructions for the
installation and operation of the
\*(4B release of the VAX
.UX
system, as distributed by The University of California at Berkeley.
.PP
It discusses procedures for installing UNIX on a new VAX,
and for upgrading an existing 4.2BSD or 4.3BSD
VAX UNIX system to the new release.
An explanation of how to lay out file systems on available disks,
how to set up terminal lines and user accounts,
and how to do system-specific tailoring is provided.
A description of how to install and configure the networking
facilities included with \*(4B is included.
Finally, the document details system operation procedures:
shutdown and startup,
hardware error reporting and diagnosis, file system backup procedures,
resource control, performance monitoring, and procedures for recompiling
and reinstalling system software.
.AE
