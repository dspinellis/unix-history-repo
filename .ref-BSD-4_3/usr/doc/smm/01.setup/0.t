.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)0.t	6.1 (Berkeley) 5/13/86
.\"
.EH 'SMM:1-%''Installing and Operating 4.3BSD on the VAX'
.OH 'Installing and Operating 4.3BSD on the VAX''SMM:1-%'
.ds 4B 4.3BSD
.bd S B 3
.TL
Installing and Operating \*(4B on the VAX
.br
April 1, 1986
.AU
Michael J. Karels
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
* DEC, VAX, IDC, SBI, UNIBUS and MASSBUS are trademarks of
Digital Equipment Corporation.
.FE
.FS
** \s-2UNIX\s0 is a Trademark of Bell Laboratories.
.FE
This document contains instructions for the
installation and operation of the
\*(4B release of the VAX*
.UX **
system, as distributed by The University of California at Berkeley.
.PP
It discusses procedures for installing UNIX on a new VAX,
and for upgrading an existing 4.2BSD VAX UNIX system to the new release.
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
