.\" Copyright (c) 1988 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)0.t	1.6 (Berkeley) %G%
.\"
.nr Th 1		\" Tahoe version
.ds Th Tahoe
.ds Ux \s-1UNIX\s0
.ds Bs \s-1BSD\s0
.\" Current version:
.ds 4B 4.3\*(Bs-tahoe
.\" machine:
.ds Mc \*(Th
.ds mC tahoe
.ds Dk dk
.ds Dn Eagle
.ds Pa c
.ds Ps 4.3\*(Bs-beta
.ds Vs \s-1VERSA\s0bus
.ds Vm "\s-1VME\s0 bus
.de Sm
\s-1\\$1\s0\\$2
..
.bd S B 3
.EH 'SMM:1-%''Installing and Operating \*(4B \*(Ux on the \*(Th'
.OH 'Installing and Operating \*(4B \*(Ux on the \*(Th''SMM:1-%'
.TL
Installing and Operating \*(4B \*(Ux* on the \*(Th
.br
July 14, 1988
.AU
Samuel J. Leffler
.AU
Keith Bostic
.AU
Michael J. Karels
.AU
Marshall Kirk McKusick
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
\*(Ux\\$1
..
.AB
.PP
.FS
*\*(Ux is a register trademark of AT&T in the USA and other countries.
.FE
This document contains instructions for the
installation and operation of the
\*(4B release of
.UX
as distributed by The University of California at Berkeley
for the \*(Th (CCI Power 6/32 and similar machines).
.PP
It discusses procedures for installing \*(Ux on a new machine,
and for upgrading an existing 4.2\*(Bs \*(Th \*(Ux system to the new release.
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
