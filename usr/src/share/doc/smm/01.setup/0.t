.\" Copyright (c) 1988, 1993 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)0.t	6.1 (Berkeley) %G%
.\"
.ds Ux \s-1UNIX\s0
.ds Bs \s-1BSD\s0
.\" Current version:
.ds 4B 4.4\*(Bs
.ds Ps 4.3\*(Bs
.\" tape and disk naming
.ds Mt mt
.ds Dk sd
.ds Dn disk
.ds Pa c
.\" document date
.ds Dy 
.de Sm
\s-1\\$1\s0\\$2
..
.bd S B 3
.EH 'SMM:1-%''Installing and Operating \*(4B \*(Ux'
.OH 'Installing and Operating \*(4B \*(Ux''SMM:1-%'
.TL
Installing and Operating \*(4B UNIX
.br
\*(Dy
.AU
Marshall Kirk McKusick
.AU
Keith Bostic
.AU
Mike Hibler
.AU
Michael J. Karels
.AU
Samuel J. Leffler
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
*\*(Ux is a register trademark of USL in the USA and some other countries.
.FE
This document contains instructions for the
installation and operation of the
\*(4B release of
.UX
as distributed by The University of California at Berkeley.
.PP
It discusses procedures for installing \*(Ux on a new machine,
and for upgrading an existing \*(Ps \*(Ux system to the new release.
An explanation of how to lay out file systems on available disks,
how to set up terminal lines and user accounts,
and how to do system-specific tailoring is provided.
A description of how to install and configure the networking
facilities included with \*(4B is included.
Finally, the document details system operation procedures:
shutdown and startup, file system backup procedures,
resource control, performance monitoring, and procedures for recompiling
and reinstalling system software.
.AE
