.RP
.EF '\fBC Language X Interface\fP''\fBX Version 10\fP'
.OF '\fBC Language X Interface\fP''\fBX Version 10\fP'
.EH ''\fB- % -\fP''
.OH ''\fB- % -\fP''
.DA "November 16, 1986"
.nr PS 11
.ps +1
.TA .5i 3i
.ta .5i 3i
.TL
Xlib - C Language X Interface
.br
Protocol Version 10
.AU
Jim Gettys
.AI
Digital Equipment Corporation
MIT Project Athena
.AU
Ron Newman
.AI
Massachusetts Institute of Technology
MIT Project Athena
.AU
Tony Della Fera
.AI
Digital Equipment Corporation
MIT Project Athena
.AB
.PP
X is a network transparent windowing system developed at MIT which
runs under 4.3BSD
UNIX\(dg
and Ultrix-32\(dd Version 1.2.
.PP
X display servers run on computers with either monochrome or color
bitmap terminals.
The server
distributes user input to, and accepts output requests from various
client programs located either on the same machine or elsewhere in 
your network.
Xlib is a C subroutine library that application programs (`clients')
use to interface with the window system via a stream connection.
While
a client normally runs on the same machine as the X server it is talking
to, this need not be the case.
.PP
This document describes the low level C language interface to the X
window system protocol.
It is expected and intended that much higher level libraries will be developed
for more convenient programming of the window system.
.AE
.FS
.br
.sp
\(dg UNIX is a trademark of AT&T Bell Laboratories.
.br
.sp
\(dd Ultrix, Ultrix-32, Ultrix-32m, and Ultrix-32w are trademarks
of Digital Equipment Corporation.
.br
Copyright \(co 1985, 1986, Massachusetts Institute of Technology.
.br
Permission to use, copy, modify and distribute this documentation for
any purpose and without fee is hereby granted, provided that the above
copyright notice appears in all copies and that both that copyright notice and
this permission notice appear in supporting documentation, and that the name of
M.I.T. not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  
M.I.T makes no
representations about the suitability of the software described herein for
any purpose.
It is provided "as is" without express or implied warranty.
.br
This software is not subject to any license of the American Telephone and
Telegraph Company or of the Regents of the University of California.
.FE
