.\" Copyright (c) 1983 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)0.t	6.4 (Berkeley) 5/14/86
.\"
.if n .ND
.TL
4.3BSD Line Printer Spooler Manual
.EH 'SMM:6-%''4.3BSD Line Printer Spooler Manual'
.OH '4.3BSD Line Printer Spooler Manual''SMM:6-%'
.AU
Ralph Campbell
.AI
Computer Systems Research Group
Computer Science Division
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, CA  94720
.AB
.FS
* UNIX is a trademark of Bell Laboratories.
.FE
This document describes the structure and installation procedure
for the line printer spooling system
developed for the 4.3BSD version
of the UNIX* operating system.
.de D?
.ie \\n(.$>1 Revised \\$1 \\$2 \\$3
.el DRAFT of \n(mo/\n(dy/\n(yr
..
.sp 2
.LP
.D? May 14, 1986
.AE
.de IR
\fI\\$1\fP\\$2
..
.de DT
.TA 8 16 24 32 40 48 56 64 72 80
..
