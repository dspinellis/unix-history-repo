.\" Copyright (c) 1983 The Regents of the University of California.
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
.\"	@(#)0.t	6.5 (Berkeley) %G%
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
.D? 
.AE
.de IR
\fI\\$1\fP\\$2
..
.de DT
.TA 8 16 24 32 40 48 56 64 72 80
..
