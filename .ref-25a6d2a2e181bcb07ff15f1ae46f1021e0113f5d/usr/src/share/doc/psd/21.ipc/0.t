.\" Copyright (c) 1986 The Regents of the University of California.
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
.\"	@(#)0.t	1.4 (Berkeley) %G%
.\"
.EH 'PS1:8-%''Advanced 4.3BSD IPC Tutorial'
.OH 'Advanced 4.3BSD IPC Tutorial''PS1:8-%'
.ds lq ``
.ds rq ''
.de DT
.if t .ta .5i 1.25i 2.5i 3.75i
.\" 3.5i went to 3.8i
.if n .ta .7i 1.75i 3.8i 
..
.bd S B 3
.TL
An Advanced 4.3BSD Interprocess Communication Tutorial
.AU
Samuel J. Leffler
.AU
Robert S. Fabry
.AU
William N. Joy
.AU
Phil Lapsley
.AI
Computer Systems Research Group
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California  94720
.sp 2
.AU
Steve Miller
.AU
Chris Torek
.AI
Heterogeneous Systems Laboratory
Department of Computer Science
University of Maryland, College Park
College Park, Maryland 20742
.de IR
\fI\\$1\fP\\$2
..
.de UX
UNIX\\$1
..
.AB
.PP
.FS
* \s-2UNIX\s0 is a Trademark of Bell Laboratories.
.FE
This document provides an introduction to the interprocess
communication facilities included in the
4.3BSD release of the
.UX *
system.
.PP
It discusses the overall model for interprocess communication
and introduces the interprocess communication primitives
which have been added to the system.  The majority of the
document considers the use of these primitives in developing
applications.  The reader is expected to be familiar with
the C programming language as all examples are written in C.
.AE
