.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)0.t	6.7 (Berkeley) 4/15/86
.\"
.EH 'SMM:12-%''Bug Fixes and Changes in 4.3BSD'
.OH 'Bug Fixes and Changes in 4.3BSD''SMM:12-%'
.de IR
\fI\\$1\\|\fR\\$2\\fR\\fR
..
.TL
Bug Fixes and Changes in 4.3BSD
.sp
April 15, 1986
.AU
Marshall Kirk McKusick
James M. Bloom
Michael J. Karels
.AI
Computer Systems Research Group
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California 94720
(415) 642-7780
.AB
This document briefly describes the changes in the Berkeley version of
.UX
for the VAX\(dd
.FS
\(dd \s-2DEC\s0, \s-2VAX\s0, \s-2PDP\s0, \s-2MASSBUS\s0, \s-2UNIBUS\s0,
\s-2Q-bus\s0 and \s-2ULTRIX\s0
are trademarks of Digital Equipment Corporation.
.FE
between the 4.2BSD distribution of July 1983
and this, its revision of March 1986.
It attempts only to summarize the changes that have been made.
.AE
.SH
.ce
.LG
Notable improvements
.SM
.sp
.IP \s+1\(bu\s0
The performance of the system has been improved to be at least as
good as that of 4.1BSD, and in many instances is better.
This was accomplished by improving the performance of kernel operations,
rewriting C library routines for efficiency,
and optimization of heavily used utilities.
.IP \s+1\(bu\s0
Many programs were rewritten to do I/O in optimal blocks for the filesystem.
Most of these programs were doing their own I/O and not using the standard
I/O library.
.IP \s+1\(bu\s0
The system now supports the Xerox Network System
network communication protocols.
Most of the remaining Internet dependencies in shared common code
have been removed or generalized.
.IP \s+1\(bu\s0
The signal mechanism has been extended
to allow selected signals to interrupt pending system calls.
.IP \s+1\(bu\s0
The C and Fortran 77 compilers have been modified so that they
can generate single precision floating point operations.
.IP \s+1\(bu\s0
The Fortran 77 compiler and associated I/O library have undergone
extensive changes to improve reliability and performance.  Compilation may,
optionally, include optimization phases to improve code density and
decrease execution time.
Many minor bugs in the C compiler have been fixed.
.IP \s+1\(bu\s0
The math library has been completely rewritten
by a group of numerical analysts
to improve both its speed and accuracy.
.IP \s+1\(bu\s0
Password lookup functions now use a hashed database rather than linear
search of the password file.
.IP \s+1\(bu\s0
C library string routines and several standard I/O functions
were recoded in VAX assembler for greater speed.
The C versions are available for portability.
Standard error is now buffered within a single call to perform output.
.IP \s+1\(bu\s0
The symbolic debugger, \fIdbx\fP, has been dramatically improved.
\fIDbx\fP works on C, Pascal and Fortran 77 programs and allows users
to set break points and trace execution by source code line numbers,
references to memory locations, procedure entry, etc.  \fIDbx\fP allows
users to reference structured and local variables using
the program's programming language syntax.
.IP \s+1\(bu\s0
A new internet name domain server has been added to allow sites to
administer their name space locally and export it to the rest of the Internet.
Sites not using the name server may use a static host table with a hashed
lookup mechanism.
.IP \s+1\(bu\s0
A new time synchronization server has been added to allow a set of machines to
keep their clocks within tens of milliseconds of each other.
.br
.ne 10
.LP
