.\" Copyright (c) 1983, 1993, 1994
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)1.0.t	8.3 (Berkeley) %G%
.\"
.Sh 1 "Kernel primitives
.PP
The facilities available to a user process are logically
divided into two parts: kernel facilities directly implemented by
code running in the operating system, and system facilities
implemented either by the system, or in cooperation with a
\fIserver process\fP.
The kernel facilities are described in section
.Xr 1 .
.PP
The facilities implemented in the kernel are those which define the
\fI4.4BSD virtual machine\fP in which each process runs.
Like many real machines, this virtual machine has memory management hardware,
an interrupt facility, timers and counters.  The 4.4BSD
virtual machine allows access to files and other objects through a set of
\fIdescriptors\fP.  Each descriptor resembles a device controller,
and supports a set of operations.  Like devices on real machines, some
of which are internal to the machine and some of which are external,
parts of the descriptor machinery are built-in to the operating system, while
other parts are implemented in server processes on other machines.
The facilities provided through the descriptor machinery are described in
section
.Xr 2 .
