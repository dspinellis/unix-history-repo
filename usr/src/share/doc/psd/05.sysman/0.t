.\" Copyright (c) 1983, 1993
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)0.t	8.2 (Berkeley) %G%
.\"
.if n .ND
.TL
Berkeley Software Architecture Manual
.br
4.4BSD Edition
.AU
M. Kirk McKusick, Michael Karels
.AU
Samuel Leffler, William Joy
.AU
Robert Fabry
.AI
Computer Systems Research Group
Computer Science Division
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, CA  94720
.EH 'PSD:5-%''4.4BSD Architecture Manual'
.OH '4.4BSD Architecture Manual''PSD:5-%'
.AB
This document summarizes the system calls
provided by the 4.4BSD operating system.
It does not attempt to act as a tutorial for use of the system
nor does it attempt to explain or justify the design of the
system facilities.
It gives neither motivation nor implementation details,
in favor of brevity.
.PP
The first section describes the basic kernel functions
provided to a process: process naming and protection,
memory management, software interrupts,
time and statistics functions,
object references (descriptors),
and resource controls.
These facilities, as well as facilities for
bootstrap, shutdown and process accounting,
are provided solely by the kernel.
.PP
The second section describes the standard system
abstractions for
files and filesystems,
communication,
terminal handling,
and process control and debugging.
These facilities are implemented by the operating system or by
network server processes.
.AE
.LP
.bp +3
.sy echo -n >toc
.de Sh
.sy echo >>toc '.L\\$1 "\\$2" \\n%'
.ie \\$1=0 \{\
\fB\\$2\fP
.\}
.el \{\
.NH \\$1
\\$2
.LP
.\}
..
.de Fn
\fI\\$1\fP\\$2
..
.de Xr
\\$1\\$2
..
.de Fd
.sy echo >>toc '.Nm \\$1 \\$2 "\\$3'
..
.Sh 0 "Notation and Types
.PP
The notation used to describe system calls is a variant of a
C language call, consisting of a prototype call followed by
declaration of parameters and results.
An additional keyword \fBresult\fP, not part of the normal C language,
is used to indicate which of the declared entities receive results.
As an example, consider the \fIread\fP call, as described in
section
.Xr 2.1.1 :
.DS
cc = read(fd, buf, nbytes);
result int cc; int fd; result char *buf; int nbytes;
.DE
The first line shows how the \fIread\fP routine is called, with
three parameters.
As shown on the second line \fIcc\fP is an integer and \fIread\fP also
returns information in the parameter \fIbuf\fP.
.PP
Description of all error conditions arising from each system call
is not provided here; they appear in section
.Xr 2
of the programmer's reference manual.
In particular, when accessed from the C language,
many calls return a characteristic \-1 value
when an error occurs, returning the error code in the global variable
\fIerrno\fP.
Other languages may present errors in different ways.
.PP
A number of system standard types are defined in the include file
.I <sys/types.h>
and used in the specifications here and in many C programs.
These include \fBcaddr_t\fP giving a memory address (typically as
a character pointer), 
\fBoff_t\fP giving a file offset (typically as a 64-bit integer),
and a set of unsigned types \fBu_char\fP, \fBu_short\fP, \fBu_int\fP
and \fBu_long\fP, shorthand names for \fBunsigned char\fP, \fBunsigned
short\fP, etc.
