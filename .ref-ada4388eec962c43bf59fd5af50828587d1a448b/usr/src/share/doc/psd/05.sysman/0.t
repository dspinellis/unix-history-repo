.\" Copyright (c) 1983, 1993, 1994
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)0.t	8.3 (Berkeley) %G%
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
It does not attempt to act as a tutorial for use of the system,
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
C language function call, consisting of a prototype call followed by
the declaration of parameters and results.
An additional keyword \fBresult\fP, not part of the normal C language,
is used to indicate which of the declared entities receive results.
As an example, consider the \fIread\fP call, as described in
section
.Xr 2.1.1 :
.DS
cc = read(fd, buf, nbytes);
result ssize_t cc; int fd; result void *buf; size_t nbytes;
.DE
The first line shows how the \fIread\fP routine is called, with
three parameters.
As shown on the second line,
the return value \fIcc\fP is a size_t and \fIread\fP
also returns information in the parameter \fIbuf\fP.
.PP
The descriptions of error conditions arising from each system call
are not provided here; they appear in section
.Xr 2
of the Programmer's Reference Manual.
In particular, when accessed from the C language,
many calls return a characteristic \-1 value
when an error occurs, returning the error code in the global variable
\fIerrno\fP.
Other languages may present errors in different ways.
.PP
A number of system standard types are defined by the include file
.I <sys/types.h>
and used in the specifications here and in many C programs.
.sp
.ft C
.TS
l l l.
Type	Value
_
caddr_t	char *	/* a memory address */
clock_t	unsigned long	/* count of CLK_TCK's */
gid_t	unsigned long	/* group ID */
int16_t	short	/* 16-bit integer */
int32_t	int	/* 32-bit integer */
int64_t	long long	/* 64-bit integer */
int8_t	signed char	/* 8-bit integer */
mode_t	unsigned short	/* file permissions */
off_t	quad_t	/* file offset */
pid_t	long	/* process ID */
qaddr_t	quad_t *
quad_t	long long
size_t	unsigned int	/* count of bytes */
ssize_t	int	/* signed size_t */
time_t	long	/* seconds since the Epoch */
u_char	unsigned char
u_int	unsigned int
u_int16_t	unsigned short	/* unsigned 16-bit integer */
u_int32_t	unsigned int	/* unsigned 32-bit integer */
u_int64_t	unsigned long long	/* unsigned 64-bit integer */
u_int8_t	unsigned char	/* unsigned 8-bit integer */
u_long	unsigned long
u_quad_t	unsigned long long
u_short	unsigned short
uid_t	unsigned long	/* user ID */
uint	unsigned int	/* System V compatibility */
ushort	unsigned short	/* System V compatibility */
.TE
.ft R
