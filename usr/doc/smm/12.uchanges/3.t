.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)3.t	6.10 (Berkeley) 4/14/86
.\"
.SH
.LG
.ce
Section 3
.SM
.sp
.PP
The Section 3 documentation has been reorganized into just two sections. 
The first section contains everything previously in Section 3
except the Fortran library routines.
The second section contains the Fortran library routines.
.PP
The routines \fImemccpy, memchr, memcmp, memcpy, memset, strchr, strcspn,
strpbrk, strrchr, strspn,\fP and \fIstrtok\fP have been added for
compatibility with System V.
These routines are similar to the string and block handling ones
described in the
.I bstring
and
.I string
manual pages.
The 4.3BSD \fIstring\fP and \fIbstring\fP versions should be faster than
these compatibility routines on the VAX.
.PP
.BP abort
Sets SIGILL signal action to the default
to avoid looping if SIGILL had been ignored or blocked. 
.BP ctime
Daylight savings time calculations have been fixed for Europe and Canada.
Programs making multiple calls to \fIctime\fP will make fewer system calls.
The include file has moved from <\fIsys/time.h\fP> to <\fItime.h\fP>.
.BP ctype
.I iscntrl
has been fixed to correspond to the manual page.  Space is a printing
character.  \fIisgraph\fP is a new function that
returns true for characters that leave a mark on
the paper.
.I toupper,
.I tolower,
and
.I toascii
have all been documented.
.BP curses
The library handles larger termcap definitions and
handles more of the ``funny'' termcap capabilities.
The old \fIcrmode\fP and \fInocrmode\fP macros have been
renamed \fIcbreak\fP and \fInocbreak\fP respectively;
backwards compatible definitions for these macros are provided.
The erase and kill characters and the terminal's baudrate
may be accessed via \fIerasechar\fP, \fIkillchar\fP,
and \fIbaudrate\fP macros defined in <\fIcurses.h\fP>.
A \fItouchoverlap\fP function has been provided,
and bugs in \fIoverlay\fP and \fIoverwrite\fP have been fixed.
.BP dbm
Has been rewritten to use the multiple-database version of the
library, \fIndbm\fP.
.BP disktab
Has added support for two new fields indicating the use of
\fIbad144\fP-style bad sector forwarding and filesystem offsets
specified in sectors.
.BP encrypt
Now works correctly when called directly.
.BP execvp
No longer recognizes ``-'' as a path separator.
.BP frexp
Now handles 0 and powers of 2 correctly.  This routine is now
written in assembly language for the VAX.
.BP gethost*
\fIgethostbyaddr\fP and \fIgethostbyname\fP 
have been modified to 
make calls to the name server.  If the name server is not running,
a linear scan of the host table is made.
With an optional C library configuration, these routines may instead
use an \fIndbm\fP database for the host table.
One of these lookup mechanisms must be specified when compiling
the C library.  The default is to use the name server.
\fIgethostent\fP has no equivalent when using the routines
calling the name server.
The \fIhostent\fP structure
has been modified to support the return of multiple addresses.
The external variable \fIh_errno\fP has been added for returning error
status information from the name server,
such as whether a transient error was encountered.
.BP getopt
A new routine for parsing command line arguments.  It is compatible
with the System V routine by the same name.
.BP getpw*
.I getpwnam
and
.I getpwuid
use a hashed database using
.I ndbm
for faster lookups by user name and id.
.BP gettty*
.I getttyent
and
.I getttynam
are new routines for looking up entries in the
new version of \fI/etc/ttys\fP.
The new header file <\fIttyent.h\fP>
describes the associated structures.
.BP getusershell
A new routine for retrieving shell names from a file listing
the standard interactive shells, \fI/etc/shells\fP, for the use
of
.IR passwd (1)
and servers providing remote host access.
.BP getwd
\fIGetwd\fP no longer changes directories in calculating the
working directory; this eliminates
problems with return to the current directory,
and results in fewer \fIstat\fP calls.
.BP inet_makeaddr
Properly handles INADDR_BROADCAST.
.BP longjmp
On errors, \fIlongjmp\fP calls the routine \fIlongjmperror\fP.
The default routine still prints ``longjmp botch'' and exits;
this may be replaced if a program
wants to provide its own error handler.
.BP malloc
\fIMalloc\fP underwent a major rework.
Memory requests of page size or larger are always page aligned,
and are now optimized for sizes that are a power of two.
The debugging code has been improved.
.BP math
The math library has been rewritten to improve the speed and
accuracy of the routines on VAXen with D-format floating
point support and machines that conform to the IEEE
standard 754 for double precision floating point arithmetic.
The library also has improved error detection and handling;
for the VAX, the library generates reserved operand faults
for invalid operands.
Many new functions have been added.
Two functions have changed their names;
\fIgamma\fP is now \fIlgamma\fP and
\fIfmod\fP is now \fImodf\fP.
The old math library is available as \-lom.
.BP mkstemp
Is a new routine similar to
.I mktemp
except that it returns an open file descriptor for a temporary file.    
It is intended to replace \fImktemp\fP in programs (run as root or setuid)
that must be concerned with atomic creation of temporary files
without the possibility of having the temporary file relocated
to an unexpected location by a symbolic link.
.BP ndbm
A new version of \fIdbm\fP that allows multiple databases to be open 
simultaneously.
.BP nlist
Now returns \-1 on error or the number of unfound items.
.BP perror
A few of the error messages have been made more accurate.
.BP plot
Supports many new devices: Tektronix 4013, AED graphics terminal,
BBN Bitgraph terminal, terminals using the DEC GiGi protocol,
HP 2648 terminals and 7221 plotters,
and Imagen laser printers (240 or 300 dots per inch).  Libraries
also exist for generating plot files from Fortran programs and
for plotting on ``dumb'' devices such as a standard line printer.
.BP popen
Dynamically allocates an array for file descriptors.
The new signal interface is now used.
.BP psignal
New signals have been added to the list.
.BP random
An initialization bug that messed up default generation was fixed.
.BP rcmd
Cleans up properly.
A problem with doing multiple calls within one program was fixed.
.BP ruserok
Now is more flexible about the format of \fI.rhosts\fP.
Domain style hostnames do not need full specification
if they are a part of the local domain, as determined by \fIhostname\fP\|(1).
\fIRuserok\fP is more paranoid about ownership of \fI.rhosts\fP.
.BP scandir
Handling of overflow has been fixed.
.BP setjmp
The signal stack status is now set correctly.
.BP siginterrupt
A new routine to set the signals for which system calls are not restarted after
signal delivery.
.BP signal
Keeps track of new features when changing signal handlers.
.BP sleep
A couple of races have been fixed.
.BP stdio
Has been modified to dynamically allocate slots for file pointers.
Output on unbuffered files is now buffered within a call
to \fIprintf\fP or \fIfputs\fP for efficiency.
\fIFseek\fP now returns zero
if it was successful.
\fIFread\fP and \fIfwrite\fP have been rewritten to improve performance.
On the VAX, \fIfgets, gets, fputs\fP and \fIputs\fP
were rewritten to take advantage of VAX string instructions
and thus improve performance.
Line buffering now works on any file descriptor,
not just \fIstdout\fP and \fIstderr\fP.
\fIPutc\fP is implemented completely within a macro
except when the buffer is full
or when a newline is output on a line-buffered file.
Some sign extension bugs with the return value of \fIputc\fP
have been fixed.
.BP string
The routines \fIindex, rindex, strcat, strcmp, strcpy, strlen,
strncat\fP, and \fIstrncpy\fP have been rewritten in VAX assembly language for
efficiency.  The C routines are included for use on other machines.
Only \fIMakefile\fPs
need to be modified to select the version to be used.
.BP syslog
The third parameter to \fIopenlog\fP
is a ``\fIfacility code\fP'' used to classify messages.
References to <\fIsyslog.h\fP> should be replaced with references
to <\fIsys/syslog.h\fP>.
.BP ttyslot
Uses the new \fIgetttyent\fP routine.
.BP ualarm
A simplified interface to \fIsetitimer\fP, similar to \fIalarm\fP
but with its argument in microseconds.
.BP usleep
A new routine which resembles \fIsleep\fP
but takes an argument in microseconds.
