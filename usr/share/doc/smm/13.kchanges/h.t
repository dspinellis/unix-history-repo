.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)h.t	1.9 (Berkeley) 4/11/86
.\"
.nr i \w'\fBkern_resource.c\0'u/1n
.de XP
.IP \\fB\\$1\\fP \\ni
..
.NH
Header files
.PP
This section details changes in the header files in \fB/sys/h\fP.
.XP acct.h
Process accounting is now done in units of 1/\fBAHZ\fP (64) seconds
rather than seconds.
.XP buf.h
The size of the buffer hash table has been increased substantially.
.XP cmap.h
The core map has had a number of fields enlarged to support larger
memories and filesystems.
The limits imposed by this structure are now commented.
The current limits are 64 Mb of physical memory, 255 filesystems,
1 Gb process segments, 8 Gb per filesystem,
and 65535 processes and text entries.
The machine-language support now derives its definitions of these limits
and the cmap structure from this file.
.XP dmap.h
The swap map per process segment was enlarged to allow images up to 64Mb.
.XP domain.h
New entry points to each domain have been added,
for initialization, externalization of access rights,
and disposal of access rights.
.XP errno.h
A definition of EDEADLK was added for System V compatibility.
.XP fs.h
One spare field in the superblock was allocated to store an option
for the fragment allocation policy.
.XP inode.h
New fields were added to the in-core inode to hold a cache key
and a pointer to any text image mapping the file.
A new macro, ITIMES, is provided for updating the timestamps in an inode
without writing the inode back to the disk.
The inode is marked as modified with the IMOD flag.
A flag has been added to allow serialization of directory renames.
.XP ioctl.h
New \fIioctl\fP operations have been added to get and set a terminal or window's
size.
The size is stored in a \fIwinsize\fP structure defined here.
Other new \fIioctl\fPs have been defined to pass a small set of special
commands from pseudo-terminals to their controllers.
A new terminal option, LPASS8, allows a full 8-bit data path on input.
The two tablet line disciplines have been merged.
A new line discipline is provided for use with IP over serial data lines.
.XP mbuf.h
The handling of mbuf page clusters has been broken
into macros separate from those that handle mbufs.
MCLALLOC(\fIm\fP, \fIi\fP) is used to allocate \fIi\fP mbuf clusters
(where \fIi\fP is currently restricted to 1)
and MCLFREE(\fIm\fP) frees them.
MCLGET(\fIm\fP) adds a page cluster to the already-allocated mbuf \fIm\fP,
setting the mbuf length to CLBYTES if successful.
The new macro M_HASCL(\fIm\fP)
returns true if the mbuf \fIm\fP has an associated cluster,
and MTOCL(\fIm\fP) returns a pointer to such a cluster.
.XP mtio.h
Definitions have been added for the TMSCP tape controllers
and to enable or disable the use of an on-board tape buffer.
.XP namei.h
This header file was renamed, completed and put into use.
.XP param.h
Several limits have been increased.  Old values are listed in parentheses
after each item.  The new limits are: 255 mounted filesystems (15),
40 processes per user (25), 64 open files (20), 20480 characters per argument
list (10240), and 16 groups per user (8).
The maximum length of a host name supported by the kernel is defined
here as MAXHOSTNAMELEN.
The default creation mask is now set to 022 by the kernel; previously
that value was set by login, with the effect that remote shell processes
used a different default.
Clist blocks were doubled in size to 64 bytes.
.XP proc.h
Pointers were added to the \fIproc\fP structure
to allow process entries to be linked
onto lists of active, zombie or free processes.
.XP protosw.h
The address family field in the \fIprotosw\fP structure
was replaced with a pointer to the \fIdomain\fP structure for the address
family.
Definitions were added for the arguments to the protocol \fIctloutput\fP 
routines.
.XP signal.h
New signals have been defined for window size changes (SIGWINCH)
and for user-defined functions (SIGUSR1 and SIGUSR2).
The \fIsv_onstack\fP field in the \fIsigvec\fP structure has been redefined
as a flags field, with flags defined for use of the signal stack
and for signals to interrupt pending systems calls rather than restarting them.
The \fIsigcontext\fP structure now includes the frame and argument pointers
for the VAX so that the complete return sequence can be done by the kernel.
A new macro, \fIsigmask\fP, is provided to simplify the use of \fIsigsetmask,
sigblock\fP, and \fIsigpause\fP.
.XP socket.h
Definitions were added for new options set with \fIsetsockopt\fP.
SO_BROADCAST requests permission to send to the broadcast address,
formerly a privileged operation, while
SO_SNDBUF and SO_RCVBUF may be used to examine or change the amount
of buffer space allocated for a socket.
Two new options are used only with \fIgetsockopt\fP:
SO_ERROR obtains any current error status and clears it,
and SO_TYPE returns the type of the socket.
A new structure was added for use with SO_LINGER.
Several new address families were defined.
.XP socketvar.h
The character and mbuf counts and limits in the \fIsockbuf\fP structure
were changed from \fIshort\fP to \fIu_short\fP.
SB_MAX defines the limit to the amount that can be placed in a \fIsockbuf\fP.
The \fIsosendallatonce\fP macro was corrected; it previously
returned true for sockets using non-blocking I/O.
\fISoreadable\fP and \fIsowriteable\fP now return true if there
is error status to report.
.XP syslog.h
The system logging facility has been extended to allow kernel use,
and the header file has thus been moved from \fB/usr/include\fP.
.XP tablet.h
A new file that contains the definitions for use
of the tablet line discipline.
.XP text.h
Linkage fields have been added to the text structure for use in
constructing a text table free list.
The structure used in recording text table usage statistics is defined here.
.XP time.h
The \fItime.h\fP header file has been split.  Those definitions relating to
the \fIgettimeofday\fP system call remain
in this file, included as <\fIsys/time.h\fP>.
The original <\fItime.h\fP> file has returned and contains the definitions
for the C library time routines.
.XP tty.h
The per-terminal data structure now contains the terminal size
so that it can be changed dynamically.
Files that include <\fIsys/tty.h\fP> now require <\fIsys/ioctl.h\fP> as well
for the \fIwinsize\fP structure definition.
.XP types.h
The new typedefs for user and group ID's are located here.
For compatibility and sensibility, the \fIsize_t\fP,
\fItime_t\fP and \fIoff_t\fP types
have all been changed from \fIint\fP to \fIlong\fP.
New definitions have been added for integer masks and bit operators
for use with the \fIselect\fP system call.
.XP uio.h
The offset field in the \fIuio\fP structure was changed from \fIint\fP
to \fIoff_t\fP.
Manifest constants for the \fIuio\fP segment values are now provided.
.XP un.h
The path in the Unix-domain version of a \fIsockaddr\fP was reduced
so that use of the entire pathname array would still allow space for
a null after the structure when stored in an mbuf.
.XP unpcb.h
A Unix-domain socket's own address is now stored in the protocol
control block rather than that of the socket to which it is connected.
Fields have been added for flow control on stream connections.
If a \fIstat\fP has caused the assignment of a dummy inode number to the socket,
that number is stored here.
.XP user.h
The user ID's, group ID's and groups array are declared using the new types
for these ID's.
A new field was added to handle the new signal flag
avoiding system call restarts.
The index of the last used file descriptor for the process is maintained
in \fIu.u_lastfile\fP.
The global fields \fIu_base, u_count,\fP and \fIu_offset\fP
have been eliminated,
with the new \fInameidata\fP structure replacing their remaining function.
The \fIa.out\fP header is no longer kept in the user structure.
.XP vmmac.h
Several macros have been rewritten to improve the code generated by
the compiler.
New macros were added to lock and unlock \fIcmap\fP entries, substituting
for \fImlock\fP and \fImunlock\fP.
.XP vmmeter.h
All counters are now uniformly declared as \fIlong\fP.
Software interrupts are now counted.
