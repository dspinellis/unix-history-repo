.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)2.t	6.6 (Berkeley) 4/14/86
.\"
.SH
.LG
.ce
Section 2
.SM
.sp
.PP
The error codes for Section 2 entries have been carefully
scrutinized to insure that the documentation properly reflects
the source code.  User-visible changes in this section lie
mostly in the area of the interprocess communication facilities;
the Xerox Network System communication procotocols have
been added and the existing communication facilities have
been extended and made more robust.
.PP
.BP adjtime
A new system call which skews the system clock
to correct the time of day.
.BP fcntl
The FASYNC option to enable the SIGIO signal
now works with sockets as well as with ttys.
The interpretation of process groups set with F_SETOWN
is the same for sockets and for ttys:
negative values refer to process groups,
positive values to processes.
This is the reverse of the previous interpretation
of socket process groups set using \fIioctl\fP to enable SIGURG. 
.BP kill
The error returned when trying to signal one's own process group
when no process group is set was changed to ESRCH.
Signal 0 can now be used as documented.
.BP lseek
Returns an ESPIPE error when seeking on sockets
(including pipes) for backward compatibility.
.BP open
When doing an open with flags O_CREAT and O_EXCL (create only if the file
did not exist), it is now considered to be an error if the target exists
and is a symbolic link, even if the symbolic link refers to a nonexistent
file.
This behavior was added for the security of programs that
need to create
files with predictable names.
.BP ptrace
A new header file, <\fIsys/ptrace.h\fP>, defines the request types.
When the process being traced stops, the parent now receives a SIGCHLD.
.BP readlink
Returns EINVAL instead of ENXIO when trying to read something other
than a symbolic link.
.BP rename
If the ISVTX (sticky text) bit is set in the mode of a directory,
files in that directory may not be the source or target of a \fIrename\fP
except by the owner of the file, the owner of the directory, or the superuser.
.BP select
Now handles more descriptors.
The mask arguments to \fIselect\fP are now treated as
pointers to arrays of integers, with the first argument determining the size
of the array.
A set of macros in
<\fIsys/types.h\fP> is provided for manipulating the file descriptor sets.
The descriptor masks are only modified when no error is returned.
.BP setsockopt
Options that could only be \fIset\fP in 4.2BSD (e.g. SO_DEBUG, SO_REUSEADDR)
can now be set or reset.  To implement this change all options must now
supply an option value which specifies if the option is to be turned on or off.
The SO_LINGER option takes a structure as its option value, including both
a boolean and an interval.
New options have been added: to get or set the amount of buffering allocated
for the socket, to get the type of the socket, and to check on error status.
Options can be set in any protocol layer that supports them;
IP, TCP and SPP all use this mechanism.
.BP setpriority
The error returned on an attempt to change another user's priority
was changed from EACCES to EPERM.
.BP setreuid
Now sets the process \fIp_uid\fP to the new effective user ID instead
of the real ID for consistency with usage elsewhere.
This avoids problems with processes that are not able
to signal themselves.
.BP sigreturn
Is a new system call designed for restoring a process' context to a
previously saved one (see \fIsetjmp/longjmp\fP).
.BP sigvec
Three new signals have been added, SIGWINCH, SIGUSR1, and
SIGUSR2.  The first is for notification of window size changes
and the other two have been reserved for users.
.BP socket
The usage of the (undocumented) SIOCSPGRP \fIioctl\fP has changed.
For consistency with \fIfcntl\fP, the argument is treated
as a process if positive and as a process group if negative.
Asynchronous I/O using SIGIO is now possible on sockets.
.BP swapon
The error returned for when requesting a device which was not
configured as a swap device was changed from ENODEV to EINVAL.
In addition, \fIswapon\fP now searches the swap device tables from
from the beginning instead of the second entry.
.BP unlink
If the ISVTX (sticky text) bit is set in the mode of a directory,
files may only be removed from that directory by the owner of the file,
the owner of the directory, or the superuser.
