.\" Copyright (c) 1983, 1993, 1994
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)1.7.t	8.4 (Berkeley) %G%
.\"
.Sh 2 "System operation support
.LP
Unless noted otherwise,
the calls in this section are permitted only to a privileged user.
.Sh 3 "Monitoring system operation
.PP
The
.Fn sysctl
function allows any process to retrieve system information
and allows processes with appropriate privileges to set system configurations.
.DS
.Fd sysctl 6 "get or set system information
sysctl(name, namelen, oldp, oldlenp, newp, newlen);
int *name; u_int namelen; result void *oldp; result size_t *oldlenp;
void *newp; size_t newlen;
.DE
The information available from
.Fn sysctl
consists of integers, strings, and tables.
.Fn Sysctl
returns a consistent snapshot of the data requested.
Consistency is obtained by locking the destination
buffer into memory so that the data may be copied out without blocking.
Calls to
.Fn sysctl
are serialized to avoid deadlock.
.PP
The object to be interrogated or set is named
using a ``Management Information Base'' (MIB)
style name, listed in \fIname\fP,
which is a \fInamelen\fP length array of integers.
This name is from a hierarchical name space,
with the most significant component in the first element of the array.
It is analogous to a file pathname, but with integers as components
rather than slash-separated strings.
.PP
The information is copied into the buffer specified by \fIoldp\fP.
The size of the buffer is given by the location specified by \fIoldlenp\fP
before the call,
and that location is filled in with the amount of data copied after
a successful call.
If the amount of data available is greater
than the size of the buffer supplied,
the call supplies as much data as fits in the buffer provided
and returns an error.
.PP
To set a new value, \fInewp\fP
is set to point to a buffer of length \fInewlen\fP
from which the requested value is to be taken.
If a new value is not to be set, \fInewp\fP
should be set to NULL and \fInewlen\fP set to 0.
.PP
The top level names (those used in the first element of the \fIname\fP array)
are defined with a CTL_ prefix in \fI<sys/sysctl.h>\fP,
and are as follows.
The next and subsequent levels down are found
in the include files listed here:
.DS
.TS
l l l.
Name	Next Level Names	Description
_
CTL\_DEBUG	sys/sysctl.h	Debugging
CTL\_FS	sys/sysctl.h	Filesystem
CTL\_HW	sys/sysctl.h	Generic CPU, I/O
CTL\_KERN	sys/sysctl.h	High kernel limits
CTL\_MACHDEP	sys/sysctl.h	Machine dependent
CTL\_NET	sys/socket.h	Networking
CTL\_USER	sys/sysctl.h	User-level
CTL\_VM	vm/vm_param.h	Virtual memory
.TE
.DE
.Sh 3 "Bootstrap operations
.LP
The call:
.DS
.Fd mount 4 "mount a filesystem
mount(type, dir, flags, data);
int type; char *dir; int flags; caddr_t data;
.DE
extends the name space. The
.Fn mount
call grafts a filesystem object onto the system file tree
at the point specified in \fIdir\fP.
The argument \fItype\fP specifies the type of filesystem to be mounted.
The argument \fIdata\fP describes the filesystem object to be mounted
according to the \fItype\fP.
The contents of the filesystem become available through the
new mount point \fIdir\fP.
Any files in or below \fIdir\fP at the time of a successful mount
disappear from the name space until the filesystem is unmounted.
The \fIflags\fP value specifies generic properties,
such as a request to mount the filesystem read-only.
.LP
Information about all mounted filesystems can be obtained with the call:
.DS
.Fd getfsstat 3 "get list of all mounted filesystems
getfsstat(buf, bufsize, flags);
result struct statfs *buf; long bufsize, int flags;
.DE
.LP
The call:
.DS
.Fd swapon 1 "add a swap device for interleaved paging/swapping
swapon(blkdev);
char *blkdev;
.DE
specifies a device to be made available for paging and swapping.
.Sh 3 "Shutdown operations
.LP
The call:
.DS
.Fd unmount 2 "dismount a filesystem
unmount(dir, flags);
char *dir; int flags;
.DE
unmounts the filesystem mounted on \fIdir\fP.
This call will succeed only if the filesystem is
not currently being used or if the MNT_FORCE flag is specified.
.LP
The call:
.DS
.Fd sync 0 "force completion of pending disk writes (flush cache)
sync();
.DE
schedules I/O to flush all modified disk blocks resident in the kernel.
(This call does not require privileged status.)
Files can be selectively flushed to disk using the
.Fn fsync
call (see section
.Xr 2.2.6 ).
.LP
The call:
.DS
.Fd reboot 1 "reboot system or halt processor
reboot(how);
int how;
.DE
causes a machine halt or reboot.  The call may request a reboot
by specifying \fIhow\fP as RB_AUTOBOOT, or that the machine be halted
with RB_HALT, among other options.
These constants are defined in \fI<sys/reboot.h>\fP.
.Sh 3 "Accounting
.PP
The system optionally keeps an accounting record in a file
for each process that exits on the system.
The format of this record is beyond the scope of this document.
The accounting may be enabled to a file \fIname\fP by doing:
.DS
.Fd acct 1 "enable or disable process accounting
acct(path);
char *path;
.DE
If \fIpath\fP is NULL, then accounting is disabled.
Otherwise, the named file becomes the accounting file.
