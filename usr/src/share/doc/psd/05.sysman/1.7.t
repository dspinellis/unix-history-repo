.\" Copyright (c) 1983 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)1.7.t	6.4 (Berkeley) %G%
.\"
.sh "System operation support
.PP
Unless noted otherwise,
the calls in this section are permitted only to a privileged user.
.NH 3
Bootstrap operations
.PP
The call
.DS
mount(blkdev, dir, ronly);
char *blkdev, *dir; int ronly;
.DE
extends the UNIX name space.  The \fImount\fP call specifies
a block device \fIblkdev\fP containing a UNIX file system
to be made available starting at \fIdir\fP.  If \fIronly\fP is
set then the file system is read-only; writes to the file system
will not be permitted and access times will not be updated
when files are referenced.
\fIDir\fP is normally a name in the root directory.
.PP
The call
.DS
swapon(blkdev, size);
char *blkdev; int size;
.DE
specifies a device to be made available for paging and swapping.
.PP
.NH 3
Shutdown operations
.PP
The call
.DS
unmount(dir);
char *dir;
.DE
unmounts the file system mounted on \fIdir\fP.
This call will succeed only if the file system is
not currently being used.
.PP
The call
.DS
sync();
.DE
schedules input/output to clean all system buffer caches.
(This call does not require privileged status.)
.PP
The call
.DS
reboot(how)
int how;
.DE
causes a machine halt or reboot.  The call may request a reboot
by specifying \fIhow\fP as RB_AUTOBOOT, or that the machine be halted
with RB_HALT.  These constants are defined in \fI<sys/reboot.h>\fP.
.NH 3
Accounting
.PP
The system optionally keeps an accounting record in a file
for each process that exits on the system.
The format of this record is beyond the scope of this document.
The accounting may be enabled to a file \fIname\fP by doing
.DS
acct(path);
char *path;
.DE
If \fIpath\fP is null, then accounting is disabled.  Otherwise,
the named file becomes the accounting file.
