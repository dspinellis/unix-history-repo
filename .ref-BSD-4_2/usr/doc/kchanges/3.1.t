.NH 2
/sys/h
.PP
Files residing here are intended to be machine independent.
Consequently, the header files for device drivers which
were present in this directory in 4.1BSD have been moved
to other directories;
e.g. /sys/vaxuba.  Many files which had been duplicated in
/usr/include are now present only in /sys/h.  Further, the
4.1BSD /usr/include/sys directory is now normally a symbolic link
to this directory.  By having only a single copy of these files
the ``multiple update'' problem no longer occurs.  (It is still
possible to have /usr/include/sys be a copy of the /sys/h for
sites where it is not feasible to allow the general user
community access to the system source code.)
.PP
The following files are new to /sys/h in 4.2BSD:
.IP \fBdomain.h\fP 15
describes the internal structure of a communications domain; part of
the new ipc facilities
.IP \fBerrno.h\fP 15
had previously been only in /usr/include; the file /usr/include/errno.h
is now a symbolic link to this file
.IP \fBfs.h\fP 15
replaces the old filsys.h description of the file system organization
.IP \fBgprof.h\fP 15
describes various data structures used in profiling the
kernel; see \fIgprof\fP\|(1) for details
.IP \fBkernel.h\fP 15
is an offshoot of systm.h and param.h; contains constants and
definitions related to the logical UNIX ``kernel''
.IP \fBmbuf.h\fP 15
describes the memory managment support used mostly by the network;
see ``4.2BSD Networking Implementation Notes'' for more information
.IP \fBmman.h\fP 15
contains definitions for planned changes to the memory management
facilities (not implemented in 4.2BSD)
.IP \fBnami.h\fP 15
defines various structures and manifest constants used in
conjunctions with the \fInamei\fP routine (part of this file
reflects future plans for changes to \fInamei\fP rather
than current use)
.IP \fBprotosw.h\fP 15
contains a description of the protocol switch table and related
manifest constants and data structures use in communicating with
routines located in the table
.IP \fBquota.h\fP 15
contains definitions related to the new disk quota facilities
.IP \fBresource.h\fP 15
contains definitions used in the \fIgetrusage\fP,
\fIgetrlimit\fP, and \fIgetpriority\fP system calls (among others)
.IP \fBsocket.h\fP 15
contains user-visible definitions related to the new socket ipc
facilities
.IP \fBsocketvar.h\fP 15
contains implementation definitions for the socket ipc facilities
.IP \fBttychars.h\fP 15
contains definitions related to tty character handling; in particular,
manifest constants for the system standard erase, kill, interrupt,
quit, etc. characters are stored here (all the appropriate user
programs use these manifest definitions)
.IP \fBttydev.h\fP 15
contains definitions related to hardware specific portions of tty 
handling (such as baud rates); to be expanded in the future
.IP \fBuio.h\fP 15
contains definitions for users wishing to use the new
scatter-gather i/o facilities; also contains the kernel \fIuio\fP
structure used in implementing scatter-gather i/o
.IP \fBun.h\fP 15
contains user-visible definitions related to the ``unix'' ipc domain
.IP \fBunpcb.h\fP 15
contains the definition of the protocol control block used
in the ``unix'' ipc domain
.IP \fBwait.h\fP 15
contains definitions used in the \fIwait\fP and \fIwait3\fP\|(2)
system calls; previously in /usr/include/wait.h
.PP
The following files have undergone significant change:
.IP \fBbuf.h\fP 15
reflects the changes made to the buffer cache for the
new file system organization \- buffers
are variable sized with pages allocated to buffers on
demand from a pool of pages dedicated to the buffer cache;
one new structure member has been added
to eliminate overloading of a commonly unreferenced structure
member; a new flag B_CALL, when set, causes the function
\fIb_iodone\fP to be called when i/o completes on a buffer
(this is used to wakeup the pageout daemon); macros have
been added for manipulating the buffer queues, these replace
the previous subroutines used to insert and delete buffers
from the queues
.IP \fBconf.h\fP 15
reflects changes made in the handling of swap space and
changes made for the new \fIselect\fP\|(2) system call;
the block device table
has a new member, \fId_psize\fP,
which returns the size of a disk partition, in sectors,
given a major/minor value; the character device table has a
new member, \fId_select\fP, which is passed
a \fIdev_t\fP value and an FREAD (FWRITE) flag and returns
1 when data may be read (written), and 0 otherwise; the
\fIswdevt\fP structure now includes the size, in sectors,
of a swap partition
.IP \fBdir.h\fP 15
is completely different since directory entries are now
variable length; definitions for the user level interface
routines described in \fIdirectory\fP\|(3) are also present
.IP \fBfile.h\fP 15
has a very different \fIfile\fP structure definition and
definitions for the new \fIopen\fP and \fIflock\fP system
calls; symbolic definitions for many constants commonly
supplied to \fIaccess\fP and \fIlseek\fP, are also present
.IP \fBinode.h\fP 15
reflects the new hashed cacheing scheme as well additions
made to the on-disk and in-core inodes; on-disk inodes
now contain a count of the actual number of disk
blocks allocated a file (used mostly by the disk quota
facilities), larger time stamps (for planned changes),
more direct block pointers, and room for future growth;
in-core inodes have new fields for the advisory locking
facilities, a back pointer to the file system super block
information (to eliminate lookups), and a pointer to
a structure used in implementing disk quotas.
.IP \fBioctl.h\fP 15
has all request codes constructed from _IO, _IOR,
_IOW, and _IOWR macros which encode whether the request
requires data copied in, out, or in and out of the kernel
address space; the size of the data parameter (in bytes) is
also encoded in the request, allowing the \fIioctl\fP\|()
routine to perform all user-kernel address space copies
.IP \fBmount.h\fP 15
the \fImount\fP structure has a new member used in
the disk quota facilities
.IP \fBparam.h\fP 15
has had numerous items deleted from it; in
particular, many definitions logically part of the ``kernel''
have been moved to kernel.h, and machine-dependent values and
definitions are now found in param.h files located in
machine/param.h; contains a manifest constant, NGROUPS,
which defines the maximum size of the group access list
.IP \fBproc.h\fP 15
has changed extensively as a result of the new signals, the
different resource usage structure, the disk quotas,
and the new timers; in addition, new members are present
to simplify searching the process tree for siblings; the
SDLYU and SDETACH bits are gone, the former is replaced
by a second parameter to \fIpagein\fP, the latter is no
longer needed due to changes in the handling of open's
on /dev/tty by processes which have had their controlling
terminal revoked with \fIvhangup\fP
.IP \fBsignal.h\fP 15
reflects the new signal facilities; several new signals
have been added: SIGIO for signal driven i/o; SIGURG for
notification when an urgent condition arises; and SIGPROF and
SIGVTALRM for the new timer facilities; structures used
in the \fIsigvec\fP\|(2) and \fIsigstack\fP\|(2) system
calls, as well as signal handler invocations are defined
here
.IP \fBstat.h\fP 15
has been updated to reflect the changes to the inode
structure; in addition a new field \fIst_blksize\fP
contains an ``optimal blocking factor'' for performing
i/o (for files this is the block size of the underlying
file system)
.IP \fBsystm.h\fP 15
has been trimmed back a bit as various items were moved
to kernel.h
.IP \fBtime.h\fP 15
contains the definitions for the new time and interval
timer facilities; time zone definitions for the half dozen
time zones understood by the system are also included here
.IP \fBtty.h\fP 15
reflects changes made to the internal structure of the
terminal handler; the ``local'' structures have been
merged into the standard flags and character definitions
though the user interface is virtually identical to that
of 4.1BSD; the TTYHOG value has been changed from 256 to
255 to account for a counting problem in the terminal 
handler on input buffer overflow
.IP \fBuser.h\fP 15
has been extensively modified; members have been grouped
and categorized to reflect the ``4.2BSD System Manual''
presentation; new members have been added and existing members 
changed to reflect: the new groups facilities, 
changes to resource accounting and limiting, new timer facilities,
and new signal facilities
.IP \fBvmmac.h\fP 15
has had many macro definitions changed to eliminate
assumptions about the hardware virtual memory support;
in particular, the stack and user area page table
maps are no longer assumed to be adjacent or
mapped by a single page table base register
.IP \fBvmparam.h\fP 15
now includes machine-dependent definitions from a file
machine/vmparam.h.
.IP \fBvmsystm.h\fP 15
has had several machine-dependent definitions moved
to machine/vmparam.h
