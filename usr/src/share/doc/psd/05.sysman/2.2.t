.\" Copyright (c) 1983, 1993
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)2.2.t	8.2 (Berkeley) %G%
.\"
.Sh 2 "Filesystem
.Sh 3 "Overview
.PP
The filesystem abstraction provides access to a hierarchical
filesystem structure.
The filesystem contains directories (each of which may contain
other sub-directories) as well as files and references to other
objects such as devices and inter-process communications sockets.
.PP
Each file is organized as a linear array of bytes.  No record
boundaries or system related information is present in
a file.
Files may be read and written in a random-access fashion.
The user may read the data in a directory as though
it were an ordinary file to determine the names of the contained files,
but only the system may write into the directories.
The filesystem stores only a small amount of ownership, protection and usage
information with a file.
.Sh 3 "Naming
.PP
The filesystem calls take \fIpath name\fP arguments.
These consist of a zero or more component \fIfile names\fP
separated by ``/\^'' characters, where each file name
is up to NAME_MAX (255) ASCII characters excluding null and ``/\^''.
Each pathname is up to PATH_MAX (1024) ASCII characters excluding null.
.PP
Each process always has two naming contexts: one for the
root directory of the filesystem and one for the
current working directory.  These are used
by the system in the filename translation process.
If a path name begins with a ``/\^'', it is called
a full path name and interpreted relative to the root directory context.
If the path name does not begin with a ``/\^'' it is called
a relative path name and interpreted relative to the current directory
context.
.PP
The file name ``..'' in each directory refers to
the parent directory of that directory.
The parent directory of the root of the filesystem is always that directory.
.LP
The calls:
.DS
.Fd chdir 1 "change current working directory
chdir(path);
char *path;
.DE
.DS
.Fd fchdir 1 "change current working directory
fchdir(fd);
int fd;
.DE
.DS
.Fd chroot 1 "change root directory
chroot(path)
char *path;
.DE
change the current working directory and root directory context of a process.
Only the super-user can change the root directory context of a process.
.LP
Information about a filesystem that contains a particular
file can be obtained using the calls:
.DS
.Fd statfs 2 "get file system statistics
statfs(path, buf);
char *path; struct statfs *buf;
.DE
.DS
.Fd fstatfs 2 "get file system statistics
fstatfs(fd, buf);
int fd; struct statfs *buf;
.DE
.Sh 3 "Creation and removal
.PP
The filesystem allows directories, files, special devices,
and fifos to be created and removed from the filesystem.
.Sh 4 "Directory creation and removal
.LP
A directory is created with the
.Fn mkdir
system call:
.DS
.Fd mkdir 2 "make a directory file
mkdir(path, mode);
char *path; mode_t mode;
.DE
where the mode is defined as for files (see section
.Xr 2.2.3.2 ).
Directories are removed with the
.Fn rmdir
system call:
.DS
.Fd rmdir 1 "remove a directory file
rmdir(path);
char *path;
.DE
A directory must be empty if it is to be deleted.
.LP
Although directories can be read as files,
the usual interface is to use the call:
.DS
.Fd getdirentries 4 "get directory entries in a filesystem independent format
getdirentries(fd, buf, nbytes, basep)
int fd; char *buf; int nbytes; long *basep;
.DE
The
.Fn getdirentries
system call returns a canonical array of directory entries
in a filesystem independent format described in \fI<dirent.h>\fP.
Application programs usually use the library routines
.Fn opendir ,
.Fn readdir ,
and
.Fn closedir
that provide a more convenient interface to
.Fn getdirentries .
.Sh 4 "File creation
.LP
Files are created with the
.Fn open
system call:
.DS
.Fd open 3 "open or create a file for reading or writing
fd = open(path, oflag, mode);
result int fd; char *path; int oflag; mode_t mode;
.DE
The \fIpath\fP parameter specifies the name of the
file to be opened.
The \fIoflag\fP parameter must
include O_CREAT to cause the file to be created.
Bits for \fIoflag\fP are
defined in \fI<sys/fcntl.h>\fP:
.DS
.TS
l l.
O_RDONLY	/* open for reading only */
O_WRONLY	/* open for writing only */
O_RDWR	/* open for reading and writing */
O_NONBLOCK	/* no delay */
O_APPEND	/* set append mode */
O_SHLOCK	/* open with shared file lock */
O_EXLOCK	/* open with exclusive file lock */
O_ASYNC	/* signal pgrp when data ready */
O_FSYNC	/* synchronous writes */
O_CREAT	/* create if nonexistent */
O_TRUNC	/* truncate to zero length */
O_EXCL	/* error if already exists */
.TE
.DE
.PP
One of O_RDONLY, O_WRONLY and O_RDWR should be specified,
indicating what types of operations are desired to be done
on the open file.  The operations will be checked against the user's
access rights to the file before allowing the
.Fn open
to succeed.
Specifying O_APPEND causes writes to automatically append to the file.
Specifying O_TRUNC causes the file to be truncated when opened.
The flag O_CREAT causes the file to be created if it does not
exist, owned by the current user
and the group of the containing directory.
The protection for the new file is specified in \fImode\fP.
The file mode is used as a three digit octal number.
Each digit encodes read access as 4, write access as 2 and execute
access as 1, or'ed together.  The 0700 bits describe owner
access, the 070 bits describe the access rights for processes in the same
group as the file, and the 07 bits describe the access rights
for other processes.
The process \fIumask\fP clears specified permissions.
The \fIumask\fP can be changed with the call:
.DS
.Fd umask 1 "set file creation mode mask
oldmask = umask(newmask);
result mode_t oldmask; mode_t newmask;
.DE
.PP
If the open specifies to create the file with O_EXCL
and the file already exists, then the
.Fn open
will fail without affecting the file in any way.
This mechanism provides a simple exclusive access facility.
If the file exists but is a symbolic link, the open will fail
regardless of the existence of the file specified by the link.
The O_SHLOCK and O_EXLOCK allow the file to be atomically
.Fn open 'ed
and
.Fn flock 'ed;
see section
.Xr 2.2.8
for the semantics of
.Fn flock
style locks.
.Sh 4 "Creating references to devices
.PP
The filesystem allows entries which reference peripheral devices.
Peripherals are distinguished as \fIblock\fP or \fIcharacter\fP
devices according by their ability to support block-oriented
operations.
Devices are identified by their ``major'' and ``minor''
device numbers.  The major device number determines the kind
of peripheral it is, while the minor device number indicates
one of possibly many peripherals of that kind.
Structured devices have all operations done internally
in ``block'' quantities while
unstructured devices often have a number of
special
.Fn ioctl
operations, and may have input and output
done in varying units.
The
.Fn mknod
call creates special entries:
.DS
.Fd mknod 3 "make a special file node
mknod(path, mode, dev);
char *path; mode_t mode; dev_t dev;
.DE
where \fImode\fP is formed from the object type
and access permissions.  The parameter \fIdev\fP is a configuration
dependent parameter used to identify specific character or
block I/O devices.
.LP
Fifo's can be created in the filesystem using the call:
.DS
.Fd mkfifo 2 "make a fifo file
mkfifo(path, mode);
char *path; mode_t mode;
.DE
The \fImode\fP parameter is used solely specify the access permissions
on the newly created fifo.
.Sh 4 "File, device, and fifo removal
.LP
A reference to a file, special device or fifo may be removed with the
.Fn unlink
call:
.DS
.Fd unlink 1 "remove directory entry
unlink(path);
char *path;
.DE
The caller must have write access to the directory in which
the file is located for this call to be successful.
.LP
All current access to a file can be revoked using the call:
.DS
.Fd revoke 1 "revoke file access
revoke(path);
char *path;
.DE
Subsequent operations on any descriptors open at the time of the
.Fn revoke
fail, with the exceptions that a
.Fn read
from a character device file which has been
revoked returns a count of zero (end of file), and a
.Fn close
call will succeed.
If the file is a special file for a device which is open,
the device close function is called as if all open references
to the file had been closed.
.Fn Open 's
done after the
.Fn revoke
will succeed.
This call is most useful for revoking access to a terminal line after
a hangup in preparation for reuse by a new login session.
.Sh 3 "Reading and modifying file attributes
.LP
Detailed information about the attributes of a file
may be obtained with the calls:
.DS
.Fd stat 2 "get file status
stat(path, stb);
char *path; result struct stat *stb;
.DE
.DS
.Fd fstat 2 "get file status
fstat(fd, stb);
int fd; result struct stat *stb;
.DE
The \fIstat\fP structure includes the file
type, protection, ownership, access times,
size, and a count of hard links.
If the file is a symbolic link, then the status of the link
itself (rather than the file the link references)
may be found using the
.Fn lstat
call:
.DS
.Fd lstat 2 "get file status
lstat(path, stb);
char *path; result struct stat *stb;
.DE
.PP
Newly created files are assigned the user-id of the
process that created it and the group-id of the directory
in which it was created.  The ownership of a file may
be changed by either of the calls:
.DS
.Fd chown 3 "change owner and group of a file
chown(path, owner, group);
char *path; int owner, group;
.DE
.DS
.Fd fchown 3 "change owner and group of a file
fchown(fd, owner, group);
int fd, owner, group;
.DE
.PP
In addition to ownership, each file has three levels of access
protection associated with it.  These levels are owner relative,
group relative, and global (all users and groups).  Each level
of access has separate indicators for read permission, write
permission, and execute permission.
The protection bits associated with a file may be set by either
of the calls:
.DS
.Fd chmod 2 "change mode of file
chmod(path, mode);
char *path; int mode;
.DE
.DS
.Fd fchmod 2 "change mode of file
fchmod(fd, mode);
int fd, mode;
.DE
where \fImode\fP is a value indicating the new protection
of the file, as listed in section
.Xr 2.2.3.2 .
.PP
Each file has a set of thirty-two flags associated with it.
These flags are returned in the \fIstat\fP structure and
are set using the calls:
.DS
.Fd chflags 2 "set file flags
chflags(path, flags);
char *path; u_long flags;
.DE
.DS
.Fd fchflags 2 "set file flags
fchflags(fd, flags);
int fd; u_long flags;
.DE
The flags specified are formed by or'ing the following values:
.DS
.TS
l l.
UF_NODUMP	Do not dump the file.
UF_IMMUTABLE	The file may not be changed.
UF_APPEND	The file may only be appended to.
SF_IMMUTABLE	The file may not be changed.
SF_APPEND	The file may only be appended to.
.TE
.DE
The UF_IMMUTABLE and UF_APPEND
flags may be set or unset by either the owner of a file or the super-user.
The SF_IMMUTABLE and SF_APPEND
flags may only be set or unset by the super-user.
They may be set at any time, but normally may only be unset when
the system is in single-user mode.
.LP
Finally, the access and modify times on a file may be set by the call:
.DS
.Fd utimes 2 "set file access and modification times
utimes(path, tvp)
char *path; struct timeval *tvp[2];
.DE
This is particularly useful when moving files between media,
to preserve relationships between the times the file was modified.
.Sh 3 "Links and renaming
.PP
Links allow multiple names for a file to exist.
Links exist independently of the file to which they are linked.
.PP
Two types of links exist, \fIhard\fP links and \fIsymbolic\fP
links.  A hard link is a reference counting mechanism that
allows a file to have multiple names within the same filesystem.
Symbolic links cause string substitution
during the pathname interpretation process.
.PP
Hard links and symbolic links have different
properties.  A hard link ensures the target
file will always be accessible, even after its original
directory entry is removed; no such guarantee exists for a symbolic link.
Symbolic links can span filesystems boundaries.
.LP
The following calls create a new link, named \fIpath2\fP,
to \fIpath1\fP:
.DS
.Fd link 2 "make a hard file link
link(path1, path2);
char *path1, *path2;
.DE
.DS
.Fd symlink 2 "make a symbolic link to a file
symlink(path1, path2);
char *path1, *path2;
.DE
The
.Fn unlink
primitive may be used to remove
either type of link. 
.LP
If a file is a symbolic link, the ``value'' of the
link may be read with the
.Fn readlink
call:
.DS
.Fd readlink 3 "read value of a symbolic link
len = readlink(path, buf, bufsize);
result int len; char *path; result char *buf; int bufsize;
.DE
This call returns, in \fIbuf\fP, the null-terminated string
substituted into pathnames passing through \fIpath\fP\|.
.LP
Atomic renaming of filesystem resident objects is possible with the
.Fn rename
call:
.DS
.Fd rename 2 "change the name of a file
rename(oldname, newname);
char *oldname, *newname;
.DE
where both \fIoldname\fP and \fInewname\fP must be
in the same filesystem.
If \fInewname\fP exists and is a directory, then it must be empty.
.Sh 3 "Extension and truncation
.PP
Files are created with zero length and may be extended
simply by writing or appending to them.  While a file is
open the system maintains a pointer into the file
indicating the current location in the file associated with
the descriptor.  This pointer may be moved about in the
file in a random access fashion.
To set the current offset into a file, the
.Fn lseek
call may be used:
.DS
.Fd lseek 3 "reposition read/write file offset
oldoffset = lseek(fd, offset, type);
result off_t oldoffset; int fd; off_t offset; int type;
.DE
where \fItype\fP is given in \fI<sys/unistd.h>\fP as one of:
.DS
.TS
l l.
SEEK_SET	/* set file offset to offset */
SEEK_CUR	/* set file offset to current plus offset */
SEEK_END	/* set file offset to EOF plus offset */
.TE
.DE
The call ``lseek(fd, 0, SEEK_CUR)''
returns the current offset into the file.
.PP
Files may have ``holes'' in them.  Holes are void areas in the
linear extent of the file where data has never been
written.  These may be created by seeking to
a location in a file past the current end-of-file and writing.
Holes are treated by the system as zero valued bytes.
.LP
A file may be truncated with either of the calls:
.DS
.Fd truncate 2 "truncate a file to a specified length
truncate(path, length);
char *path; off_t length;
.DE
.DS
.Fd ftruncate 2 "truncate a file to a specified length
ftruncate(fd, length);
int fd; off_t length;
.DE
reducing the size of the specified file to \fIlength\fP bytes.
.PP
Unless opened with the O_FSYNC flag,
writes to files are held for an indeterminate period of time
in the system buffer cache.
The call:
.DS
.Fd fsync 1 "synchronize in-core state of a file with that on disk
fsync(fd)
int fd;
.DE
ensures that the contents of a file are committed to disk
before returning.
This feature is used by applications such as editors that
want to ensure the integrity of a new file before
deleting the backup copy.
.Sh 3 "Checking accessibility
.PP
A process running with
different real and effective user-ids
may interrogate the accessibility of a file to the
real user by using the
.Fn access
call:
.DS
.Fd access 2 "check access permissions of a file or pathname
accessible = access(path, how);
result int accessible; char *path; int how;
.DE
Here \fIhow\fP is constructed by or'ing the following bits, defined
in \fI<unistd.h>\fP:
.DS
.TS
l l.
F_OK	/* file exists */
X_OK	/* file is executable */
W_OK	/* file is writable */
R_OK	/* file is readable */
.TE
.DE
The presence or absence of advisory locks does not affect the
result of
.Fn access .
.PP
The
.Fn pathconf
and
.Fn fpathconf
functions provides a method for applications to determine the current
value of a configurable system limit or option variable associated
with a pathname or file descriptor:
.DS
.Fd pathconf 2 "get configurable pathname variables
ans = pathconf(path, name)
result int ans; char *path; int name;
.DE
.DS
.Fd fpathconf 2 "get configurable pathname variables
ans = fpathconf(fd, name)
result int ans; int fd, name;
.DE
For
.Fn pathconf ,
the \fIpath\fP argument is the name of a file or directory.
For
.Fn fpathconf ,
the \fIfd\fP argument is an open file descriptor.
The \fIname\fP argument specifies the system variable to be queried.
Symbolic constants for each name value are found in the include file
\fI<unistd.h>\fP.
.Sh 3 "Locking
.PP
The filesystem provides basic facilities that allow cooperating processes
to synchronize their access to shared files.  A process may
place an advisory \fIread\fP or \fIwrite\fP lock on a file,
so that other cooperating processes may avoid interfering
with the process' access.  This simple mechanism
provides locking with file granularity.
Byte range locking is available with
.Fn fcntl ;
see section
.Xr 1.5.4 .
The system does not force processes to obey the locks;
they are of an advisory nature only.
.LP
Locking can be done as part of the
.Fn open
call (see section
.Xr 2.2.3.2 )
or after an
.Fn open
call by applying the
.Fn flock
primitive:
.DS
.Fd flock 2 "apply or remove an advisory lock on an open file
flock(fd, how);
int fd, how;
.DE
where the \fIhow\fP parameter is formed from bits
defined in \fI<sys/fcntl.h>\fP:
.DS
.TS
l l.
LOCK_SH	/* shared file lock */
LOCK_EX	/* exclusive file lock */
LOCK_NB	/* don't block when locking */
LOCK_UN	/* unlock file */
.TE
.DE
Successive lock calls may be used to increase or
decrease the level of locking.  If an object is currently
locked by another process when a
.Fn flock
call is made, the caller will be blocked until the current lock owner
releases the lock; this may be avoided by including LOCK_NB
in the \fIhow\fP parameter.
Specifying LOCK_UN removes all locks associated with the descriptor.
Advisory locks held by a process are automatically deleted when
the process terminates.
.Sh 3 "Disk quotas
.PP
As an optional facility, each local filesystem may be requested to
impose limits on a user's or a group's disk usage.
Two quantities are limited: the total amount of disk space which
a user or group may allocate in a filesystem and the total number of files
a user or group may create in a filesystem.  Quotas are expressed as
\fIhard\fP limits and \fIsoft\fP limits.  A hard limit is
always imposed; if a user or group would exceed a hard limit, the operation
which caused the resource request will fail.  A soft limit results
in the user or group receiving a warning message,
but with allocation succeeding.
Facilities are provided to turn soft limits into hard limits if a
user or group has exceeded a soft limit for an unreasonable period of time.
.LP
The
.Fn quotactl
call enables, disables and manipulates filesystem quotas:
.DS
.Fd quotactl 4 "manipulate filesystem quotas
quotactl(path, cmd, id, addr)
char *path; int cmd; int id; char *addr;
.DE
A quota control command given by cmd operates on the given filename path
for the given user id. The address of an optional command specific data
structure, addr, may be given.
The supported commands include:
.DS
.TS
l l.
Q_QUOTAON	/* enable quotas */
Q_QUOTAOFF	/* disable quotas */
Q_GETQUOTA	/* get limits and usage */
Q_SETQUOTA	/* set limits and usage */
Q_SETUSE	/* set usage */
Q_SYNC	/* sync disk copy of a filesystems quotas */
.TE
.DE
.Sh 3 "Remote filesystems
.LP
There are two system calls intended to help support remote filesystems.
The call:
.DS
.Fd nfssvc 2 "NFS services
nfssvc(flags, argstructp);
int flags, void *argstructp;
.DE
is used by the NFS daemons to pass information into
and out of the kernel and also to enter the kernel as a server daemon.
The flags argument consists of several bits that show what action is to
be taken once in the kernel and the argstructp points to one of three
structures depending on which bits are set in flags.
.LP
The call:
.DS
.Fd getfh 2 "get file handle
getfh(path, fhp);
char *path; result fhandle_t *fhp;
.DE
returns a file handle for the specified file or directory in the
file handle pointed to by fhp.
This file handle can then be used in future calls to NFS to access 
the file without the need to repeat the pathname translation.
This system call is restricted to the superuser.
.Sh 3 "Other filesystems
.LP
The kernel supports many other filesystems.
These include:
.IP \(bu
The log-structured filesystem. It provides an alternate disk
layout than the fast filesystem optimized for writing rather
than reading.
For further information see the
.Xr mount_lfs (8)
manual page.
.\"
.\" We currently do not document the LFS calls
.\" .Fd lfs_bmapv 3
.\" .Fd lfs_markv 3
.\" .Fd lfs_segclean 2
.\" .Fd lfs_segwait 2
.IP \(bu
The ISO-standard 9660 filesystem with Rock Ridge extensions used for CD-ROMs.
For further information see the
.Xr mount_cd9660 (8)
manual page.
.IP \(bu
The file descriptor mapping filesystem.
For further information see the
.Xr mount_fdesc (8)
manual page.
.IP \(bu
The /proc filesystem as an alternative for debuggers.
For further information see section
.Xr 2.5.1
and the
.Xr mount_procfs (8)
manual page.
.IP \(bu
The memory-based filesystem,
used primarily for fast but ethereal uses such as /tmp.
For further information see the
.Xr mount_mfs (8)
manual page.
.IP \(bu
The kernel variable filesystem, used as an alternative to
.Fn sysctl .
For further information see section
.Xr 1.7.1
and the
.Xr mount_kernfs (8)
manual page.
.IP \(bu
The portal filesystem, used to mount processes in the filesystem.
For further information see the
.Xr mount_portal (8)
manual page.
.IP \(bu
The uid/gid remapping filesystem, used primary above NFS filesystems
exported to an outside administrative domain.
For further information see the
.Xr mount_umap (8)
manual page.
.IP \(bu
The union filesystem, used to place a writable filesystem above
a read-only filesystem.
This filesystem is useful for compiling sources on a CD-ROM
without having to copy the CD-ROM contents to writable disk.
For further information see the
.Xr mount_union (8)
manual page.
