.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)5.t	5.1 (Berkeley) %G%
.\"
.ds RH Functional enhancements
.NH 
File system functional enhancements
.PP
The speed enhancements to the UNIX file system did not require
any changes to the semantics or data structures viewed by the
users.
However several changes have been generally desired for some 
time but have not been introduced because they would require users to 
dump and restore all their file systems.
Since the new file system already
requires that all existing file systems
be dumped and restored, 
these functional enhancements have been introduced at this time.
.NH 2
Long file names
.PP
File names can now be of nearly arbitrary length.
The only user programs affected by this change are
those that access directories.
To maintain portability among UNIX systems that
are not running the new file system, a set of directory
access routines have been introduced that provide a uniform
interface to directories on both old and new systems.
.PP
Directories are allocated in units of 512 bytes.
This size is chosen so that each allocation can be transferred
to disk in a single atomic operation.
Each allocation unit contains variable-length directory entries.
Each entry is wholly contained in a single allocation unit.
The first three fields of a directory entry are fixed and contain
an inode number, the length of the entry, and the length
of the name contained in the entry.
Following this fixed size information is the null terminated name,
padded to a 4 byte boundary.
The maximum length of a name in a directory is currently 255 characters.
.PP
Free space in a directory is held by
entries that have a record length that exceeds the space
required by the directory entry itself.
All the bytes in a directory unit are claimed by the directory entries.
This normally results in the last entry in a directory being large.
When entries are deleted from a directory,
the space is returned to the previous entry in the same directory
unit by increasing its length.
If the first entry of a directory unit is free, then its 
inode number is set to zero to show that it is unallocated.
.NH 2
File locking
.PP
The old file system had no provision for locking files.
Processes that needed to synchronize the updates of a
file had to create a separate ``lock'' file to synchronize
their updates.
A process would try to create a ``lock'' file. 
If the creation succeeded, then it could proceed with its update;
if the creation failed, then it would wait, and try again.
This mechanism had three drawbacks.
Processes consumed CPU time, by looping over attempts to create locks.
Locks were left lying around following system crashes and had
to be cleaned up by hand.
Finally, processes running as system administrator
are always permitted to create files,
so they had to use a different mechanism.
While it is possible to get around all these problems,
the solutions are not straight-forward,
so a mechanism for locking files has been added.
.PP
The most general schemes allow processes to concurrently update a file.
Several of these techniques are discussed in [Peterson83].
A simpler technique is to simply serialize access with locks.
To attain reasonable efficiency,
certain applications require the ability to lock pieces of a file.
Locking down to the byte level has been implemented in the
Onyx file system by [Bass81].
However, for the applications that currently run on the system,
a mechanism that locks at the granularity of a file is sufficient.
.PP
Locking schemes fall into two classes,
those using hard locks and those using advisory locks.
The primary difference between advisory locks and hard locks is the
decision of when to override them. 
A hard lock is always enforced whenever a program tries to
access a file;
an advisory lock is only applied when it is requested by a program.
Thus advisory locks are only effective when all programs accessing
a file use the locking scheme.
With hard locks there must be some override policy implemented in the kernel,
with advisory locks the policy is implemented by the user programs.
In the UNIX system, programs with system administrator
privilege can override any protection scheme.
Because many of the programs that need to use locks run as
system administrators,
we chose to implement advisory locks rather than 
create a protection scheme that was contrary to the UNIX 
philosophy or could not be used by system administration
programs.
.PP
The file locking facilities allow cooperating programs to apply
advisory
.I shared
or
.I exclusive
locks on files.
Only one process has an exclusive
lock on a file while multiple shared locks may be present.
Both shared and exclusive locks cannot be present on
a file at the same time.
If any lock is requested when
another process holds an exclusive lock,
or an exclusive lock is requested when another process holds any lock,
the open will block until the lock can be gained.
Because shared and exclusive locks are advisory only,
even if a process has obtained a lock on a file,
another process can override the lock by
opening the same file without a lock.
.PP
Locks can be applied or removed on open files,
so that locks can be manipulated without
needing to close and reopen the file.
This is useful, for example, when a process wishes
to open a file with a shared lock to read some information,
to determine whether an update is required.
It can then get an exclusive lock so that it can do a read,
modify, and write to update the file in a consistent manner.
.PP
A request for a lock will cause the process to block if the lock
can not be immediately obtained.
In certain instances this is unsatisfactory.
For example, a process that
wants only to check if a lock is present would require a separate
mechanism to find out this information.
Consequently, a process may specify that its locking
request should return with an error if a lock can not be immediately
obtained.
Being able to poll for a lock is useful to ``daemon'' processes
that wish to service a spooling area.
If the first instance of the
daemon locks the directory where spooling takes place,
later daemon processes can
easily check to see if an active daemon exists.
Since the lock is removed when the process exits or the system crashes,
there is no problem with unintentional locks files
that must be cleared by hand.
.PP
Almost no deadlock detection is attempted.
The only deadlock detection made by the system is that the file
descriptor to which a lock is applied does not currently have a
lock of the same type (i.e. the second of two successive calls
to apply a lock of the same type will fail).
Thus a  process can deadlock itself by
requesting locks on two separate file descriptors for the same
object.
.NH 2
Symbolic links
.PP
The 512 byte UNIX file system allows multiple
directory entries in the same file system
to reference a single file.
The link concept is fundamental;
files do not live in directories, but exist separately and
are referenced by links.
When all the links are removed,
the file is deallocated.
This style of links does not allow references across physical file
systems, nor does it support inter-machine linkage. 
To avoid these limitations
.I "symbolic links"
have been added similar to the scheme used by Multics [Feiertag71].
.PP
A symbolic link is implemented as a file that contains a pathname.
When the system encounters a symbolic link while
interpreting a component of a pathname,
the contents of the symbolic link is prepended to the rest
of the pathname, and this name is interpreted to yield the
resulting pathname.
If the symbolic link contains an absolute pathname,
the absolute pathname is used,
otherwise the contents of the symbolic link is evaluated
relative to the location of the link in the file hierarchy.
.PP
Normally programs do not want to be aware that there is a
symbolic link in a pathname that they are using.
However certain system utilities
must be able to detect and manipulate symbolic links.
Three new system calls provide the ability to detect, read, and write
symbolic links, and seven system utilities were modified to use these calls.
.PP
In future Berkeley software distributions 
it will be possible to mount file systems from other
machines within a local file system.
When this occurs,
it will be possible to create symbolic links that span machines.
.NH 2
Rename
.PP
Programs that create new versions of data files typically create the
new version as a temporary file and then rename the temporary file
with the original name of the data file.
In the old UNIX file systems the renaming required three calls to the system.
If the program were interrupted or the system crashed between
these calls,
the data file could be left with only its temporary name.
To eliminate this possibility a single system call
has been added that performs the rename in an
atomic fashion to guarantee the existence of the original name.
.PP
In addition, the rename facility allows directories to be moved around
in the directory tree hierarchy.
The rename system call performs special validation checks to insure
that the directory tree structure is not corrupted by the creation
of loops or inaccessible directories.
Such corruption would occur if a parent directory were moved
into one of its descendants.
The validation check requires tracing the ancestry of the target
directory to insure that it does not include the directory being moved.
.NH 2
Quotas
.PP
The UNIX system has traditionally attempted to share all available
resources to the greatest extent possible.
Thus any single user can allocate all the available space
in the file system.
In certain environments this is unacceptable.
Consequently, a quota mechanism has been added for restricting the
amount of file system resources that a user can obtain.
The quota mechanism sets limits on both the number of files
and the number of disk blocks that a user may allocate.
A separate quota can be set for each user on each file system.
Each resource is given both a hard and a soft limit.
When a program exceeds a soft limit,
a warning is printed on the users terminal;
the offending program is not terminated
unless it exceeds its hard limit.
The idea is that users should stay below their soft limit between
login sessions,
but they may use more space while they are actively working.
To encourage this behavior,
users are warned when logging in if they are over
any of their soft limits.
If they fail to correct the problem for too many login sessions,
they are eventually reprimanded by having their soft limit
enforced as their hard limit.
.ds RH Software engineering
.bp
