.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)sys.ufs.t	1.5 (Berkeley) 4/11/86
.\"
.NH
Changes in the filesystem
.PP
The major change in the filesystem was the addition of a name translation
cache.
A table of recent name-to-inode translations is maintained by \fInamei\fP,
and used as a lookaside cache when translating each component of each
file pathname.
Each \fInamecache\fP entry contains the parent directory's device and inode,
the length of the name, and the name itself, and is hashed on the name.
It also contains a pointer to the inode for the file whose name it contains.
Unlike most inode pointers, which hold a ``hard'' reference
by incrementing the reference count,
the name cache holds a ``soft'' reference, a pointer to an inode
that may be reused.
In order to validate the inode from a name cache reference,
each inode is assigned a unique ``capability'' when it is brought
into memory.
When the inode entry is reused for another file,
or when the name of the file is changed,
this capability is changed.
This allows the inode cache to be handled normally,
releasing inodes at the head of the LRU list without regard for name
cache references,
and allows multiple names for the same inode to be in the cache simultaneously
without complicating the invalidation procedure.
An additional feature of this scheme is that when opening
a file, it is possible to determine whether the file was previously open.
This is useful when beginning execution of a file, to check whether
the file might be open for writing, and for similar situations.
.PP
Other changes that are visible throughout the filesystem
include greater use of the ILOCK and IUNLOCK macros rather than the
subroutine equivalents.
The inode times are updated on each \fIirele\fP, not only when
the reference count reaches zero,
if the IACC, IUPD or ICHG flags are set.
This is accomplished with the ITIMES macro;
the inode is marked as modified with the new IMOD flag,
that causes it to be written to disk when released, or on the next sync.
.PP
The remainder of this section describes the filesystem changes that are
localized to individual files.
.XP ufs_alloc.c
The algorithm for extending file fragments was changed
to take advantage of the observation that fragments that were once extended
were frequently extended again, that is, that the file was being written
in fragments.
Therefore, the first time a given fragment is allocated,
a best-fit strategy is used.
Thereafter, when this fragment is to be extended,
a full-sized block is allocated, the fragment removed from it,
and the remainder freed for use in subsequent expansion.
As this policy may result in increased fragmentation,
it is not used when the filesystem becomes excessively
fragmented (i.e. when the number of free fragments falls to 2%
of the minfree value);
the policy is stored in the superblock and may be changed with \fItunefs\fP.
The \fIfserr\fP routine was converted to use \fIlog\fP rather than \fIprintf\fP.
.XP ufs_bio.c
I/O operations traced now include the size where relevant.
.XP ufs_inode.c
The size of the buffer hash table was increased substantially
and changed to a power of two to allow the modulus to be computed with a mask
operation.
\fIIget\fP invalidates the capability in each inode that is flushed
from the inode cache for reuse.
The new \fIigrab\fP routine is used instead of \fIiget\fP
when fetching an inode from a name cache reference;
it waits for the inode to be unlocked if necessary,
and removes it from the free list if it was free.
The caller must check that the inode is still valid after the \fIigrab\fP.
A bug was fixed in \fIitrunc\fP that allowed old contents to creep back into
a file.
When truncating to a location within a block,
\fIitrunc\fP must clear the remainder of the block.
Otherwise, if the file is extended by seeking past the end of file
and then writing, the old contents reappear.
.\" \fIItrunc\fP also waits for 
.XP ufs_mount.c
The \fImount\fP system call was modified to return different error numbers
for different types of errors.
\fIMount\fP now examines the superblock more carefully
before using size field it contains as the amount to copy into a new buffer.
If a mount fails for a reason other than the device already being
mounted, the device is closed again.
When performing the name lookup for the mount point,
\fImount\fP must prevent the name translation from being left
in the name cache;
\fIumount\fP must flush all name translations for the device.
A bug in \fIgetmdev\fP caused an inode to remain locked
if the specified device was not a block special file; this has been fixed.
.XP ufs_namei.c
This file was previously called ufs_nami.c.
The \fInamei\fP function has a new calling convention
with its arguments, associated context, and side effects
encapsulated in a single structure.
It has been extensively modified to implement the name cache
and to cache directory offsets for each process.
It may now return ENAMETOOLONG when appropriate,
and returns EINVAL if the 8th bit is set on one of the pathname
characters.
Directories may be foreshortened if the last one or more blocks
contain no entries;
this is done when files are being created, as the entire directory 
must already be searched.
An entry is provided for invalidating the entire name cache
when the 32-bit prototype for capabilities wraps around.
This is expected to happen after 13 months of operation,
assuming 100 name lookups per second, all of which miss the cache.
.XP
A change in filesystem semantics is the introduction
of ``sticky'' directories.
If the ISVTX (sticky text) bit is set in the mode of a directory,
files may only be removed from that directory by the owner of the file,
the owner of the directory, or the superuser.
This is enforced by \fInamei\fP when the lookup operation is DELETE. 
.XP ufs_subr.c
The strategy for \fIsyncip\fP, the internal routine implementing \fIfsync\fP,
has been modified for large files (those larger than half of the buffer
cache).
For large files all modified buffers for the device are written out.
The old algorithm could run for a very long time on a very large file,
that might not actually have many data blocks.
The \fIupdate\fP routine now saves some work by calling \fIiupdate\fP
only for modified inodes.
The C replacements for the special VAX instructions have been collected
in this file.
.XP ufs_syscalls.c
When doing an open with flags O_CREAT and O_EXCL (create only if the file
did not exist), it is now considered to be an error if the target exists
and is a symbolic link, even if the symbolic link refers to a nonexistent
file.
This behavior is desirable for reasons of security
in programs that create files with predictable names.
\fIRename\fP follows the policy of \fInamei\fP in disallowing removal
of the target of a rename if the target directory is ``sticky''
and the user is not the owner of the target or the target directory.
A serious bug in the open code which allowed directories and other unwritable
files to be truncated has been corrected.
Interrupted opens no longer lose file descriptors.
The \fIlseek\fP call returns an ESPIPE error when seeking on sockets
(including pipes) for backward compatibility.
The error returned from \fIreadlink\fP when reading something other than
a symbolic link was changed from ENXIO to EINVAL.
Several calls that previously failed silently on read-only filesystems
(\fIchmod\fP, \fIchown\fP, \fIfchmod\fP, \fIfchown\fP and \fIutimes\fP)
now return EROFS.
The \fIrename\fP code was reworked to avoid several races
and to invalidate the name cache.
It marks a directory being renamed with IRENAME
to avoid races due to concurrent renames of the same directory.
\fIMkdir\fP now sets the size of all new directories to DIRBLKSIZE.
\fIRmdir\fP purges the name cache of entries for the removed directory.
.XP ufs_xxx.c
The routines \fIuchar\fP and \fIschar\fP are no longer used
and have been removed.
.XP quota_kern.c
The quota hash size was changed to a power of 2 so that the modulus could
be computed with a mask.
.XP quota_ufs.c
If a user has run out of warnings and had the hard limit enforced
while logged in,
but has then brought his allocation below the hard limit,
the quota system reverts to enforcing the soft limit,
and resets the warning count;
users previously were required to log out and in again to
get this affect.
