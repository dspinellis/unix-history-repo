.\" Copyright (c) 1982 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)4.t	4.1 (Berkeley) %G%
.\"
.ds RH Appendix A \- Fsck Error Conditions
.NH
Appendix A \- Fsck Error Conditions
.NH 2 
Conventions
.PP
.I Fsck
is
a multi-pass file system check program.
Each file system pass invokes a different Phase of the
.I fsck
program.
After the initial setup,
.I fsck
performs successive Phases over each file system,
checking blocks and sizes,
path-names,
connectivity,
reference counts,
and the map of free blocks,
(possibly rebuilding it),
and performs some cleanup.
.LP
Normally
.I fsck
is run non-interactively to
.I preen
the file systems after an unclean halt.
While preen'ing a file system,
it will only fix corruptions that are expected
to occur from an unclean halt.
These actions are a proper subset of the actions that 
.I fsck
will take when it is running interactively.
Throughout this appendix many errors have several options
that the operator can take.
When an inconsistency is detected,
.I fsck
reports the error condition to the operator.
If a response is required,
.I fsck
prints a prompt message and
waits for a response.
When preen'ing most errors are fatal.
For those that are expected,
the response taken is noted.
This appendix explains the meaning of each error condition,
the possible responses, and the related error conditions.
.LP
The error conditions are organized by the
.I Phase
of the
.I fsck
program in which they can occur.
The error conditions that may occur
in more than one Phase
will be discussed in initialization.
.NH 2 
Initialization
.PP
Before a file system check can be performed, certain
tables have to be set up and certain files opened.
This section concerns itself with the opening of files and
the initialization of tables.
This section lists error conditions resulting from
command line options,
memory requests,
opening of files,
status of files,
file system size checks,
and creation of the scratch file.
All of the initialization errors are fatal
when the file system is being preen'ed.
.sp
.LP
.B "\fIC\fP option?"
.br
\fIC\fP is not a legal option to
.I fsck ;
legal options are \-b, \-y, \-n, and \-p.
.I Fsck
terminates on this error condition.
See the
.I fsck (8)
manual entry for further detail.
.sp
.LP
.B "cannot alloc NNN bytes for blockmap"
.br
.B "cannot alloc NNN bytes for freemap"
.br
.B "cannot alloc NNN bytes for statemap"
.br
.B "cannot alloc NNN bytes for lncntp"
.br
.I Fsck 's
request for memory for its virtual
memory tables failed.
This should never happen.
.I Fsck
terminates on this error condition.
See a guru.
.sp
.LP
.B "Can't open checklist file: \fIF\fP"
.br
The file system checklist file
\fIF\fP (usually
.I /etc/fstab )
can not be opened for reading.
.I Fsck
terminates on this error condition.
Check access modes of \fIF\fP.
.sp
.LP
.B "Can't stat root"
.br
.I Fsck 's
request for statistics about the root directory ``/'' failed.
This should never happen.
.I Fsck
terminates on this error condition.
See a guru.
.sp
.LP
.B "Can't stat \fIF\fP"
.br
.B "Can't make sense out of name \fIF\fP"
.br
.I Fsck 's
request for statistics about the file system \fIF\fP failed.
When running manually,
it ignores this file system
and continues checking the next file system given.
Check access modes of \fIF\fP.
.sp
.LP
.B "Can't open \fIF\fP"
.br
.I Fsck 's
request attempt to open the file system \fIF\fP failed.
When running manually, it ignores this file system
and continues checking the next file system given.
Check access modes of \fIF\fP.
.sp
.LP
.B "\fIF\fP: (NO WRITE)"
.br
Either the \-n flag was specified or
.I fsck 's
attempt to open the file system \fIF\fP for writing failed.
When running manually,
all the diagnostics are printed out,
but no modifications are attempted to fix them.
.sp
.LP
.B "file is not a block or character device; OK"
.br
You have given
.I fsck
a regular file name by mistake.
Check the type of the file specified.
.LP
Possible responses to the OK prompt are:
.IP YES
Ignore this error condition.
.IP NO
ignore this file system and continues checking
the next file system given.
.sp
.LP
One of the following messages will appear:
.br
.B "MAGIC NUMBER WRONG"
.br
.B "NCG OUT OF RANGE"
.br
.B "CPG OUT OF RANGE"
.br
.B "NCYL DOES NOT JIVE WITH NCG*CPG"
.br
.B "SIZE PREPOSTEROUSLY LARGE"
.br
.B "TRASHED VALUES IN SUPER BLOCK"
.sp
and will be followed by the message:
.br
.B "\fIF\fP: BAD SUPER BLOCK: \fIB\fP"
.br
.B "USE -b OPTION TO FSCK TO SPECIFY LOCATION OF AN ALTERNATE"
.br
.B "SUPER-BLOCK TO SUPPLY NEEDED INFORMATION; SEE fsck(8)."
.br
The super block has been corrupted. 
An alternative super block must be selected from among those
listed by
.I newfs
(8) when the file system was created.
For file systems with a blocksize less than 32K,
specifying \-b 32 is a good first choice.
.sp
.LP
.B "INTERNAL INCONSISTENCY: \fIM\fP"
.br
.I Fsck 's
has had an internal panic, whose message is specified as \fIM\fP.
This should never happen.
See a guru.
.sp
.LP
.B "CAN NOT SEEK: BLK \fIB\fP (CONTINUE)"
.br
.I Fsck 's
request for moving to a specified block number \fIB\fP in
the file system failed.
This should never happen.
See a guru.
.LP
Possible responses to the CONTINUE prompt are:
.IP YES
attempt to continue to run the file system check.
Often,
however the problem will persist.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck
should be made to re-check this file system.
If the block was part of the virtual memory buffer
cache,
.I fsck
will terminate with the message ``Fatal I/O error''.
.IP NO
terminate the program.
.sp
.LP
.B "CAN NOT READ: BLK \fIB\fP (CONTINUE)"
.br
.I Fsck 's
request for reading a specified block number \fIB\fP in
the file system failed.
This should never happen.
See a guru.
.LP
Possible responses to the CONTINUE prompt are:
.IP YES
attempt to continue to run the file system check.
Often,
however,
the problem will persist.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck
should be made to re-check this file system.
If the block was part of the virtual memory buffer
cache,
.I fsck
will terminate with the message ``Fatal I/O error''.
.IP NO
terminate the program.
.sp
.LP
.B "CAN NOT WRITE: BLK \fIB\fP (CONTINUE)"
.br
.I Fsck 's
request for writing a specified block number \fIB\fP
in the file system failed.
The disk is write-protected.
See a guru.
.LP
Possible responses to the CONTINUE prompt are:
.IP YES
attempt to continue to run the file system check.
Often,
however,
the problem will persist.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck
should be made to re-check this file system.
If the block was part of the virtual memory buffer
cache,
.I fsck
will terminate with the message ``Fatal I/O error''.
.IP NO
terminate the program.
.NH 2 
Phase 1 \- Check Blocks and Sizes
.PP
This phase concerns itself with
the inode list.
This section lists error conditions resulting from
checking inode types,
setting up the zero-link-count table,
examining inode block numbers for bad or duplicate blocks,
checking inode size,
and checking inode format.
All errors in this phase except
.B "INCORRECT BLOCK COUNT"
are fatal if the file system is being preen'ed,
.sp
.LP
.B "CG \fIC\fP: BAD MAGIC NUMBER"
The magic number of cylinder group \fIC\fP is wrong.
This usually indicates that the cylinder group maps have been destroyed.
When running manually the cylinder group is marked as needing
to be reconstructed.
.sp
.LP
.B "UNKNOWN FILE TYPE I=\fII\fP (CLEAR)"
The mode word of the inode \fII\fP indicates that the inode is not a
special block inode, special character inode, socket inode, regular inode,
symbolic link, or directory inode.
.LP
Possible responses to the CLEAR prompt are:
.IP YES
de-allocate inode \fII\fP by zeroing its contents.
This will always invoke the UNALLOCATED error condition in Phase 2
for each directory entry pointing to this inode.
.IP NO
ignore this error condition.
.sp
.LP
.B "LINK COUNT TABLE OVERFLOW (CONTINUE)"
.br
An internal table for
.I fsck
containing allocated inodes with a link count of
zero has no more room.
Recompile
.I fsck
with a larger value of MAXLNCNT.
.LP
Possible responses
to the CONTINUE prompt are:
.IP YES
continue with the program.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck
should be made to re-check this file system.
If another allocated inode with a zero link count is found,
this error condition is repeated.
.IP NO
terminate the program.
.sp
.LP
.B "\fIB\fP BAD I=\fII\fP"
.br
Inode \fII\fP contains block number \fIB\fP with a number
lower than the number of the first data block in the file system or
greater than the number of the last block
in the file system.
This error condition may invoke the
.B "EXCESSIVE BAD BLKS"
error condition in Phase 1 if
inode \fII\fP has too many block numbers outside the file system range.
This error condition will always invoke the
.B "BAD/DUP"
error condition in Phase 2 and Phase 4.
.sp
.LP
.B "EXCESSIVE BAD BLKS I=\fII\fP (CONTINUE)"
.br
There is more than a tolerable number (usually 10) of blocks with a number
lower than the number of the first data block in the file system or greater than
the number of last block in the file system associated with inode \fII\fP.
.LP
Possible responses to the CONTINUE prompt are:
.IP YES
ignore the rest of the blocks in this inode
and continue checking with the next inode in the file system.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck
should be made to re-check this file system.
.IP NO
terminate the program.
.sp
.LP
.B "\fIB\fP DUP I=\fII\fP"
.br
Inode \fII\fP contains block number \fIB\fP which is already claimed by
another inode.
This error condition may invoke the
.B "EXCESSIVE DUP BLKS"
error condition in Phase 1 if
inode \fII\fP has too many block numbers claimed by other inodes.
This error condition will always invoke Phase 1b and the
.B "BAD/DUP"
error condition in Phase 2 and Phase 4.
.sp
.LP
.B "EXCESSIVE DUP BLKS I=\fII\fP (CONTINUE)"
.br
There is more than a tolerable number (usually 10) of blocks claimed by other
inodes.
.LP
Possible responses to the CONTINUE prompt are:
.IP YES
ignore the rest of the blocks in this inode
and continue checking with the next inode in the file system.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck
should be made to re-check this file system.
.IP NO
terminate the program.
.sp
.LP
.B "DUP TABLE OVERFLOW (CONTINUE)"
.br
An internal table in
.I fsck
containing duplicate block numbers has no more room.
Recompile
.I fsck
with a larger value of DUPTBLSIZE.
.LP
Possible responses to the CONTINUE prompt are:
.IP YES
continue with the program.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck
should be made to re-check this file system.
If another duplicate block is found, this error condition will repeat.
.IP NO
terminate the program.
.sp
.LP
.B "PARTIALLY ALLOCATED INODE I=\fII\fP (CLEAR)"
.br
Inode \fII\fP is neither allocated nor unallocated.
.LP
Possible responses to the CLEAR prompt are:
.IP YES
de-allocate inode \fII\fP by zeroing its contents.
.IP NO
ignore this error condition.
.sp
.LP
.B "INCORRECT BLOCK COUNT I=\fII\fP (\fIX\fP should be \fIY\fP) (CORRECT)"
.br
The block count for inode \fII\fP is \fIX\fP blocks,
but should be \fIY\fP blocks.
When preen'ing the count is corrected.
.LP
Possible responses to the CORRECT prompt are:
.IP YES
replace the block count of inode \fII\fP with \fIY\fP.
.IP NO
ignore this error condition.
.NH 2 
Phase 1B: Rescan for More Dups
.PP
When a duplicate block is found in the file system, the file system is
rescanned to find the inode which previously claimed that block.
This section lists the error condition when the duplicate block is found.
.sp
.LP
.B "\fIB\fP DUP I=\fII\fP"
.br
Inode \fII\fP contains block number \fIB\fP that
is already claimed by another inode.
This error condition will always invoke the
.B "BAD/DUP"
error condition in Phase 2.
You can determine which inodes have overlapping blocks by examining
this error condition and the DUP error condition in Phase 1.
.NH 2 
Phase 2 \- Check Pathnames
.PP
This phase concerns itself with removing directory entries
pointing to
error conditioned inodes
from Phase 1 and Phase 1b.
This section lists error conditions resulting from
root inode mode and status,
directory inode pointers in range,
and directory entries pointing to bad inodes.
All errors in this phase are fatal if the file system is being preen'ed.
.sp
.LP
.B "ROOT INODE UNALLOCATED. TERMINATING."
.br
The root inode (usually inode number 2) has no allocate mode bits.
This should never happen.
The program will terminate.
.sp
.LP
.B "NAME TOO LONG \fIF\fP"
.br
An excessively long path name has been found.
This is usually indicative of loops in the file system name space.
This can occur if the super user has made circular links to directories.
The offending links must be removed (by a guru).
.sp
.LP
.B "ROOT INODE NOT DIRECTORY (FIX)"
.br
The root inode (usually inode number 2)
is not directory inode type.
.LP
Possible responses to the FIX prompt are:
.IP YES
replace the root inode's type to be a directory.
If the root inode's data blocks are not directory blocks,
a VERY large number of error conditions will be produced.
.IP NO
terminate the program.
.sp
.LP
.B "DUPS/BAD IN ROOT INODE (CONTINUE)"
.br
Phase 1 or Phase 1b have found duplicate blocks
or bad blocks in the root inode (usually inode number 2) for the file system.
.LP
Possible responses to the CONTINUE prompt are:
.IP YES
ignore the
.B "DUPS/BAD"
error condition in the root inode and
attempt to continue to run the file system check.
If the root inode is not correct,
then this may result in a large number of other error conditions.
.IP NO
terminate the program.
.sp
.LP
.B "I OUT OF RANGE I=\fII\fP NAME=\fIF\fP (REMOVE)"
.br
A directory entry \fIF\fP has an inode number \fII\fP which is greater than
the end of the inode list.
.LP
Possible responses to the REMOVE prompt are:
.IP YES
the directory entry \fIF\fP is removed.
.IP NO
ignore this error condition.
.sp
.LP
.B "UNALLOCATED I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP (REMOVE)"
.br
A directory entry \fIF\fP has a directory inode \fII\fP
without allocate mode bits.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP, modify time \fIT\fP,
and directory name \fIF\fP are printed.
.LP
Possible responses to the REMOVE prompt are:
.IP YES
the directory entry \fIF\fP is removed.
.IP NO
ignore this error condition.
.sp
.LP
.B "UNALLOCATED I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP FILE=\fIF\fP (REMOVE)"
.br
A directory entry \fIF\fP has an inode \fII\fP
without allocate mode bits.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP, modify time \fIT\fP, and file name \fIF\fP are printed.
.LP
Possible responses to the REMOVE prompt are:
.IP YES
the directory entry \fIF\fP is removed.
.IP NO
ignore this error condition.
.sp
.LP
.B "DUP/BAD I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP (REMOVE)"
.br
Phase 1 or Phase 1b have found duplicate blocks or bad blocks
associated with directory entry \fIF\fP, directory inode \fII\fP.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP, modify time \fIT\fP, and directory name \fIF\fP are printed.
.LP
Possible responses to the REMOVE prompt are:
.IP YES
the directory entry \fIF\fP is removed.
.IP NO
ignore this error condition.
.sp
.LP
.B "DUP/BAD I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP FILE=\fIF\fP (REMOVE)"
.br
Phase 1 or Phase 1b have found duplicate blocks or bad blocks
associated with directory entry \fIF\fP, inode \fII\fP.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP,
modify time \fIT\fP, and file name \fIF\fP are printed.
.LP
Possible responses to the REMOVE prompt are:
.IP YES
the directory entry \fIF\fP is removed.
.IP NO
ignore this error condition.
.sp
.LP
.B "ZERO LENGTH DIRECTORY I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP (REMOVE)"
.br
A directory entry \fIF\fP has a size \fIS\fP that is zero.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP, modify time \fIT\fP,
and directory name \fIF\fP are printed.
.LP
Possible responses to the REMOVE prompt are:
.IP YES
the directory entry \fIF\fP is removed;
this will always invoke the BAD/DUP error condition in Phase 4.
.IP NO
ignore this error condition.
.sp
.LP
.B "DIRECTORY TOO SHORT I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP (FIX)"
.br
A directory \fIF\fP has been found whose size \fIS\fP
is less than the minimum size directory.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP, modify time \fIT\fP,
and directory name \fIF\fP are printed.
.LP
Possible responses to the FIX prompt are:
.IP YES
increase the size of the directory to the minimum directory size.
.IP NO
ignore this directory.
.sp
.LP
.B "DIRECTORY CORRUPTED I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP (SALVAGE)"
.br
A directory with an inconsistent internal state has been found.
.LP
Possible responses to the FIX prompt are:
.IP YES
throw away all entries up to the next 512-byte boundary.
This rather drastic action can throw away up to 42 entries,
and should be taken only after other recovery efforts have failed.
.IP NO
Skip up to the next 512-byte boundary and resume reading,
but do not modify the directory.
.sp
.LP
.B "BAD INODE NUMBER FOR `.' I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP (FIX)"
.br
A directory \fII\fP has been found whose inode number for `.' does
does not equal \fII\fP.
.LP
Possible responses to the FIX prompt are:
.IP YES
change the inode number for `.' to be equal to \fII\fP.
.IP NO
leave the inode number for `.' unchanged.
.sp
.LP
.B "MISSING `.' I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP (FIX)"
.br
A directory \fII\fP has been found whose first entry is unallocated.
.LP
Possible responses to the FIX prompt are:
.IP YES
make an entry for `.' with inode number equal to \fII\fP.
.IP NO
leave the directory unchanged.
.sp
.LP
.B "MISSING `.' I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP"
.br
.B "CANNOT FIX, FIRST ENTRY IN DIRECTORY CONTAINS \fIF\fP"
.br
A directory \fII\fP has been found whose first entry is \fIF\fP.
.I Fsck
cannot resolve this problem. 
The file system should be mounted and the offending entry \fIF\fP
moved elsewhere.
The file system should then be unmounted and
.I fsck
should be run again.
.sp
.LP
.B "MISSING `.' I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP"
.br
.B "CANNOT FIX, INSUFFICIENT SPACE TO ADD `.'"
.br
A directory \fII\fP has been found whose first entry is not `.'.
.I Fsck
cannot resolve this problem as it should never happen.
See a guru.
.sp
.LP
.B "EXTRA `.' ENTRY I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP (FIX)"
.br
A directory \fII\fP has been found that has more than one entry for `.'.
.LP
Possible responses to the FIX prompt are:
.IP YES
remove the extra entry for `.'.
.IP NO
leave the directory unchanged.
.sp
.LP
.B "BAD INODE NUMBER FOR `..' I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP (FIX)"
.br
A directory \fII\fP has been found whose inode number for `..' does
does not equal the parent of \fII\fP.
.LP
Possible responses to the FIX prompt are:
.IP YES
change the inode number for `..' to be equal to the parent of \fII\fP.
.IP NO
leave the inode number for `..' unchanged.
.sp
.LP
.B "MISSING `..' I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP (FIX)"
.br
A directory \fII\fP has been found whose second entry is unallocated.
.LP
Possible responses to the FIX prompt are:
.IP YES
make an entry for `..' with inode number equal to the parent of \fII\fP.
.IP NO
leave the directory unchanged.
.sp
.LP
.B "MISSING `..' I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP"
.br
.B "CANNOT FIX, SECOND ENTRY IN DIRECTORY CONTAINS \fIF\fP"
.br
A directory \fII\fP has been found whose second entry is \fIF\fP.
.I Fsck
cannot resolve this problem. 
The file system should be mounted and the offending entry \fIF\fP
moved elsewhere.
The file system should then be unmounted and
.I fsck
should be run again.
.sp
.LP
.B "MISSING `..' I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP"
.br
.B "CANNOT FIX, INSUFFICIENT SPACE TO ADD `..'"
.br
A directory \fII\fP has been found whose second entry is not `..'.
.I Fsck
cannot resolve this problem as it should never happen.
See a guru.
.sp
.LP
.B "EXTRA `..' ENTRY I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP DIR=\fIF\fP (FIX)"
.br
A directory \fII\fP has been found that has more than one entry for `..'.
.LP
Possible responses to the FIX prompt are:
.IP YES
remove the extra entry for `..'.
.IP NO
leave the directory unchanged.
.NH 2 
Phase 3 \- Check Connectivity
.PP
This phase concerns itself with the directory connectivity seen in
Phase 2.
This section lists error conditions resulting from
unreferenced directories,
and missing or full
.I lost+found
directories.
.sp
.LP
.B "UNREF DIR I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP (RECONNECT)"
.br
The directory inode \fII\fP was not connected to a directory entry
when the file system was traversed.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP, and modify time \fIT\fP of directory
inode \fII\fP are printed.
When preen'ing, the directory is reconnected if its size is non-zero,
otherwise it is cleared.
.LP
Possible responses to the RECONNECT prompt are:
.IP YES
reconnect directory inode \fII\fP to the file system in the
directory for lost files (usually \fIlost+found\fP).
This may invoke the
.I lost+found
error condition in Phase 3
if there are problems connecting directory inode \fII\fP to \fIlost+found\fP.
This may also invoke the CONNECTED error condition in Phase 3 if the link
was successful.
.IP NO
ignore this error condition.
This will always invoke the UNREF error condition in Phase 4.
.sp
.LP
.B "SORRY. NO lost+found DIRECTORY"
.br
There is no
.I lost+found
directory in the root directory of the file system;
.I fsck
ignores the request to link a directory in
.I lost+found .
This will always invoke the UNREF error condition in Phase 4.
Check access modes of
.I lost+found .
See
.I fsck (8)
manual entry
for further detail.
This error is fatal if the file system is being preen'ed.
.sp
.LP
.B "SORRY. NO SPACE IN lost+found DIRECTORY"
.br
There is no space to add another entry to the
.I lost+found
directory in the root directory
of the file system;
.I fsck
ignores the request to link a directory in \fIlost+found\fP.
This will always invoke the UNREF error condition in Phase 4.
Clean out unnecessary entries in
.I lost+found
or make
.I lost+found
larger.
See
.I fsck (8)
manual entry for further detail.
This error is fatal if the file system is being preen'ed.
.sp
.LP
.B "DIR I=\fII1\fP CONNECTED. PARENT WAS I=\fII2\fP"
.br
This is an advisory message indicating a directory inode \fII1\fP was
successfully connected to the
.I lost+found
directory.
The parent inode \fII2\fP of the directory inode \fII1\fP is
replaced by the inode number of the
.I lost+found
directory.
.NH 2 
Phase 4 \- Check Reference Counts
.PP
This phase concerns itself with the link count information
seen in Phase 2 and Phase 3.
This section lists error conditions resulting from
unreferenced files,
missing or full
.I lost+found
directory,
incorrect link counts for files, directories, symbolic links, or special files,
unreferenced files, symbolic links, and directories,
bad and duplicate blocks in files, symbolic links, and directories,
and incorrect total free-inode counts.
All errors in this phase are correctable if the file system is being preen'ed
except running out of space in the lost+found directory.
.sp
.LP
.B "UNREF FILE I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP (RECONNECT)"
.br
Inode \fII\fP was not connected to a directory entry
when the file system was traversed.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP, and
modify time \fIT\fP of inode \fII\fP are printed.
When preen'ing the file is cleared if either its size or its
link count is zero,
otherwise it is reconnected.
.LP
Possible responses to the RECONNECT prompt are:
.IP YES
reconnect inode \fII\fP to the file system in the directory for
lost files (usually \fIlost+found\fP).
This may invoke the
.I lost+found
error condition in Phase 4
if there are problems connecting inode \fII\fP to
.I lost+found.
.IP NO
ignore this error condition.
This will always invoke the CLEAR error condition in Phase 4.
.sp
.LP
.B "(CLEAR)"
.br
The inode mentioned in the immediately previous error condition can not be
reconnected.
This cannot occur if the file system is being preen'ed,
since lack of space to reconnect files is a fatal error.
.LP
Possible responses to the CLEAR prompt are:
.IP YES
de-allocate the inode mentioned in the immediately previous error condition by zeroing its contents.
.IP NO
ignore this error condition.
.sp
.LP
.B "SORRY. NO lost+found DIRECTORY"
.br
There is no
.I lost+found
directory in the root directory of the file system;
.I fsck
ignores the request to link a file in
.I lost+found.
This will always invoke the CLEAR error condition in Phase 4.
Check access modes of
.I lost+found .
This error is fatal if the file system is being preen'ed.
.sp
.LP
.B "SORRY. NO SPACE IN lost+found DIRECTORY"
.br
There is no space to add another entry to the
.I lost+found
directory in the root directory of the
file system;
.I fsck
ignores the request to link a file in
.I lost+found.
This will always invoke the CLEAR error condition in Phase 4.
Check size and contents of
.I lost+found.
This error is fatal if the file system is being preen'ed.
.sp
.LP
.B "LINK COUNT FILE I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP COUNT=\fIX\fP SHOULD BE \fIY\fP (ADJUST)"
.br
The link count for inode \fII\fP which is a file,
is \fIX\fP but should be \fIY\fP.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP, and modify time \fIT\fP
are printed.
When preen'ing the link count is adjusted.
.LP
Possible responses to the ADJUST prompt are:
.IP YES
replace the link count of file inode \fII\fP with \fIY\fP.
.IP NO
ignore this error condition.
.sp
.LP
.B "LINK COUNT DIR I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP COUNT=\fIX\fP SHOULD BE \fIY\fP (ADJUST)"
.br
The link count for inode \fII\fP which is a directory,
is \fIX\fP but should be \fIY\fP.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP, and modify time \fIT\fP
of directory inode \fII\fP are printed.
When preen'ing the link count is adjusted.
.LP
Possible responses to the ADJUST prompt are:
.IP YES
replace the link count of directory inode \fII\fP with \fIY\fP.
.IP NO
ignore this error condition.
.sp
.LP
.B "LINK COUNT \fIF\fP I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP COUNT=\fIX\fP SHOULD BE \fIY\fP (ADJUST)"
.br
The link count for \fIF\fP inode \fII\fP is \fIX\fP but should be \fIY\fP.
The name \fIF\fP,
owner \fIO\fP, mode \fIM\fP, size \fIS\fP, and modify time
\fIT\fP
are printed.
When preen'ing the link count is adjusted.
.LP
Possible responses to the ADJUST prompt are:
.IP YES
replace the link count of inode \fII\fP with \fIY\fP.
.IP NO
ignore this error condition.
.sp
.LP
.B "UNREF FILE I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP (CLEAR)"
.br
Inode \fII\fP which is a file, was not connected to a directory entry when the
file system was traversed.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP,
and modify time \fIT\fP of inode \fII\fP
are printed.
When preen'ing,
this is a file that was not connected because its size or link count was zero,
hence it is cleared.
.LP
Possible responses to the CLEAR prompt are:
.IP YES
de-allocate inode \fII\fP by zeroing its contents.
.IP NO
ignore this error condition.
.sp
.LP
.B "UNREF DIR I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP (CLEAR)"
.br
Inode \fII\fP which is a directory,
was not connected to a directory entry when the
file system was traversed.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP,
and modify time \fIT\fP of inode \fII\fP
are printed.
When preen'ing,
this is a directory that was not connected
because its size or link count was zero,
hence it is cleared.
.LP
Possible responses to the CLEAR prompt are:
.IP YES
de-allocate inode \fII\fP by zeroing its contents.
.IP NO
ignore this error condition.
.sp
.LP
.B "BAD/DUP FILE I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP (CLEAR)"
.br
Phase 1 or Phase 1b have found duplicate blocks
or bad blocks associated with
file inode \fII\fP.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP,
and modify time \fIT\fP of inode \fII\fP
are printed.
This error cannot arise when the file system is being preen'ed,
as it would have caused a fatal error earlier.
.LP
Possible responses to the CLEAR prompt are:
.IP YES
de-allocate inode \fII\fP by zeroing its contents.
.IP NO
ignore this error condition.
.sp
.LP
.B "BAD/DUP DIR I=\fII\fP OWNER=\fIO\fP MODE=\fIM\fP SIZE=\fIS\fP MTIME=\fIT\fP (CLEAR)"
.br
Phase 1 or Phase 1b have found duplicate blocks or
bad blocks associated with directory inode \fII\fP.
The owner \fIO\fP, mode \fIM\fP, size \fIS\fP,
and modify time \fIT\fP of inode \fII\fP
are printed.
This error cannot arise when the file system is being preen'ed,
as it would have caused a fatal error earlier.
.LP
Possible responses to the CLEAR prompt are:
.IP YES
de-allocate inode \fII\fP by zeroing its contents.
.IP NO
ignore this error condition.
.sp
.LP
.B "FREE INODE COUNT WRONG IN SUPERBLK (FIX)"
.br
The actual count of the free inodes does not
match the count in the super-block
of the file system.
When preen'ing,
the count is fixed.
.LP
Possible responses to the FIX prompt are:
.IP YES
replace the count in the super-block by the actual count.
.IP NO
ignore this error condition.
.NH 2 
Phase 5 - Check Cyl groups
.PP
This phase concerns itself with the free-block maps.
This section lists error conditions resulting from
allocated blocks in the free-block maps,
free blocks missing from free-block maps,
and the total free-block count incorrect.
.sp
.LP
.B "CG \fIC\fP: BAD MAGIC NUMBER"
.br
The magic number of cylinder group \fIC\fP is wrong.
This usually indicates that the cylinder group maps have been destroyed.
When running manually the cylinder group is marked as needing
to be reconstructed.
This error is fatal if the file system is being preen'ed.
.sp
.LP
.B "EXCESSIVE BAD BLKS IN BIT MAPS (CONTINUE)"
.br
An inode contains
more than a tolerable number (usually 10) of blocks
claimed by other inodes or that are out of the legal range
for the file system.
This error is fatal if the file system is being preen'ed.
.LP
Possible responses to the CONTINUE prompt are:
.IP YES
ignore the rest of the free-block maps and continue the execution of
.I fsck.
.IP NO
terminate the program.
.sp
.LP
.B "SUMMARY INFORMATION \fIT\fP BAD"
.br
where \fIT\fP is one or more of:
.br
.B "(INODE FREE)"
.br
.B "(BLOCK OFFSETS)"
.br
.B "(FRAG SUMMARIES)"
.br
.B "(SUPER BLOCK SUMMARIES)"
.br
The indicated summary information was found to be incorrect.
This error condition will always invoke the 
.B "BAD CYLINDER GROUPS"
condition in Phase 6.
When preen'ing,
the summary information is recomputed.
.sp
.LP
.B "\fIX\fP BLK(S) MISSING"
.br
\fIX\fP blocks
unused by the file system were not found in the free-block maps.
This error condition will always invoke the 
.B "BAD CYLINDER GROUPS"
condition in Phase 6.
When preen'ing,
the block maps are rebuilt.
.sp
.LP
.B "BAD CYLINDER GROUPS (SALVAGE)"
.br
Phase 5 has found
bad blocks in the free-block maps,
duplicate blocks in the free-block maps,
or blocks missing from the file system.
When preen'ing,
the cylinder groups are reconstructed.
.LP
Possible responses to the SALVAGE prompt are:
.IP YES
replace the actual free-block maps with a new free-block maps.
.IP NO
ignore this error condition.
.sp
.LP
.B "FREE BLK COUNT WRONG IN SUPERBLOCK (FIX)"
.br
The actual count of free blocks does not match the count in the
super-block of the file system.
When preen'ing,
the counts are fixed.
.LP
Possible responses to the FIX prompt are:
.IP YES
replace the count in the super-block by the actual count.
.IP NO
ignore this error condition.
.NH 2 
Phase 6 - Salvage Cylinder Groups
.PP
This phase concerns itself with the free-block maps reconstruction.
No error messages are produced.
.NH 2 
Cleanup
.PP
Once a file system has been checked, a few cleanup functions are performed.
This section lists advisory messages about
the file system
and modify status of the file system.
.sp
.LP
.B "\fIV\fP files, \fIW\fP used, \fIX\fP free (\fIY\fP frags, \fIZ\fP blocks)"
.br
This is an advisory message indicating that
the file system checked contained
\fIV\fP files using
\fIW\fP fragment sized blocks leaving
\fIX\fP fragment sized blocks free in the file system.
The numbers in parenthesis breaks the free count down into
\fIY\fP free fragments and
\fIZ\fP free full sized blocks.
.sp
.LP
.B "***** REBOOT UNIX *****"
.br
This is an advisory message indicating that
the root file system has been modified by
.I fsck.
If UNIX is not rebooted immediately,
the work done by
.I fsck
may be undone by the in-core copies of tables
UNIX keeps.
When preen'ing,
.I fsck
will exit with a code of 4.
The auto-reboot script interprets an exit code of 4
by issuing a reboot system call.
.sp
.LP
.B "***** FILE SYSTEM WAS MODIFIED *****"
.br
This is an advisory message indicating that
the current file system was modified by
.I fsck.
If this file system is mounted or is the current root file system,
.I fsck
should be halted and UNIX rebooted.
If UNIX is not rebooted immediately,
the work done by
.I fsck
may be undone by the in-core copies of tables
UNIX keeps.
