.ds q U\s-1NIX\s+1
.ds u U\s-1NIX\s+1
.de Hu
.br
.ss 18
.if n .SP
.if t .SP .5v
\!.tm \\$1 \fI\fP \\\\nP
.S -1
.B "\&\\$1"
.S
.if n .SP
.if t .SP .5v
.ss 12
..
.de Hh
.tm .y
.tm .x "\\$1"
.nr Hc 1
.H 1 "\\$1"
.nr Hc 0
..
.ds :? FSCK
.PH ""
.OH "'\s9\f2\*(:?\fP''\\\\nP\s0'"
.EH "'\s9\\\\nP''\f2\*(:?\^\fP\s0'"
.nr Hu 1
.nr Cl 3
.nr Pt 0
.ND "July 1, 1979"
.TL 49579-220 40125-001
F\s-2SCK\s+2\-The U\s-2NIX\s+2 File System Check Program
.AU "T. J. Kowalski" TJK MH 3624 2771 7E-318
.TM 79-3624-4
.AS
The \*u\(dg\s-1\s+1
.FS \(dg
UNIX is a Trademark of Bell Telephone Laboratories.
.FE
File System Check Program (\c
.I fsck )
is an
interactive file system check and repair program.
.I Fsck\^
uses the redundant structural information in the \*q file system to perform
several consistency checks.
If an inconsistency is detected, it is reported
to the operator, who may elect to fix or ignore
each inconsistency.
These inconsistencies result from the permanent interruption
of the file system updates, which are performed every
time a file is modified.
.I Fsck\^
is frequently able to repair corrupted file systems
using procedures based upon the order in which \*q honors
these file system update requests.
.P
The purpose of this document is to describe the normal updating
of the file system,
to discuss the possible causes of file system corruption,
and to present the corrective actions implemented
by
.I fsck .
Both the program and the interaction between the
program and the operator are described.
.AE
.OK \fIfsck\fP "file system"
.MT 4
.H 1 "INTRODUCTION"
When a \*q
operating system is brought up, a consistency
check of the file systems should always be performed.
This precautionary measure helps to insure
a reliable environment for file storage on disk.
If an inconsistency is discovered,
corrective action must be taken.
No changes are made to any file system by
.I fsck\^
without prior operator approval.
.P
The purpose of this memo is to dispel the
mystique surrounding
file system inconsistencies.
It first describes the updating of the file system
(the calm before the storm) and
then describes file system corruption (the storm).
Finally,
the set of heuristically sound corrective actions
used by
.I fsck\^
(the Coast Guard
to the rescue) is presented.
.P
.H 1 "UPDATE OF THE FILE SYSTEM"
Every working day hundreds of files
are created, modified, and removed.
Every time a file is modified, the
\*u operating system performs a
series of file system updates.
These updates, when written on disk, yield a consistent file system.
To understand what happens
in the event of a permanent interruption in
this sequence, it is important
to understand the order in which the update
requests were probably being honored.
Knowing which pieces
of information were probably written to the file system first,
heuristic procedures can be developed to
repair a corrupted file system.
.P
There are five types of file system updates.
These involve the super-block,
inodes, indirect blocks,
data blocks (directories and files), and free-list blocks.
.H 2 "Super-Block"
The super-block contains information about
the size of the file system,
the size of the inode list,
part of the free-block list,
the count of free blocks,
the count of free inodes,
and part of the free-inode list.
.P
The super-block of a mounted file system
(the root file system is always mounted) is
written to the file system whenever the
file system is unmounted or a
.I sync\^
command is issued.
.H 2 "Inodes"
An inode contains information about the type
of inode (directory, data, or special),
the number of directory
entries linked to the inode,
the list of blocks claimed by the
inode,
and the size of the inode.
.P
An inode is written to the file system upon closure\*F
.FS
All in core blocks are also written to the file system upon issue of
a
.I sync\^
system call.
.FE
of the file associated
with the inode.
.H 2 "Indirect Blocks"
There are three types of indirect blocks:
single-indirect, double-indirect and triple-indirect.
A single-indirect block contains a list of some of the block
numbers claimed by an inode.
Each one of the 128 entries in an indirect block is a data-block number.
A double-indirect block contains a list of single-indirect block
numbers.
A triple-indirect block contains a list of double-indirect
block numbers.
.P
Indirect blocks are written to the file system whenever they have been modified
and
released\*F
.FS
More precisely, they are queued for eventual writing.
Physical I/O is deferred until the buffer is needed by \*q
or a
.I sync\^
command is issued.
.FE
by the operating system.
.H 2 "Data Blocks"
A data block may
contain file information
or directory entries.
Each directory entry consists of a file name
and an inode number.
.P
Data blocks are written to the file system whenever they have been modified
and released
by the operating system.
.H 2 "First Free-List Block"
The super-block contains the first free-list block.
The free-list blocks are a list of all blocks that are not allocated to
the super-block, inodes, indirect blocks, or data blocks.
Each free-list block contains
a count of the number of entries in this free-list block,
a pointer to
the next free-list block, and
a partial list
of free blocks in the file system.
.P
Free-list blocks are written to the file system whenever they have been modified
and released by the operating system.
.H 1 "CORRUPTION OF THE FILE SYSTEM"
A file system
can become corrupted in a variety of ways.
The most common of these ways are
improper shutdown procedures
and hardware failures.
.H 2 "Improper System Shutdown and Startup"
File systems may become corrupted when proper shutdown
procedures are not observed, e.g.,
forgetting to
.I sync\^
the system prior to halting the CPU,
physically write-protecting a mounted file system, or
taking a mounted file system off-line.
.P
File systems may become further corrupted if proper startup
procedures are not observed, e.g.,
not checking a file system for inconsistencies,
and not repairing inconsistencies.
Allowing a corrupted file system to be used (and, thus, to be modified
further) can be disastrous.
.H 2 "Hardware Failure"
Any piece of hardware can fail at any time.
Failures
can be as subtle as a bad block
on a disk pack, or as blatant as a non-functional disk-controller.
.H 1 "DETECTION AND CORRECTION OF CORRUPTION"
A quiescent\*F
.FS
I.e., unmounted and not being written on.
.FE
file system may be checked for structural integrity
by performing consistency checks on the
redundant data intrinsic to a file system.
The redundant data is either read from
the file system or computed from other
known values.
A quiescent state is important during the checking of a file system
because of the multi-pass
nature of the
.I fsck\^
program.
.P
When an inconsistency is discovered
.I fsck\^
reports the inconsistency for the operator to
chose a corrective action.
.P
Discussed in this section are how to discover inconsistencies
and possible corrective actions
for the super-block, the inodes, the indirect blocks,
the data blocks containing directory entries, and the free-list blocks.
These corrective actions can be performed interactively by the
.I fsck\^
command
under control of the operator.
.H 2 "Super-Block"
One of the most common corrupted items is the super-block.
The super-block is prone to corruption
because every change to the file
system's blocks or
inodes modifies the super-block.
.P
The super-block and its associated parts are most often corrupted when
the computer is halted and the last command involving output to the
file system was not a
.I sync\^
command.
.P
The super-block can be checked for inconsistencies
involving file-system size, inode-list size,
free-block list,
free-block count,
and the free-inode count.
.H 3 "File-System Size and Inode-List Size."
The file-system size must be larger than the
number of blocks used by the super-block
and the number of blocks used by the list of inodes.
The number of inodes must be less than 65,535.
The file-system size and inode-list size
are critical pieces of information
to the
.I fsck\^
program.
While there is no way to actually check these sizes,
.I fsck\^
can check for them being within reasonable
bounds.
All other checks of the file system depend on the correctness
of these sizes.
.H 3 "Free-Block List."
The free-block list starts in the super-block and continues through the
free-list blocks of the file system.
Each free-list block can be checked for
a list count out of range,
for block numbers
out of range,
and for blocks already
allocated within the file system.
A check is made to see that all the blocks in the file system were found.
.P
The first free-block list is in the super-block.
.I Fsck\^
checks the list count for a value of less than zero or greater than fifty.
It also checks each block number for a value of less than
the first data block in the file system
or greater than the last block in the file system.
Then it compares each block number to a list of already allocated blocks.
If the free-list block pointer is non-zero, the next free-list
block is read in and the process is repeated.
.P
When all the blocks have been accounted for, a check is made to see if
the number of blocks used by the free-block list plus the number of blocks
claimed by the inodes equals the total number of blocks in the file system.
.P
If anything is wrong with the free-block list, then
.I fsck\^
may
rebuild it, excluding all blocks in the list of allocated
blocks.
.H 3 "Free-Block Count."
The super-block contains a count of
the total number of free blocks within the file system.
.I Fsck\^
compares this count to the
number of blocks it found free within the
file system.
If they don't agree, then
.I fsck\^
may replace the count in the
super-block by the actual free-block count.
.H 3 "Free-Inode Count."
The super-block contains a count of the total number of free inodes within
the file system.
.I Fsck\^
compares this count to the number
of inodes it found free within the file
system.
If they don't agree, then
.I fsck\^
may replace the count in the
super-block by the actual free-inode count.
.H 2 "Inodes"
An individual inode is not as likely to be corrupted as
the super-block.
However, because of the great number of active inodes, there is almost as likely
a chance for corruption in the inode list as in the super-block.
.P
The list of inodes is checked sequentially starting with inode 1
(there is no inode 0)
and going to
the last inode in the file system.
Each inode can be checked for
inconsistencies involving format and type,
link count,
duplicate blocks,
bad blocks,
and inode size.
.H 3 "Format and Type."
Each inode contains a mode word.
This mode word describes the type and state of the inode.
Inodes may be one of four types:
regular inode, directory inode, special block inode,
and special character inode.
If an inode is not one of these types, then the inode has an illegal type.
Inodes may be found in one of three states:
unallocated, allocated, and neither unallocated nor allocated.
This last state indicates an incorrectly formatted inode.
An inode can get in this state if
bad data is written into the inode list
through,
for example,
a hardware failure.
The only possible corrective action is for
.I fsck\^
is to clear the inode.
.H 3 "Link Count."
Contained in each inode is a count of the
total number of directory entries
linked to the inode.
.P
.I Fsck\^
verifies the link count of each inode
by traversing down the total directory
structure, starting from the root directory,
calculating
an actual link count for each inode.
.P
If the stored link count is non-zero and the actual
link count is zero,
it means that
no directory entry appears for the inode.
If the stored and actual link counts are non-zero and unequal,
a directory entry may have been added or removed without the inode being
updated.
.P
If the stored link count is non-zero and the actual link count
is zero,
.I fsck\^
may link the disconnected file to the
.I lost+found\^
directory.
If the stored and actual link counts are non-zero and unequal,
.I fsck\^
may replace the stored link count
by the actual link count.
.H 3 "Duplicate Blocks."
Contained in each inode is a list or pointers to
lists (indirect blocks)
of all the blocks claimed by the inode.
.P
.I Fsck\^
compares each block number claimed by an inode to a list of
already allocated blocks.
If a block number is already claimed by another inode,
the block number is added to a list of duplicate blocks.
Otherwise, the list of allocated blocks is updated to include the block number.
If there are any duplicate blocks,
.I fsck\^
will make a partial second
pass of the inode list to find the inode of the duplicated block,
because without examining the files associated with these inodes for correct content,
there is not enough information available
to decide which inode is corrupted and should be cleared.
Most times,
the inode with the earliest modify time is incorrect,
and should be cleared.
.P
This condition can occur by using
a file system with blocks claimed by
both the free-block list and by other parts of
the file system.
.P
If there is
a large number of duplicate blocks in an inode,
this may be due to
an indirect block not being written to the file system.
.P
.I Fsck\^
will prompt the operator to clear both inodes.
.H 3 "Bad Blocks."
Contained in each inode is a list or pointer to lists
of all the blocks claimed by the inode.
.P
.I Fsck\^
checks each block number claimed by an inode for a value lower
than that of the first data block, or greater than the last
block in the file system.
If the block number is outside this range, the block number is a bad block number.
.P
If there is
a large number of bad blocks in an inode,
this may be due to
an indirect block not being written to the file system.
.P
.I Fsck\^
will prompt the operator to clear both inodes.
.H 3 "Size Checks."
Each inode contains a thirty-two bit (four-byte) size field.
This size indicates the number of characters
in the file associated with the inode.
This size can be checked for inconsistencies,
e.g.,
directory sizes that are not a multiple of sixteen characters,
or the number of blocks actually used not matching
that indicated by the inode size.
.P
A directory inode within the \*u
file system has the directory bit on
in the inode mode word.
The directory size must be a multiple of sixteen because
a directory entry contains sixteen bytes of information
(two bytes for the inode number and fourteen bytes for the file or directory name).
.P
.I Fsck\^
will warn of such directory misalignment.
This is only a warning because not enough information can be gathered
to correct the misalignment.
.P
A rough check of the consistency of
the size field of an inode can be performed
by computing from the size field the number of blocks
that should be associated with the inode and
comparing it to the actual number of blocks
claimed by the inode.
.P
.I Fsck\^
calculates the number of blocks that there should be in an inode by
dividing the number of characters in a inode by the number
of characters per block (512) and rounding up.
.I Fsck\^
adds one block for
each indirect block associated with the inode.
If the actual number of blocks does not match the computed number of blocks,
.I fsck\^
will warn of a possible file-size error.
This is only a warning because \*q does not
fill in blocks in files created in random order.
.H 2 "Indirect Blocks"
Indirect blocks are owned by an inode.
Therefore,
inconsistencies in indirect blocks directly
affect the inode that owns it.
.P
Inconsistencies that can be checked
are blocks already claimed by another inode
and block numbers outside the range of the file system.
.P
For a discussion of detection and correction of the
inconsistencies associated with indirect blocks,
apply iteratively
Sections 4.2.3 and 4.2.4
to each level of indirect blocks.
.H 2 "Data Blocks"
The two types of data blocks are
plain data blocks and directory data blocks.
Plain data blocks contain the information stored in a file.
Directory data blocks contain directory entries.
.I Fsck\^
does not attempt to check the validity of
the contents of
a plain data block.
.P
Each directory data block can be checked for inconsistencies
involving directory inode numbers pointing to
unallocated inodes, directory inode numbers
greater than the number of inodes in the file
system, incorrect directory inode numbers for ``\fB.\fP'' and ``\fB..\fP'',
and directories which are disconnected from the file system.
.P
If a directory entry inode number points
to an unallocated inode, then
.I fsck\^
may
remove that directory entry.
This condition probably occurred because the
data blocks containing the directory entries were modified and written
to the file system while the inode was not yet
written out.
.P
If a directory entry inode number is
pointing beyond the end of the inode list,
.I fsck\^
may remove that directory entry.
This condition occurs if bad data is written into a directory data block.
.P
The directory inode number entry for ``\fB.\fP''
should be the first entry in the directory data block.
Its value should be equal to the inode number for the directory data block.
.P
The directory inode number entry
for ``\fB..\fP'' should be
the second entry in the directory data block.
Its value should be equal to the inode number for the
parent of the directory entry (or the inode number of the directory
data block if the directory is the
root directory).
.P
If the directory inode numbers are
incorrect,
.I fsck\^
may replace them by the
correct values.
.P
.I Fsck\^
checks the general connectivity of the file system.
If directories are found not to be linked into the file system,
.I fsck\^
will link the directory back into the file system in the
.I lost+found\^
directory.
This condition can be caused by
inodes being written to the file system with the corresponding
directory data blocks not being written to the file system.
.H 2 "Free-List Blocks"
Free-list blocks are owned by the super-block.
Therefore, inconsistencies in free-list blocks directly affect
the super-block.
.P
Inconsistencies that can be checked are
a list count outside of range,
block numbers
outside of range,
and blocks already
associated with the file system.
.P
For a discussion of detection and correction of the inconsistencies
associated with free-list blocks see
Section 4.1.2.
.HU "ACKNOWLEDGEMENT"
.P
I would like to thank Larry A. Wehr for advice that lead
to the first version of
.I fsck\^
and Rick B. Brandt for adapting
.I fsck\^
to
\*q/TS.
.SG
.HU "REFERENCES"
.RL
.LI
Ritchie, D. M., and Thompson, K.,
The \*u Time-Sharing System,
.I "The Bell System Technical Journal\^"
.B 57 ,
6 (July-August 1978, Part 2), pp. 1905-29.
.LI
Dolotta, T. A., and Olsson, S. B. eds.,
.I "\*q User's Manual, Edition 1.1\^"
(January 1978).
.LI
Thompson, K.,
\*u Implementation,
.I "The Bell System Technical Journal\^"
.B 57 ,
6 (July-August 1978, Part 2), pp. 1931-46.
.LE
.bp
.nr Hb 3
.nr Hc 1
.nr Hu 1
.HU "Appendix\*(EMFSCK ERROR CONDITIONS"
.nr Hc 0
.nr Hu 2
.nr H1 0
.nr H2 0
.H 1 "CONVENTIONS"
.I Fsck\^
is
a multi-pass file system check program.
Each file system pass invokes a different Phase of the
.I fsck\^
program.
After the initial setup,
.I fsck\^
performs successive Phases over each file system,
checking blocks and sizes,
path-names,
connectivity,
reference counts,
and the free-block list
(possibly rebuilding it),
and performs some cleanup.
.P
When an inconsistency is detected,
.I fsck\^
reports the error condition to the operator.
If a response is required,
.I fsck\^
prints a prompt message and
waits for a response.
This appendix explains the meaning of each error condition,
the possible responses, and the related error conditions.
.P
The error conditions are organized by the
.I Phase\^
of the
.I fsck\^
program in which they can occur.
The error conditions that may occur
in more than one Phase
will be discussed in initialization.
.tm .x "INITIALIZATION"
.nr Hc 1
.H 1 "INITIALIZATION"
.nr Hc 0
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
.DS 0 1
.Hu "\fBC\fP option?"
\fBC\fP is not a legal option to
.I fsck ;
legal options are \-y, \-n, \-s, \-S, and \-t.
.I Fsck\^
terminates on this error condition.
See the
.I fsck (1M)
manual entry for further detail.
.DE
.DS 0 1
.Hu "Bad \(emt option"
The \-t option is not followed by a file name.
.I Fsck\^
terminates on this error condition.
See the
.I fsck (1M)
manual entry for further detail.
.DE
.DS 0 1
.Hu "Invalid \(ems argument, defaults assumed"
The \-s option is not suffixed by 3, 4, or blocks-per-cylinder:blocks-to-skip.
.I Fsck\^
assumes a default value of 400 blocks-per-cylinder and
9 blocks-to-skip.
See the
.I fsck (1M)
manual entry for more details.
.DE
.DS 0 1
.Hu "Incompatible options: \(emn and \(ems"
It is not possible to salvage the free-block list without modifying
the file system.
.I Fsck\^
terminates on this error condition.
See the
.I fsck (1M)
manual entry for further detail.
.DE
.DS 0 1
.Hu "Can't get memory"
.I Fsck 's
request for memory for its virtual
memory tables failed.
This should never happen.
.I Fsck\^
terminates on this error condition.
See a guru.
.DE
.DS 0 1
.Hu "Can't open checklist file: \fBF\fP"
The default file system checklist file
\fBF\fP (usually
.I /etc/checklist )
can not be opened for reading.
.I Fsck\^
terminates on this error condition.
Check access modes of \fBF\fP.
.DE
.DS 0 1
.Hu "Can't stat root"
.I Fsck 's
request for statistics about the root directory ``/'' failed.
This should never happen.
.I Fsck\^
terminates on this error condition.
See a guru.
.DE
.DS 0 1
.Hu "Can't stat \fBF\fP"
.I Fsck 's
request for statistics about the file system \fBF\fP failed.
It ignores this file system
and continues checking the next file system given.
Check access modes of \fBF\fP.
.DE
.DS 0 1
.Hu "\fBF\fP is not a block or character device"
You have given
.I fsck\^
a regular file name by mistake.
It ignores this file system and continues checking the next file system given.
Check file type of \fBF\fP.
.DE
.DS 0 1
.Hu "Can't open \fBF\fP"
The file system \fBF\fP can not be opened for reading.
It ignores this file system and continues checking the next file system given.
Check access modes of \fBF\fP.
.DE
.DS 0 1
.Hu "Size check: fsize \fBX\fP isize \fBY\fP"
More blocks are used for the inode list \fBY\fP than there are blocks in the
file system \fBX\fP, or
there are more than 65,535 inodes in the file system.
It ignores this file system and continues checking the next file system given.
See Section 4.1.1.
.DE
.DS 0 1
.Hu "Can't create \fBF\fP"
.I Fsck 's
request to create a scratch file \fBF\fP failed.
It ignores this file system and continues checking the next file system given.
Check access modes of \fBF\fP.
.DE
.DS 0 1
.Hu "CAN NOT SEEK: BLK \fBB\fP (CONTINUE)"
.I Fsck 's
request for moving to a specified block number \fBB\fP in
the file system failed.
This should never happen.
See a guru.
.P
Possible responses to the CONTINUE prompt are:
.VL 10 2 1
.LI YES
attempt to continue to run the file system check.
Often,
however the problem will persist.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck\^
should be made to re-check this file system.
If the block was part of the virtual memory buffer
cache,
.I fsck\^
will terminate with the message ``Fatal I/O error''.
.LI NO
terminate the program.
.LE
.DE
.DS 0 1
.Hu "CAN NOT READ: BLK \fBB\fP (CONTINUE)"
.I Fsck 's
request for reading a specified block number \fBB\fP in
the file system failed.
This should never happen.
See a guru.
.P
Possible responses to the CONTINUE prompt are:
.VL 10 2 1
.LI YES
attempt to continue to run the file system check.
Often,
however,
the problem will persist.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck\^
should be made to re-check this file system.
If the block was part of the virtual memory buffer
cache,
.I fsck\^
will terminate with the message ``Fatal I/O error''.
.LI NO
terminate the program.
.LE
.DE
.DS 0 1
.Hu "CAN NOT WRITE: BLK \fBB\fP (CONTINUE)"
.I Fsck 's
request for writing a specified block number \fBB\fP
in
the file system failed.
The disk is write-protected.
See a guru.
.P
Possible responses to the CONTINUE prompt are:
.VL 10 2 1
.LI YES
attempt to continue to run the file system check.
Often,
however,
the problem will persist.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck\^
should be made to re-check this file system.
If the block was part of the virtual memory buffer
cache,
.I fsck\^
will terminate with the message ``Fatal I/O error''.
.LI NO
terminate the program.
.LE
.DE
.ne 20
.Hh "PHASE 1: CHECK BLOCKS AND SIZES"
This phase concerns itself with
the inode list.
This section lists error conditions resulting from
checking inode types,
setting up the zero-link-count table,
examining inode block numbers for bad or duplicate blocks,
checking inode size,
and checking inode format.
.DS 0 1
.Hu "UNKNOWN FILE TYPE I=\fBI\fP (CLEAR)"
The mode word of the inode \fBI\fP indicates that the inode is not a
special character inode, special character inode, regular inode, or directory
inode.
See Section 4.2.1.
.P
Possible responses to the CLEAR prompt are:
.VL 10 2 1
.LI YES
de-allocate inode \fBI\fP by zeroing its contents.
This will always invoke the UNALLOCATED error condition in Phase 2
for each directory entry pointing to this inode.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "LINK COUNT TABLE OVERFLOW (CONTINUE)"
An internal table for
.I fsck\^
containing allocated inodes with a link count of
zero has no more room.
Recompile
.I fsck\^
with a larger value of MAXLNCNT.
.P
Possible responses
to the CONTINUE prompt are:
.VL 10 2 1
.LI YES
continue with the program.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck\^
should be made to re-check this file system.
If another allocated inode with a zero link count is found,
this error condition is repeated.
.LI NO
terminate the program.
.LE
.DE
.DS 0 1
.Hu "\fBB\fP BAD I=\fBI\fP"
Inode \fBI\fP contains block number \fBB\fP with a number
lower than the number of the first data block in the file system or
greater than the number of the last block
in the file system.
This error condition may invoke the EXCESSIVE BAD BLKS error condition in Phase 1 if
inode \fBI\fP has too many block numbers outside the file system range.
This error condition will always invoke the BAD/DUP error condition in Phase 2 and Phase 4.
See Section 4.2.4.
.DE
.DS 0 1
.Hu "EXCESSIVE BAD BLKS I=\fBI\fP (CONTINUE)"
There is more than a tolerable number (usually 10) of blocks with a number
lower than the number of the first data block in the file system or greater than
the number of last block in the file system associated with inode \fBI\fP.
See Section 4.2.4.
.P
Possible responses to the CONTINUE prompt are:
.VL 10 2 1
.LI YES
ignore the rest of the blocks in this inode
and continue checking with the next inode in the file system.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck\^
should be made to re-check this file system.
.LI NO
terminate the program.
.DE
.DS 0 1
.Hu "\fBB\fP DUP I=\fBI\fP"
Inode \fBI\fP contains block number \fBB\fP which is already claimed by
another inode.
This error condition may invoke the EXCESSIVE DUP BLKS error condition in Phase 1 if
inode \fBI\fP has too many block numbers claimed by other inodes.
This error condition will always invoke Phase 1b and
the BAD/DUP error condition in Phase 2 and Phase 4.
See Section 4.2.3.
.DE
.DS 0 1
.Hu "EXCESSIVE DUP BLKS I=\fBI\fP (CONTINUE)"
There is more than a tolerable number (usually 10) of blocks claimed by other
inodes.
See Section 4.2.3.
.P
Possible responses to the CONTINUE prompt are:
.VL 10 2 1
.LI YES
ignore the rest of the blocks in this inode
and continue checking with the next inode in the file system.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck\^
should be made to re-check this file system.
.LI NO
terminate the program.
.DE
.DS 0 1
.Hu "DUP TABLE OVERFLOW (CONTINUE)"
An internal table in
.I fsck\^
containing duplicate block numbers has no more room.
Recompile
.I fsck\^
with a larger value of DUPTBLSIZE.
.P
Possible responses to the CONTINUE prompt are:
.VL 10 2 1
.LI YES
continue with the program.
This error condition will not allow a complete check of the file system.
A second run of
.I fsck\^
should be made to re-check this file system.
If another duplicate block is found, this error condition will repeat.
.LI NO
terminate the program.
.LE
.DE
.DS 0 1
.Hu "POSSIBLE FILE SIZE ERROR I=\fBI\fP"
The inode \fBI\fP size does not match the actual number of blocks
used by the inode.
This is only a warning.
See Section 4.2.5.
.DE
.DS 0 1
.Hu "DIRECTORY MISALIGNED I=\fBI\fP"
The size of a directory inode is not a multiple of the size
of a directory entry (usually 16).
This is only a warning.
See Section 4.2.5.
.DE
.DS 0 1
.Hu "PARTIALLY ALLOCATED INODE I=\fBI\fP (CLEAR)"
Inode \fBI\fP is neither allocated nor unallocated.
See Section 4.2.1.
.P
Possible responses to the CLEAR prompt are:
.VL 10 2 1
.LI YES
de-allocate inode \fBI\fP by zeroing its contents.
.LI NO
ignore this error condition.
.LE
.DE
.Hh "PHASE 1B: RESCAN FOR MORE DUPS"
When a duplicate block is found in the file system, the file system is
rescanned to find the inode which previously claimed that block.
This section lists the error condition when the duplicate block is found.
.DS 0 1
.Hu "\fBB\fP DUP I=\fBI\fP"
Inode \fBI\fP contains block number \fBB\fP which is already claimed by another
inode.
This error condition will always invoke the BAD/DUP error condition in Phase 2.
You can determine which inodes have overlapping blocks by examining
this error condition and the DUP error condition in Phase 1.
See Section 4.2.3.
.DE
.ne 20
.Hh "PHASE 2: CHECK PATH-NAMES"
This phase concerns itself with removing directory entries
pointing to
error conditioned inodes
from Phase 1 and Phase 1b.
This section lists error conditions resulting from
root inode mode and status,
directory inode pointers in range,
and directory entries pointing to bad inodes.
.DS 0 1
.Hu "ROOT INODE UNALLOCATED. TERMINATING."
The root inode (usually inode number 2) has no allocate mode bits.
This should never happen.
The program will terminate.
See Section 4.2.1.
.DE
.DS 0 1
.Hu "ROOT INODE NOT DIRECTORY (FIX)"
The root inode (usually inode number 2)
is not directory inode type.
See Section 4.2.1.
.P
Possible responses to the FIX prompt are:
.VL 10 2 1
.LI YES
replace the root inode's type to be a directory.
If the root inode's data blocks are not directory blocks,
a VERY large number of error conditions will be produced.
.LI NO
terminate the program.
.LE
.DE
.DS 0 1
.Hu "DUPS/BAD IN ROOT INODE (CONTINUE)"
Phase 1 or Phase 1b have found duplicate blocks
or bad blocks in the root inode (usually inode number 2) for the file system.
See Section 4.2.3 and 4.2.4.
.P
Possible responses to the CONTINUE prompt are:
.VL 10 2 1
.LI YES
ignore the DUPS/BAD error condition in the root inode and
attempt to continue to run the file system check.
If the root inode is not correct,
then this may result in a large number of other error conditions.
.LI NO
terminate the program.
.LE
.DE
.DS 0 1
.Hu "I OUT OF RANGE I=\fBI\fP NAME=\fBF\fP (REMOVE)"
A directory entry \fBF\fP has an inode number \fBI\fP which is greater than
the end of the inode list.
See Section 4.4.
.P
Possible responses to the REMOVE prompt are:
.VL 10 2 1
.LI YES
the directory entry \fBF\fP is removed.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "UNALLOCATED I=\fBI\fP OWNER=\fBO\fP MODE=\fBM\fP SIZE=\fBS\fP MTIME=\fBT\fP NAME=\fBF\fP (REMOVE)"
A directory entry \fBF\fP has an inode \fBI\fP
without allocate mode bits.
The owner \fBO\fP, mode \fBM\fP, size \fBS\fP, modify time \fBT\fP, and file name \fBF\fP are printed.
See Section 4.4.
.P
Possible responses to the REMOVE prompt are:
.VL 10 2 1
.LI YES
the directory entry \fBF\fP is removed.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "DUP/BAD I=\fBI\fP OWNER=\fBO\fP MODE=\fBM\fP SIZE=\fBS\fP MTIME=\fBT\fP DIR=\fBF\fP (REMOVE)"
Phase 1 or Phase 1b have found duplicate blocks or bad blocks
associated with directory entry \fBF\fP, directory inode \fBI\fP.
The owner \fBO\fP, mode \fBM\fP, size \fBS\fP, modify time \fBT\fP, and directory name \fBF\fP are printed.
See Section 4.2.3 and 4.2.4.
.P
Possible responses to the REMOVE prompt are:
.VL 10 2 1
.LI YES
the directory entry \fBF\fP is removed.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "DUP/BAD I=\fBI\fP OWNER=\fBO\fP MODE=\fBM\fP SIZE=\fBS\fP MTIME=\fBT\fP FILE=\fBF\fP (REMOVE)"
Phase 1 or Phase 1b have found duplicate blocks or bad blocks
associated with directory entry \fBF\fP, inode \fBI\fP.
The owner \fBO\fP, mode \fBM\fP, size \fBS\fP, modify time \fBT\fP, and file name \fBF\fP are printed.
See Section 4.2.3 and 4.2.4.
.P
Possible responses to the REMOVE prompt are:
.VL 10 2 1
.LI YES
the directory entry \fBF\fP is removed.
.LI NO
ignore this error condition.
.LE
.DE
.ne 20
.Hh "PHASE 3: CHECK CONNECTIVITY"
This phase concerns itself with the directory connectivity seen in
Phase 2.
This section lists error conditions resulting from
unreferenced directories,
and missing or full
.I lost+found\^
directories.
.DS 0 1
.Hu "UNREF DIR I=\fBI\fP OWNER=\fBO\fP MODE=\fBM\fP SIZE=\fBS\fP MTIME=\fBT\fP (RECONNECT)"
The directory inode \fBI\fP was not connected to a directory entry
when the file system was traversed.
The owner \fBO\fP, mode \fBM\fP, size \fBS\fP, and modify time \fBT\fP of directory
inode \fBI\fP are printed.
See Section 4.4 and 4.2.2.
.P
Possible responses to the RECONNECT prompt are:
.VL 10 2 1
.LI YES
reconnect directory inode \fBI\fP to the file system in the
directory for lost files (usually
.I lost+found ).
This may invoke the
.I lost+found\^
error condition in Phase 3
if there are problems connecting directory inode \fBI\fP
to
.I lost+found .
This may also invoke the CONNECTED error condition in Phase 3 if the link
was successful.
.LI NO
ignore this error condition.
This will always invoke the UNREF error condition in Phase 4.
.LE
.DE
.DS 0 1
.Hu "SORRY. NO lost+found DIRECTORY"
There is no
.I lost+found\^
directory in the root directory of the file system;
.I fsck\^
ignores the request to link a directory in
.I lost+found .
This will always invoke the UNREF error condition in Phase 4.
Check access modes of
.I lost+found .
See
.I fsck (1M)
manual entry
for further detail.
.DE
.DS 0 1
.Hu "SORRY. NO SPACE IN lost+found DIRECTORY"
There is no space to add another entry to the
.I lost+found\^
directory in the root directory
of the file system;
.I fsck\^
ignores the request to link a directory in
.I lost+found .
This will always invoke the UNREF error condition in Phase 4.
Clean out unnecessary entries in
.I lost+found\^
or make
.I lost+found\^
larger.
See
.I fsck (1M)
manual entry for further detail.
.DE
.DS 0 1
.Hu "DIR I=\fBI1\fP CONNECTED. PARENT WAS I=\fBI2\fP"
This is an advisory message indicating a directory inode \fBI1\fP was
successfully connected to the
.I lost+found\^
directory.
The parent inode \fBI2\fP of the directory inode \fBI1\fP is
replaced by the inode number of the
.I lost+found\^
directory.
See Section 4.4 and 4.2.2.
.DE
.ne 20
.Hh "PHASE 4: CHECK REFERENCE COUNTS"
This phase concerns itself with the link count information
seen in Phase 2 and Phase 3.
This section lists error conditions resulting from
unreferenced files,
missing or full
.I lost+found\^
directory,
incorrect link counts for files, directories, or special files,
unreferenced files and directories,
bad and duplicate blocks in files and directories,
and incorrect total free-inode counts.
.DS 0 1
.Hu "UNREF FILE I=\fBI\fP OWNER=\fBO\fP MODE=\fBM\fP SIZE=\fBS\fP MTIME=\fBT\fP (RECONNECT)"
Inode \fBI\fP was not connected to a directory entry
when the file system was traversed.
The owner \fBO\fP, mode \fBM\fP, size \fBS\fP, and
modify time \fBT\fP of inode \fBI\fP are printed.
See Section 4.2.2.
.P
Possible responses to the RECONNECT prompt are:
.VL 10 2 1
.LI YES
reconnect inode \fBI\fP to the file system in the directory for
lost files (usually
.I lost+found ).
This may invoke
the
.I lost+found\^
error condition in Phase 4
if there are problems connecting inode \fBI\fP to
.I lost+found .
.LI NO
ignore this error condition.  This will always invoke the CLEAR error condition in
Phase 4.
.LE
.DE
.DS 0 1
.Hu "SORRY. NO lost+found DIRECTORY"
There is no
.I lost+found\^
directory in the root directory of the file system;
.I fsck\^
ignores the request to link a file in
.I lost+found .
This will always invoke the CLEAR error condition in Phase 4.
Check access modes of
.I lost+found .
.DE
.DS 0 1
.Hu "SORRY. NO SPACE IN lost+found DIRECTORY"
There is no space to add another entry to the
.I lost+found\^
directory in the root directory of the
file system;
.I fsck\^
ignores the request to link a file in
.I lost+found .
This will always invoke the CLEAR error condition in Phase 4.
Check size and contents of
.I lost+found .
.DE
.DS 0 1
.Hu "(CLEAR)"
The inode mentioned in the immediately previous error condition can not be
reconnected.
See Section 4.2.2.
.P
Possible responses to the CLEAR prompt are:
.VL 10 2 1
.LI YES
de-allocate the inode mentioned in the immediately previous error condition by zeroing its contents.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "LINK COUNT FILE I=\fBI\fP OWNER=\fBO\fP MODE=\fBM\fP SIZE=\fBS\fP MTIME=\fBT\fP COUNT=\fBX\fP SHOULD BE \fBY\fP (ADJUST)"
The link count for inode \fBI\fP which is a file, is \fBX\fP but should be \fBY\fP.
The owner \fBO\fP, mode \fBM\fP, size \fBS\fP, and modify time \fBT\fP
are printed.
See Section 4.2.2.
.P
Possible responses to the ADJUST prompt are:
.VL 10 2 1
.LI YES
replace the link count of file inode \fBI\fP with \fBY\fP.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "LINK COUNT DIR I=\fBI\fP OWNER=\fBO\fP MODE=\fBM\fP SIZE=\fBS\fP MTIME=\fBT\fP COUNT=\fBX\fP SHOULD BE \fBY\fP (ADJUST)"
The link count for inode \fBI\fP which is a directory, is \fBX\fP but should be \fBY\fP.
The owner \fBO\fP, mode \fBM\fP, size \fBS\fP, and modify time \fBT\fP
of directory inode \fBI\fP are printed.
See Section 4.2.2.
.P
Possible responses to the ADJUST prompt are:
.VL 10 2 1
.LI YES
replace the link count of directory inode \fBI\fP with \fBY\fP.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "LINK COUNT \fBF\fP I=\fBI\fP OWNER=\fBO\fP MODE=\fBM\fP SIZE=\fBS\fP MTIME=\fBT\fP COUNT=\fBX\fP SHOULD BE \fBY\fP (ADJUST)"
The link count for \fBF\fP inode \fBI\fP is \fBX\fP but should be \fBY\fP.
The name \fBF\fP,
owner \fBO\fP, mode \fBM\fP, size \fBS\fP, and modify time
\fBT\fP
are printed.
See Section 4.2.2.
.P
Possible responses to the ADJUST prompt are:
.VL 10 2 1
.LI YES
replace the link count of inode \fBI\fP with \fBY\fP.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "UNREF FILE I=\fBI\fP OWNER=\fBO\fP MODE=\fBM\fP SIZE=\fBS\fP MTIME=\fBT\fP (CLEAR)"
Inode \fBI\fP which is a file, was not connected to a directory entry when the
file system was traversed.
The owner \fBO\fP, mode \fBM\fP, size \fBS\fP, and modify time \fBT\fP of inode \fBI\fP
are printed.
See Section 4.2.2 and 4.4.
.P
Possible responses to the CLEAR prompt are:
.VL 10 2 1
.LI YES
de-allocate inode \fBI\fP by zeroing its contents.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "UNREF DIR I=\fBI\fP OWNER=\fBO\fP MODE=\fBM\fP SIZE=\fBS\fP MTIME=\fBT\fP (CLEAR)"
Inode \fBI\fP which is a directory, was not connected to a directory entry when the
file system was traversed.
The owner \fBO\fP, mode \fBM\fP, size \fBS\fP, and modify time \fBT\fP of inode \fBI\fP
are printed.
See Section 4.2.2 and 4.4.
.P
Possible responses to the CLEAR prompt are:
.VL 10 2 1
.LI YES
de-allocate inode \fBI\fP by zeroing its contents.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "BAD/DUP FILE I=\fBI\fP OWNER=\fBO\fP MODE=\fBM\fP SIZE=\fBS\fP MTIME=\fBT\fP (CLEAR)"
Phase 1 or Phase 1b have found duplicate blocks or bad blocks associated with
file inode \fBI\fP.
The owner \fBO\fP, mode \fBM\fP, size \fBS\fP, and modify time \fBT\fP of inode \fBI\fP
are printed.
See Section 4.2.3 and 4.2.4.
.P
Possible responses to the CLEAR prompt are:
.VL 10 2 1
.LI YES
de-allocate inode \fBI\fP by zeroing its contents.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "BAD/DUP DIR I=\fBI\fP OWNER=\fBO\fP MODE=\fBM\fP SIZE=\fBS\fP MTIME=\fBT\fP (CLEAR)"
Phase 1 or Phase 1b have found duplicate blocks or
bad blocks associated with directory inode \fBI\fP.
The owner \fBO\fP, mode \fBM\fP, size \fBS\fP, and modify time \fBT\fP of inode \fBI\fP
are printed.
See Section 4.2.3 and 4.2.4.
.P
Possible responses to the CLEAR prompt are:
.VL 10 2 1
.LI YES
de-allocate inode \fBI\fP by zeroing its contents.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "FREE INODE COUNT WRONG IN SUPERBLK (FIX)"
The actual count of the free inodes does not match the count in the super-block
of the file system.
See Section 4.1.4.
.P
Possible responses to the FIX prompt are:
.VL 10 2 1
.LI YES
replace the count in the super-block by the actual count.
.LI NO
ignore this error condition.
.LE
.DE
.ne 20
.Hh "PHASE 5: CHECK FREE LIST"
This phase concerns itself with the free-block list.
This section lists error conditions resulting from
bad blocks in the free-block list,
bad free-blocks count,
duplicate blocks in the free-block list,
unused blocks from the file system not in the free-block list,
and the total free-block count incorrect.
.DS 0 1
.Hu "EXCESSIVE BAD BLKS IN FREE LIST (CONTINUE)"
The free-block list contains
more than a tolerable number (usually 10) of blocks with a value less than
the first data block in the file system or
greater than the last block in the file system.
See Section 4.1.2 and 4.2.4.
.P
Possible responses to the CONTINUE prompt are:
.VL 10 2 1
.LI YES
ignore the rest of the free-block list and continue
the execution of
.I fsck .
This error condition will always invoke the BAD BLKS IN FREE LIST error condition
in Phase 5.
.LI NO
terminate the program.
.LE
.DE
.DS 0 1
.Hu "EXCESSIVE DUP BLKS IN FREE LIST (CONTINUE)"
The free-block list contains
more than a tolerable number (usually 10) of blocks
claimed by inodes or earlier parts of the free-block list.
See Section 4.1.2 and 4.2.3.
.P
Possible responses to the CONTINUE prompt are:
.VL 10 2 1
.LI YES
ignore the rest of the free-block list and continue
the execution of
.I fsck .
This error condition will always invoke the DUP BLKS IN FREE LIST error condition
in Phase 5.
.LI NO
terminate the program.
.LE
.DE
.DS 0 1
.Hu "BAD FREEBLK COUNT"
The count
of free blocks in a free-list block is greater than 50
or less than zero.
This error condition will always invoke the BAD FREE LIST condition in Phase 5.
See Section 4.1.2.
.DE
.DS 0 1
.Hu "\fBX\fP BAD BLKS IN FREE LIST"
\fBX\fP blocks
in the free-block list have a block number lower
than the first data block in the file system or greater than
the last block in the file system.
This error condition will always invoke the BAD FREE LIST condition
in Phase 5.
See Section 4.1.2 and 4.2.4.
.DE
.DS 0 1
.Hu "\fBX\fP DUP BLKS IN FREE LIST"
\fBX\fP blocks
claimed by inodes or earlier parts of the free-list block were
found in the free-block list.
This error condition will always invoke the
BAD FREE LIST condition
in Phase 5.
See Section 4.1.2 and 4.2.3.
.DE
.DS 0 1
.Hu "\fBX\fP BLK(S) MISSING"
\fBX\fP blocks
unused by the file system were not found in the free-block list.
This error condition will always invoke the BAD FREE LIST condition
in Phase 5.
See Section 4.1.2.
.DE
.DS 0 1
.Hu "FREE BLK COUNT WRONG IN SUPERBLOCK (FIX)"
The actual count of free blocks does not match the count in the
super-block of the file system.
See Section 4.1.3.
.P
Possible responses to the FIX prompt are:
.VL 10 2 1
.LI YES
replace the count in the super-block by the actual count.
.LI NO
ignore this error condition.
.LE
.DE
.DS 0 1
.Hu "BAD FREE LIST (SALVAGE)"
Phase 5 has found
bad blocks in the free-block list,
duplicate blocks in the free-block list,
or blocks missing from the file system.
See Section 4.1.2, 4.2.3, and 4.2.4.
.P
Possible responses to the SALVAGE prompt are:
.VL 10 2 1
.LI YES
replace the actual free-block list with a new free-block list.
The new free-block list will be ordered to reduce
time spent by the disk waiting for the disk to rotate into
position.
.LI NO
ignore this error condition.
.LE
.DE
.ne 20
.Hh "PHASE 6: SALVAGE FREE LIST"
This phase concerns itself with the free-block list reconstruction.
This section lists error conditions resulting from the blocks-to-skip and
blocks-per-cylinder values.
.DS 0 1
.Hu "Default free-block list spacing assumed"
This is an advisory message indicating the blocks-to-skip is greater
than the blocks-per-cylinder, the blocks-to-skip is less than one,
the blocks-per-cylinder is less than one, or the blocks-per-cylinder
is greater than 500.
The default values of 9 blocks-to-skip and 400 blocks-per-cylinder are used.
See the
.I fsck (1M)
manual entry for further detail.
.DE
.Hh "CLEANUP"
Once a file system has been checked, a few cleanup functions are performed.
This section lists advisory messages about
the file system
and modify status of the file system.
.DS 0 1
.Hu "\fBX\fP files \fBY\fP blocks \fBZ\fP free"
This is an advisory message indicating that
the file system checked contained
\fBX\fP files using
\fBY\fP blocks leaving
\fBZ\fP blocks free in the file system.
.DE
.DS 0 1
.Hu "***** BOOT UNIX (NO SYNC!) *****"
This is an advisory message indicating that
a mounted file system or the root file system has been modified
by
.I fsck .
If \*q is not rebooted immediately,
the work done by
.I fsck\^
may be undone by the in-core copies of tables
\*q keeps.
.DE
.DS 0 1
.Hu "***** FILE SYSTEM WAS MODIFIED *****"
This is an advisory message indicating that
the current file system was modified by
.I fsck .
If this file system is mounted or is the current root file system,
.I fsck\^
should be halted and \*q rebooted.
If \*q is not rebooted immediately,
the work done by
.I fsck\^
may be undone by the in-core copies of tables
\*q keeps.
.DE
.tm .y
.sp
.I "May 1979"
