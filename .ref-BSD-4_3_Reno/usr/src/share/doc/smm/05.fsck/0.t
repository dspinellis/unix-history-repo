.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)0.t	4.3 (Berkeley) 5/20/86
.\"
.if n .ND
.TL
Fsck \- The UNIX\(dg File System Check Program
.EH 'SMM:5-%''The \s-2UNIX\s+2 File System Check Program'
.OH 'The \s-2UNIX\s+2 File System Check Program''SMM:5-%'
.AU
Marshall Kirk McKusick
.AI
Computer Systems Research Group
Computer Science Division
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, CA  94720
.AU
T. J. Kowalski
.AI
Bell Laboratories
Murray Hill, New Jersey 07974
.AB
.FS
\(dgUNIX is a trademark of Bell Laboratories.
.FE
.FS
This work was done under grants from
the National Science Foundation under grant MCS80-05144,
and the Defense Advance Research Projects Agency (DoD) under
Arpa Order No. 4031 monitored by Naval Electronic System Command under
Contract No. N00039-82-C-0235.
.FE
This document reflects the use of
.I fsck
with the 4.2BSD and 4.3BSD file system organization.  This
is a revision of the
original paper written by
T. J. Kowalski.
.PP
File System Check Program (\fIfsck\fR)
is an interactive file system check and repair program.
.I Fsck
uses the redundant structural information in the
UNIX file system to perform several consistency checks.
If an inconsistency is detected, it is reported
to the operator, who may elect to fix or ignore
each inconsistency.
These inconsistencies result from the permanent interruption
of the file system updates, which are performed every
time a file is modified.
Unless there has been a hardware failure,
.I fsck
is able to repair corrupted file systems
using procedures based upon the order in which UNIX honors
these file system update requests.
.PP
The purpose of this document is to describe the normal updating
of the file system,
to discuss the possible causes of file system corruption,
and to present the corrective actions implemented
by
.I fsck.
Both the program and the interaction between the
program and the operator are described.
.sp 2
.LP
Revised July 16, 1985
.AE
.LP
.bp
.ce
.B "TABLE OF CONTENTS"
.LP
.sp 1
.nf
.B "1.  Introduction"
.LP
.sp .5v
.nf
.B "2.  Overview of the file system
2.1.    Superblock
2.2.    Summary Information
2.3.    Cylinder groups
2.4.    Fragments
2.5.    Updates to the file system
.LP
.sp .5v
.nf
.B "3.  Fixing corrupted file systems
3.1.    Detecting and correcting corruption
3.2.    Super block checking
3.3.    Free block checking
3.4.    Checking the inode state
3.5.    Inode links
3.6.    Inode data size
3.7.    Checking the data associated with an inode
3.8.    File system connectivity
.LP
.sp .5v
.nf
.B Acknowledgements
.LP
.sp .5v
.nf
.B References
.LP
.sp .5v
.nf
.B "4.  Appendix A
4.1.     Conventions
4.2.     Initialization
4.3.     Phase 1 - Check Blocks and Sizes
4.4.     Phase 1b - Rescan for more Dups
4.5.     Phase 2 - Check Pathnames
4.6.     Phase 3 - Check Connectivity
4.7.     Phase 4 - Check Reference Counts
4.8.     Phase 5 - Check Cyl groups
4.9.     Cleanup
.ds RH Introduction
.bp
