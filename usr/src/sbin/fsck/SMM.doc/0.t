.\" Copyright (c) 1982 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)0.t	4.1 (Berkeley) %G%
.\"
.if n .ND
.TL
Fsck \- The UNIX\(dg File System Check Program
.sp
Revised July 28, 1983
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
with the 4.2BSD file system organization.  This
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
.AE
.LP
.de PT
.lt \\n(LLu
.pc %
.nr PN \\n%
.tl '\\*(LH'\\*(CH'\\*(RH'
.lt \\n(.lu
..
.af PN i
.ds LH Fsck
.ds RH Contents
.bp 1
.if t .ds CF July 28, 1983
.if t .ds LF CSRG TR/9
.if t .ds RF McKusick, et. al.
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
\0.1.    Superblock
\0.2.    Summary Information
\0.3.    Cylinder groups
\0.4.    Fragments
\0.5.    Updates to the file system
.LP
.sp .5v
.nf
.B "3.  Fixing corrupted file systems
\0.1.    Detecting and correcting corruption
\0.2.    Super block checking
\0.3.    Free block checking
\0.4.    Checking the inode state
\0.5.    Inode links
\0.6.    Inode data size
\0.7.    Checking the data associated with an inode
\0.8.    File system connectivity
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
\0.1.     Conventions
\0.2.     Initialization
\0.3.     Phase 1 - Check Blocks and Sizes
\0.4.     Phase 1b - Rescan for more Dups
\0.5.     Phase 2 - Check Pathnames
\0.6.     Phase 3 - Check Connectivity
\0.7.     Phase 4 - Check Reference Counts
\0.8.     Phase 5 - Check Cyl groups
\0.9.     Phase 6 - Salvage Cylinder Groups
\0.10.    Cleanup
.ds RH Introduction
.af PN 1
.bp 1
.de _d
.if t .ta .6i 2.1i 2.6i
.\" 2.94 went to 2.6, 3.64 to 3.30
.if n .ta .84i 2.6i 3.30i
..
.de _f
.if t .ta .5i 1.25i 2.5i
.\" 3.5i went to 3.8i
.if n .ta .7i 1.75i 3.8i
..
