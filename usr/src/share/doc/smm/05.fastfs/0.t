.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)0.t	6.1 (Berkeley) %G%
.\"
.EH 'SMM:14-%''A Fast File System for \s-2UNIX\s+2'
.OH 'A Fast File System for \s-2UNIX\s+2''SMM:14-%'
.EQ
delim $$
.EN
.if n .ND
.TL
A Fast File System for UNIX*
.sp
Revised February 18, 1984
.AU
Marshall Kirk McKusick, William N. Joy\(dg,
Samuel J. Leffler\(dd, Robert S. Fabry
.AI
Computer Systems Research Group
Computer Science Division
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, CA  94720
.AB
.FS
* UNIX is a trademark of Bell Laboratories.
.FE
.FS
\(dg William N. Joy is currently employed by:
Sun Microsystems, Inc, 2550 Garcia Avenue, Mountain View, CA 94043
.FE
.FS
\(dd Samuel J. Leffler is currently employed by:
Lucasfilm Ltd., PO Box 2009, San Rafael, CA 94912
.FE
.FS
This work was done under grants from
the National Science Foundation under grant MCS80-05144,
and the Defense Advance Research Projects Agency (DoD) under
ARPA Order No. 4031 monitored by Naval Electronic System Command under
Contract No. N00039-82-C-0235.
.FE
A reimplementation of the UNIX file system is described.
The reimplementation provides substantially higher throughput
rates by using more flexible allocation policies
that allow better locality of reference and can
be adapted to a wide range of peripheral and processor characteristics.
The new file system clusters data that is sequentially accessed
and provides two block sizes to allow fast access to large files
while not wasting large amounts of space for small files.
File access rates of up to ten times faster than the traditional
UNIX file system are experienced.
Long needed enhancements to the programmers'
interface are discussed.
These include a mechanism to place advisory locks on files, 
extensions of the name space across file systems,
the ability to use long file names,
and provisions for administrative control of resource usage.
.AE
.LP
.sp 2
CR Categories and Subject Descriptors:
D.4.3
.B "[Operating Systems]":
File Systems Management \-
.I "file organization, directory structures, access methods";
D.4.2
.B "[Operating Systems]":
Storage Management \-
.I "allocation/deallocation strategies, secondary storage devices";
D.4.8
.B "[Operating Systems]":
Performance \-
.I "measurements, operational analysis";
H.3.2
.B "[Information Systems]":
Information Storage \-
.I "file organization"
.sp
Additional Keywords and Phrases:
UNIX,
file system organization,
file system performance,
file system design,
application program interface.
.sp
General Terms:
file system,
measurement,
performance.
.de PT
.lt \\n(LLu
.pc %
.nr PN \\n%
.tl '\\*(LH'\\*(CH'\\*(RH'
.lt \\n(.lu
..
.af PN i
.ds LH File System
.ds RH Contents
.bp 1
.if t .ds CF February 18, 1984
.if t .ds LF CSRG TR/7
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
.B "2.  Old file system
.LP
.sp .5v
.nf
.B "3.  New file system organization
\0.1.    Optimizing storage utilization
\0.2.    File system parameterization
\0.3.    Layout policies
.LP
.sp .5v
.nf
.B "4.  Performance
.LP
.sp .5v
.nf
.B "5.  File system functional enhancements
\0.1.     Long file names
\0.2.     File locking
\0.3.     Symbolic links
\0.4.     Rename
\0.5.     Quotas
.LP
.sp .5v
.nf
.B Acknowledgements
.LP
.sp .5v
.nf
.B References
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
