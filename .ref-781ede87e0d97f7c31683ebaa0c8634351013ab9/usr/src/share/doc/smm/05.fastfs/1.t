.\" Copyright (c) 1986 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" Redistribution and use in source and binary forms are permitted
.\" provided that the above copyright notice and this paragraph are
.\" duplicated in all such forms and that any documentation,
.\" advertising materials, and other materials related to such
.\" distribution and use acknowledge that the software was developed
.\" by the University of California, Berkeley.  The name of the
.\" University may not be used to endorse or promote products derived
.\" from this software without specific prior written permission.
.\" THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
.\" IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
.\" WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
.\"
.\"	@(#)1.t	6.2 (Berkeley) %G%
.\"
.ds RH Introduction
.NH
Introduction
.PP
This paper describes the changes from the original 512 byte UNIX file
system to the new one released with the 4.2 Berkeley Software Distribution.
It presents the motivations for the changes,
the methods used to effect these changes,
the rationale behind the design decisions,
and a description of the new implementation.
This discussion is followed by a summary of
the results that have been obtained,
directions for future work,
and the additions and changes
that have been made to the facilities that are
available to programmers.
.PP
The original UNIX system that runs on the PDP-11\(dg
.FS
\(dg DEC, PDP, VAX, MASSBUS, and UNIBUS are
trademarks of Digital Equipment Corporation.
.FE
has simple and elegant file system facilities.  File system input/output
is buffered by the kernel;
there are no alignment constraints on
data transfers and all operations are made to appear synchronous.
All transfers to the disk are in 512 byte blocks, which can be placed
arbitrarily within the data area of the file system.  Virtually
no constraints other than available disk space are placed on file growth
[Ritchie74], [Thompson78].*
.FS
* In practice, a file's size is constrained to be less than about
one gigabyte.
.FE
.PP
When used on the VAX-11 together with other UNIX enhancements,
the original 512 byte UNIX file
system is incapable of providing the data throughput rates
that many applications require.
For example, 
applications
such as VLSI design and image processing
do a small amount of processing
on a large quantities of data and
need to have a high throughput from the file system.
High throughput rates are also needed by programs
that map files from the file system into large virtual
address spaces.
Paging data in and out of the file system is likely
to occur frequently [Ferrin82b].
This requires a file system providing
higher bandwidth than the original 512 byte UNIX
one that provides only about
two percent of the maximum disk bandwidth or about
20 kilobytes per second per arm [White80], [Smith81b].
.PP
Modifications have been made to the UNIX file system to improve
its performance.
Since the UNIX file system interface
is well understood and not inherently slow,
this development retained the abstraction and simply changed
the underlying implementation to increase its throughput.
Consequently, users of the system have not been faced with
massive software conversion.
.PP
Problems with file system performance have been dealt with
extensively in the literature; see [Smith81a] for a survey.
Previous work to improve the UNIX file system performance has been
done by [Ferrin82a].
The UNIX operating system drew many of its ideas from Multics,
a large, high performance operating system [Feiertag71].
Other work includes Hydra [Almes78],
Spice [Thompson80],
and a file system for a LISP environment [Symbolics81].
A good introduction to the physical latencies of disks is
described in [Pechura83].
.ds RH Old file system
.sp 2
.ne 1i
