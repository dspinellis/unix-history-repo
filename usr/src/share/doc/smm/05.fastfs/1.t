.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)1.t	5.1 (Berkeley) %G%
.\"
.ds RH Introduction
.NH
Introduction
.PP
This paper describes the changes from the original 512 byte UNIX file
system to the new one released with the 4.2 Berkeley Software Distribution.
It presents the motivations for the changes,
the methods used to affect these changes,
the rationale behind the design decisions,
and a description of the new implementation.
This discussion is followed by a summary of
the results that have been obtained,
directions for future work,
and the additions and changes
that have been made to the user visible facilities.
The paper concludes with a history of the software engineering
of the project.
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
arbitrarily within the data area of the file system.  No constraints
other than available disk space are placed on file growth
[Ritchie74], [Thompson79].
.PP
When used on the VAX-11 together with other UNIX enhancements,
the original 512 byte UNIX file
system is incapable of providing the data throughput rates
that many applications require.
For example, 
applications that need to do a small amount of processing
on a large quantities of data
such as VLSI design and image processing,
need to have a high throughput from the file system.
High throughput rates are also needed by programs with
large address spaces that are constructed by mapping
files from the file system into virtual memory.
Paging data in and out of the file system is likely
to occur frequently.
This requires a file system providing
higher bandwidth than the original 512 byte UNIX one which provides only about
two percent of the maximum disk bandwidth or about
20 kilobytes per second per arm [White80], [Smith81b].
.PP
Modifications have been made to the UNIX file system to improve
its performance.
Since the UNIX file system interface
is well understood and not inherently slow,
this development retained the abstraction and simply changed
the underlying implementation to increase its throughput.
Consequently users of the system have not been faced with
massive software conversion.
.PP
Problems with file system performance have been dealt with
extensively in the literature; see [Smith81a] for a survey.
The UNIX operating system drew many of its ideas from Multics,
a large, high performance operating system [Feiertag71].
Other work includes Hydra [Almes78],
Spice [Thompson80],
and a file system for a lisp environment [Symbolics81a].
.PP
A major goal of this project has been to build a file system that is
extensible into a networked environment [Holler73].
Other work on network file systems describe
centralized file servers [Accetta80],
distributed file servers [Dion80], [Luniewski77], [Porcar82],
and protocols to reduce the amount of information that must be
transferred across a network [Symbolics81b], [Sturgis80].
.ds RH Old file system
.bp
