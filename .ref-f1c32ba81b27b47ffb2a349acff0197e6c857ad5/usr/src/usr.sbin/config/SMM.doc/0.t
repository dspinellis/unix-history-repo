.\" Copyright (c) 1983 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)0.t	6.4 (Berkeley) %G%
.\"
.bd S B 3
.de UX
.ie \\n(GA>0 \\$2UNIX\\$1
.el \{\
.if n \\$2UNIX\\$1*
.if t \\$2UNIX\\$1\\f1\(dg\\fP
.FS
.if n *UNIX
.if t \(dgUNIX
.ie \\$3=1 is a Footnote of Bell Laboratories.
.el is a Trademark of Bell Laboratories.
.FE
.nr GA 1\}
..
.TL
Building Berkeley 
.UX
Kernels with Config
.AU
Samuel J. Leffler and Michael J. Karels
.AI
Computer Systems Research Group
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California  94720
.de IR
\fI\\$1\fP\\$2
..
.de DT
.TA 8 16 24 32 40 48 56 64 72 80
..
.AB
.PP
This document describes the use of
\fIconfig\fP\|(8) to configure and create bootable
4.3BSD system images.
It discusses the structure of system
configuration files and how to configure
systems with non-standard hardware configurations.
Sections describing the preferred way to
add new code to the system and how the system's autoconfiguration
process operates are included.  An appendix
contains a summary of the rules used by the system
in calculating the size of system data structures,
and also indicates some of the standard system size
limitations (and how to change them).
Other configuration options are also listed.
.sp
.LP
Revised 
.AE
.LP
.OH 'Building Kernels with Config''SMM:2-%'
.EH 'SMM:2-%''Building Kernels with Config'
