.\" Copyright (c) 1983, 1993
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)a.t	8.2 (Berkeley) %G%
.\"
.de L0
.nr l1 0
.nr l2 0
.nr l3 0
.nr l4 0
..
.de L1
.nr l1 +1
.nr l2 0
.nr l3 0
.nr l4 0
.sp 0.5
\\n(l1	\fB\\$1\fP
..
.de L2
.nr l2 +1
.nr l3 0
.nr l4 0
.sp 0.25
\\n(l1.\\n(l2	\fB\\$1\fP
..
.de L3
..
.de L4
..
.de Nm
.br
 		\\$1	\\$3
..
.bp
.Sh 1 "Summary of facilities
.sp 1
.ta \w'8.8\ \ 'u +0.25i +\w'gettimeofday\ \ 'u
.so Toc
.pn 2
.bp
.de L0
.nr l1 0
.nr l2 0
.nr l3 0
.nr l4 0
.br
 		\fB\\$1\fP	\\$2
..
.de L1
.nr l1 +1
.nr l2 0
.nr l3 0
.nr l4 0
.sp 0.5
 	\\n(l1	\fB\\$1\fP	\\$2
..
.de L2
.nr l2 +1
.nr l3 0
.nr l4 0
.sp 0.25
 	\\n(l1.\\n(l2	\fB\\$1\fP	\\$2
..
.de L3
.nr l3 +1
.nr l4 0
.br
 	\\n(l1.\\n(l2.\\n(l3	\\$1	\\$2
..
.de L4
.nr l4 +1
.br
 	\\n(l1.\\n(l2.\\n(l3.\\n(l4	\\$1	\\$2
..
.de Nm
..
.ce 1
\s+4\fBContents\fP\s0
.sp 2
.ta \w'8.8.8.88\ \ 'uR +\w'\ \ \ 'u 6iR
.so Toc
.sy mv toc Toc
