.\"	@(#)0.t	1.4	(Copyright 1990 M. K. McKusick)	90/04/16
.rm CM
.nr PO 1.25i
.ds CH "
.ds CF "%
.nr Fn 0 1
.ds b3 4.3\s-1BSD\s+1
.de KI
.ds Lb "Fig. \\n+(Fn
.KF
.ce 1
Figure \\n(Fn - \\$1.
..
.de SM
\\s-1\\$1\\s+1\\$2
..
.de NM
\&\fI\\$1\fP\\$2
..
.de RN
\&\fI\\$1\fP\^(\^)\\$2
..
.de PN
\&\fB\\$1\fP\\$2
..
.TL
A Pageable Memory Based Filesystem
.AU
Marshall Kirk McKusick
.AU
Michael J. Karels
.AU
Keith Bostic
.AI
Computer Systems Research Group
Computer Science Division
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California  94720
.sp
email: mckusick@cs.Berkeley.EDU
telephone: 415-642-4948
.AB
This paper describes the motivations for memory-based filesystems.
It compares techniques used to implement them and
describes the drawbacks of using dedicated memory to
support such filesystems.
To avoid the drawbacks of using dedicated memory,
it discusses building a simple memory-based
filesystem in pageable memory.
It details the performance characteristics of this filesystem
and concludes with areas for future work.
.AE
.LP
