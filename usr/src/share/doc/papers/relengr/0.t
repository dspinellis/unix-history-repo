.\"	@(#)0.t	1.4	(Copyright 1989 M. K. McKusick)	89/02/24
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
The Release Engineering of 4.3\s-1BSD\s0
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
.AB
This paper describes an approach used by a small group of people
to develop and integrate a large software system.
It details the development and release engineering strategy
used during the preparation of the \*(b3 version of the UNIX\(dg
.FS
\(dgUNIX is a registered trademark of AT&T in the US and other countries.
.FE
operating system.
Each release cycle is divided into an initial development phase
followed by a release engineering phase.
The release engineering of the distribution is done in three steps.
The first step has an informal control policy for tracking modifications;
it results in an alpha distribution.
The second step has more rigid change mechanisms in place;
it results in a beta release.
During the final step changes are tracked very closely;
the result is the final distribution.
.AE
.LP
