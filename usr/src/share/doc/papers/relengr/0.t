.\"	@(#)0.t	1.1	(Copyright 1989 M. K. McKusick)	89/02/18
.rm CM
.nr Fn 0 1
.ds b3 4.3\s-1BSD\s+1
.de KI
.ds Lb "Fig. \\n+(Fn
.KF
.ce 1
Figure \\n(Fn - \\$1.
..
.de SM
\\s-1\\$1\\s+1
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
The Release Engineering of 4.3BSD
.AU
Marshall Kirk McKusick
.AU
Michael J. Karels
.AI
Computer Systems Research Group
Computer Science Division
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California  94720
.AB
This paper gives a brief overview of the release engineering
techniques used during the release of the 4.3BSD UNIX\(dg
.FS
\(dgUNIX is a registered trademark of AT&T in the US and other countries.
.FE
operating system.
It describes the approach that a small group of people can
use to develop and integrate a large software system.
It then describes the three step methodology used to
engineer a clean, correct, and coherent release.
.AE
.LP
