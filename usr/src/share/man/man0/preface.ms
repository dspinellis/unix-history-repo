.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)preface.ms	6.3 (Berkeley) 5/13/86
.\"
.nr PS 9
.nr VS 11
.vs 11p
.ps 9p
.TL
PREFACE
.OH 'Preface''- % -'
.EH '- % -''Preface'
.af % i
.LP
This update to the 4.2 distribution of August 1983 provides
substantially improved performance, reliability, and security,
the addition of Xerox Network System (NS) to the set of networking domains,
and partial support for the VAX 8600 and MICROVAXII.
.LP
We were greatly assisted by the DEC UNIX Engineering group who
provided two full time employees, Miriam Amos and Kevin Dunlap,
to work at Berkeley. They were responsible for developing and
debugging the distributed domain based name server
and integrating it into the mail system.
Mt Xinu provided the bug list distribution service as well as
donating their MICROVAXII port to 4.3BSD.
Drivers for the MICROVAXII were done by Rick Macklem
at the University of Guelph.
Sam Leffler provided valuable assistance and advice with many projects.
Keith Sklower coordinated with William Nesheim and J. Q. Johnson at Cornell, 
and Chris Torek and James O'Toole at the University of Maryland
to do the Xerox Network Systems implementation.
Robert Elz at the University of Melbourne contributed greatly
to the performance work in the kernel.
Donn Seeley and Jay Lepreau at the University of Utah
relentlessly dealt with a miriad of details;
Donn completed the unfinished performance work on Fortran 77
and fixed numerous C compiler bugs.
Ralph Campbell handled innumerable questions and problem reports
and had time left to write rdist.
George Goble was invaluable in shaking out the bugs on his
production systems long before we were confident enough to
inflict it on our users.
Bill Shannon at Sun Microsystems has been helpful in 
providing us with bug fixes and improvements.
Tom Ferrin, in his capacity as Board Member of Usenix Association,
handled the logistics of large-scale reproduction
of the 4.2BSD and 4.3BSD manuals.
Mark Seiden helped with the typesetting and indexing of the 4.3BSD manuals.
Special mention goes to Bob Henry for keeping ucbvax running
in spite of new and improved software and
an ever increasing mail, news, and uucp load.
.LP
Numerous others contributed their time and energy in creating
the user contributed software for the release.
As always, we are grateful to the UNIX user community for 
encouragement and support. 
.LP
Once again, the financial support of the Defense Advanced Research
Projects Agency is gratefully acknowledged.
.sp 1
.in 4i
.nf
M. K. McKusick
M. J. Karels
J. M. Bloom
.fi
.in 0
.sp 2
.ne 1i
.ce
\fIPreface to the 4.2 Berkeley distribution\fP
.sp 1
This update to the 4.1 distribution of June 1981 provides support
for the VAX 11/730, full networking and interprocess communication
support, an entirely new file system, and many other new features.
It is certainly the most ambitious release of software ever prepared
here and represents many man-years of work.
Bill Shannon (both at DEC and at Sun Microsystems)
and Robert Elz of the University
of Melbourne contributed greatly to this distribution
through new device drivers and painful debugging episodes.
Rob Gurwitz of BBN wrote the initial version of the code upon
which the current networking support is based.
Eric Allman of Britton-Lee donated countless hours to the mail system.
Bill Croft (both at SRI and Sun Microsystems) aided in the
debugging and development of the networking facilities.
Dennis Ritchie of Bell Laboratories also
contributed greatly to this distribution, providing
valuable advise and guidance.  Helge Skrivervik
worked on the device drivers which enabled
the distribution to be delivered with a TU58
console cassette and RX01 console flopppy disk, and
rewrote major portions of the standalone i/o system
to support formatting of non-DEC peripherals.
.LP
Numerous others contributed their time and energy in organizing
the user software for release, while many groups of people on
campus suffered patiently through the low spots of development.
As always, we are grateful to the UNIX user community for 
encouragement and support. 
.LP
Once again, the financial support of the Defense Advanced Research
Projects Agency is gratefully acknowledged.
.sp 1
.in 4i
.nf
S. J. Leffler
W. N. Joy
M. K. McKusick
.fi
.in 0
.sp 2
.ne 1i
.ce
\fIPreface to the 4.1 Berkeley distribution\fP
.sp 1
This update to the fourth distribution of November 1980 provides
support for the VAX 11/750 and for the full interconnect architecture
of the VAX 11/780.  Robert Elz of the University of Melbourne contributed
greatly to this distribution especially in the boot-time system
configuration code;  Bill Shannon of DEC supplied us with the
implementation of DEC standard bad block handling.  The research
group at Bell Laboratories and DEC Merrimack provided us with access
to 11/750's in order to debug its support.
.LP
Other individuals too numerous to mention provided us with bug reports,
fixes and other enhancements which are reflected in the system.  We
are grateful to the UNIX user community for encouragement and
support.
.LP
The financial support of the Defence Advanced Research Projects Agency
in support of this work is gratefully acknowledged.
.sp 1
.in 4i
.nf
W. N. Joy
R. S. Fabry
K. Sklower
.fi
.in 0
.sp 2
.ne 1i
.ce
\fIPreface to the Fourth Berkeley distribution\fP
.sp 1
This manual reflects the Berkeley system mid-October, 1980.
A large amount of tuning has been done in the system since the last release;
we hope this provides as noticeable an improvement for you as it did for us.
This release finds the system in transition; a number of facilities
have been added in experimental versions (job control, resource limits)
and the implementation of others is imminent (shared-segments, higher
performance from the file system, etc.).
Applications which use facilities that are in transition should be aware
that some of the system calls and library routines will change
in the near future.  We have tried to be conscientious and make it
very clear where this is likely.
.LP
A new group has been formed
at Berkeley, to assume responsibility for the future
development and support of a version of UNIX on the VAX.
The group has received funding from the
Defense Advanced Research Projects Agency (DARPA)
to supply a standard version of the system to DARPA contractors.
The same version of the system will be made available to other licensees
of UNIX on the VAX for a duplication charge.
We gratefully acknowledge
the support of this contract.
.LP
We wish to acknowledge the contribution of a number of individuals to
the the system.
.LP
We would especially like to thank
Jim Kulp of IIASA,
Laxenburg Austria and his colleagues,
who first put job control facilities into UNIX;
Eric Allman, Robert Henry, Peter Kessler and Kirk McKusick, who
contributed major new pieces of software;
Mark Horton, who contributed to the improvement of facilities and
substantially improved the quality of our bit-mapped fonts,
our hardware support staff:
Bob Kridle,
Anita Hirsch,
Len Edmondson
and
Fred Archibald,
who helped us to debug a number of new peripherals;
Ken Arnold who did much of the leg-work in getting this version of the
manual prepared, and did the final editing of sections 2-6,
some special individuals within Bell Laboratories:
Greg Chesson,
Stuart Feldman,
Dick Haight,
Howard Katseff,
Brian Kernighan,
Tom London,
John Reiser,
Dennis Ritchie,
Ken Thompson,
and
Peter Weinberger
who helped out by answering questions;
our excellent local DEC field service people,
Kevin Althaus and Frank Chargois
who kept our machine running virtually all the time, and fixed it quickly
when things broke;
and,
Mike Accetta of Carnegie-Mellon University,
Robert Elz of the University of Melbourne,
George Goble of Purdue University,
and
David Kashtan of the Stanford Research Institute
for their technical advice and support.
.LP
Special thanks to Bill Munson of DEC who helped by augmenting
our computing facility
and to Eric Allman for carefully proofreading the
``last'' draft of the manual and finding the bugs which we knew were
there but couldn't see. 
.LP
We dedicate this to the memory of David Sakrison, late chairman of our
department, who gave his support to the establishment of our VAX
computing facility, and to our department as a whole.
.sp 1
.in 4i
.nf
W. N. Joy
\v'-3p'\h'2p'\*:\v'3p'\h'-2p'O. Babao\*~glu
R. S. Fabry
K. Sklower
.fi
.in 0
.sp 2
.ne 1i
.ce
\fIPreface to the Third Berkeley distribution\fP
.sp 1
This manual reflects the state of the Berkeley system, December 1979.
We would like to thank all the people at Berkeley who have contributed to
the system, and particularly thank
Prof. Richard Fateman for creating and administrating a hospitable environment,
Mark Horton who helped prepare this manual, and
Eric Allman, Bob Kridle, Juan Porcar
and Richard Tuck for their contributions to the kernel.
.LP
The cooperation of Bell Laboratories in providing us with an early version of
\s-2UNIX\s0/32V is greatly appreciated.  We would especially like to thank
Dr. Charles Roberts of Bell Laboratories for helping us obtain this release,
and acknowledge
T. B. London,
J. F. Reiser,
K. Thompson,
D. M. Ritchie,
G. Chesson and
H. P. Katseff
for their advice and support.
.sp 1
.in 4i
W. N. Joy
.br
\v'-3p'\h'2p'\*:\v'3p'\h'-2p'O. Babao\*~glu
.in 0
.sp 2
.ne 1i
.ce
\fIPreface to the UNIX/32V distribution\fP
.sp 1
The
.UX
operating system for the VAX*-11
.FS
*VAX and PDP are Trademarks of Digital Equipment Corporation.
.FE
provides substantially the same facilities as the
\s-2UNIX\s0
system for the PDP*-11.
.LP
We acknowledge the work of many who came before us, and particularly thank
G. K. Swanson, W. M. Cardoza, D. K. Sharma, and J. F. Jarvis for assistance
with the implementation for the VAX-11/780.
.sp 1
.in 4i
T. B. London
.br
J. F. Reiser
.in 0
.sp 2
.ne 1i
.ce
\fIPreface to the Seventh Edition\fP
.sp 1
.LP
Although this Seventh Edition no longer bears their byline,
Ken Thompson and Dennis Ritchie remain the fathers
and preceptors of the
\s-2UNIX\s0
time-sharing system.
Many of the improvements here described bear their mark.
Among many, many other people who have contributed to
the further flowering of
\s-2UNIX\s0,
we wish especially to
acknowledge the contributions of
A. V. Aho,
S. R. Bourne,
L. L. Cherry,
G. L. Chesson,
S. I. Feldman,
C. B. Haley,
R. C. Haight,
S. C. Johnson,
M. E. Lesk,
T. L. Lyon,
L. E. McMahon,
R. Morris,
R. Muha,
D. A. Nowitz,
L. Wehr,
and
P. J. Weinberger.
We appreciate also 
the effective advice and criticism of
T. A. Dolotta,
A. G. Fraser,
J. F. Maranzano,
and
J. R. Mashey;
and we remember the important work of
the late Joseph F. Ossanna.
.sp 1
.in 4i
B. W. Kernighan
.br
M. D. McIlroy
.in 0
