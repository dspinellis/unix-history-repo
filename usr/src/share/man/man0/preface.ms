.\" Copyright (c) 1980, 1993
.\"	Regents of the University of California.  All rights reserved.
.\"
.\" Redistribution and use in source and binary forms, with or without
.\" modification, are permitted provided that the following conditions
.\" are met:
.\" 1. Redistributions of source code must retain the above copyright
.\"    notice, this list of conditions and the following disclaimer.
.\" 2. Redistributions in binary form must reproduce the above copyright
.\"    notice, this list of conditions and the following disclaimer in the
.\"    documentation and/or other materials provided with the distribution.
.\" 3. All advertising materials mentioning features or use of this software
.\"    must display the following acknowledgement:
.\"	This product includes software developed by the University of
.\"	California, Berkeley and its contributors.
.\" 4. Neither the name of the University nor the names of its contributors
.\"    may be used to endorse or promote products derived from this software
.\"    without specific prior written permission.
.\"
.\" THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
.\" ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
.\" IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
.\" ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
.\" FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
.\" DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
.\" OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
.\" HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.\" LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
.\" OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
.\" SUCH DAMAGE.
.\"
.\"	@(#)preface.ms	8.1 (Berkeley) 7/5/93
.\"
\&
.pn 4
.af % i
.\"
.\" The contributor list below is derived from the file that resides in
.\" vangogh:~admin/contrib/contrib:
.\"
.\"	@(#)contrib	5.51 (Berkeley) 7/1/93
.\"
.\" This file should not be editted, rather the original contrib file
.\" should be used to recrete this one following the directions at its top.
.\" Contrib starts here and continues to the comment `END OF CONTRIB'.
.\"
\&
.sp |1i
.ps 16
.ce
\fBThe Computer Systems Research Group 1979 \- 1993\fP
.if \n%>1 \{
.	OH '\s10Community''- % -\s0'
.	EH '\s10- % -''Community\s0'
.\}
.sp 3
.nr PS 11
.nr VS 12
.ps 11
.vs 12
.nf
.in +0.5i
\fBCSRG Technical Staff\fP
.sp 1
.in +1i
Jim Bloom
Keith Bostic
Ralph Campbell
Kevin Dunlap
William N. Joy
Michael J. Karels
Samuel J. Leffler
Marshall Kirk McKusick
Miriam Amos Nihart
Keith Sklower
Marc Teitelbaum
Michael Toy
.in -1i
.sp 3
\fBCSRG Administration and Support\fP
.sp 1
.in +1i
Robert Fabry
Domenico Ferrari
Susan L. Graham
Bob Henry
Anne Hughes
Bob Kridle
David Mosher
Pauline Schwartz
Mark Seiden
Jean Wood
.in -1i
.fi
.sp 3
\fBOrganizations that funded the CSRG with grants,
gifts, personnel, and/or hardware.\fP
.sp 1
.nf
.in +1i
Center for Advanced Aviation System Development, The MITRE Corp.
Compaq Computer Corporation
Cray Research Inc.
Department of Defense Advance Research Projects Agency (DARPA)
Digital Equipment Corporation
The Hewlett-Packard Company
NASA Ames Research Center
The National Science Foundation
The Open Software Foundation
UUNET Technologies Inc.
.in -1.5i
.fi
.bp
.nr PS 10
.nr VS 11
.ps 10
.vs 11
.LP
\fBThe following are people and organizations that provided a
large subsystem for the BSD releases.\fP
.sp
.TS
l l.
ANSI C library	Chris Torek
ANSI C prototypes	Donn Seeley and John Kohl
Autoconfiguration	Robert Elz
C library documentation	American National Standards Committee X3
CCI 6/32 support	Computer Consoles Inc.
DEC 3000/5000 support	Ralph Campbell
Disklabels	Symmetric Computer Systems
Documentation	Cynthia Livingston and The USENIX Association
Franz Lisp	Richard Fateman, John Foderaro, Keith Sklower, Kevin Layer
GCC, GDB	The Free Software Foundation
Groff	James Clark (The FSF)
HP300 support	Jeff Forys, Mike Hibler, Jay Lepreau, Donn Seeley and the Systems
	Programming Group; University of Utah Computer Science Department
ISODE	Marshall Rose
Ingres	Mike Stonebraker, Gene Wong, and the Berkeley Ingres Research Group
Intel 386/486 support	Bill Jolitz and TeleMuse
Job control	Jim Kulp
Kerberos	Project Athena and MIT
Kernel support	Bill Shannon and Sun Microsystems Inc.
LFS	Margo Seltzer, Mendel Rosenblum, Carl Staelin
MIPS support	Trent Hein
Math library	K.C. Ng, Zhishun Alex Liu, S. McDonald, P. Tang and W. Kahan
NFS	Rick Macklem
NFS automounter	Jan-Simon Pendry
Network device drivers	Micom-Interlan and Excelan
Omron Luna support	Akito Fujita and Shigeto Mochida
Quotas	Robert Elz
RPC support	Sun Microsystems Inc.
Shared library support	Rob Gingell and Sun Microsystems Inc.
Sony News 3400 support	Kazumasa Utashiro
Sparc I/II support	Computer Systems Engineering Group, Lawrence Berkeley Laboratory
Stackable file systems	John Heidemann
Stdio	Chris Torek
System documentation	The Institute of Electrical and Electronics Engineers, Inc.
TCP/IP	Rob Gurwitz and Bolt Beranek and Newman Inc.
Timezone support	Arthur David Olson
Transport/Network OSI layers	IBM Corporation and the University of Wisconsin
Kernel XNS assistance	William Nesheim, J. Q. Johnson, Chris Torek, and James O'Toole
User level XNS	Cornell University
VAX 3000 support	Mt. Xinu and Tom Ferrin
VAX BI support	Chris Torek
VAX device support	Digital Equipment Corporation and Helge Skrivervik
Versatec printer/plotter support	University of Toronto
Virtual memory implementation	Avadis Tevanian, Jr., Michael Wayne Young,
	and the Carnegie-Mellon University Mach project
X25 	University of British Columbia
.TE
.bp
.LP
\fBThe following are people and organizations that provided a specific
item, program, library routine or program maintenance for the BSD system.
(Their contribution may not be part of the final 4.4BSD release.)\fP
.nr PS 9
.nr VS 10
.ps 9
.vs 10
.sp
.TS
l l.
386 device drivers	Carnegie-Mellon University Mach project
386 device drivers	Don Ahn, Sean Fagan and Tim Tucker
HCX device drivers	Harris Corporation
Kernel enhancements	Robert Elz, Peter Ivanov, Ian Johnstone, Piers Lauder,
	John Lions, Tim Long, Chris Maltby, Greg Rose and John Wainwright 
.TE
.TS
l l l l.
adventure(6)	Don Woods	look(1)	David Hitz
adventure(6)	Jim Gillogly	ls(1)	Elan Amir
adventure(6)	Will Crowther	ls(1)	Michael Fischbein
apply(1)	Rob Pike	lsearch(3)	Roger L. Snyder
ar(1)	Hugh A. Smith	m4(1)	Ozan Yigit
arithmetic(6)	Eamonn McManus	mail(1)	Kurt Schoens
arp(8)	Sun Microsystems Inc.	make(1)	Adam de Boor
at(1)	Steve Wall	me(7)	Eric Allman
atc(6)	Ed James	mergesort(3)	Peter McIlroy
awk(1)	Arnold Robbins	mh(1)	Marshall Rose
awk(1)	David Trueman	mh(1)	The Rand Corporation
backgammon(6)	Alan Char	mille(6)	Ken Arnold
banner(1)	Mark Horton	mknod(8)	Kevin Fall
battlestar(6)	David Riggle	monop(6)	Ken Arnold
bcd(6)	Steve Hayman	more(1)	Eric Shienbrood
bdes(1)	Matt Bishop	more(1)	Mark Nudleman
berknet(1)	Eric Schmidt	mountd(8)	Herb Hasler
bib(1)	Dain Samples	mprof(1)	Ben Zorn
bib(1)	Gary M. Levin	msgs(1)	David Wasley
bib(1)	Timothy A. Budd	multicast	Stephen Deering
bitstring(3)	Paul Vixie	mv(1)	Ken Smith
boggle(6)	Barry Brachman	named/bind(8)	Douglas Terry
bpf(4)	Steven McCanne	named/bind(8)	Kevin Dunlap
btree(3)	Mike Olson	news(1)	Rick Adams (and a cast of thousands)
byte-range locking	Scooter Morris	nm(1)	Hans Huebner
caesar(6)	John Eldridge	pascal(1)	Kirk McKusick
caesar(6)	Stan King	pascal(1)	Peter Kessler
cal(1)	Kim Letkeman	paste(1)	Adam S. Moskowitz
cat(1)	Kevin Fall	patch(1)	Larry Wall
chess(6)	Stuart Cracraft (The FSF)	pax(1)	Keith Muller
ching(6)	Guy Harris	phantasia(6)	C. Robertson
cksum(1)	James W. Williams	phantasia(6)	Edward A. Estes
clri(8)	Rich $alz	ping(8)	Mike Muuss
col(1)	Michael Rendell	pom(6)	Keith E. Brandt
comm(1)	Case Larsen	pr(1)	Keith Muller
compact(1)	Colin L. McMaster	primes(6)	Landon Curt Noll
compress(1)	James A. Woods	qsort(3)	Doug McIlroy
compress(1)	Joseph Orost	qsort(3)	Earl Cohen
compress(1)	Spencer Thomas	qsort(3)	Jon Bentley
courier(1)	Eric Cooper	quad(3)	Chris Torek
cp(1)	David Hitz	quiz(6)	Jim R. Oldroyd
cpio(1)	AT&T	quiz(6)	Keith Gabryelski
crypt(3)	Tom Truscott	radixsort(3)	Dan Bernstein
csh(1)	Christos Zoulas	radixsort(3)	Peter McIlroy
csh(1)	Len Shar	rain(6)	Eric P. Scott
curses(3)	Elan Amir	ranlib(1)	Hugh A. Smith
curses(3)	Ken Arnold	rcs(1)	Walter F. Tichy
cut(1)	Adam S. Moskowitz	rdist(1)	Michael Cooper
cut(1)	Marciano Pitargue	regex(3)	Henry Spencer
dbx(1)	Mark Linton	robots(6)	Ken Arnold
dd(1)	Keith Muller	rogue(6)	Timothy C. Stoehr
dd(1)	Lance Visser	rs(1)	John Kunze
des(1)	Jim Gillogly	sail(6)	David Riggle
des(1)	Phil Karn	sail(6)	Edward Wang
des(1)	Richard Outerbridge	sccs(1)	Eric Allman
dipress(1)	Xerox Corporation	scsiformat(1)	Lawrence Berkeley Laboratory
disklabel(8)	Symmetric Computer Systems	sdb(1)	Howard Katseff
du(1)	Chris Newcomb	sed(1)	Diomidis Spinellis
dungeon(6)	R.M. Supnik	sendmail(8)	Eric Allman
ed(1)	Rodney Ruddock	setmode(3)	Dave Borman
emacs(1)	Richard Stallman	sh(1)	Kenneth Almquist
erf(3)	Peter McIlroy, K.C. Ng	slattach(8)	Rick Adams
error(1)	Robert R. Henry	slip(8)	Rick Adams
ex(1)	Mark Horton	spms(1)	Peter J. Nicklin
factor(6)	Landon Curt Noll	strtod(3)	David M. Gay
find(1)	Cimarron Taylor	swab(3)	Jeffrey Mogul
finger(1)	Tony Nardo	sysconf(3)	Sean Eric Fagan
fish(6)	Muffy Barkocy	sysline(1)	J.K. Foderaro
fmt(1)	Kurt Schoens	syslog(3)	Eric Allman
fnmatch(3)	Guido van Rossum	systat(1)	Bill Reeves
fold(1)	Kevin Ruddy	systat(1)	Robert Elz
fortune(6)	Ken Arnold	tail(1)	Edward Sze-Tyan Wang
fpr(1)	Robert Corbett	talk(1)	Clem Cole
fsdb(8)	Computer Consoles Inc.	talk(1)	Kipp Hickman
fsplit(1)	Asa Romberger	talk(1)	Peter Moore
fsplit(1)	Jerry Berkman	telnet(1)	Dave Borman
gcc/groff integration	UUNET Technologies, Inc.	telnet(1)	Paul Borman
gcore(1)	Eric Cooper	termcap(5)	John A. Kunze
getcap(3)	Casey Leedom	termcap(5)	Mark Horton
glob(3)	Guido van Rossum	test(1)	Kenneth Almquist
gprof(1)	Peter Kessler	tetris(6)	Chris Torek
gprof(1)	Robert R. Henry	tetris(6)	Darren F. Provine
hack(6)	Andries Brouwer (and a cast of thousands)	timed(8)	Riccardo Gusella
hangman(6)	Ken Arnold	timed(8)	Stefano Zatti
hash(3)	Margo Seltzer	tn3270(1)	Gregory Minshall
heapsort(3)	Elmer Yglesias	tr(1)	Igor Belchinskiy
heapsort(3)	Kevin Lew	traceroute(8)	Van Jacobson
heapsort(3)	Ronnie Kon	trek(6)	Eric Allman
hunt(6)	Conrad Huang	tset(1)	Eric Allman
hunt(6)	Greg Couch	tsort(1)	Michael Rendell
icon(1)	Bill Mitchell	unifdef(1)	Dave Yost
icon(1)	Ralph Griswold	uniq(1)	Case Larsen
indent(1)	David Willcox	uucpd(8)	Rick Adams
indent(1)	Eric Schmidt	uudecode(1)	Mark Horton
indent(1)	James Gosling	uuencode(1)	Mark Horton
indent(1)	Sun Microsystems	uuq(1)	Lou Salkind
init(1)	Donn Seeley	uuq(1)	Rick Adams
j0(3)	Sun Microsystems, Inc.	uusnap(8)	Randy King
j1(3)	Sun Microsystems, Inc.	uusnap(8)	Rick Adams
jn(3)	Sun Microsystems, Inc.	vacation(1)	Eric Allman
join(1)	David Goodenough	vi(1)	Steve Kirkendall
join(1)	Michiro Hikida	which(1)	Peter Kessler
join(1)	Steve Hayman	who(1)	Michael Fischbein
jot(1)	John Kunze	window(1)	Edward Wang
jove(1)	Jonathon Payne	worm(6)	Michael Toy
kermit(1)	Columbia University	worms(6)	Eric P. Scott
kvm(3)	Peter Shipley	write(1)	Craig Leres
kvm(3)	Steven McCanne	write(1)	Jef Poskanzer
lam(1)	John Kunze	wump(6)	Dave Taylor
larn(6)	Noah Morgan	X25/Ethernet	Univ. of Erlangen-Nuremberg
lastcomm(1)	Len Edmondson	X25/LLC2	Dirk Husemann
lex(1)	Vern Paxson	xargs(1)	John B. Roll Jr.
libm(3)	Peter McIlroy	xneko(6)	Masayuki Koba
libm(3)	UUNET Technologies, Inc.	XNSrouted(1)	Bill Nesheim
locate(1)	James A. Woods	xroach(6)	J.T. Anderson
lock(1)	Bob Toxen	yacc(1)	Robert Paul Corbett
log(3)	Peter McIlroy
.TE
.\"
.\" END OF CONTRIB: Contrib ends here.
.\"
.nr PS 9
.nr VS 10
.ps 9
.vs 10
.OH ''''
.EH ''''
.bp
\&
.sp |2i
.ce
\fB\s+3PREFACE\s-3\fP
.OH '\s10Preface''- % -\s0'
.EH '\s10- % -''Preface\s0'
.sp 3
.LP
This update to the 4.3 distribution of April 1986 provides
substantially improved functionality, security, and standardization,
a Sun-compatible Network Filesystem (NFS),
a new virtual memory implementation based on Carnegie-Mellon's MACH, and
the addition of ISO networking protocols to the set of networking domains.
It adds support for several new architectures including
SPARC-based Sparcstations 1 and 2,
MIPS-based Decstation 3100 and 5000 and Sony NEWS,
68000-based Hewlett-Packard 9000/300 and Omron Luna, and
386-based Personal Computers.
.LP
We were greatly assisted by the recent employees of the
Computer Systems Research Group: Mike Karels, Marc Tietelbaum,
and Keith Sklower.
Our distribution coordinator, Pauline Schwartz,
has reliably managed the finances
and the mechanics of gettings distributions shipped for
nearly the entire fourteen years of the group's existence.
However, the vast majority of the distribution comes from the
numerous people in the UNIX community
that provided their time and energy in creating
the software contributed to this release.
We dedicate this distribution to them.
.LP
This distribution is the last
that will be done by the Computer Systems Research
Group at the University of California at Berkeley.
We acknowledge the following three people for convincing us
that this should be the final distribution:
David Hodges, Dean of the College of Engineering at Berkeley,
Steve Squires, Program Manager for the
Defense Advance Research Projects Agency, and
Sanford Tannenbaum, Executive Vice President and General Counsel for
UNIX Systems Laboratories.
.sp 1
.in 4i
.nf
M. K. McKusick
K. Bostic
.fi
.in 0
.sp 2
.ne 1i
.ce
\fIPreface to the 4.3 Berkeley distribution\fP
.sp 1
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
