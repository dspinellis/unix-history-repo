.\"	@(#)contrib.ack	5.2	90/08/03
.so HEADERS
.if n \{\
.ND
.rm CH
.ll 7i
.nr LL 7i
.ad l
.\}
.HE D
.nr PS 11
.nr VS 13
.nr LL 6.5i
.nr OI 0.5i
.in 0.5i
.ps 11p
.ti 4i
.nf
.pp
Much of the software in the Berkeley Software Distribution is contributed
by people and organizations in the user community.
We gratefully acknowledge these contributions; without their support
this distribution would not be possible.
.pp
As part of the 4.3BSD-Reno distribution, we have continued to
identify portions of the distribution which are freely
redistributable to and by anyone, except as restricted by law.
(As a note to anyone interested in international redistribution, we
have recently obtained a General License GTDA from the Commerce
Department covering the BSD Networking Release #1.)
Source code with the following copyright notice may be freely
redistributed, under the specified conditions, without further
license.
.sp
.in +.5i
Redistribution and use in source and binary forms are permitted provided
that: (1) source distributions retain this entire copyright notice and
comment, and (2) distributions including binaries display the following
acknowledgement:  ``This product includes software developed by the
University of California, Berkeley and its contributors'' in the
documentation or other materials provided with the distribution and in
all advertising materials mentioning features or use of this software.
Neither the name of the University nor the names of its contributors may
be used to endorse or promote products derived from this software without
specific prior written permission.
.br
THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
.in -.5i
.sp
.pp
The following list are people and organizations that have contributed
source code that will be part of the 4.4BSD release.
The first group provided large subsystems, the second, a specific
program or library routine.
Please report any errors or omissions to ``bigbug@ucbvax.berkeley.edu''.
Also, if you're interested in contributing software, please contact
the CSRG at this address.
.sp
.TS
l l.
HP300 support	Systems Programming Group of the University of
	Utah Computer Science Department.
386 support	William Jolitz
CCI 6/32 support	Computer Consoles Inc.
VAX device support	Digital Equipment Corp.
VAX BI support	Chris Torek
Stdio	Chris Torek
ANSI C library support	Chris Torek
Compiler support	The Free Software Foundation
NFS support	Rick Macklem
RPC support	Sun Microsystems Inc.
NFS automounter	Jan Pendry
Autoconfiguration	Robert Elz
Quota support	Robert Elz
Job control	Jim Kulp
Network device drivers	Micom-Interlan, Excelan
X11	Project Athena, MIT
Transport/Network OSI layers	IBM Corp., and the University of Wisconsin
VAX 3000 support	Mt. Xinu
Programming Language C	American National Standards Committee X3
System documentation	IEEE (POSIX)
.TE
.sp 2
.TS
l l l l.
cal(1)	Kim Letkeman	cat(1)	Kevin Fall
col(1)	Michael Rendell	comm(1)	Case Larsen
compress(1)	James A. Woods, Spencer Thomas	cp(1)	David Hitz
	and Joseph Orost
cut(1)	Adam S. Moskowitz	des(1)	Phil Karn, Jim Gillogly
	and Marciano Pitargue		and Richard Outerbridge
du(1)	Chris Newcomb	emacs(1)	Richard Stallman
find(1)	Cimarron Taylor	finger(1)	Tony Nardo
fold(1)	Kevin Ruddy	fpr(1)	Robert Corbett
fsplit(1)	Jerry Berkman and Asa Romberger	gprof(1)	Peter Kessler
indent(1)	David Willcox, Eric Schmidt,	jove(1)	Jonathon Payne
	James Gosling and Sun Microsystems
lex(1)	Vern Paxson	locate(1)	James A. Woods
ls(1)	Michael Fischbein	m4(1)	Ozan Yigit
mail(1)	Kurt Schoens	make(1)	Adam de Boor
mh(1)	Rand Corporation	mv(1)	Ken Smith
news(1)	Rick Adams (and a cast of thousands)	nm(1)	Hans Huebner
paste(1)	Adam S. Moskowitz	patch(1)	Larry Wall
rcs(1)	Walter F. Tichy	tn3270(1)	Gregory Minshall
tsort(1)	Michael Rendell	unifdef(1)	Dave Yost
uniq(1)	Case Larsen	uuq(1)	Lou Salkind and Rick Adams
who(1)	Michael Fischbein	window(1)	Edward Wang
write(1)	Craig Leres and Jef Poskanzer	xargs(1)	John B. Roll Jr.
yacc(1)	Robert Paul Corbett
	
bitstring(3)	Paul Vixie	ctime(3) (timezone support)	Arthur David Olson
fnmatch(3)	Guido van Rossum	glob(3)	Guido van Rossum
lsearch(3)	Roger L. Snyder	nsaddr(3)	J.Q. Johnson
	
arithmetic(6)	Eamonn McManus	atc(6)	Ed James
bcd(6)	Steve Hayman	caesar(6)	Rick Adams
ching(6)	Guy Harris	factor(6)	Landon Curt Noll
fortune(6)	Ken Arnold	hack(6)	Andries Brouwer
			(and a cast of thousands)
phantasia(6)	Edward Estes	primes(6)	Landon Curt Noll
rogue(6)	Timothy C. Stoehr	wump(6)	Dave Taylor
	
arp(8)	Sun Microsystems Inc.	clri(8)	Rich $alz
disklabel(8)	Symmetric Computer Systems	mknod(8)	Kevin Fall
ping(8)	Mike Muuss	sendmail(8)	Eric Allman
slip(8)	Rick Adams	traceroute(8)	Van Jacobson
uucpd(8)	Rick Adams	uusnap(8)	Randy King and Rick Adams
.TE
