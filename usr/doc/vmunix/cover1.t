.nr FM .5i
.ev 0
.nf
.LP
.nf
.ev 1
.ll +10
.ND
.ps 12
.ft B
UNIVERSITY OF CALIFORNIA, BERKELEY
.ft R 
.sp
.vs 7p
\s20\l'25_'\s6
.br
BERKELEY \  \(bu \  DAVIS \  \(bu \  IRVINE \  \(bu \ 
LOS ANGELES \  \(bu \  RIVERSIDE \  \(bu \  SAN DIEGO
\  \(bu \  SAN FRANCISCO \  \(bu \  SANTA BARBARA \  \(bu \  SANTA CRUZ
.br
\s20\l'25_'\s7
.nf
.vs 9p
COLLEGE OF ENGINEERING  \l'25 '                                         BERKELEY, CALIFORNIA 94720
DEPARTMENT OF ELECTRICAL ENGINEERING
  AND COMPUTER SCIENCES \l'30 ' \s10\*(DY\s0
COMPUTER SCIENCE DIVISION
.fi
.sp
.ev 0
.ti +3.5i
July 8, 1981
.LP
.LP
This is a full distribution kit for the second release of the Fourth Berkeley
software tape, known as 4.1bsd.
The package you received should have contained:
.nr a 0 1
.IP \n+a)
Either a 2400' 1600 bpi magnetic tape or two RK07 disk cartridges
containing the basic system software; this is the bootstrap distribution
media.
A second 2400' 1600 bpi tape or a third RK07 disk cartridge
contains additional material beyond the basic
system on the first tape (INGRES, source for documents in the manuals,
bit mapped fonts, etc.)
.IP \n+a)
Documents titled
``Installing and operating 4.1bsd'',
``Bug fixes and changes in 4.1bsd'',
``Changes to the kernel in 4.1bsd'',
and
``Hints on configuring VAX systems for UNIX''
.IP \n+a)
A two sided copy of volume 1 of the programmer's manual.
.nr b \na 1
.IP \n+a)
A single sided, reproduction-quality copy of Volume 1 of the
programmer's manual for the system. 
.IP \n+a)
A copy of a document describing
.I fsck.
.IP \n+a)
A two sided copy of volumes 2a and 2b of the programmer's manual.
.IP \n+a)
A single sided, reproduction-quality, copy of Volume 2c of the
programmer's manual for the system.
.IP \n+a)
2 Vi Reference Cards and a master for reproducing cards.
.IP \n+a)
Three documents describing the Berkeley Network.
.IP \n+a)
Two documents on the internals of the Pascal system.
manual and a new table of contents for volume 2c.
.LP
To set up the first tape, follow the instructions in the ``Setting up''
document.  It is a good idea to look through the whole document before
you begin.
If you have any questions, you can call us at (415) 642-7780.
.LP
The second tape contains the following hierarchies:
.so cover1a.t
.LP
Bug reports about this distribution can be mailed to
``arpavax.4bsd-bugs@berkeley'' on the ARPANET, or to ``ucbvax!4bsd-bugs''
on the uucp network (you will have to figure out how to get to ucbvax
on the uucp net).  Ideas for improvement of the system should be sent
to ``4bsd-ideas'', in the same network context.
Please let us know of problems you have with the distribution tape.
If you bring up new devices or other software which you would be willing
to share with others, let us know.
.sp 2
.ti +2.5i
Good Luck,
.sp
.ti +3i
Bill Joy
.br
.ti +3.5i
wnj@berkeley
.br
.ti +3.5i
ucbvax!wnj
.sp
.ti +3i
David Mosher
.br
.ti +3.5i
arpavax.mosher@berkeley
.br
.ti +3.5i
ucbvax!mosher
