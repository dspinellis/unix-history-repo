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
This package completes the 4.1bsd release, for which you received
an incomplete distribution before.
.LP
The 2400' 1600 bpi tape contains the
rest of the machine readable material for the distribution and some
fixes for the material you previously received.
.LP
The printed materials are:
.IP *
A two-sided copy of Volume 1 of the programmers manual.
.IP *
A one-sided copy of Volume 1 for use in reproduction,
.IP *
Documentation for the Ingres database system which is included
on the second tape.
.IP *
Documentation explaining the fixes which are on this tape that
update the materials you received earlier.
.LP
The tape contains 2 tape files.
.IP 1)
The first tape file contains all the material
not present on the first tape.
.IP 2)
The second tape file contains fixes to the
tape you received earlier.
.LP
.SH
Using the first tape file
.LP
The first tape file on the tape contains the following hierarchies:
.so cover1a.t
.SH
Using the second tape file
.LP
To install the fixes which have been made to the material you already
received you will replace existing binaries on your system.
You can see what files will be replaced by doing:
.DS
# mt fsf 1
# tar t
.DE
You can save any of these files if you wish and then do the
following to install the fixes:
.DS
# cd /usr/src/cmd
# rm \-rf lisp pascal pi px pxp pc0
# cd /
# mv sys sys.old
# rm \-rf /usr/lib/lisp /usr/src/lib/libpc
# mt fsf 1
# tar x
.DE
.LP
There are then a few changes you will have to make:
.IP *
Copy your system configuration file from /sys.old/conf/PICKLE to
/sys/conf/PICKLE and rebuild the system, as per the instructions
for tape1.
.IP *
Rebuild some binaries which, unfortunately, are not site-independent:
.DS
# cd /usr/src/cmd
# MAKE login.c getty.c
# cd /usr/src/cmd/berknet
# make; make install
.DE
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
