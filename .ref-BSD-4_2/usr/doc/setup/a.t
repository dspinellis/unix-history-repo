.de IR
\fI\\$1\fP\|\\$2
..
.ds LH "Installing/Operating 4.2BSD
.nr H1 6
.nr H2 0
.ds RH "Appendix A \- bootstrap details
.ds CF \*(DY
.bp
.LG
.B
.ce
APPENDIX A \- BOOTSTRAP DETAILS
.sp 2
.R
.NL
.PP
This appendix contains pertinent files and numbers regarding the
bootstrapping procedure for 4.2BSD.  You should never have to
look at this appendix.  However, if there are problems in installing
the distribution on your machine, the material contained here may
prove useful.
.SH
Contents of the distribution tapes
.PP
The distribution normally consists of two 1600bpi 2400' magnetic
tapes.  The first tape contains the following files on it.  All
tape files are blocked in 10 kilobytes records, except for the
first file on the first tape which has 512 byte records.
.DS L
.TS
l l l.
Tape file	Records*	Contents
_
one	194	8 bootstrap monitor programs and a
		\fItp\fP\|(1) file containing \fIboot\fP, \fIformat\fP, and \fIcopy\fP
two	205	``mini root'' file system
three	380	\fIdump\fP\|(8) of distribution root file system
four	440	\fItar\fP\|(1) image of /sys, including GENERIC system
five	2111	\fItar\fP\|(1) image of binaries and libraries in /usr
six	576	\fItar\fP\|(1) image of /usr/lib/vfont
.TE
.FS
* The number of records in each tape file
may not be precisely that shown in this table;
these values reflect the contents of the distribution tape
at the time this document was written.
.FE
.DE
The second tape contains the following files.
.DS L
.TS
l l l.
Tape file	# Records	Contents
_
one	2100	\fItar\fP\|(1) image of /usr/src
two	973	\fItar\fP\|(1) image of user contributed software
three	420	\fItar\fP\|(1) image of /usr/ingres
.TE
.DE
.PP
The distribution tape is made with the shell scripts located
in the directory /sys/dist.  To construct a distribution tape
one must first build a mini root file system with the \fIbuildmini\fP
shell script.
.DS
#! /bin/sh
#	@(#)buildmini	4.4	7/9/83
#
miniroot=hp0g
minitype=rm80
#
date
umount /dev/${miniroot}
newfs -s 4096 ${miniroot} ${minitype}
fsck /dev/r${miniroot}
mount /dev/${miniroot} /mnt
cd /mnt; sh /sys/dist/get
cd /sys/dist; sync
umount /dev/${miniroot}
fsck /dev/${miniroot}
date
.DE
The \fIbuildmini\fP
script uses the \fIget\fP script to construct the actual
file system.
.DS
#! /bin/sh
#	@(#)get	4.13	7/19/83
#
# Shell script to build a mini-root file system
# in preparation for building a distribution tape.
# The file system created here is image copied onto
# tape, then image copied onto disk as the "first"
# step in a cold boot of 4.2 systems.
#
DISTROOT=/nbsd
#
if [ `pwd` = '/' ]
then
	echo You just '(almost)' destroyed the root
	exit
fi
cp $DISTROOT/a/sys/GENERIC/vmunix .
rm -rf bin; mkdir bin
rm -rf etc; mkdir etc
rm -rf a; mkdir a
rm -rf tmp; mkdir tmp
rm -rf usr; mkdir usr usr/mdec
rm -rf sys; mkdir sys sys/floppy sys/cassette
cp $DISTROOT/etc/disktab etc
cp $DISTROOT/etc/newfs etc; strip etc/newfs
cp $DISTROOT/etc/mkfs etc; strip etc/mkfs
cp $DISTROOT/etc/restore etc; strip etc/restore
cp $DISTROOT/etc/init etc; strip etc/init
cp $DISTROOT/etc/mount etc; strip etc/mount
cp $DISTROOT/etc/mknod etc; strip etc/mknod
cp $DISTROOT/etc/fsck etc; strip etc/fsck
cp $DISTROOT/etc/umount etc; strip etc/umount
cp $DISTROOT/etc/arff etc; strip etc/arff
cp $DISTROOT/etc/flcopy etc; strip etc/flcopy
cp $DISTROOT/bin/mt bin; strip bin/mt
cp $DISTROOT/bin/ls bin; strip bin/ls
cp $DISTROOT/bin/sh bin; strip bin/sh
cp $DISTROOT/bin/mv bin; strip bin/mv
cp $DISTROOT/bin/sync bin; strip bin/sync
cp $DISTROOT/bin/cat bin; strip bin/cat
cp $DISTROOT/bin/mkdir bin; strip bin/mkdir
cp $DISTROOT/bin/stty bin; strip bin/stty; ln bin/stty bin/STTY
cp $DISTROOT/bin/echo bin; strip bin/echo
cp $DISTROOT/bin/rm bin; strip bin/rm
cp $DISTROOT/bin/cp bin; strip bin/cp
cp $DISTROOT/bin/expr bin; strip bin/expr
cp $DISTROOT/bin/awk bin; strip bin/awk
cp $DISTROOT/bin/make bin; strip bin/make
cp $DISTROOT/usr/mdec/* usr/mdec
cp $DISTROOT/a/sys/floppy/[Ma-z0-9]* sys/floppy
cp $DISTROOT/a/sys/cassette/[Ma-z0-9]* sys/cassette
cp $DISTROOT/a/sys/stand/boot boot
cp $DISTROOT/.profile .profile
cat >etc/passwd <<EOF
root::0:10::/:/bin/sh
EOF
cat >etc/group <<EOF
wheel:*:0:
staff:*:10:
EOF
cat >etc/fstab <<EOF
/dev/hp0a:/a:xx:1:1
/dev/up0a:/a:xx:1:1
/dev/hk0a:/a:xx:1:1
/dev/ra0a:/a:xx:1:1
/dev/rb0a:/a:xx:1:1
EOF
cat >xtr <<'EOF'
: ${disk?'Usage: disk=xx0 type=tt tape=yy xtr'}
: ${type?'Usage: disk=xx0 type=tt tape=yy xtr'}
: ${tape?'Usage: disk=xx0 type=tt tape=yy xtr'}
echo 'Build root file system'
newfs ${disk}a ${type}
sync
echo 'Check the file system'
fsck /dev/r${disk}a
mount /dev/${disk}a /a
cd /a
echo 'Rewind tape'
mt -t /dev/${tape}0 rew
echo 'Restore the dump image of the root'
restore rsf 3 /dev/${tape}0
cd /
sync
umount /dev/${disk}a
sync
fsck /dev/r${disk}a
echo 'Root filesystem extracted'
echo
echo 'If this is a 780, update floppy'
echo 'If this is a 730, update the cassette'
EOF
chmod +x xtr
rm -rf dev; mkdir dev
cp $DISTROOT/sys/dist/MAKEDEV dev
chmod +x dev/MAKEDEV
cp /dev/null dev/MAKEDEV.local
cd dev
./MAKEDEV std hp0 hk0 up0 ra0 rb0
./MAKEDEV ts0; mv rmt12 ts0; rm *mt*;
./MAKEDEV tm0; mv rmt12 tm0; rm *mt*;
./MAKEDEV ht0; mv rmt12 ht0; rm *mt*;
./MAKEDEV ut0; mv rmt12 ut0; rm *mt*;
./MAKEDEV mt0; mv rmt4 xt0; rm *mt*; mv xt0 mt0
cd ..
sync
.DE
The mini root file system must have enough space to hold the
files found on a floppy or cassette.
.PP
Once a mini root file system is constructed, the \fImaketape\fP
script is used to make a distribution tape.  
.DS
#! /bin/sh
#	@(#)maketape	4.12	8/4/83
#
miniroot=hp0g
#
trap "rm -f /tmp/tape.$$; exit" 0 1 2 3 13 15
mt rew
date
umount /dev/hp2g /dev/hp2h
umount /dev/hp2a
mount -r /dev/hp2a /nbsd
mount -r /dev/hp2g /nbsd/usr
mount -r /dev/hp2h /nbsd/a
cd /nbsd/tp
tp cmf /tmp/tape.$$ boot copy format
cd /nbsd/sys/mdec
echo "Build 1st level boot block file"
cat tsboot htboot tmboot mtboot utboot noboot noboot /tmp/tape.$$ | \e
	dd of=/dev/rmt12 bs=512 conv=sync
cd /nbsd
sync
echo "Add dump of mini-root file system"
dd if=/dev/r${miniroot} of=/dev/rmt12 bs=20b count=205 conv=sync
echo "Add full dump of real file system"
/etc/dump 0uf /dev/rmt12 /nbsd
echo "Add tar image of system sources"
cd /nbsd/a/sys; tar cf /dev/rmt12 .
echo "Add tar image of /usr"
cd /nbsd/usr; tar cf /dev/rmt12 adm bin dict doc games \e
	guest hosts include lib local man mdec msgs new \e
	old preserve pub spool tmp ucb
echo "Add varian fonts"
cd /usr/lib/vfont; tar cf /dev/rmt12 .
echo "Done, rewinding first tape"
mt rew
echo "Mount second tape and hit return when ready"; read x
echo "Add user source code"
cd /nbsd/usr/src; tar cf /dev/rmt12 .
echo "Add user contributed software"
cd /usr/src/new; tar cf /dev/rmt12 .
echo "Add ingres source"
cd /nbsd/usr/ingres; tar cf /dev/rmt12 .
echo "Done, rewinding second tape"
mt rew
.DE
.PP
Summarizing then, to construct a distribution tape you can
use the above scripts and the following commands.
.DS
\fB#\fP buildmini
\fB#\fP maketape
\&...
\fIDone, rewinding first tape\fP
\fIMount second tape and hit return when ready\fP
(remove the first tape and place a fresh one on the drive)
\&...
\fIDone, rewinding second tape\fP
.DE
.SH
Control status register addresses
.PP
The distribution uses many standalone device drivers
which presume the location of a UNIBUS device's control status
register (CSR).
The following table summarizes these values.
.DS
.TS
l l l.
Device name	Controller	CSR address (octal)
_
ra	DEC UDA50	0172150
rb	DEC 730 IDC	0175606
rk	DEC RK11	0177440
rl	DEC RL11	0174400
tm	EMULEX TC-11	0172520
ts	DEC TS11	0172520
up	EMULEX SC-21V	0176700
ut	SI 9700	0172440
.TE
.DE
All MASSBUS controllers are located at standard offsets
from the base address of the MASSBUS adapter register bank.
.SH
Generic system control status register addresses
.PP
The 
.I generic
version of the operating system supplied with the distribution
contains the UNIBUS devices indicated below. 
These devices will be recognized
if the appropriate control status registers respond at any of the
indicated UNIBUS addresses.
.DS
.TS
l l l.
Device name	Controller	CSR addresses (octal)
_
hk	DEC RK11	0177440
tm	EMULEX TC-11	0172520
ut	SI 9700	0172440
up	EMULEX SC-21V	0176700, 0174400, 0176300
ra	DEC UDA-50	0172150, 0172550, 0177550
rb	DEC 730 IDC	0175606
rl	DEC RL11	0174400
dn	DEC DN11	0160020
dm	DM11 equivalent	0170500
dh	DH11 equivalent	0160040
dz	DEC DZ11	0160100, 0160110, ... 0160170
ts	DEC TS11	0172520
dmf	DEC DMF32	0160340
lp	DEC LP11	0177514
.TE
.DE
If devices other than the above are located at any 
of the addresses indicated, the system may not bootstrap
properly.
