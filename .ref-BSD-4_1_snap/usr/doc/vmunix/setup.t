.bd S B 3
.TL
Installing and Operating 4.1bsd
.br
May 18, 1981
.br
Revised: February 8, 1982
.AU
William N. Joy
.AU
Samuel J. Leffler
.AI
Computer Systems Research Group
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California  94720
(415) 642-7780
.de IR
\fI\\$1\fP\\$2
..
.de UX
UNIX\\$1
..
.AB
.PP
.FS
* DEC, VAX, UNIBUS and MASSBUS are trademarks of
Digital Equipment Corporation.
.FE
.FS
** \s-2UNIX\s0 is a Trademark of Bell Laboratories.
.FE
This document contains instructions for installation and operation of the
4.1bsd release of the VAX*
.UX **
system, as distributed by U. C. Berkeley.
.PP
It discusses procedures for installing UNIX on a new VAX,
and for upgrading an existing VAX UNIX system to the new release.
It explains how to configure the kernel for available devices
and user load, how to lay out file systems on available disks,
how to set up terminal lines and user accounts,
and how to perform system-specific tailoring.
It also details system operations procedures\(em
shutdown and startup,
hardware error reporting and diagnosis, file system backup procedures,
resource control, performance monitoring, and procedures for recompiling
and reinstalling system software.
.AE
.ds LH "Installing/Operating 4.1bsd
.ds RH Introduction
.ds CF \*(DY
.LP
.nr H1 1
.bp
.LG
.B
.ce
1. INTRODUCTION
.sp 2
.R
.NL
.PP
This document explains how to install the 4.1bsd release of the Berkeley
version of UNIX for the VAX on your system.  If you are running the
November 1980 release of the system, which was called 4bsd and is now
called 4.0bsd, and have no 11/750's, you can avoid a full bootstrap from
the new tape by extracting only the software that has changed.
If you have an 11/750, or are running any other version of UNIX on your
VAX, you will have to do a full bootstrap.
.NH 2
Hardware supported
.PP
This distribution can be booted on a VAX 11/780 or VAX 11/750
cpu with any of the following disks:
.DS
.TS
lw(1.5i) l.
DEC MASSBUS:	RM03, RM05, RM80, RP06, RP07
SI MASSBUS:	CDC 300M
DEC UNIBUS:	RK07, RA80
EMULEX SC-21V UNIBUS*:	AMPEX 300M, CDC 300M, FUJITSU 160M
.TE
.DE
.FS
* Other UNIBUS controllers and drives may be easily usable with the system,
but will likely require minor modifications to the system
to allow bootstrapping.
The EMULEX and SI controllers, and the drives shown here are known
to work as bootstrap devices.
.FE
.PP
The tape drives supported by this distribution are:
.DS
.TS
lw(1.5i) l.
DEC MASSBUS:	TE16, TU45, TU77, TU78
DEC UNIBUS:	TS11
EMULEX TC-11 UNIBUS:	KENNEDY 9300
.	\"TU45 UNIBUS*:	System Industries 9700
.TE
.DE
.PP
The tapes and disks may be on any available UNIBUS or MASSBUS adapter
at any slot with the proviso that the tape device
must be slave number 0 on the formatter if it is a MASSBUS tape drive.
.NH 2
Distribution formats
.PP
There are two formats for the basic distribution:
two RK disk packs or a 1600bpi 2400' magnetic tape.
If you are set up to do it,
it is a good idea immediately to copy the
tapes or disks in the distribution kit to guard against disaster.
RK07 packs use the DEC-standard bad sector forwarding scheme.
The tapes are 9-track 1600 BPI and contain some 512-byte records
followed by many 10240-byte records.
There are interspersed tape marks; end-of-tape is signaled
by a double end-of-file.
.PP
The basic bootstrap material is present in two
short files at the beginning of the bootstrap tape.
The first file on the tape contains preliminary bootstrapping programs.
This is followed by a dump of the ``root'' file system (see
\fIdump\fP\|(8)**).
.FS
** References of the form X(Y) mean the subsection named
X in section Y of the 
.UX
programmer's manual.
.FE
Additional files on the tape contain tape archive images (see
\fItar\fP\|(1)):
the third file on the tape contains most of the files in the file system
/usr, except the source (/usr/src)
which is in the fourth file on the tape.
A fifth (and final) file on the tape is the small update file
for converting from 4.0bsd to 4.1bsd; contained in this file
is also a shell script
\fBget\fP which extracts additional material from the other files
on the tape.
.PP
On RK07 distributions, the bootstrap and root file system data
is on both packs, in the first part of each pack.  The material
from the third tape file is stored on the large file system partition
of the pack to be in RK drive 0; the source material from the fourth
tape file is stored in the large file system partition of RK drive 1.
.PP
Both RK packs contain bootable UNIX systems.
The first pack contains all the files
needed for normal operation of the system.
A dual drive RK system would normally be run with the first pack
and a local user file pack.  The system source pack would be mounted only
occasionally, as there simply isn't enough space to have both
distribution packs on-line all the time.
.PP
An additional 1200' tape supplied with the distribution contains
material that does not fit on the first tape or in two RK packs:
the INGRES data base system (/usr/ingres),
user-level drivers and bit-mapped
fonts for the raster device typesetter emulators (/usr/lib/vfont, etc.),
source for the documents describing the system (/usr/doc), 
source for the Berkeley network (/usr/net/...), etc.
A list of the contents of the second tape and instructions for setting
it up are supplied separately.
.NH 2
VAX hardware terminology
.PP
This section gives a short discussion of VAX hardware terminology
to help you get your bearings.
.PP
If you have MASSBUS disks and tapes it is necessary to know the
MASSBUS they are attached to, at least for the purposes of bootstrapping
and system description.  The MASSBUSes can have up to 8 devices attached
to them.  A disk counts as a device.  A tape \fIformatter\fP counts
as a device, and several tape drives may be attached to a formatter.
If you have a separate MASSBUS adapter for a disk and one for a tape
then it is conventional to put the disk as unit 0 on the MASSBUS with
the lowest ``TR'' number, and the tape formatter as unit 0 on the next
MASSBUS.  On a 11/780 this would correspond to having the disk on
``mba0'' at ``tr8'' and the tape on ``mba1'' at ``tr9''.  Here the
MASSBUS adapter with the lowest TR number has been called ``mba0''
and the one with the next lowest number is called ``mba1''.
.PP
To find out the MASSBUS your tape and disk are on you can examine
the cabling and the unit numbers or your site maintenance guide.
Do not be fooled into thinking that the number on the front of the
tape drive is a device number; it is a \fIslave\fP number,
one of several possible
tapes on the single tape formatter.
For bootstrapping the slave number \fBmust\fP be 0.  The formatter
unit number may be anything distinct from the other numbers on the
same MASSBUS, but you must know what it is.
.PP
The MASSBUS devices are known by several different names by DEC software
and by UNIX.  At various times it is necessary to know both
names.  There is, of course, the name of the device like ``RM03''
or ``RM80''; these are easy to remember because they are printed
on the front of the device.  DEC also gives devices names by the names
of the driver in the system using a naming convention that reflects
the interconnect topology of the machine.  The first letter of such
a name is a ``D'' for a disk, the second letter depends on the type
of the drive, ``DR'' for RM03,RM05 and RM80's, ``DB'' for RP06's.
The next letter is related to the interconnect; DEC calls the first
MASSBUS adapter ``A'', the second ``B'', etc.  Thus ``DRA'' is
a RM drive on the first MASSBUS adapter.  Finally, the name ends
in a digit corresponding to the unit number for the device on the
MASSBUS, i.e. ``DRA0'' is a disk at the first device slot on the
first MASSBUS adapter and is a RM disk.
.NH 2
UNIX device naming
.PP
UNIX has a set of names for devices, which are different
from the DEC names for the devices, viz.:
.DS
.TS
l l.
RM/RP disks	hp
TE/TU tapes	ht
TU78 tape	mt
.TE
.DE
.PP
The standalone system, used to bootstrap the full UNIX system,
uses device names:
.DS
xx(y,z)
.DE
Where \fIxx\fP is either \fBhp\fP, \fBht\fP, or \fBmt\fP.
The value \fIy\fP
specifies the MASSBUS to use and also the device.  It is computed as
.DS
8 * mba + device
.DE
Thus mba0 device 0 would have a \fIy\fP value of 0 while
mba1 device 0 would have a \fIy\fP value of 8.  The \fIz\fP
value is interpreted differently for tapes and disks:
for disks it is a disk \fIpartition\fP (in the range 0-7),
and for tapes it is a file number on the tape.
.PP
Each UNIX physical disk is divided into 8 logical disk partitions,
each of which may occupy any consecutive cylinder range on the
physical device.  The cylinders occupied
by the 8 partitions for each drive type
are specified in section 4 of the programmers manual.*
.FS
* It is possible to change the partitions
by changing the code for the table in the disk driver; since it
is often desirable to do this it is clear that these tables should
be read off each pack; they may be in a future version of the system.
.FE
Each partition may be used
for either a raw data area such as a paging area or to store a
UNIX file system.
It is conventional for the first partition on a disk to be used
to store a root file system, from which UNIX may be bootstrapped.
The second partition is traditionally used as a paging area, and the
rest of the disk is divided into spaces for additional ``mounted
file systems'' by use of one or more additional partitions.
.PP
The third logical partition of each physical disk also has a conventional
usage: it allows access to the entire physical device, including the bad
sector forwarding information recorded in the last three tracks.  It is
occasionally used to store a single large file system or to access
the entire pack when making a copy of it on another.  Care must be taken when
using this partition to not overwrite the last three tracks and thereby
clobber the bad sector information.
.PP
The disk partitions have names in the standalone system of the form
``hp(x,y)'' with varying \fIy\fP as described above.  Thus partition
1 of a RM05 on mba0 at drive 0 would be ``hp(0,1)''.  When not running
standalone, this partition would normally be available as ``/dev/hp0b''.
Here the prefix ``/dev'' is the name of the directory where all
``special files'' normally live, the ``hp'' serves an obvious purpose,
the ``0'' identifies this as a partition of hp drive number ``0''
and the ``b'' identifies this as the first partition (where we number
from 0, the 0'th partition being ``hp0a''.)
.PP
In all simple cases, a drive with unit number 0 (in its unit
plug on the front of the drive) will be called unit 0 in its UNIX
file name.  This is not, however, strictly necessary, since the system
has a level of indirection in this naming.  This can be taken
advantage of to make the system less dependent on the interconnect
topology, and to make reconfiguration after hardware
failure extremely easy.  We will not discuss that now.
.PP
Returning to the discussion of the standalone system, we recall
that tapes also took two integer parameters.  In the normal case
where the tape formatter is unit 0 on the second mba (mba1), the
files on the tape have names ``ht(8,0)'', ``ht(8,1)'', etc.
Here ``file'' means a tape file containing a single data stream.
The distribution tapes have data structures in the tape
files and though the tapes contain only 5 tape files, they contain
several thousand UNIX files.
.PP
For the UNIBUS, there are also conventional names.  The important
DEC name to know is DM?? for RK07 drives.  Thus RK07 drive 0 on a
controller on the first UNIBUS on the machine is ``DMA0''.  UNIX
calls such a device a ``hk'' and the standalone name for the
first partition of such a device is ``hk(0,0)''.  If the controller
were on the second UNIBUS its name would be ``hk(8,0)''.  If we wished
to access the first partition of a RK07 drive 1 on uba0 we would use
``hk(1,0)''.
.PP
The UNIBUS disk and tape names used by UNIX are:
.DS
.TS
l l.
RK disks	hk
TS tapes	ts
UDA disks	ra
SMD disks	up
TM tapes	tm
.	\"TU tapes	ut
.TE
.DE
.PP
.	\"Here SMD disks are disks on a rm emulating controller on the UNIBUS,
.	\"TM tapes are tapes on a controller that emulates the DEC TM-11, and
.	\"TU tapes are tapes on a controller that emulates the DEC TU45.
Here SMD disks are disks on an rm emulating controller on the
UNIBUS and TM tapes are tapes on a controller that emulates the
DEC TM-11.
The naming conventions for partitions in UNIBUS disks and files
in UNIBUS tapes are the same as those for MASSBUS disks and tapes.
.NH 2
UNIX devices: block and raw
.PP
UNIX makes a distinction between ``block'' and ``raw'' (character)
devices.  Each disk has a block device interface where
the system makes the device byte addressable and you can write
a single byte in the middle of the disk.  The system will read
out the data from the disk sector, insert the byte you gave it
and put the modified data back.  The disks with the names
``/dev/xx0a'', etc are block devices.  There are also raw devices available.
These have names like ``/dev/rxx0a'', the
``r'' here standing for ``raw''.  In the bootstrap procedures we
will often suggest using the raw devices, because these tend
to work faster in some cases.  In general, however, the block
devices are used.  They are where file systems are ``mounted''.
.PP
You should just be aware that it is sometimes important to use
the character device (for efficiency) or not (because it wouldn't
work, e.g. to write a single byte in the middle of a sector).
Don't change the instructions by using the wrong type of device
indiscriminantly.
.ds LH "Installing/Operating 4.1bsd
.ds RH Bootstrapping
.ds CF \*(DY
.bp
.nr H1 2
.nr H2 0
.bp
.LG
.B
.ce
2. BOOTSTRAP PROCEDURES
.sp 2
.R
.NL
.PP
This section explains the bootstrap procedures that can be used
to get the kernel supplied with this tape running on your machine.
If you are not yet running UNIX or are running a version of UNIX other
than 4.0bsd, then you will have to do a full bootstrap.
There are two full bootstrap procedures: procedure 1 if you received tapes
as your distribution media, or procedure 2 if you received RK07
packs.
.PP
If you are
running 4.0bsd then you can use the update bootstrap procedure 3 described
below instead of a full bootstrap.  This will affect modifications
to the local system less than a full bootstrap.
However, you may want to
do a full bootstrap even if you are running 4.0bsd, for two reasons:
.IP 1)
The binaries distributed with 4.0bsd will
.B not
work properly on a VAX 11/750, because of a difference in
the \fImovtuc\fP instruction
between the two machines.  It is unfortunate, indeed, that
the
.I movtuc
instruction is part of the
.I printf
code in 4.0bsd, that is included in virtually every program
on the system.
.IP 2)
The new distribution includes a new standard i/o library that is
more compatible than the previous with
the PDP-11 version of standard i/o, including facilities
for simultaneous reading and writing of files.  The addition of
these facilities unfortunately involved a change to an internal
data structure of the standard i/o library.  This structure change
is visible to compiled programs as it affects the macro expansions
compiled into programs that use \fIstdio\fP.
.PP
If you are running 4.0bsd and
decide that you do not need full binary compatibility with 11/750,
and do not need the new standard i/o library features to make stdio
compatible with the PDP-11, then you will be able to save time by using
bootstrap path 3.
Otherwise you should start with a full bootstrap from the new
distribution tape.
.PP
If you are already running UNIX and need to do a full bootstrap you should
first save your existing files on magnetic tape.  4.1bsd
uses generally different file system partition sizes than 4.0bsd, to
allow for standard bad-sector handling.  It is thus necessary to adjust
the sizes of the file systems downward slightly.  The easiest way
to save the current files on tape is by doing a full dump and then restoring
in the new system.  This works also in converting 32/V file systems;
although the dump format is different on 32/V, the program ``512restor''
can restore old format dump tapes into the file system format used by 4.1bsd.
.NH 2
Bootstrap path 1: Booting from tape
.PP
The tape bootstrap procedure involves four steps: formatting a disk
pack, loading the tape bootstrap monitor, creating and initializing
a UNIX ``root'' file system
system on the disk, and booting the system.
.NH 3
Formatting the disk
.PP
First prepare the bootstrap disk by making it drive 0, and formatting the pack
in the drive.  Use the standard DEC formatter for DEC drives; see
.IR format (8)
in the programmer's manual for details on formatting.
.PP
This version of UNIX incorporates code to use the bad sector formatting
information standardly recorded on the packs by the DEC supplied formatters.
The code to handle bad sectors has been brought up on all DEC drives
except the RP06.  It is not yet installed in the driver for second vendor
UNIBUS storage module drives.  Support for bad sector handling on these devices
is planned.  For the time being, however, you should have error free packs
to use these devices.  You can use the procedures in
.IR badsect (8)
to handle a few bad sectors on such packs.
.PP
If you have run an older version of UNIX on the packs you are planning
to use for bootstrapping it is likely that the bad sector information
on the packs has been destroyed, since it was accessible as normal
data in the last three tracks of the disk.  You should therefore run the
formatter again to make sure the information is valid.
.PP
There are some critical sections of all packs that must not
have errors on them: to boot from the disk you will need good sectors
in sector 0 of
the pack, the disk sectors in the files ``/vmunix'' (the
system image), ``/boot'' (the program that loads the system image),
and the file system indices that lead to these last two files.
If the first 15884 sectors of your disk are clean you are safe; if not
you can take your chances.
.NH 3
Loading the tape bootstrap monitor
.PP
To load the tape bootstrap monitor, first
mount the magnetic tape on drive 0 at load point, making
sure that the write ring is not inserted.
Temporarily
set the reboot switch on an 11/780 off; set an 11/750 to power-on action halt.
(In normal operation an 11/780 will have the reboot switch on, and an 11/750
will have the power-on action set to boot/restart.)
.PP
If you have an 11/780 give the commands:
.RT
.DS
\fB>>>\|\fPHALT
\fB>>>\|\fPUNJAM
.DE
Then, on either machine, give the init command:
.DS
\fB>>>\|\fPI
.DE
and then
.	\"key in at location 200 and execute either the TS, HT, TM, MT, or UT
key in at location 200 and execute either the TS, HT, TM, or MT
bootstrap that follows, as appropriate.
The machine's printouts are shown in boldface,
explanatory comments are within ( ).
(You can use `delete' to delete a character and `control U' to kill the
whole line.)
.br
.ne 5
.sp
.ID
.nf
TS bootstrap

\fB>>>\|\fPD/P 200 3AEFD0
\fB>>>\|\fPD + D05A0000
\fB>>>\|\fPD + 3BEF
\fB>>>\|\fPD + 800CA00
\fB>>>\|\fPD + 32EFC1
\fB>>>\|\fPD + CA010000
\fB>>>\|\fPD + EFC10804
\fB>>>\|\fPD + 24
\fB>>>\|\fPD + 15508F
\fB>>>\|\fPD + ABB45B00
\fB>>>\|\fPD + 2AB9502
\fB>>>\|\fPD + 8FB0FB18
\fB>>>\|\fPD + 956B024C
\fB>>>\|\fPD + FB1802AB
\fB>>>\|\fPD + 25C8FB0
\fB>>>\|\fPD + 6B
        (The next two deposits set up the addresses of the UNIBUS)
        (adapter and its memory; the 20006000 here is the address of)
        (the 11/780 uba0 and the 2013E000 the address of the 11/780 umem0)
\fB>>>\|\fPD + 20006000		(780 uba0) (780 uba1: 20008000, 750 uba: F30000)
\fB>>>\|\fPD + 2013E000		(780 umem0) (780 umem1: 2017E000, 750 umem: FFE000)
\fB>>>\|\fPD + 80000000
\fB>>>\|\fPD + 254C004
\fB>>>\|\fPD + 80000
\fB>>>\|\fPD + 264
\fB>>>\|\fPD + E
\fB>>>\|\fPD + C001
\fB>>>\|\fPD + 2000000
\fB>>>\|\fPS 200

HT bootstrap

\fB>>>\|\fPD/P 200 3EEFD0
\fB>>>\|\fPD + C55A0000
\fB>>>\|\fPD + 3BEF
\fB>>>\|\fPD + 808F00
\fB>>>\|\fPD + C15B0000
\fB>>>\|\fPD + C05B5A5B
\fB>>>\|\fPD + 4008F
\fB>>>\|\fPD + D05B00
\fB>>>\|\fPD + 9D004AA
\fB>>>\|\fPD + C08F326B
\fB>>>\|\fPD + D424AB14
\fB>>>\|\fPD + 8FD00CAA
\fB>>>\|\fPD + 80000000
\fB>>>\|\fPD + 320800CA
\fB>>>\|\fPD + AAFE008F
\fB>>>\|\fPD + 6B39D010
\fB>>>\|\fPD + 0
        (The next two deposits set up the addresses of the MASSBUS)
        (adapter and the drive number for the tape formatter)
        (the 20012000 here is the address of the 11/780 mba1 and the 0)
        (reflects that the formatter is drive 0 on mba1)
\fB>>>\|\fPD + 20012000		(780 mba1) (780 mba0: 20010000, 750 mba0: F28000)
\fB>>>\|\fPD + 0				(Formatter unit number in range 0-7)
\fB>>>\|\fPS 200
\fB>>>\|\fPS 200

TM bootstrap

\fB>>>\|\fPD/P 200 2AEFD0
\fB>>>\|\fPD + D0510000
\fB>>>\|\fPD + 2000008F
\fB>>>\|\fPD + 800C180
\fB>>>\|\fPD + 804C1D4
\fB>>>\|\fPD + 1AEFD0
\fB>>>\|\fPD + C8520000
\fB>>>\|\fPD + F5508F
\fB>>>\|\fPD + 8FAE5200
\fB>>>\|\fPD + 4A20200
\fB>>>\|\fPD + B006A2B4
\fB>>>\|\fPD + 2A203
        (The following two numbers are uba0 and umem0; see TS above)
        (for some hints on other values if your TM isn't on UBA0 on a 780)
\fB>>>\|\fPD + 20006000		(780 uba0)
\fB>>>\|\fPD + 2013E000		(780 umem0)
\fB>>>\|\fPS 200
\fB>>>\|\fPS 200
\fB>>>\|\fPS 200

MT bootstrap

\fB>>>\|\fPD/P 200 46EFD0
\fB>>>\|\fPD + C55A0000
\fB>>>\|\fPD + 43EF
\fB>>>\|\fPD + 808F00
\fB>>>\|\fPD + C15B0000
\fB>>>\|\fPD + C05B5A5B
\fB>>>\|\fPD + 4008F
\fB>>>\|\fPD + 19A5B00
\fB>>>\|\fPD + 49A04AA
\fB>>>\|\fPD + AAD408AB
\fB>>>\|\fPD + 8FD00C
\fB>>>\|\fPD + CA800000
\fB>>>\|\fPD + 8F320800
\fB>>>\|\fPD + 10AAFE00
\fB>>>\|\fPD + 2008F3C
\fB>>>\|\fPD + ABD014AB
\fB>>>\|\fPD + FE15044
\fB>>>\|\fPD + 399AF850
\fB>>>\|\fPD + 6B
        (The next two deposits set up the addresses of the MASSBUS)
        (adapter and the drive number for the tape formatter)
        (the 20012000 here is the address of the 11/780 mba1 and the 0)
        (reflects that the formatter is drive 0 on mba1)
\fB>>>\|\fPD + 20012000
\fB>>>\|\fPD + 0
\fB>>>\|\fPS 200
\fB>>>\|\fPS 200
\fB>>>\|\fPS 200
\fB>>>\|\fPS 200
.	\"
.	\"UT bootstrap
.	\"
.	\"\fB>>>\|\fPD/P 200 3AEFD0
.	\"\fB>>>\|\fPD + D0510000
.	\"\fB>>>\|\fPD + 2000008F
.	\"\fB>>>\|\fPD + 800C180 
.	\"\fB>>>\|\fPD + 804C1D4
.	\"\fB>>>\|\fPD + 2AEFD0
.	\"\fB>>>\|\fPD + C8520000
.	\"\fB>>>\|\fPD + F5208F
.	\"\fB>>>\|\fPD + EFA85200
.	\"\fB>>>\|\fPD + 498
.	\"\fB>>>\|\fPD + 8FAE1AA2
.	\"\fB>>>\|\fPD + 2A20100
.	\"\fB>>>\|\fPD + 2008FAE
.	\"\fB>>>\|\fPD + A2B406A2
.	\"\fB>>>\|\fPD + 6239B004
.	\"\fB>>>\|\fPD + 0
.	\"        (The following two numbers are uba0 and umem0; see TS above)
.	\"        (for some hints on other values if your UT isn't on UBA0 on a 780)
.	\"\fB>>>\|\fPD + 20006000
.	\"\fB>>>\|\fPD + 2013E000 
.	\"\fB>>>\|\fPS 200
.	\"\fB>>>\|\fPS 200
.	\"\fB>>>\|\fPS 200
.	\"\fB>>>\|\fPS 200
.	\"\fB>>>\|\fPS 200
.fi
.DE
.PP
If the tape doesn't move the first time you start the bootstrap
program with ``S 200'' you probably have entered the program
incorrectly (but also check that the tape is online).
Start over and check your typing.
.	\"For the HT, UT, MT, and TM bootstraps you will not be able to see the
For the HT, MT, and TM bootstraps you will not be able to see the
tape motion as you advance through the first few blocks
as the tape motion is all within the vacuum columns.
.PP
Next, deposit in RA the address of the tape MBA/UBA and in RB the
address of the device registers or unit number from one of:
.DS
.TS
lw(1.5i) l.
\fB>>>\|\fPD/G A 20006000	(for tapes on 780 uba0)
\fB>>>\|\fPD/G A 20008000	(for tapes on 780 uba1)
\fB>>>\|\fPD/G A 20012000	(for tapes on 780 mba1)
\fB>>>\|\fPD/G A 20010000	(for tapes on 780 mba0)
\fB>>>\|\fPD/G A F30000	(for tapes on 750 uba0)
\fB>>>\|\fPD/G A F2A000	(for tapes on 750 mba1)
\fB>>>\|\fPD/G A F28000	(for tapes on 750 mba0)
.TE
.DE
and for register B:
.DS
.TS
lw(1.5i) l.
\fB>>>\|\fPD/G B 0	(for tm03/tm78 formatters at mba? drive 0)
\fB>>>\|\fPD/G B 1	(for tm03/tm78 formatters at mba? drive 1)
\fB>>>\|\fPD/G B 2013F550	(for tm11/ts11 tapes on 780 uba0)
\fB>>>\|\fPD/G B FFF550	(for tm11/ts11 tapes on 750 uba0)
.	\"\fB>>>\|\fPD/G B FFF520	(for tu45 tapes on 750 uba0)
.TE
.DE
Then start the bootstrap program with
.DS
\fB>>>\|\fPS 0
.DE
.PP
The console should type
.DS
.I
\fB=\fP
.R
.DE
You are now talking to the tape bootstrap monitor.
At any point in the following procedure you can return
to this section, reload the tape bootstrap, and restart the
procedure.
.NH 3
Creating and initializing a UNIX root filesystem
.PP
Create the root file system using the following procedures,
substituting for the \fIxx\fP in the file system names:
.DS
hk	for RK07
hp	for RM03/RM05/RM80/RP06/RP07
ra*	for RA80
up	for UNIBUS storage modules (sc-21)
.DE
and the tape drive code for \fIyy\fP from:
.DS
ht	for TE16/TU45/TU77
ts	for TS11
mt	for TU78
tm	for UNIBUS tm-11 emulations (e.g. EMULEX tc-11)
.	\"ut	for UNIBUS tu45 emulations (e.g. SI 9700)
.DE
.FS
* \fBNote\fP: since DEC does not (and will
not) support booting via block zero of a UDA50 controlled disk,
machines with the root file system on an RA80 will probably
have to use the console storage device to load the bootstrap.
.FE
and also figure out the adapter/unit codes \fIx\fP and \fIy\fP
for the disk and the tape.  As described earlier, these should
be 8 times the MASSBUS or UNIBUS adapter number plus the drive number
on the MASSBUS or the unit number on the UNIBUS.
Thus \fIx\fP would be 0 for a disk on MBA0 drive 0,
\fIy\fP would be 8 for a tape on MBA1 drive 0, etc.
.PP
First we run a standalone version of the \fImkfs\fP (8) program:
(Note that mistakes cannot be erased when talking to the tape
monitor.)
.DS
.TS
lw(1.5i) l.
\fB=\|\fPmkfs
\fBfile sys size:\fP 7942	(count of 1024 byte blocks in root)
\fBfile system:\fP \fIxx\fP(x,0)	(root is on drive zero; first filsys there)
\fBisize = 5072\fP	(count of inodes in root filesystem)
\fBm/n = 3 500\fP	(interleave parameters)
\fB=\fP	(back at tape boot level)
.TE
.DE
You now have an empty UNIX root filesystem.
To restore the data that you need to boot the system, type
.DS
.TS
lw(1.5i) l.
(bring in a standalone \fIrestor\fP\|(8) program)
\fB=\|\fPrestor
\fBTape?\fP \fIyy\fP(y,1)	(unit 0, second tape file)
\fBDisk?\fP \fIxx\fP(x,0)	(into root file system)
\fBLast chance before scribbling on disk.\fP	(just hit return)
(30 second pause then tape should move)
(tape moves for a few minutes)
\fBEnd of tape\fP
\fB=\fP	(back at tape boot level)
.TE
.DE
Now, you are ready to boot up from disk:
.DS
.TS
lw(1.5i) l.
(load bootstrap program)
\fB=\fP\|boot
\fBBoot\fP
\fB: \fP\fIxx\fP(x,0)vmunix	(bring in \fIvmunix\fP off root system)
.TE
.DE
(Note that `#' erases characters and `@' erases lines in the
bootstrap program.)
.LP
The standalone boot program should then read the system from
the root file system you just created, and the system should boot:
.DS
.B
104460+23536+32232 start 0x500
Berkeley VAX/UNIX Version 4.1   Fri April 24 13:06:40 PST 1981
real mem  = \fIxxx\fP
avail mem = \fIyyy\fP
\fI\&... information about available devices ...\fP
root device? 
.R
.DE
.PP
The first three numbers are printed out by the bootstrap
programs and are the sizes of different
parts of the system (text, initialized and uninitialized data).  The
system also allocates several system data structures after it starts
running.  The sizes of these structures are based on the amount of available
memory and the maximum count of active users expected, as declared in a system
configuration description.  This will be discussed later.
.PP
UNIX itself then runs for the first time and begins by printing out a banner
identifying the version of the system that is in use and the date it was
compiled.  Note that this version is different from the system release number,
and applies only to the operating system kernel.
.PP
Next the
.I mem
messages give the
amount of real (physical) memory and the
memory available to user programs
in bytes.
For example, if your machine has only 512K bytes of memory, then
xxx will be 523264, 1024 bytes less than 512K.
The system reserves the last 1024 bytes of memory for use in
error logging and doesn't count it as part of real memory.
.PP
The messages that came out next show what devices were found on
the current processor.  These messages are described in
.IR autoconf (4).
The distributed system may not have
found all the communications devices you have (dh's and dz's),
or all the mass storage peripherals you have if you have more than
two of anything.  This will be corrected soon, when you create
a description of your machine to configure UNIX from.
The messages printed at boot here contain much of the information
that will be used in creating the configuration.
In a correctly configured system most of the information
present in the configuration description
is printed out at boot time as the system verifies that each device
is present.
.PP
The ``root device?'' prompt was printed by the system 
and is now asking you for the name of the root file system to use.
This happens because the distribution system is a \fIgeneric\fP
system.  It can be bootstrapped on any VAX cpu and with its root device
and paging area on any available disk drive.  You should respond
to the root device question with \fIxx\fP0, as this is the device
containing the root file system.
You will later build a system tailored to your configuration that
will not ask this question when it is bootstrapped.
.DS
\fBroot device?\fP \fIxx\fP0
WARNING: preposterous time in file system \-\- CHECK AND RESET THE DATE!
\fBerase ^?, kill ^U, intr ^C\fP
\fB#\fP
.DE
.PP
The ``erase ...'' message is part of /.profile
that was executed by the root shell when it started.  The file
/.profile contained commands to
set the UNIX erase, line erase and interrupt characters
to be what is standard on DEC systems so that it is
consistent with the DEC console interface characters.
This is not normal for UNIX, but is convenient when working
on the console.
.PP
UNIX is now running,
and the `UNIX Programmer's manual' applies.
The `#' is the prompt from the Shell,
and lets you know that you are the super-user, whose login name is ``root''.
.NH 3
Setting up the /usr file system
.PP
First set a shell variable to the name of your disk, so
the commands we give will work regardless of the disk you
have; do
.DS
.TS
l l.
\fB#\fP disk=hp	(if you have an RP06, RM03, RM05 or RM80)
\fB#\fP disk=hk	(if you have RK07s)
\fB#\fP disk=up	(if you have UNIBUS storage module drives)
\fB#\fP disk=ra	(if you have RA80s)
.TE
.DE
Then check the integrity of the root file system by giving the command
.DS
\fB#\fP fsck /dev/r${disk}0a
.DE
The output from
.I fsck
should look something like:
.DS
.B
/dev/r\fIxx\fP0a
File System:  Volume: 

** Checking /dev/r\fIxx\fP0a
** Phase 1 - Check Blocks and Sizes
** Phase 2 - Check Pathnames
** Phase 3 - Check Connectivity
** Phase 4 - Check Reference Counts
** Phase 5 - Check Free List 
675 files 3806 blocks 3817 free
.R
.R
.DE
.PP
If there are inconsistencies in the file system, you may be prompted
to apply corrective action; see the document describing
.I fsck
for information.
.PP
The next thing to do is to extract the rest of the data from
the tape.
Comments are enclosed in ( ); don't type these.
The number in the first command is the
size of the filesystem to be created, in 1024 character blocks,
just as given to the standalone version of
.I mkfs
above.
Find the disk you have in the following table and execute
the commands in the right hand portion of the table:
.DS
.TS
l l.
DEC RM03	\fB#\fP size=40992; name=hp0g; n=80
DEC RM05	\fB#\fP size=145640; name=hp0h; n=304
DEC RM80	\fB#\fP size=40992; name=hp0g; n=217
DEC RP06	\fB#\fP size=145640; name=hp0g; n=209
DEC RP07	\fB#\fP size=224000; name=hp0h; n=800
SI/CDC 9766	\fB#\fP size=145640; name=hp0h; n=304
DEC RK07	\fB#\fP size=13893; name=hk0g; n=33
DEC RA80	\fB#\fP size=40992; name=ra0g; n=217
EMULEX AMPEX 300M	\fB#\fP size=145640; name=up0h; n=304
EMULEX FUJITSU 160M	\fB#\fP size=106832; name=up0h; n=160
.TE
.DE
See
\fImkfs\fP\|(8)
for an explanation of the values of \fIn\fP.
Note that the \fIsize\fPs here are the count of sectors
in the disk partitions (from the numbers in section 4 of the
manual) divided by 2, because file system blocks occupy two
disk sectors (disk blocks) each.*
.FS
* The numbers for some of the drives have been rounded down very
slightly to make the partitions between the various drive types
be the same size; this is so that the partitions can be copied
easily in a mixed-drive type installations.
The numbers for the EMULEX drives also allow for bad-block forwarding
information that is not yet integrated into the drivers.
.FE
.PP
Find the tape you have in the following table and execute the
commands in the right hand portion of the table:
.DS
.TS
l l.
DEC TE16/TU45/TU77	\fB#\fP cd /dev; MAKE ht0; sync
DEC TU78	\fB#\fP cd /dev; MAKE mt0; sync
DEC TS11	\fB#\fP cd /dev; MAKE ts0; sync
EMULEX TC11	\fB#\fP cd /dev; MAKE tm0; sync
.	\"SI 9700	\fB#\fP cd /dev; MAKE ut0; sync
.TE
.DE
Then execute the following commands
.br
.ne 5
.sp
.ID
.DT
.nf
\fB#\fP date \fIyymmddhhmm\fP			(set date, see \fIdate\fP\|(1))

\fB#\fP passwd root					(set password for super-user)
\fBNew password:\fP				(password will not echo)
\fBRetype new password:\fP
\fB#\fP /etc/mkfs /dev/r${name} ${size} 3 $n	(create empty user filesystem)
\fBisize = \fP\fInnnnn\fP				(the count of available inodes)
\fBm/n = 3\fP $n					(free list interleave parameters)
(this takes a few minutes)
\fB#\fP /etc/mount /dev/${name} /usr	(mount the usr filesystem)
\fB#\fP cd /usr					(make /usr the current directory)
\fB#\fP mt fsf 2					(skip first two tape files)
\fB#\fP tar xpbf 20 /dev/rmt12 		(extract the usr filesystem)
(this takes about 10 minutes)
\fB#\fP rmdir lost+found
\fB#\fP /etc/mklost+found			(a directory for \fIfsck\fP)
\fB#\fP dd if=/usr/mdec/${disk}boot of=/dev/r${disk}0a bs=1b count=1
(write boot block so block 0 disk boots will work)
\fB#\fP cd /						(back to root)
\fB#\fP chmod 755  /  /usr
\fB#\fP /etc/umount /dev/${name}	(unmount /usr)
.fi
.DE
The data on the third tape file has now been extracted.
The tape is now positioned in front of the fourth tape file.
.PP
You can now check the consistency of the /usr file system by doing
.DS
\fB#\fP fsck /dev/r${name}
.DE
To use the /usr file system, you should now remount it by
saying
.DS
\fB#\fP /etc/mount /dev/${name} /usr
.DE
You can now extract the fourth tape file (the source for the commands).
If you have RK07's you must first put a formatted pack in drive 1 and
set up a UNIX file system on it
by doing:
.DS
\fB#\fP /etc/mkfs /dev/rhk1g ${size} 3 $n
\fBisize = \fP\fInnnnn\fP
\fBm/n = 3\fP $n
(this takes a few minutes)
\fB#\fP /etc/mount /dev/hk1g /usr/src
\fB#\fP cd /usr/src
\fB#\fP /etc/mklost+found
.DE
In any case you can then extract the source code for the commands
(except on RK07's this will fit in the /usr file system):
.DS
\fB#\fP cd /usr/src
\fB#\fP tar xpb 20
.DE
If you get an error at this point, you can reposition the tape with the
following command and try the above commands again.
.DS
\fB#\fP mt fsf 3
.DE
.NH 2
Bootstrap path 2: Booting RK07 distribution packs
.PP
Temporarily
set the reboot switch on an 11/780 off; set an 11/750 to power-on action halt.
(In normal operation an 11/780 will have the reboot switch on, and an 11/750
will have the power-on action set to boot/restart.)
.PP
If you have an
11/750 you should mount the packs in units 0 and 1 as shown on the
labels on the packs and type
.DS
>>>B/2 DMA0
.DE
This will bootstrap the system to single user mode from ``hk(0,0)vmunix'',
the vmunix on unit 0 in the root file system root directory.
.PP
If you have an 11/780, you should
put in the standard VMS console floppy and type
.DS
>>>B DM0
.DE
This will attempt to bootstrap from hk0 as though it were a VMS pack,
and then fail, complaining about missing boot information or some
such nonsense.  You can then do
.DS
>>>D/G 5 8
>>>C
.DE
and UNIX should come up single user on the console, with messages
as explained in the previous section on tape bootstrapping.
Begin by setting the date and the password for the super-user:
.DS
.DT
\fB#\fP date \fIyymmddhhmm\fP		(set date, see \fIdate\fP\|(1))
\fB#\fP passwd root				(set password for super-user)
\fBNew password:\fP			(password will not echo)
\fBRetype new password:\fP
.DE
.PP
The RK07 disks you received have the \fB/usr\fP file system in
partition \fBhk0g\fP and the \fB/usr/src\fP source code for
the user-level software in partition \fBhk1g\fP.  You can check
the consistency of these file systems by giving the command:
.DS
\fB#\fP fsck /dev/rhk0g /dev/rhk1g
.DE
The output from
.I fsck
should look something like:
.br
.ne 5
.sp
.ID
.nf
/dev/rhk0g
File System: /usr

** Checking /dev/rhk0g
** Phase 1 - Check Blocks and Sizes
** Phase 2 - Check Pathnames
** Phase 3 - Check Connectivity
** Phase 4 - Check Reference Counts
** Phase 5 - Check Free List 
1369 files 9416 blocks 3920 free

/dev/rhk1g
File System: /usr/src

** Checking /dev/rhk1g
** Phase 1 - Check Blocks and Sizes
** Phase 2 - Check Pathnames
** Phase 3 - Check Connectivity
** Phase 4 - Check Reference Counts
** Phase 5 - Check Free List 
2327 files 10693 blocks 2643 free
#
.R
.fi
.DE
.PP
If there are inconsistencies in the file systems, you may be prompted
to apply corrective action; see the document describing
.I fsck
for information.
.NH 2
Bootstrap path 3: upgrading 4.0bsd
.PP
Begin by reading the other parts of this document to see what
has changed since the last time you bootstrapped the system.
Also read the ``Changes in 4.1bsd'' document, and look
at the new manual sections provided to you.
If you have local system modifications to the kernel to install, look at the
document ``Kernel changes in 4.1bsd'' to get an idea of how
the system changes will affect your local mods.
.PP
There are 3 major areas of changes that you will need to incorporate
to convert to the new system:
.IP 1.
The new kernel and the associated programs that read the system symbol
table: ps, w, uptime, pstat, renice, etc.
.IP 2.
The programs related to systems reboots and shutdowns.
.IP 3.
Other programs with significant bug fixes.
.PP
Here is a step-by-step guide to converting.
Before you begin you should do a full backup of your root and /usr file
systems as a precaution against irreversable mistakes.
.IP 1.
Set the shell variable ``nbsd'' to the name of a directory where
a quantity of material from the tape (you should allow for about 10
megabytes) can be extracted.  Change directory to ``$nbsd'', and
extract the fifth file from the distribution tape.*
.FS
* We dis-recommend reading the tape on top of the
current source code; strange and mysterious things can happen
if you do this (since some old files not in re-supplied subdirectories
will not be removed!).
Also if you have locally modified any re-distributed commands, the
local mods will vanish without a trace!
.FE
(To position the tape at the fifth file do ``cp /dev/rmt12 /dev/null''
four times.  Then do ``tar xbf 20 /dev/rmt8''.)
Then run the extracted ``get'' shell script via ``csh get''
to get more material off the other files from the tape.
.IP 2.
Copy the file $nbsd/usr/include/signal.h to /usr/include/signal.h.
Install the C compiler and optimizer from the new system.
by copying $nbsd/lib/ccom to /lib/ccom and $nbsd/lib/c2 to /lib/c2.
They have bugs fixed that may otherwise prevent the rest of
these instructions from working.
Reconfigure the system in $nbsd/sys to correspond to your configuration
according to the instructions in section 3.2.**
.FS
** One nasty point: the new system will normally use DEC-standard bad sector
information recorded on the packs to handle bad sectors.  This information
and forwarding sectors are located on the last three tracks of each pack,
and thus these packs are \fBnot\fP in any normal file system partition
in the new system.  If you have all perfect packs, you can define the
option ``NOBADSECT'' in the system configuration; this will cause the
\fIhp\fP\|(4)
driver to be recompiled with the disk partitions overlaying these three
tracks as in the previous distribution, and disable the bad-sector
recovery code in all the drivers.
.FE
Install the new version of arff by copying $nbsd/etc/arff to /etc/arff.
Build a new console floppy according to the instructions in section 3.1.
Copy the newly constructed system to /newvmunix and attempt to bootstrap it.
It should boot passably on your old root filesystem, but don't
bring it up multi-user as a several commands (ps, sh, ...) will not fully
work yet.
.IP 3.
Put the header files for the new system in /usr/include/sys by
copying them from $nbsd/sys/h.  Put in the new versions of the following
programs:
.RS
.IP
/bin: adb, login, ps, sh
.IP
/etc:
bad144, badsect, config, dmesg, dump, init, fsck, halt, pstat, reboot, renice,
savecore, shutdown
.IP
/usr/bin:
iostat, sdb
.IP
/usr/ucb:
vmstat, w
.RE
.IP
Take the file $nbsd/etc/rc and merge any local changes to /etc/rc into
it.  Put the resulting file in /etc/rc.  Create a directory /usr/crash,
and perhaps some files in this directory (read
\fIcrash\fP\|(8)).
Remove the file /usr/include/execargs.h.
.IP 4.
Try bootstrapping the new system; it should now work.
Make sure to write new instructions to your operators.
.IP 5.
Incorporate some other important bug fixes:
.RS
.IP a)
Replace the files main.c, pow_zi.c and z_sqrt.c in
/usr/src/lib/libF77 and rebuild and reinstall this library.
.IP b)
Replace the files fmtlib.c and lread.c in
/usr/src/lib/libI77 and rebuild and reinstall this library.
.IP c)
Replace the directory /usr/src/lib/libcurses with the new
library source and recompile and reinstall it, making sure to
put the new <curses.h> and <unctrl.h> files
in /usr/include and the new lint library
for curses in /usr/lib/lint.
.IP d)
Replace the files gen/atoi.c, gen/modf.s, and stdio/doprnt.s
in /usr/src/libc, and rebuild and reinstall /lib/libc.a.
This will make future loaded binaries compatible with 750's without
changing the standard i/o interface or affecting the suitability of
old binaries or libraries.
.IP e)
Replace the file
/usr/src/lib/libdbm/dbm.c and recompile and reinstall the library.
.IP f)
Replace the file
/usr/src/lib/libjobs/sigset.c and recompile and reinstall the library.
.IP g)
Replace the file
/usr/src/lib/libnm/acos.c and recompile and reinstall the library.
.IP h)
Install the new version of
.I tar
from
$nbsd/usr/src/cmd/tar.c
and also the program
.I mt
from
$nbsd/usr/src/cmd/mt.c.
.IP i)
If you want the new version of the Pascal system incorporating several
minor bug fixes, remake the directories
.I pascal,
.I pc0,
.I pi,
.I px,
and
.I pxp
in $nbsd/usr/src/cmd, and rebuild the library
$nbsd/src/lib/libpc, and reinstall them all.
This will also install the new program
.I pmerge.
.IP j)
Install the new version of the Fortran compiler, fixing several
minor bugs relating to the compilation of block data routines
and the interface to the symbolic debugger; the important file that
has changed here is /usr/lib/f77pass1.  Changed source files in
/usr/src/cmd/f77 are init.c, main.c, proc.c, and vax.c.
.IP k)
Install new versions of as many of the following programs with minor
bug fixes as you choose to:
bc, calendar, checknr, csh, ctags, cu, delivermail, diff, error, expand,
ld, lisp, lpr, ls, mail, make, man, mv, oldcsh, od, oldcsh, prof, ranlib,
reset, rm, rmdir, uusend, wall, who, write,
.IP l)
Install the new games: chase, doctor, hangman, rain, rogue, worm, worms
.IP m)
Install the modified or new administrative programs:
dumpdir, getty, icheck, last, lastcomm, mkfs, restor, sa
.IP n)
Install manual pages corresponding to the new and changed programs.
.IP o)
The include file /usr/include/vadvise.h needs to be re-installed.
.IP p)
Note: the names of ttys have
changed.  What was tty0 in 4BSD is now tty00, and the documentation
doesn't warn of that.
.IP q)
The second tape file of the second distribution tape contains a number of fixes
which were made to the 4.1bsd system after the installation instructions
were written.  Both sources and binaries for the changes are given, in a manner
similar to that of the update material extracted from the first tape.
.IP
You can extract the additional material by mounting the
second tape and doing:
.DS
# mt fsf 1
# tar x
.DE
This is best done in the directory ``$nbsd'' (as on page 15).
Binaries and sources are given for all the programs which are corrected.
Some of the material extracted here
duplicates that extracted by the procedure on page 15.
.IP
You can install this material in a manner similar to the material installed
above.
The \fBpatches:1-41\fR document describes the bugs that were fixed, albeit
in a rough fashion.
.RE
.PP
Complete the installation of the update by extracting any other desired
material from the second distribution tape and installing this
material as therein described.
.de IR
\fI\\$1\fP\|\\$2
..
.ds LH "Installing/Operating 4.1bsd
.nr H1 3
.nr H2 0
.ds CF \*(DY
.ds RH "System setup
.bp
.LG
.B
.ce
3. SYSTEM SETUP
.sp 2
.R
.NL
.PP
This section describes procedures used to setup a VAX UNIX system.
Procedures described here are used when a system is first installed
or when the system configuration changes.  Procedures for normal
system operation are described in the next section.
.NH 2
Making a UNIX boot floppy
.PP
If you have an 11/780 you will want to create a
.UX
boot floppy by adding some files to a copy of your current console floppy,
using
.I flcopy
and
\fIarff\fP\|(8).
This floppy will make standalone system operations such as
bootstrapping much easier.
.PP
Someday a similar procedure to the one described here will be possible
on 11/750's to create a console cassette; as we do not have a reliable
driver for the 11/750 console cassette yet, this is not currently feasible.
.PP
Place your local console floppy in the console floppy drive
and issue the following commands:
.DS
.DT
\fB#\fP cd /sys/floppy
\fB#\fP mkdir fromdec		(scratch sub-directory)
\fB#\fP cd fromdec
\fB#\fP arff xv			(extract all files from floppy)
(list of files prints out)
\fB#\fP flcopy \-t3
(system reads header information off current floppy)
\fBChange Floppy, Hit return when done.\fP
(waits for you to put clean floppy in console)
(copies header information back out after you hit return)
\fB#\fP rm floppy			(don't need copy of old headers)
\fB#\fP rm dm* db* s[mbr]* vmb.exe	(don't need these files with \s-2UNIX\s0)
\fB#\fP arff cr *			(add basic files)
\fBAre you sure you want to clobber the floppy?\fP
yes				(clobbering is, essentially, a \fBmkfs\fP)
\fB#\fP cd ..
\fB#\fP rm \-r fromdec		(remove scratch directory)
.DE
If you have an RK07 as your primary root do:
.DS
\fB#\fP cp defboo.hk defboo.cmd
.DE
If you have a UNIBUS storage module as your primary root do:
.DS
\fB#\fP cp defboo.up defboo.cmd
.DE
Otherwise:
.DS
\fB#\fP cp defboo.hp defboo.cmd
.DE
Then finish the procedure by doing:
.DS
\fB#\fP arff r *				(add UNIX boot files to floppy)
.DE
More copies of this floppy can be made using
.IR flcopy (8).
.NH 2
Kernel configuration
.PP
This section describes the layout of the kernel code,
how files for devices are made and drivers for the devices
are configured into the system and how the kernel is rebuilt
to include the needed drivers.
We also suggest ways to organize local changes to the kernel
and discuss how some size limitations imposed by the kernel can
be overcome.
.NH 3
Kernel organization
.PP
As distributed, the kernel source is kept in the subdirectories of /sys.
The directory /sys/sys
contains the mainline kernel code, implementing system calls, the
file system, virtual memory, etc.
The directory /sys/dev
contains device drivers and other low-level routines.
The directory /sys/conf
contains system configuration information and files used in
generating new system configurations.
The directory /sys/h
contains the header files, defining structures and system constants.
N.B.: The system header files in /usr/include/sys are copies
of the files in /sys/h.
.PP
Unless you add devices not supported by the standard distribution
you should only need to create a single file in the conf
directory to describe your system and run the procedures described
here to reconfigure the supplied system to have the needed drivers.
.NH 3
Devices and device drivers
.PP
Devices to be supported by UNIX are implemented in the kernel
by drivers, whose source is kept in /sys/dev, and that are loaded
into the system when included in a cpu specific configuration file
kept in the conf directory.  Devices are accessed through special
files in the file system, made by the
.IR mknod (8)
program and normally kept in the /dev directory.
For all the devices supported by the distribution system, these
files in /dev are created by the /dev/MAKE
shell script.
.PP
Determine the set of devices that you have and create a new /dev
directory by running the MAKE script.
First create a new directory
/newdev, copy MAKE into it, edit MAKE
to provide an entry for local needs, replacing the case LOCAL,
and run it to generate a /newdev directory.
For instance, if your machine had a single dz-11, a single
dh-11, a rm03 disk, a EMULEX controller and a AMPEX-9300 disk
and a te16 tape drive you would do:
.DS
\fB#\fP cd /
\fB#\fP mkdir newdev
\fB#\fP cp /dev/MAKE /newdev/MAKE
\fB#\fP cd newdev
\fB#\fP ./MAKE dz0 dh0 hp0 up0 ht0 std LOCAL
.DE
Note the ``std'' argument here that causes standard devices
such as /dev/console, the machine console, and /dev/floppy,
the console floppy disk interface for the 11/780, to be created.
.PP
You can then do
.DS
\fB#\fP cd /
\fB#\fP mv dev olddev ; mv newdev dev
\fB#\fP sync
.DE
to install the new device directory.
.NH 3
Creating a System Configuration File
.PP
The kernel configuration of each VAX UNIX system is described by
a single configuration file, stored in the /sys/conf directory.
To learn about the format of this file,
start by reading
.IR config (8),
looking at the manual pages in section 4
of the UNIX manual for the devices you have
and looking at the configuration files in the /sys/conf
.PP
If you have just bootstrapped for the first time
you can save time by cannibalizing the GENERIC directory
supplied with the tape.  Pick a name for you machine (for definiteness
call it PICKLE).  Then in the /sys directory do:
.DS
\fB# \fPmv GENERIC PICKLE
\fB# \fPcd conf
\fB# \fPcp GENERIC PICKLE
.DE
Then edit the PICKLE file.
First change the line
.DS
.ta 1i
ident	GENERIC
.DE
to
.DS
ident	PICKLE
.DE
and the line
.DS
.ta 1i 2i
config	generic	vmunix
.DE
to
.DS
.ta 1i 2i
config	\fIxx\fP	vmunix
.DE
For systems that page on a single disk, \fIxx\fP should be given as
one of \fBhp\fP, \fBhk\fP or \fBup\fP.
This causes the root file system and paging device to be on drive 0
of the device of the corresponding type.
.PP
When more drives are available it is desirable to interleave the paging
between drives.  If you have two drives of the same type you can
use hphp, upup or hkhk, and the system will
page on drives 0 and 1.  If you are fortunate
enough to have two good disk controllers, you can use hpup
to put the root on the MASSBUS drive hp0 and then interleave this
with UNIBUS drive up0, or uphp to start on up0 and interleave
it with MASSBUS drive hp0.
.PP
Now edit the rest of the description of the machine to fit yours,
setting the time zone, the maximum count of active users expected,
listing the interconnects that you have,
and listing each device.
Refer to section 4 of the manual and your DEC site maintenance
guide for some of the magic numbers and patterns here.
When you are done try running
.DS
\fB#\fP /etc/config PICKLE
.DE
and get out any errors that \fIconfig\fP detects.
Then do
.DS
\fB#\fP cd ../PICKLE
\fB#\fP make depend
\fB#\fP make
.DE
to create a new system.
.PP
The final object file ``vmunix'' should be
copied to the root, and then booted to try it out.
It is best to name it /newvmunix so as not to destroy
the working system until you're sure it does work:
.DS
\fB#\fP cp vmunix /newvmunix
\fB#\fP sync
.DE
It is also a good idea to keep the old system around under some other
name.  In particular, we recommend that you save the generic distribution
version of the system permanently as /genvmunix for use in emergencies.
.PP
To boot the new version of the system you should follow the
bootstrap procedures outlined in section 4.1 below.
A systematic scheme for numbering and saving old versions
of the system is best.
.PP
You can repeat these steps whenever it is necessary to change
the system configuration.
.NH 3
Making changes to the kernel
.PP
If you wish to make local mods to the kernel you should bracket them with
.DS
#ifdef PICKLE
\&...
#endif
.DE
perhaps saving old code between
.DS
#ifndef PICKLE
\&...
#endif
.DE
This will allow you to find changed code easily.
.PP
To add devices not supported by the distribution system you will have to place
the drivers for the device in the directory /sys/dev,
edit a line into the block or character device table in /sys/conf/conf.c,
and add the driver to the list of kernel file in /sys/conf/files with
a line like
.DS
dev/\fIzz\fP.c	optional \fIzz\fP device driver
.DE
You can then reconfigure the system, including the new device in
the system configuration and rebuild the system.  After rebooting
the resulting kernel and making appropriate entries in the /dev
directory, you can test out the new device and driver.
Section 4.1 explains shutdown and reboot procedures.
.NH 3
System size limitations
.PP
(This section may be skipped at first reading.)
.PP
As distributed, the sum of the virtual sizes of the core-resident
processes is limited to 64M bytes.  The size of the text, and data
segments of a single process are currently limited to 6M bytes each, and
the stack segment size is limited to 512K bytes as a soft, user-changeable
limit, and may be increased to 6M by calling
\fIvlimit\fP\|(2).
If these are insufficient, they
can be increased by changing the constants MAXTSIZ, MAXDSIZ and MAXSSIZ
in the file
/sys/h/vm.h, while
changing the definitions in
/sys/h/dmap.h and /sys/h/text.h.
You must be careful in doing this that you have adequate paging space.
As configured above, the system has only 16M bytes of paging area,
since there is only one paging area.  The best way to get more
space is to provide multiple, thereby interleaved, paging areas
by using a file other than confhp.c; see the first line of the makefile
in the sys directory and the disk layout section above.
.PP
To increase the amount of resident virtual space possible,
you can alter the constant USRPTSIZE (in
/sys/h/vmparam.h).
Thus to allow 128 megabytes of resident virtual space one would
change the 8 to a 16.
.PP
The system has 12 pages of page tables for its text+data+bss areas and the
per-page system information.  This limits this part of the system
to 12*64K = 768K bytes.  This should be enough until the next release
of the system comes out, and allocates the system page table at boot time.
Increase USRPTSIZE if you must.
.PP
Because the file system block numbers are stored in
page table
.B pg_blkno
entries, the maximum size of a file system is limited to
2^20 1024 byte blocks.  Thus no file system can be larger than 1024M bytes.
.PP
The count of mountable file systems is limited to 15.  This should
be sufficient.  If you have many disks it makes sense to make some of
them single file systems, and the paging areas don't count in this total.
To increase this it will be necessary to change the core-map
/sys/h/cmap.h since there is a 4 bit field used here.  The size
of the core-map will then expand to 16 bytes per 1024 byte page and you should
change /sys/h/cmap.m also.  (Don't forget to change MSWAPX and
NMOUNT in /sys/h/param.h also.)
.PP
The maximum value NOFILE (open files per process limit)
can be raised to
is 30 because of a bit field in the page table entry in
/sys/h/pte.h.
We plan to remove this limitation soon.
.NH 2
Disk configuration
.PP
This section describes how to layout file systems to make use
of the available space and to balance disk load for better system
performance.
.NH 3
Initializing /etc/fstab
.PP
Change into the directory /etc and copy the appropriate file from:
.DS
fstab.rm03
fstab.rm05
fstab.rm80
fstab.ra80
fstab.rp06
fstab.rp07
fstab.rk07
fstab.up300m
fstab.up160m
.DE
to the file /etc/fstab, i.e.:
.DS
\fB#\fP cd /etc
\fB#\fP cp \fIfstab.xxx\fP fstab
.DE
.PP
This will setup the initial information about the usage of disk
partitions, which we see how to update more below.
.NH 3
Disk naming and divisions
.PP
Each physical disk drive can be divided into upto 8 partitions;
UNIX typically
use only 3 partitions (or 4 on 300M drives).  For instance, on an RM03
or RP06, the first partition,
.B hp0a
is used for a root file system, a backup thereof, or a small file system like
/tmp;
the second partition,
.B hp0b,
is used for paging and swapping; and
the third partition
.B hp0g
holds a user file system.
.NH 3
Space available
.PP
The space available on a disk varies per device.  The amount of space
available on the common disk partitions is listed in the following table.
Not shown in the table are the partitions of each drive devoted
to the root file system and the paging area.
.DS
.TS
l l n l n.
Type	Name	Size	Name	Size
_
rk07	hk?g	13 Mb
rm03	hp?g	41 Mb
rp06	hp?g	145 Mb
rm05	hp?h	145 Mb	hp?g	80 Mb
rm80	hp?g	41 Mb	hp?h	55 Mb
ra80	ra?g	41 Mb	ra?h	52 Mb
rp07	hp?g	240 Mb	hp?h	224 Mb
up300	up?h	145 Mb	up?g	80 Mb
up160	up?h	106 Mb
.TE
.DE
.LP
Here up300 refers to either an AMPEX or CDC 300 Megabyte disk on a
UNIBUS disk controller, and up160 refers to a FUJITSU 160 Megabyte disk
on the UNIBUS.
.PP
Each disk also has a paging area, typically of 16 Megabytes, and
a root file sytem of 8 Megabytes.
The distributed system binaries and sources occupy about 10 Megabytes
each, filling most of the two \fBg\fP partitions on RK07's, but fitting
easily into the partitions of the other drives.
.PP
Be aware that the disks have their sizes
measured in ``sectors'' of 512 bytes each, while the UNIX file
system blocks are 1024 bytes each.  Thus if a disk partition has
10000 sectors (disk blocks), it will have only 5000 UNIX file system
blocks, and you \fBmust\fP divide by 2 to use 5000 when
specifying the size to the \fImkfs\fP command.
.NH 3
Layout considerations
.PP
There are several considerations in deciding how to adjust the arrangement
of things on your disks:
the most important is making sure there is adequate space
for what is required;
secondarily, throughput should be maximized.
Paging space is an important parameter.
The system
as distributed has 16 Megabytes of space in which to page in the
primary space on most devices.*
.FS
* RK07 systems actually have only 5 Megabytes of paging area;
the corresponding configuration file for such systems knows this.
RP07 based systems have a 32 Megabyte paging area, but the
system configurations don't know this; you will have to change
them to make use of the available space.  Look at the file
/sys/dev/swaphp.c and change the 32xxx constant there.
.FE
Additional devices may be provided, with the
paging interleaved between them.
.PP
Many common system programs (C, the editor, the assembler etc.)
create intermediate files in the /tmp directory,
so the file system where this is stored also should be made
large enough to accommodate
most high-water marks; if you have several disks, it makes
sense to mount this in a ``root'' (i.e. first partition)
file system on another disk.
If the system source is moved out of /sys and put in /usr/src/sys
(where it was kept in previous releases of the systems),
there should be adequate space in the root file system for /tmp.
On RK systems, where there is little space in the \fBhk0g\fP or \fBhk1g\fP
file systems to store the system source,
it is normal to mount /tmp on /dev/hk1a.
All the programs that create files in /tmp take
care to delete them, but are not immune to rare events
and can leave dregs.
The directory should be examined every so often and the old
files deleted.
.PP
The efficiency with which UNIX is able to use the CPU
is often strongly affected by the configuration of disk controllers.
For general time-sharing applications,
the best strategy is to try to split the root file system (/), system binaries
(/usr), the temporary files (/tmp),
and the user files among several disk arms, and to interleave
the paging activity among a several arms.
.PP
It is critical for good performance to balance disk load.
There are at least five components of the disk load that you can
divide between the available disks:
.DS
1. The root file system.
2. The /tmp file system.
3. The /usr file system.
4. The user files.
5. The paging activity.
.DE
The following possibilities are ones we have used at times
when we had 2, 3 and 4 disks:
.TS
center doublebox;
l | c s s
l | lw(5) | lw(5) | lw(5).
	disks
what	2	3	4
_
/	1	2	2
tmp	1	3	4
usr	1	1	1
paging	1+2	1+3	1+3+4
users	2	2+3	2+3
archive	x	x	4
.TE
.PP
The most important things to consider are to
even out the disk load as much as possible, and to do this by
decoupling file systems (on separate arms) between which heavy copying occurs.
Note that a long term average balanced load is not important... it is
much more important to have instantaneously balanced
load when the system is busy.
.PP
Intelligent experimentation with a few file system arrangements can
pay off in much improved performance.  It is particularly easy to
move the root, the
/tmp
file system and the paging areas.  Place the
user files and the
/usr
directory as space needs dictate and experiment
with the other, more easily moved file systems.
.NH 3
Implementing a layout
.PP
To put a chosen disk layout into effect, you should use the
.IR mkfs (8)
and
.IR mklost+found (8)
commands to create each new file system.
Each file system must also be added to the file
/etc/fstab
so that it will be checked and mounted when the system is bootstrapped.
.PP
As an example, consider a system with rm03's.  On the first rm03, \fBhp0\fP,
we will put the root file system in \fBhp0a\fP, and the \fB/usr\fP
file system in \fBhp0g\fP, which has enough space to hold it and then some.
The /tmp directory will be part of the root file system,
as no file system will be mounted on /tmp.
If we had only one rm03, we would put user files
in the \fBhp0g\fP partition with the system source and binaries.
.PP
If we had a second rm03, we would create a file system in \fBhp1g\fP
and put user files there, calling the file system /mnt.
We would also interleave the paging
between the 2 rm03's.  To do this we would build a system configuration
that specified:
.DS
config	hphp	vmunix
.DE
to get the swap interleaved, and add the lines
.DS
/dev/hp1b::sw::
/dev/hp1g:/mnt:rw:1:2
.DE
to the /etc/fstab file.  We would keep a backup copy of the root
file system in the \fBhp1a\fP disk partition.
.PP
To make the /mnt file system we would do:
.DS
\fB#\fP cd /dev
\fB#\fP MAKE hp1
\fB#\fP /etc/mkfs /dev/rhp1g 40992 3 80
(information about file system prints out)
\fB#\fP mkdir /mnt
\fB#\fP mount /dev/hp1g /mnt
\fB#\fP cd /mnt
\fB#\fP /etc/mklost+found
.DE
.NH 2
Configuring terminals
.PP
If UNIX is to support simultaneous
access from more than just the console terminal,
the file /etc/ttys (\fIttys\fP\|(5)) has to be edited.
.PP
Terminals connected via dz interfaces are conventionally named \fBttyDD\fP
where DD is a decimal number, the ``minor device'' number.
The lines on dz0 are named /dev/tty00, /dev/tty01, ... /dev/tty07.
Lines on dh interfaces are conventionally named \fBttyh\fPX, where X
is a hexadecimal digit.  If more than one dh interface is present
in a configuration, successive terminals would be named
\fBttyi\fPX, \fBttyj\fPX, etc.
.PP
To add a new terminal be sure the device is configured into the system,
that the special file for the device has been made by /dev/MAKE,
and the special file exists, then set
the first character of the appropriate line of /etc/ttys to 1
(or add a new line).
.PP
The second character of each line in the /etc/ttys file lists
the speed and initial parameter settings for the terminal.
The commonly used choices are:
.DS
0	300-1200-150-110
2	9600
3	1200-300
5	300-1200
.DE
Here the first speed is the speed a terminal starts at, and
``break'' switches speeds.
Thus a newly added terminal /dev/tty00 could be added as
.DS
12tty00
.DE
if it was wired to run at 9600 baud.
.PP
Dialup terminals should be wired so that carrier is asserted only when the
phone line is dialed up.
For non-dialup terminals from which modem control
is not available, you must either wire back the signals so that
the carrier appears to always be present, or show in the system
configuration that carrier is to be assumed to be present.  See
.IR dh (4)
and
.IR dz (4)
for details.
.PP
You should also edit the file
/etc/ttytype
placing the type of each new terminal there (see \fIttytype\fP\|(5)).
.PP
When the system is bootstrapped, all terminals that are listed
in /etc/ttys having a 1 as the first character of
their line are enabled.  If, during normal operations, it is desired
to disable a terminal line, you can edit the file
/etc/ttys
and change the first character of the corresponding line to be a 0 and
then send a hangup signal to the \fIinit\fP process, by doing
.DS
\fB#\fP kill \-1 1
.DE
Terminals can similarly be enabled by changing the first character of a line
from a 0 to a 1 and sending a hangup to \fIinit\fP.
.PP
Note that /usr/src/cmd/init.c and
/usr/src/cmd/comsat.c will have to be recompiled if there are to be
more than 100 terminals.
Also note that if a special file is inaccessible when \fIinit\fP tries
to create a process for it, init will print a message on the console
and try to reopen the terminal every minute, reprinting the warning
message every 10 minutes.
.PP
Finally note that you should change the names of any dialup
terminals to ttyd?
where ? is in [0-9a-f] as some programs use this property of the
names to determine if a terminal is a dialup.
Shell commands to do this should be put in the /dev/MAKE
script under case LOCAL.
.PP
While it is possible to use truly arbitrary strings for terminal names,
the accounting and noticeably the
\fIps\fP\|(1)
command make good use of the convention that tty names
(by default, and also after dialups are named as suggested above)
are distinct in the last 2 characters.  Change this and you may be sorry later,
as the heuristic
\fIps\fP\|(1)
uses based on these conventions will then break down and ps will
run MUCH slower.
.NH 2
Adding users
.PP
New users can be added to the system by adding a line to the
password file
/etc/passwd.
The procedure for adding a new user is described in
.IR adduser (8).
.PP
You should add accounts for the initial user community, giving
each a directory and a password, and putting users who will wish
to share software in the same group.
.PP
A number of guest accounts have been provided on the distribution
system; these accounts are for people at Berkeley, DEC and at Bell Laboratories
who have done major work on UNIX in the past.  You can delete these accounts,
or leave them on the system if you expect that these people would have
occasion to login as guests on your system.
.NH 2
System tailoring
.PP
We now describe how to modify programs that vary per machine
or have the machine name compiled in.
.NH 3
Recompiling commands with names compiled in
.PP
A few of the commands on the system have local network topology
and the machine name compiled into them.  To fix these programs for your
system you should edit the files
.DS
/usr/include/ident.h
/usr/include/whoami
/usr/include/whoami.h
.DE
and then recompile and reinstall the commands:
.DS
\fB#\fP DESTDIR=/; export DESTDIR
\fB#\fP cd /usr/src/cmd
\fB#\fP MAKE getty.c mail.c wall.c who.c
.DE
.NH 2
Setting up the mail system
.PP
The mail system consists of the following commands:
.DS
.TS
l l.
/bin/mail	old standard mail program (from 32/V)
/usr/ucb/mail	UCB mail program, described in mail(1)
/etc/delivermail	mail rouitng program
/usr/net/bin/v6mail	local mailman
/usr/spool/mail	mail spooling directory
/usr/spool/secretmail	secure mail directory
/usr/bin/xsend	secure mail sender
/usr/bin/xget	secure mail receiver
/usr/lib/aliases	mail forwarding information
/usr/ucb/newaliases	command to rebuild binary forwarding database
/usr/ucb/biff	mail notification enabler
/etc/comsat	mail notification daemon
/dev/mail	mail notification device
.TE
.DE
Mail is normally sent and received using the
.IR mail (1)
command, which provides a front-end to edit the messages sent
and received, and passes the messages to
.IR delivermail (8)
for routing.
The routing algorithm uses knowledge of network name syntax built
into its tables and aliasing and forwarding information built into
the file /usr/lib/aliases to process each piece of mail.
Local mail is delivered by giving it to the program /usr/net/bin/v6mail
which adds it to the mailboxes in the directory /usr/spool/mail/\fIusername\fP,
using a locking protocol to avoid problems with simultaneous updates.
After the mail is delivered, the local mail delivery daemon /etc/comsat
is notified by a message sent to /dev/mail and it will notify
users who have issued a ``biff y'' command that mail has arrived.
.PP
Mail is normally accessible in the directory /usr/spool/mail and is readable
by all users.*
.FS
* You can make your mail not readable by others by changing the mode
of the file /usr/spool/mail/\fIyourname\fR to 600.
.FE
To send mail which is secure against any possible
perusal (except by a code-breaker) you should
use the secret mail facility, which encrypts the mail so that noone can read
it.
.PP
To setup the mail facility you should read the instructions in the
file READ_ME in the directory /usr/src/cmd/delivermail and then adjust
and recompile the delivermail program, reinstalling it in /etc/delivermail.
You should also set up the file /usr/lib/aliases for your installation,
creating mail groups as appropriate.
.NH 3
Setting up a uucp connection
.PP
To connect two UNIX machines with a \fIuucp\fP network link using modems,
one site must have a automatic call unit and the other must have a
dialup port.  It is better if both sites have both.
.PP
You should first read the paper in volume 2B of the Unix Programmers Manual:
``Uucp Implementation Description''.  It describes in detail the file
formats and conventions, and will give you a little context.
For any configuration, you must recompile all system dependent programs.
.PP
Change directory to /usr/src/cmd/uucp and edit uucp.h.
Search for ``MYNAME'' and
change the site name to your own site (to match the one you installed
in /usr/include/whoami.h as \fIsysname\fP.) 
If you haven't done this yet,
the site name should uniquely identify you in the world, so avoid names
like ``a'' or ``csl''.  Uucp only looks at the first 7 letters, but mail
will look at the entire name, so the first 7 characters should make it unique.)
Recompile uucp with ``make'' and su to ``make install''.
.PP
You should ensure that the directories
/usr/spool/uucp and /usr/spool/uucppublic exist.
The former should be owned by uucp, mode 755 (or 777 is OK) and the
latter should be mode 777 (and the home directory for login uucp).
.PP
Periodically you should clean out /usr/spool/uucp and /usr/spool/uucppublic,
as they can accumulate junk, especially if you don't have a dialer.
Run ``uulog'' once a day, and ``/usr/lib/uucp/uuclean'' periodically with
appropriate options to get rid of old stuff.*
.FS
* The
\fIcron\fP\|(8)
program will arrange to execute these commands periodically.
.FE
You can also just remove
some of the files in /usr/spool/uucp, but if you do this blindly you
will cause some error messages to be generated when uucp tries to access
a file another file claims is there.  (For instance, each mail transaction
creates three files.)
The /usr/spool/uucppublic directory
is a place for people at other sites to send to when sending
files to users on your machine.  You should clean it out by hand when
it gets excessive.
.PP
If both sites have both a dialer and dialup:
Follow the directions in the volume 2B paper \- this is the intended
mode of operation and the directions fit well.  You
have to configure the following files in /usr/lib/uucp:
.DS
.TS
l l.
L.sys	setup all fields \- this lists the other sites
L-devices	your dialer
USERFILE	permissions \- this can be left alone
.TE
.DE
You must also establish a login ``uucp'' in /etc/passwd with shell
/usr/lib/uucp/uucico.  Each site must know the other sites phone
number, login, and password.
.PP
If you have only a dialup:
You can be a second-class citizen on the uucp net.  You must find
another site that has a dialer, and have them poll you regularly.
(Once a day is about the minimum that is reasonable.)  When you send
mail to another site, you must wait for them to call you.
You must set up /usr/lib/uucp/USERFILE and /usr/lib/uucp/L.sys.
Only the first 4 fields of L.sys
are necessary, and in practice only the first field (site name) is
looked at.  A typical L.sys for a passive node might be:
.DS
.TS
l l.
ucbvax	Any ACU 300
research	Any ACU 300
.TE
.DE
where the first field on each line is a site that will poll you.
You need to put a password on the uucp login and let the other site know
your phone number, uucp login name (which is usually uucp), and password.
It doesn't matter if they call you at 300 or 1200 baud.
.PP
If you have a dialer and want to poll another site:
Normally, uucp will call the other site when it has anything to send it,
and while it's at it will check to see if anything should come back.
The command
.DS
/usr/lib/uucp/uucico \-r1 \-sucbvax
.DE
will force \fIuucp\fP to poll ucbvax, even if there is nothing waiting.
This command can be conveniently put in /usr/lib/crontab to run
at 3 AM each morning.  Note that it is a bad thing for \fIuucp\fP to be
run as root, since the suid bit will not be honored.  A better procedure
is to put a shell script in /usr/lib/uucp/poll, and the line
.DS
su daemon < /usr/lib/uucp/poll
.DE
in /usr/lib/crontab;
see
\fIcron\fP\|(8).
This conveniently groups all polls into one place,
and ensures that the suid bit will be honored.  (If \fIuucp\fP is run as
root, files created are owned by root, and later invocations of \fIuucp\fP
as \fIuucp\fP will not be able to write on them.)
If you are having trouble with the connection, invoke uucico by hand:
.DS
/usr/lib/uucp/uucico \-r1 \-sucbvax \-x7
.DE
where the \fB-x\fP option turns on debugging output.  The higher the number,
the more debugging output you get; 1, 4, and 7 are reasonable choices.
.de IR
\fI\\$1\fP\|\\$2
..
.ds LH "Installing/Operating 4.1bsd
.nr H1 4
.nr H2 0
.ds RH "System Operation
.ds CF \*(DY
.bp
.LG
.B
.ce
4. SYSTEM OPERATION
.sp 2
.R
.NL
.PP
This section describes procedures used to operate a VAX UNIX system.
Procedures described here are used periodically, to reboot the system,
analyze error messages from devices, do disk backups, monitor
system performance, recompile system software and control local changes.
.NH 2
Bootstrap and shutdown procedures
.PP
In a normal reboot, the system checks the disks and comes up multi-user
without intervention at the console.
Such a reboot
can be stopped (after it prints the date) with a ^C (interrupt).
This will leave the system in single-user mode, with only the console
terminal active.
.PP
If booting from the console command level is needed, then the command
.DS
\fB>>>\fP B
.DE
will boot from the default device.
On an 11/780 the default device is determined by a ``DEPOSIT''
command stored on the floppy in the file ``DEFBOO.CMD'';
on an 11/750 the default device is determined by the setting of a switch
on the front panel.
.PP
You can boot a system up single user on a 780 by doing
.DS
\fB>>>\fP B \fIXX\fP\|S
.DE
where \fIXX\fP is one of HP, HK or UP.
The corresponding command on an 11/750 should be
.DS
\fB>>>\fP B/1
.DE
but there is a bug in the microcode and the boot flags (e.g. 1)
get lost.
Instead you have to do:
.DS
\fB>>>\fP B/1 \fIXX\fPA0
.DE
where \fIXX\fP is DM for RK07 (i.e. like HK's),
DB for RP06, RM03, RM05, RP07 and RM80.
.PP
For storage modules on the UNIBUS of an 11/750 you will need to
have a boot rom.  We are currently investigating how to create
such boot roms; contact us if you need to get one.*
.FS
* As a temporary substitute you can bootstrap from such a disk using
the boot command that the controller supplies; e.g. the EMULEX
SC21-V reads block 0 from the disk when an octal \fB75\fR is deposited
in its control-status register.  This will not, unfortunately,
allow automatic restarts; a boot rom is needed to allow this.
.FE
.PP
Other possibilities are:
.DS
\fB>>>\fP B ANY
.DE
or, on a 750
.DS
\fB>>>\fP B/3
.DE
(modulo the bug reported above)
These commands boot and ask for the name of the system to be booted.
They can be used after building a new test system to give the
boot program the name of the test version of the system.
.PP
To bring the system up to a multi-user configuration from the single-user
status after, e.g., a ``B HPS'' on an 11/780 or a ``B/1'' on an
11/750 all you have to do is hit ^D on the console.  The system
will then execute /etc/rc,
a multi-user restart script, and come up on the terminals listed as
active in the file /etc/ttys.
See
\fIinit\fP\|(8)
and
\fIttys\fP\|(5).
Note, however, that this does not cause a file system check to be performed.
Unless the system was taken down cleanly, you should run
``fsck \-p'' or force a reboot with
\fIreboot\fP\|(8)
to have the disks checked.
.PP
To take the system down to a single user state you can use
.DS
\fB#\fP kill 1
.DE
or use the
\fIshutdown\fP\|(8)
command (which is much more polite, if there are other users logged in.)
when you are up multi-user.
Either command will kill all processes and give you a shell on the console,
as if you had just booted.  File systems remain mounted after the
system is taken single-user.  If you wish to come up multi-user again, you
should do this by:
.DS
\fB#\fP cd /
\fB#\fP /etc/umount -a
\fB#\fP ^D
.DE
.PP
Each system shutdown, crash, processor halt and reboot
is recorded in the file /usr/adm/shutdownlog
with the cause.
.NH 2
Device errors and diagnostics
.PP
When errors occur on peripherals or in the system, the system
prints a warning diagnostic on the console.  These messages are collected
regularly and written into a system error log file
/usr/adm/messages.
.PP
Error messages printed by the devices in the system are described with the
drivers for the devices in section 4 of the programmer's manual.
If errors occur indicating hardware problems, you should contact
your hardware support group or field service.  It is a good idea to
examine the error log file regularly
(e.g. with ``tail \-r /usr/adm/messages'').
.NH 2
File system checks, backups and disaster recovery
.PP
Periodically (say every week or so in the absence of any problems)
and always (usually automatically) after a crash,
all the file systems should be checked for consistency
by
\fIfsck\fP\|(1).
The procedures of
\fIreboot\fP\|(8)
should be used to get the system to a state where a file system
check can be performed manually or automatically.
.PP
Dumping of the file systems should be done regularly,
since once the system is going it is easy to
become complacent.
Complete and incremental dumps are easily done with
\fIdump\fP\|(8).
You should arrange to do a towers-of-hanoi dump sequence; we tune
ours so that almost all files are dumped on two tapes and kept for at
least a week in most every case.  We take full dumps every month (and keep
these indefinitely).
Operators can execute ``dump w'' at login that will tell them what needs
to be dumped
(based on the /etc/fstab
information).
Be sure to create a group
.B operator
in the file /etc/group
so that dump can notify logged-in operators when it needs help.
.PP
More precisely, we have three sets of dump tapes: 10 daily tapes,
5 weekly sets of 2 tapes, and fresh sets of three tapes monthly.
We do daily dumps circularly on the daily tapes with sequence
`3 2 5 4 7 6 9 8 9 9 9 ...'.
Each weekly is a level 1 and the daily dump sequence level
restarts after each weekly dump.
Full dumps are level 0 and the daily sequence restarts after each full dump
also.
.PP
Thus a typical dump sequence would be:
.br
.ne 6
.TS
center;
c c c c c
n n n l l.
tape name	level number	date	opr	size
_
FULL	0	Nov 24, 1979	jkf	137K
D1	3	Nov 28, 1979	jkf	29K
D2	2	Nov 29, 1979	rrh	34K
D3	5	Nov 30, 1979	rrh	19K
D4	4	Dec 1, 1979	rrh	22K
W1	1	Dec 2, 1979	etc	40K
D5	3	Dec 4, 1979	rrh	15K
D6	2	Dec 5, 1979	jkf	25K
D7	5	Dec 6, 1979	jkf	15K
D8	4	Dec 7, 1979	rrh	19K
W2	1	Dec 9, 1979	etc	118K
D9	3	Dec 11, 1979	rrh	15K
D10	2	Dec 12, 1979	rrh	26K
D1	5	Dec 15, 1979	rrh	14K
W3	1	Dec 17, 1979	etc	71K
D2	3	Dec 18, 1979	etc	13K
FULL	0	Dec 22, 1979	etc	135K
.TE
We do weekly's often enough that daily's always fit on one tape and
never get to the sequence of 9's in the daily level numbers.
.PP
Dumping of files by name is best done by
\fItar\fP\|(1)
but the amount of data that can be moved in this way is limited
to a single tape.
Finally if there are enough drives entire
disks can be copied with
\fIdd\fP\|(1)
using the raw special files and an appropriate
block size.
.PP
It is desirable that full dumps of the root file system are made regularly.
This is especially true when only one disk is available.
Then, if the
root file system is damaged by a hardware or software failure, you
can rebuild a workable disk using a standalone restore in the
same way that \fIrestor\fP was used to build the initial root file
system.
.PP
Exhaustion of user-file space is certain to occur
now and then;
the only mechanisms for controlling this phenomenon
are occasional use of
\fIdu\fP\|(1),
\fIdf\fP\|(1),
\fIquot\fP\|(8),
threatening
messages of the day, and personal letters.
.NH 2
Moving filesystem data
.PP
If you have the equipment,
the best way to move a file system
is to dump it to magtape using
\fIdump\fP\|(8),
to use
\fImkfs\fP\|(8)
and
\fImklost+found\fP\|(8)
to create the new file system,
and restore, using \fIrestor\fP\|(8), the tape.
If for some reason you don't want to use magtape,
dump accepts an argument telling where to put the dump;
you might use another disk.
Sometimes a file system has to be increased in logical size
without copying.
The super-block of the device has a word
giving the highest address that can be allocated.
For small increases, this word can be patched
using the debugger \fIadb\fP\|(1)
and the free list reconstructed using
\fIfsck\fP\|(8).
The size should not be increased greatly
by this technique, since the file system will then be short of file slots.
Read and understand the description given in
\fIfilsys\fP\|(5)
before playing around in this way.
.PP
If you have to merge a file system into another, existing one,
the best bet is to
use
\fItar\fP\|(1).
If you must shrink a file system, the best bet is to dump
the original and restor it onto the new file system.
However, this will not work if the i-list on the smaller file system
is smaller than the maximum allocated inode on the larger.
If this is the case, reconstruct the file system from scratch
on another file system (perhaps using \fItar\fP(1)) and then dump it.
If you
are playing with the root file system and only have one drive
the procedure is more complicated.
What you do is the following:
.IP 1.
GET A SECOND PACK!!!!
.IP 2.
Dump the root file system to tape using
\fIdump\fP\|(8).
.IP 3.
Bring the system down and mount the new pack.
.IP 4.
Load the standalone versions of
\fImkfs\fP\|(8)
and
\fIrestor\fP\|(8)
from the floppy (on an 11/780)
with a procedure like:
.br
.ne 5
.sp
.ID
.nf
\fB>>>\fPUNJAM
\fB>>>\fPINIT
\fB>>>\fPLOAD MKFS
	LOAD DONE, xxxx BYTES LOADED
\fB>>>\fPST 2

\&...

\fB>>>\fPH
	HALTED AT yyyy
\fB>>>\fPU
\fB>>>\fPI
\fB>>>\fPLOAD RESTOR
	LOAD DONE, zzzz BYTES LOADED
\fB>>>\fPST 2

\&... etc
.fi
.DE
.IP
On an 11/750 you will have to use the distribution tape
boot procedures to load these programs standalone.
.IP 5.
Boot normally
using the newly created disk file system.
.PP
Note that if you change the disk partition tables or add new disk
drivers they should also be added to the standalone system in
/sys/stand.
.NH 2
Monitoring System Performance
.PP
The
.I vmstat
program provided with the system is designed to be an aid to monitoring
systemwide activity.  Together with the
\fIps\fP\|(1)
command (as in ``ps av''), it can be used to investigate systemwide
virtual activity.
By running
.I vmstat
when the system is active you can judge the system activity in several
dimensions: job distribution, virtual memory load, paging and swapping
activity, disk and cpu utilization.
Ideally, there should be few blocked (b) jobs,
there should be little paging or swapping activity, there should
be available bandwidth on the disk devices (most single arms peak
out at 30-35 tps in practice), and the user cpu utilization (us) should
be high (above 60%).
.PP
If the system is busy, then the count of active jobs may be large,
and several of these jobs may often be blocked (b).  If the virtual
memory is active, then the paging demon will be running (sr will
be non-zero).  It is healthy for the paging demon to free pages when
the virtual memory gets active; it is triggered by the amount of free
memory dropping below a threshold and increases its pace as free memory
goes to zero.
.PP
If you run
.I vmstat
when the system is busy (a ``vmstat 1'' gives all the
numbers computed by the system), you can find
imbalances by noting abnormal job distributions.  If many
processes are blocked (b), then the disk subsystem
is overloaded or imbalanced.  If you have a several non-dma
devices or open teletype lines that are ``ringing'', or user programs
that are doing high-speed non-buffered input/output, then the system
time may go high (60-70% or higher).
It is often possible to pin down the cause of high system time by
looking to see if there is excessive context switching (cs), interrupt
activity (in) or system call activity (sy).  Cumulatively on one of
our large machines we average about 60 context switches and interrupts
per second and about 90 system calls per second.
.PP
If the system is heavily loaded, or if you have little memory
for your load (1M is little in most any case), then the system
may be forced to swap.  This is likely to be accompanied by a noticeable
reduction in system performance and pregnant pauses when interactive
jobs such as editors swap out.
If you expect to be in a memory-poor environment
for an extended period you might consider administratively
limiting system load.
.NH 2
Recompiling and reinstalling system software
.PP
It is easy to regenerate the system, and it is a good
idea to try rebuilding pieces of the system to build confidence
in the procedures.
The system consists of three major parts:
the kernel itself (/sys), the user programs
(/usr/src/cmd and subdirectories), and the libraries
(/usr/src/lib).
The major part of this is /usr/src/cmd.
.PP
We have already seen how to recompile the system itself.
The three major libraries are the C library in /usr/src/libc
and the \s-2FORTRAN\s0 libraries /usr/src/lib/libI77 and
/usr/src/lib/libF77.  In each
case the library is remade by changing into the corresponding directory
and doing
.DS
\fB#\fP make
.DE
and then installed by
.DS
\fB#\fP make install
.DE
Similar to the system,
.DS
\fB#\fP make clean
.DE
cleans up.
The shell script /usr/src/MAKE does this; look at it
for enlightenment.*
.FS
* /usr/src/MAKE actually assumes that the system source is kept
in /usr/src/sys.  This used to be the case, and many sites move
the system source from /sys to /usr/src/sys.  The source is kept
in the root file system on the distribution tape because you get
that file system after completing only part of the bootstrap.
Placing it there gives you a complete, compilable system sooner,
and makes the distribution tape more resilient against bootstrapping
problems.
.FE
.PP
The source for all other libraries is kept in subdirectories of
/usr/src/lib; each has a makefile and can be recompiled by the above
recipe.
.PP
As /usr/src/MAKE shows,
you can recompile all user programs in /usr/src/cmd by using
the MAKE shell script that resides there and its associated file
DESTINATIONS.
For instance, to recompile ``date.c'', 
all one has to do is
.DS
\fB#\fP cd /usr/src/cmd
\fB#\fP MAKE date.c
.DE
this will place a stripped version of the binary of ``date''
in /4bsd/bin/date, since date normally resides in /bin, and
Admin is building a file-system like tree rooted at /4bsd.
You will have to make the directory 4bsd for this to work.
It is possible to use any directory for the destination, it isn't necessary
to use the default /4bsd; just change the instance of ``4bsd''
at the front of MAKE.
.PP
You can also override the default target by doing:
.DS
\fB#\fP DESTDIR=\fIpathname\fP
\fB#\fP export DESTDIR
.DE
.PP
To regenerate all the system source you can do
.DS
\fB#\fP DESTDIR=/usr/newsys
\fB#\fP export DESTDIR
\fB#\fP cd /usr
\fB#\fP rm \-r newsys
\fB#\fP mkdir newsys
\fB#\fP cd /usr/src/cmd
\fB#\fP MAKE * > ERRS 2>& 1 &
.DE
This will take about 4 hours on a reasonably configured machine.
When it finished you can move the hierarchy into the normal places
using
\fImv\fP\|(1)
and
\fIcp\fP\|(1),
and then execute
.DS
\fB#\fP DESTDIR=/
\fB#\fP export DESTDIR
\fB#\fP cd /usr/src/cmd
\fB#\fP MAKE ALIASES
\fB#\fP MAKE MODES
.DE
to link files together as necessary and to set all the right set-user-id
bits.
.NH 2
Making local modifications
.PP
To keep track of changes to system source we migrate changed
versions of commands in /usr/src/cmd in through the directory /usr/src/new
and out of /usr/src/cmd into /usr/src/old for a time before removing them.
Locally written commands that aren't distributed are kept in /usr/src/local
and their binaries are kept in /usr/local.  This allows /usr/bin /usr/ucb
and /bin to correspond to the distribution tape (and to the manuals that
people can buy).  People wishing to use /usr/local commands are made
aware that they aren't in the base manual.  As manual updates incorporate
these commands they are moved to /usr/ucb.
.PP
A directory /usr/junk to throw garbage into, as well as binary directories
/usr/old and /usr/new are useful.  The man command supports manual
directories such as /usr/man/manj for junk and /usr/man/manl for local
to make this or something similar practical.
.NH 2
Accounting
.PP
UNIX currently optionally records two kinds of accounting information:
connect time accounting and process resource accounting.  The connect
time accounting information is stored in the file /usr/adm/wtmp, which
is summarized by the program
.IR ac (8).
The process time accounting information is stored in the file /usr/adm/acct,
and analyzed and summarized by the program
.IR sa (8).
.PP
If you need to implement recharge for computing time, you can implement
procedures based on the information provided by these commands.
A convenient way to do this is to give commands to the clock daemon
/etc/cron
to be executed every day at a specified time.  This is done by adding
lines to /usr/adm/crontab; see
.IR cron (8)
for details.
.NH 2
Resource control
.PP
Resource control in the current version of UNIX is rather primitive.
The resources consumed by any single process can be voluntarily limited
by the mechanisms of
.IR vlimit (2).
Disk space usage can be monitored by
.IR quot (8)
or
.IR du (1)
as was previously.
No system enforced procedure for controlling a users disk space usage
is implemented under the current system, although
a modicum of control can be obtained by dividing user groups between
different disk partitions.
.NH 2
Files which need periodic attention
.PP
We conclude the discussion of system operations by listing
the files that require periodic attention or are system specific
.de BP
.IP \fB\\$1\fP
.br
..
.TS
center;
lb a.
/etc/fstab	how disk partitions are used
/etc/group	group memberships
/etc/motd	message of the day
/etc/passwd	password file; each account has a line
/etc/rc	system restart script; runs reboot; starts demons
/etc/securetty	restricted list of ttys where root can log in
/etc/ttys	enables/disables ports
/etc/ttytype	terminal types corrected to ports
/usr/lib/crontab	commands that are run periodically
/usr/lib/aliases	mail forwarding and distribution groups
/usr/adm/acct	raw process account data
/usr/adm/dnacct	raw autodialer account data
/usr/adm/messages	system error log
/usr/adm/shutdownlog	log of system reboots
/usr/adm/wtmp	login session accounting
.TE
