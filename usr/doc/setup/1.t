.ds lq ``
.ds rq ''
.ds LH "Installing/Operating 4.2BSD
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
This document explains how to install the 4.2BSD release of the Berkeley
version of UNIX for the VAX on your system.  Due to the new file system
organization used in 4.2BSD,
no matter what version of UNIX you may currently be running
you will have to perform a full bootstrap from the distribution tape;
the techniques for converting ``old'' systems are discussed in a
chapter 3 of this document.
.NH 2
Hardware supported
.PP
This distribution can be booted on a VAX 11/780, VAX 11/750, or VAX 11/730
cpu with any of the following disks:
.DS
.TS
lw(1.5i) l.
DEC MASSBUS:	RM03, RM05, RM80, RP06, RP07
EMULEX MASSBUS:	AMPEX 300M, 330M, CDC 300M, FUJITSU 404M
DEC UNIBUS:	RK07, RA80, RA81, RA60
EMULEX SC-21V UNIBUS*:	AMPEX 300M, 330M, CDC 300M, FUJITSU 160M, 404M
DEC IDC:	R80, RL02
.TE
.DE
.FS
* Other UNIBUS controllers and drives may be easily usable with the system,
but will likely require minor modifications to the system
to allow bootstrapping.
The EMULEX disk and SI tape controllers, and
the drives shown here are known
to work as bootstrap devices.
.FE
.PP
The tape drives supported by this distribution are:
.DS
.TS
lw(1.5i) l.
DEC MASSBUS:	TE16, TU45, TU77, TU78
DEC UNIBUS:	TS11, TU80
EMULEX TC-11 UNIBUS:	KENNEDY 9300, CIPHER
TU45 UNIBUS*:	SI 9700
.TE
.DE
.PP
The tapes and disks may be on any available UNIBUS or MASSBUS adapter
at any slot with the proviso that the tape device must be slave 
number 0 on the formatter if it is a MASSBUS tape drive.
.NH 2
Distribution format
.PP
The basic distribution contains the following items:
.DS
(2)\0\0 1600bpi 2400' magnetic tapes,
(1)\0\0 TU58 console cassette, and
(1)\0\0 RX01 console floppy disk.
.DE
Installation on any machine requires a tape unit. 
Since certain standard VAX packages
do not include a tape drive, this means one must either
borrow one from another VAX system or one must be purchased
separately.  The console media distributed with the system
are not suitable for use as the standard console media; their
intended use is only for installation.
.PP
\fBThe distribution does not fit on several standard
VAX configurations which contain only small disks\fP. 
If your hardware configuration does not
provide at least 75 Megabytes of disk space you can still
install the distribution, but you will probably have to operate
without source for the user level commands and, possibly, the
source for the operating system.  The previous RK07-only
distribution format provided by our group is no longer
available.  Further, no attempt has ever been made to install
the system on the standard VAX-11/730 hardware configuration
from DEC which contains only dual RL02 disk drives (though
the distribution tape may be bootstrapped on an RL11 controller
and the system provides support for RL02 disk drives either on
an IDC or an RL11).  The labels on the two distribution tapes 
indicate the amount of disk space each tape file occupies,
these should be used in selecting file system layouts on
systems with little disk space.
.PP
If you have the facilities,
it is a good idea immediately to copy the
magnetic tapes in the distribution kit to guard against disaster.
The tapes are 9-track 1600 BPI and contain some 512-byte records
followed by many 10240-byte records.
There are interspersed tape marks; end-of-tape is signaled
by a double end-of-file.
.PP
The basic bootstrap material is present in three
short files at the beginning of the bootstrap tape.
The first file on the tape contains preliminary bootstrapping programs.
This is followed by a binary image
of a 400 kilobyte ``mini root''
file system.  Following the mini root
file is a full dump of the root file system (see \fIdump\fP\|(8)**).
.FS
** References of the form X(Y) mean the subsection named
X in section Y of the 
.UX
programmer's manual.
.FE
Additional files on the first and second
tapes contain tape archive images (see
\fItar\fP\|(1)):
the fourth file on the first tape contains source for the system
(/sys); the fifth file on the first tape contains
most of the files in the file system
/usr, except the source (/usr/src)
which is in the first file on the second tape.  The
second file on the second tape contains software
contributed by the user community, refer to the accompanying
documentation for a description of its contents and an
explanation of how it should be installed.
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
of the drive, ``DR'' for RM03, RM05, and RM80's, ``DB'' for RP06's.
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
The normal standalone system, used to bootstrap the full UNIX system,
uses device names:
.DS
xx(y,z)
.DE
where \fIxx\fP is either \fBhp\fP, \fBht\fP, or \fBmt\fP.
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
are specified in section 4 of the programmers manual
and in the disk description file /etc/disktab (c.f.
\fIdisktab\fP(5)).*
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
sector forwarding information recorded at the end of the disk (one track
plus 126 sectors).  It is
occasionally used to store a single large file system or to access
the entire pack when making a copy of it on another.  Care must be taken when
using this partition to not overwrite the last few tracks and thereby
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
files and though the tapes contain only 6 tape files, they contain
several thousand UNIX files.
.PP
For the UNIBUS, there are also conventional names.  The important
DEC names to know are DM?? for RK07 drives and DU?? for drives on
a UDA50.  For example, RK07 drive 0 on a
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
IDC disks	rb
SMD disks	up
TM tapes	tm
TU tapes	ut
.TE
.DE
.PP
Here SMD disks are disks on an RM emulating controller on the UNIBUS,
and TM tapes are tapes on a controller that emulates the DEC TM-11.
TU tapes are tapes on a controller that emulates the DEC TU45.
IDC disks are disks on an 11/730 Integral Disk Controller.
TS tapes are tapes on a controller that emulates the DEC TS-11 (e.g.
a TU80).
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
You should be aware that it is sometimes important to use
the character device (for efficiency) or not (because it wouldn't
work, e.g. to write a single byte in the middle of a sector).
Don't change the instructions by using the wrong type of device
indiscriminately.
