.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)4.t	5.1 (Berkeley) %G%
.\"
.de IR
\fI\\$1\fP\|\\$2
..
.ds LH "Installing/Operating 4.2BSD
.nr H1 4
.nr H2 0
.ds CF \*(DY
.ds RH "System setup
.bp
.LG
.B
.ce
4. SYSTEM SETUP
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
boot floppy by adding some files to a copy of your current DEC
console floppy, using
.IR flcopy (8)
and
\fIarff\fP\|(8).
This floppy will make standalone system operations such as
bootstrapping much easier.
.PP
First change into the directory where the console floppy
information is stored:
.DS
\fB#\fP cd /sys/floppy
.DE
then set up the default boot device.
If you have an RK07 as your primary root do:
.DS
\fB#\fP cp defboo.hk defboo.cmd
.DE
If you have a drive on a UDA50 (e.g. an RA81) as your
primary root do:
.DS
\fB#\fP cp defboo.ra defboo.cmd
.DE
If you have a second vendor
UNIBUS storage module as your primary root do:
.DS
\fB#\fP cp defboo.up defboo.cmd
.DE
Otherwise:
.DS
\fB#\fP cp defboo.hp defboo.cmd
.DE
If the local configuration requires any changes in restar.cmd
or defboo.cmd (e.g., for interleaved memory controllers),
these should be made now.
The following command will then copy your DEC local console floppy,
updating the copy appropriately.
.DS
\fB#\fP make update
\fBChange Floppy, Hit return when done.\fP
(waits for you to put clean floppy in console)
\fBAre you sure you want to clobber the floppy?\fP yes
.DE
More copies of this floppy can be made using
.IR flcopy (8).
.NH 2
Making a UNIX boot cassette
.PP
If you have an 11/730 you will want to create a
.UX
boot cassette by adding some files to a copy of
your current DEC console cassette, using
.IR flcopy (8)
and
.IR arff (8).
This cassette will make standalone system operations such as
bootstrapping much easier.
.PP
First change into the directory where the console cassette
information is stored:
.DS
\fB#\fP cd /sys/cassette
.DE
then set up the default boot device.
If you have an IDC storage module as your primary root do:
.DS
\fB#\fP cp defboo.rb defboo.cmd
.DE
If you have an RK07 as your primary root do:
.DS
\fB#\fP cp defboo.hk defboo.cmd
.DE
If you have a drive on a UDA50 as your primary root do:
.DS
\fB#\fP cp defboo.ra defboo.cmd
.DE
Otherwise:
.DS
\fB#\fP cp defboo.up defboo.cmd
.DE
To complete the procedure place your DEC local
console cassette in 
drive 0 (the drive at front of the CPU);
the following command will then copy it,
updating the copy appropriately.
.DS
\fB#\fP make update
\fBChange Floppy, Hit return when done.\fP
(waits for you to put clean cassette in console drive 0)
\fBAre you sure you want to clobber the floppy?\fP yes
.DE
More copies of this cassette can best be made using
.IR dd (1).
.NH 2
Kernel configuration
.PP
This section briefly describes the layout of the kernel code and
how files for devices are made.
For a full discussion of configuring
and building system images, consult the document ``Building
4.2BSD UNIX Systems with Config''.
.NH 3
Kernel organization
.PP
As distributed, the kernel source is in a 
separate tar image.  The source may be physically
located anywhere within any file system so long as
a symbolic link to the location is created for the
file /sys
(many files in /usr/include are normally symbolic links
relative to /sys).  In further discussions of the
system source all path names will be given relative to
/sys.
.PP
The directory /sys/sys
contains the mainline machine independent
operating system code.
Files within this directory are conventionally
named with the following prefixes.
.DS
.TS
lw(1.0i) l.
init_	system initialization
kern_	kernel (authentication, process management, etc.)
quota_	disk quotas
sys_	system calls and similar
tty_	terminal handling
ufs_	file system
uipc_	interprocess communication
vm_	virtual memory
.TE
.DE
.PP
The remaining directories are organized as follows.
.DS
.TS
lw(1.0i) l.
/sys/h	machine independent include files
/sys/conf	site configuration files and basic templates
/sys/net	network independent, but network related code
/sys/netinet	DARPA Internet code
/sys/netimp	IMP support code
/sys/netpup	PUP-1 support code
/sys/vax	VAX specific mainline code
/sys/vaxif	VAX network interface code
/sys/vaxmba	VAX MASSBUS device drivers and related code
/sys/vaxuba	VAX UNIBUS device drivers and related code
.TE
.DE
.PP
Many of these directories are referenced through /usr/include with
symbolic links.  For example, /usr/include/sys is a symbolic
link to /sys/h.  The system code, as distributed, is totally
independent of the include files in /usr/include.  This allows
the system to be recompiled from scratch without the /usr file
system mounted.
.NH 3
Devices and device drivers
.PP
Devices supported by UNIX are implemented in the kernel
by drivers whose source is kept in /sys/vax, /sys/vaxuba,
or /sys/vaxmba.  These drivers are loaded
into the system when included in a cpu specific configuration file
kept in the conf directory.  Devices are accessed through special
files in the file system, made by the
.IR mknod (8)
program and normally kept in the /dev directory.
For all the devices supported by the distribution system, the
files in /dev are created by the /dev/MAKEDEV
shell script.
.PP
Determine the set of devices that you have and create a new /dev
directory by running the MAKEDEV script.
First create a new directory
/newdev, copy MAKEDEV into it, edit the file MAKEDEV.local
to provide an entry for local needs,
and run it to generate a /newdev directory.
For instance, if your machine has a single dz-11, a single
dh-11, a single dmf-32, an rm03 disk, an EMULEX controller, an
AMPEX-9300 disk, and a te16 tape drive you would do:
.DS
\fB#\fP cd /
\fB#\fP mkdir newdev
\fB#\fP cp dev/MAKEDEV newdev/MAKEDEV
\fB#\fP cd newdev
\fB#\fP MAKEDEV dz0 dh0 dmf0 hp0 up0 ht0 std LOCAL
.DE
Note the ``std'' argument causes standard devices
such as /dev/console, the machine console, /dev/floppy,
the console floppy disk interface for the 11/780, and
/dev/tu0 and /dev/tu1, the console cassette interfaces
for the 11/750 and 11/730, to be created.
.PP
You can then do
.DS
\fB#\fP cd /
\fB#\fP mv dev olddev ; mv newdev dev
\fB#\fP sync
.DE
to install the new device directory.
.NH 3
Building new system images
.PP
The kernel configuration of each UNIX system is described by
a single configuration file, stored in the /sys/conf directory.
To learn about the format of this file and the procedure used
to build system images,
start by reading ``Building 4.2BSD UNIX Systems with Config'',
look at the manual pages in section 4
of the UNIX manual for the devices you have,
and look at the configuration files in the /sys/conf
directory.
.PP
The configured system image ``vmunix'' should be
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
bootstrap procedures outlined in section 6.1.
A systematic scheme for numbering and saving old versions
of the system is best.
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
fstab.ra60
fstab.ra80
fstab.ra81
fstab.rb80
fstab.rp06
fstab.rp07
fstab.rk07
fstab.up160m (160Mb up drives)
fstab.up300m (300Mb up drives)
fstab.hp400m (400Mb hp drives)
fstab.up (other up drives)
fstab.hp (other hp drives)
.DE
to the file /etc/fstab, i.e.:
.DS
\fB#\fP cd /etc
\fB#\fP cp \fIfstab.xxx\fP fstab
.DE
.PP
This will set up the initial information about the usage of disk
partitions, which we see how to update more below.
.NH 3
Disk naming and divisions
.PP
Each physical disk drive can be divided into up to 8 partitions;
UNIX typically
uses only 3 or 4 partitions.
For instance, on an RM03
or RP06, the first partition, hp0a,
is used for a root file system, a backup thereof,
or a small file system like, /tmp;
the second partition, hp0b,
is used for paging and swapping; and
the third partition hp0g
holds a user file system.  On an RM05, the first three partitions
are used as for the RM03, and the fourth partition, hp0h,
is used to hold the /usr file system, including source code.
.PP
The disk partition sizes for a drive are based on a
set of four default partition tables; c.f. \fIdiskpart\fP\|(8). 
The particular
table used is dependent on the size of the drive.
The ``a'' partition is the same size across all drives,
15884 sectors.  The ``b'' partition, used for paging and
swapping, is sized according to the total space on the disk.
For drives less than about 400 megabytes the partition
is 33440 sectors, while for larger drives the partition size
is doubled to 66880 sectors.  The ``c'' partition is always
used to access the entire physical disk, including the space
at the back of the disk reserved for the bad sector
forwarding table.  If the disk is larger than about 250 megabytes,
an ``h'' partition is created with size 291346 sectors, and
no matter whether the ``h'' partition is created or not, the
remainder of the drive is allocated to the ``g'' partition.
Sites which want to split up the ``g'' partition into a number
of smaller file systems may use the ``d'', ``e'', and ``f''
partitions which overlap the ``g'' partition.  The default
sizes for these partitions are 15884, 55936, and the remainder
of the disk, respectively*.
.FS
* These rules are, unfortunately not evenly applied to all
disks.  Drives on DEC UDA50 and IDC controllers do not
completely follow these rules;
in particular, the swap partition on an RA81 is only 33440 sectors,
and no ``d'', ``e'', or ``f'' partitions are available on an RA60
or RA80.  Consult \fIuda\fP\|(4) for more information.
.FE
.NH 3
Space available
.PP
The space available on a disk varies per device.  The amount of space
available on the common disk partitions is listed in the following table.
Not shown in the table are the partitions of each drive devoted
to the root file system and the paging area.
.DS
.TS
center;
l l n l n.
Type	Name	Size	Name	Size
_
rk07	hk?g	13 Mb
rm03	hp?g	41 Mb
rp06	hp?g	145 Mb
rm05	hp?g	80 Mb	hp?h	145 Mb
rm80	hp?g	96 Mb
ra60	ra?g	41 Mb	ra?h	139 Mb
ra80	ra?g	41 Mb	ra?h	56 Mb
ra81	ra?g	41 Mb	ra?h	380 Mb
rb80	rb?g	41 Mb	rb?h	56 Mb
rp07	hp?g	315 Mb	hp?h	145 Mb
up300	up?g	80 Mb	up?h	145 Mb
hp400	hp?g	216 Mb	hp?h	145 Mb
up160	up?g	106 Mb
.TE
.DE
.LP
Here up300 refers to either an AMPEX or CDC 300 Megabyte disk on a
UNIBUS disk controller, up160 refers to a FUJITSU 160 Megabyte disk
on the UNIBUS, and hp400 refers to a FUJITSU Eagle 400 Megabyte
disk on a MASBUS disk controller.
Consult the manual pages for the specific controllers for other
supported disks or other partitions.
.PP
Each disk also has a paging area, typically of 16 Megabytes, and
a root file sytem of 8 Megabytes.
The distributed system binaries occupy about 22 Megabytes
while the major sources occupy another 25 Megabytes.
This overflows dual RK07 and dual RL02 systems,
but fits easily on most other hardware configurations.
.PP
Be aware that the disks have their sizes
measured in disk sectors (512 bytes), while the UNIX file
system blocks are variable sized.  All user programs report
disk space in kilobytes and, where needed, disk sizes are always
specified in terms of
sectors.  The /etc/disktab file used in making file systems
specifies disk partition sizes in sectors; the default sector size
of 512 bytes may be overridden with the ``se'' attribute.
.NH 3
Layout considerations
.PP
There are several considerations in deciding how
to adjust the arrangement of things on your disks:
the most important is making sure there is adequate space
for what is required; secondarily, throughput should be maximized.
Paging space is an important parameter.
The system, as distributed, sizes the configured
paging areas each time the system is booted.  Further,
multiple paging areas of different size may be interleaved.
Drives smaller than 400 megabytes have swap partitions of 16 megabytes
while drives larger than 400 megabytes have 32 megabytes.  These
values may be changed to get more paging space by changing
the appropriate partition table in the disk driver.
.PP
Many common system programs (C, the editor, the assembler etc.)
create intermediate files in the /tmp directory,
so the file system where this is stored also should be made
large enough to accommodate
most high-water marks; if you have several disks, it makes
sense to mount this in a ``root'' (i.e. first partition)
file system on another disk.
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
File system parameters
.PP
Each file system is parameterized according to its block size,
fragment size, and the disk geometry characteristics of the
medium on which it resides.  Inaccurate specification of the disk
characteristics or haphazard choice of the file system parameters
can result in substantial throughput degradation or significant
waste of disk space.  As distributed,
file systems are configured according to the following table.
.DS
.TS
center;
l l l.
File system	Block size	Fragment size
_
/	8 Kbytes	1 Kbytes
usr	4 Kbytes	512 bytes
users	4 Kbytes	1 Kbytes
.TE
.DE
.PP
The root file system block size is
made large to optimize bandwidth to the associated
disk;  this is particularly important since the
/tmp directory is normally part of the root file.
The large block size is also
important as many of the most heavily used programs
are demand paged out of the /bin directory.  The
fragment size of 1 Kbytes is a ``nominal'' value to use
with a file system.  With a 1 Kbyte fragment size
disk space utilization is approximately the same
as with the earlier versions of the file system.
.PP
The usr file system uses a 4 Kbyte block size
with 512 byte fragment size in an effort to get
high performance while conserving the amount of
space wasted by a large fragment size.  Space compaction
has been deemed important here because the source code
for the system is normally placed on this file system.
.PP
The file systems for users have a 4 Kbyte block
size with 1 Kbyte fragment size.  These parameters
have been selected based on observations of the
performance of our user file systems.  The 4 Kbyte
block size provides adequate bandwidth while the
1 Kbyte fragment size provides acceptable space compaction
and disk fragmentation.
.PP
Other parameters may be chosen in constructing file
systems, but the factors involved in choosing a block
size and fragment size are many and interact in complex
ways.  Larger block sizes result in better
throughput to large files in the file system as
larger i/o requests will then be performed by the
system.  However,
consideration must be given to the average file sizes
found in the file system and the performance of the
internal system buffer cache.   The system
currently provides space in the inode for
12 direct block pointers, 1 single indirect block
pointer, and 1 double indirect block pointer.*
.FS
* A triple indirect block pointer is also reserved, but
not currently supported.
.FE
If a file uses only direct blocks, access time to
it will be optimized by maximizing the block size.
If a file spills over into an indirect block,
increasing the block size of the file system may
decrease the amount of space used
by eliminating the need to allocate an indirect block.
However, if the block size is increased and an indirect
block is still required, then more disk space will be
used by the file because indirect blocks are allocated
according to the block size of the file system. 
.PP
In selecting a fragment size for a file system, at least
two considerations should be given.  The major performance
tradeoffs observed are between an 8 Kbyte block file system
and a 4 Kbyte block file system.  Due to implementation
constraints, the block size / fragment size ratio can not
be greater than 8.  This means that an 8 Kbyte file system
will always have a fragment size of at least 1 Kbytes.  If
a file system is created with a 4 Kbyte block size and a
1 Kbyte fragment size, then upgraded to an 8 Kbyte block size
and 1 Kbyte fragment size, identical space compaction will be
observed.  However, if a file system has a 4 Kbyte block size
and 512 byte fragment size, converting it to an 8K/1K
file system will result in significantly more space being
used.  This implies that 4 Kbyte block file systems which
might be upgraded to 8 Kbyte blocks for higher performance should
use fragment sizes of at least 1 Kbytes to minimize the amount
of work required in conversion.
.PP
A second, more important, consideration when selecting the
fragment size for a file system is the level of fragmentation 
on the disk.  With a 512 byte fragment size, storage fragmentation
occurs much sooner, particularly with a busy file system running
near full capacity.  By comparison, the level of fragmentation in a 
1 Kbyte fragment file system is an order of magnitude less severe.  This
means that on file systems where many files are created and
deleted the 512 byte fragment size is more likely to result in apparent
space exhaustion due to fragmentation.  That is, when the file 
system is nearly full, file expansion which requires locating a
contiguous area of disk space is more likely to fail on a 512
byte file system than on a 1 Kbyte file system.  To minimize
fragmentation problems of this sort, a parameter in the super
block specifies a minimum acceptable free space threshhold.  When
normal users (i.e. anyone but the super-user) attempt to allocate
disk space and the free space threshold is exceeded, the user is
returned an error as if the file system were actually full.  This
parameter is nominally set to 10%; it may be changed by supplying
a parameter to \fInewfs\fP, or by patching the super block of an
existing file system.
.PP
In general, unless a file system is to be used
for a special purpose application (for example, storing
image processing data), we recommend using the default
values supplied.  
Remember that the current
implementation limits the block size to at most 8 Kbytes
and the ratio of block size / fragment size must be in
the range 1-8.
.PP
The disk geometry information used by the file system
affects the block layout policies employed.  The file
/etc/disktab, as supplied, contains the data for most
all drives supported by the system.  When constructing
a file system you should use the \fInewfs\fP\|(8) program
and specify the type of disk on which the file system
resides.  This file also contains the default
file system partition
sizes, and default block and fragment sizes.  To
override any of the default values you can modify the file
or use one of the options to \fInewfs\fP.
.NH 3
Implementing a layout
.PP
To put a chosen disk layout into effect, you should use the
.IR newfs (8)
command to create each new file system.
Each file system must also be added to the file
/etc/fstab
so that it will be checked and mounted when the system is bootstrapped.
.PP
As an example, consider a system with rm03's.  On the first rm03, hp0,
we will put the root file system in hp0a, and the /usr
file system in hp0g, which has enough space to hold it and then some.
The /tmp directory will be part of the root file system,
as no file system will be mounted on /tmp.
If we had only one rm03, we would put user files
in the hp0g partition with the system source and binaries.
.PP
If we had a second rm03, we would create a file system in hp1g
and put user files there, calling the file system /mnt.
We would also interleave the paging
between the 2 rm03's.  To do this we would build a system configuration
that specified:
.DS
config	vmunix	root on hp0 swap on hp0 and hp1
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
\fB#\fP MAKEDEV hp1
\fB#\fP newfs hp1g rm03
(information about file system prints out)
\fB#\fP mkdir /mnt
\fB#\fP mount /dev/hp1g /mnt
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
Lines on dh or dmf interfaces are conventionally named \fBttyh\fPX, where X
is a hexadecimal digit.  If more than one dh or dmf interface is present
in a configuration, successive terminals would be named
\fBttyi\fPX, \fBttyj\fPX, etc.
.PP
To add a new terminal, be sure the device is configured into the system
and that the special file for the device has been made by /dev/MAKEDEV.
Then, set the first character of the appropriate line of /etc/ttys to 1
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
if it was wired to run at 9600 baud.  The definition
of each ``terminal type'' is located in the file /etc/gettytab
and read by the 
.I getty
program.  To make custom terminal types, consult 
.IR gettytab (5)
before modifying this file.
.PP
Dialup terminals should be wired so that carrier is asserted only when the
phone line is dialed up.
For non-dialup terminals from which modem control
is not available, you must either wire back the signals so that
the carrier appears to always be present, or show in the system
configuration that carrier is to be assumed to be present.  See
.IR dh (4),
.IR dz (4),
and
.IR dmf (4)
for details.
.PP
You should also edit the file
/etc/ttytype
placing the type of each new terminal there (see \fIttytype\fP\|(5)).
.PP
When the system is running multi-user, all terminals that are listed
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
from a 0 to a 1 and sending a hangup signal to \fIinit\fP.
.PP
Note that several programs, /usr/src/etc/init.c and
/usr/src/etc/comsat.c in particular, will have to be
recompiled if there are to be more than 100 terminals.
Also note that if a special file is inaccessible when \fIinit\fP tries
to create a process for it, init will print a message on the console
and try to reopen the terminal every minute, reprinting the warning
message every 10 minutes.
.PP
Finally note that you should change the names of any dialup
terminals to ttyd?
where ? is in [0-9a-f], as some programs use this property of the
names to determine if a terminal is a dialup.
Shell commands to do this should be put in the /dev/MAKEDEV.local
script.
.PP
While it is possible to use truly arbitrary strings for terminal names,
the accounting and noticeably the
\fIps\fP\|(1)
command make good use of the convention that tty names
(by default, and also after dialups are named as suggested above)
are distinct in the last 2 characters. 
Change this and you may be sorry later, as the heuristic
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
to share software in the same groups.
.PP
A number of guest accounts have been provided on the distribution
system; these accounts are for people at Berkeley, DEC and at Bell Laboratories
who have done major work on UNIX in the past.  You can delete these accounts,
or leave them on the system if you expect that these people would have
occasion to login as guests on your system.
.NH 2
Site tailoring
.PP
All programs which require the site's name, or some similar
characteristic, obtain the information through system calls
or from files located in /etc.  Aside from parts of the
system related to the network, to tailor the system to your
site you must simply select a site name, then edit the file
.DS
/etc/rc.local
.DE
The first line in /etc/rc.local,
.DS
/bin/hostname \fImysitename\fP
.DE
defines the value returned by the 
.IR gethostname (2)
system call.  Programs such as
.IR getty (8),
.IR mail (1),
.IR wall (1),
.IR uucp (1),
and
.IR who (1)
use this system call so that the binary images are site
independent.
.NH 2
Setting up the line printer system
.PP
The line printer system consists of at least
the following files and commands:
.DS
.TS
l l.
/usr/ucb/lpq	spooling queue examination program
/usr/ucb/lprm	program to delete jobs from a queue
/usr/ucb/lpr	program to enter a job in a printer queue
/etc/printcap	printer configuration and capability data base
/usr/lib/lpd	line printer daemon, scans spooling queues
/etc/lpc	line printer control program
.TE
.DE
.PP
The file /etc/printcap is a master data base describing line
printers directly attached to a machine and, also, printers
accessible across a network.  The manual page
.IR printcap (5)
describes the format of this data base and also
indicates the default values for such things as the directory
in which spooling is performed.  The line printer system handles
multiple printers, multiple spooling queues, local and remote
printers, and also printers attached via serial lines which require
line initialization such as the baud rate.  Raster output devices
such as a Varian or Versatec, and laser printers such as an Imagen,
are also supported by the line printer system.
.PP
Remote spooling via the network is handled with two spooling
queues, one on the local machine and one on the remote machine.
When a remote printer job is initiated with
.IR lpr ,
the job is
queued locally and a daemon process created to oversee the
transfer of the job to the remote machine.  If the destination
machine is unreachable, the job will remain queued until it is
possible to transfer the files to the spooling queue on the
remote machine.  The
.I lpq 
program shows the contents of spool
queues on both the local and remote machines.
.PP
To configure your line printers, consult the printcap manual page
and the accompanying document, ``4.2BSD Line Printer Spooler Manual''.
A call to the
.I lpd
program should be present in /etc/rc.
.NH 2
Setting up the mail system
.PP
The mail system consists of the following commands:
.DS
.TS
l l.
/bin/mail	old standard mail program (from 32/V)
/usr/ucb/mail	UCB mail program, described in mail(1)
/usr/lib/sendmail	mail routing program
/usr/spool/mail	mail spooling directory
/usr/spool/secretmail	secure mail directory
/usr/bin/xsend	secure mail sender
/usr/bin/xget	secure mail receiver
/usr/lib/aliases	mail forwarding information
/usr/ucb/newaliases	command to rebuild binary forwarding database
/usr/ucb/biff	mail notification enabler
/etc/comsat	mail notification daemon
/etc/syslog	error message logger, used by sendmail
.TE
.DE
Mail is normally sent and received using the
.IR mail (1)
command, which provides a front-end to edit the messages sent
and received, and passes the messages to
.IR sendmail (8)
for routing.
The routing algorithm uses knowledge of the network name syntax,
aliasing and forwarding information, and network topology, as
defined in the configuration file /usr/lib/sendmail.cf, to
process each piece of mail.
Local mail is delivered by giving it to the program /usr/bin/mail
which adds it to the mailboxes in the directory /usr/spool/mail/\fIusername\fP,
using a locking protocol to avoid problems with simultaneous updates.
After the mail is delivered, the local mail delivery daemon /etc/comsat
is notified, which in turn notifies
users who have issued a ``biff y'' command that mail has arrived.
.PP
Mail queued in the directory /usr/spool/mail is normally readable
only by the recipient.  To send mail which is secure against any possible
perusal (except by a code-breaker) you should
use the secret mail facility,
which encrypts the mail so that no one can read it.
.PP
To setup the mail facility you should read the instructions in the
file READ_ME in the directory /usr/src/usr.lib/sendmail and then adjust
the necessary configuration files.
You should also set up the file /usr/lib/aliases for your installation,
creating mail groups as appropriate.  Documents describing 
.IR sendmail 's
operation and installation are also included in the distribution.
.NH 3
Setting up a uucp connection
.PP
The version of \fIuucp\fP included in 4.2BSD is an
enhanced version of that originally distributed with 32/V*.
.FS
* The \fIuucp\fP included in this distribution is the result
of work by many people; we gratefully acknowledge their
contributions, but refrain from mentioning names in the 
interest of keeping this document current.
.FE
The enhancements include:
.IP \(bu 3
support for many auto call units
other than the DEC DN11,
.IP \(bu 3
breakup of the spooling area into
multiple subdirectories,
.IP \(bu 3
addition of an \fIL.cmds\fP file to control the set
of commands which may be executed by a remote site,
.IP \(bu 3
enhanced ``expect-send'' sequence capabilities when
logging in to a remote site,
.IP \(bu 3
new commands to be used in polling sites and
obtaining snap shots of \fIuucp\fP activity.
.LP
This section gives a brief overview of \fIuucp\fP and points
out the most important steps in its installation.
.PP
To connect two UNIX machines with a \fIuucp\fP network link using modems,
one site must have an automatic call unit and the other must have a
dialup port.  It is better if both sites have both.
.PP
You should first read the paper in volume 2B of the Unix Programmers Manual:
``Uucp Implementation Description''.  It describes in detail the file
formats and conventions, and will give you a little context.
In addition, the document setup.tblms, located in the directory
/usr/src/usr.bin/uucp/UUAIDS, may be of use in tailoring the
software to your needs.
.PP
The \fIuucp\fP support is located in three major directories: /usr/bin,
/usr/lib/uucp, and /usr/spool/uucp.  User commands are kept in /usr/bin,
operational commands in /usr/lib/uucp, and /usr/spool/uucp is used as
a spooling area.  The commands in /usr/bin are:
.DS
.TS
l l.
/usr/bin/uucp	file-copy command
/usr/bin/uux	remote execution command
/usr/bin/uusend	binary file transfer using mail
/usr/bin/uuencode	binary file encoder (for \fIuusend\fP)
/usr/bin/uudecode	binary file decoder (for \fIuusend\fP)
/usr/bin/uulog	scans session log files
/usr/bin/uusnap	gives a snap-shot of \fIuucp\fP activity
/usr/bin/uupoll	polls remote system until an answer is received
.TE
.DE
The important files and commands in /usr/lib/uucp are:
.DS
.TS
l l.
/usr/lib/uucp/L-devices	list of dialers and hardwired lines
/usr/lib/uucp/L-dialcodes	dialcode abbreviations
/usr/lib/uucp/L.cmds	commands remote sites may execute
/usr/lib/uucp/L.sys	systems to communicate with, how to connect, and when
/usr/lib/uucp/SEQF	sequence numbering control file
/usr/lib/uucp/USERFILE	remote site pathname access specifications
/usr/lib/uucp/uuclean	cleans up garbage files in spool area
/usr/lib/uucp/uucico	\fIuucp\fP protocol daemon
/usr/lib/uucp/uuxqt	\fIuucp\fP remote execution server
.TE
.DE
while the spooling area contains the following important
files and directories:
.DS
.TS
l l.
/usr/spool/uucp/C.	directory for command, ``C.'' files
/usr/spool/uucp/D.	directory for data, ``D.'', files
/usr/spool/uucp/X.	directory for command execution, ``X.'', files
/usr/spool/uucp/D.\fImachine\fP	directory for local ``D.'' files
/usr/spool/uucp/D.\fImachine\fPX	directory for local ``X.'' files
/usr/spool/uucp/TM.	directory for temporary, ``TM.'', files
/usr/spool/uucp/LOGFILE	log file of \fIuucp\fP activity
/usr/spool/uucp/SYSLOG	log file of \fIuucp\fP file transfers
.TE
.DE
.PP
To install \fIuucp\fP on your system, start by
selecting a site name (less than
8 characters).  
A \fIuucp\fP account must be created in
the password file and a password set up.
Then, create the appropriate
spooling directories with mode
755 and owned by user \fIuucp\fP, group \fIdaemon\fP.
.PP
If you have an auto-call unit, the L.sys, L-dialcodes, and
L-devices files should be created.  The L.sys file should
contain the phone numbers and login sequences required to
establish a connection with a \fIuucp\fP daemon on another
machine.  For example, our L.sys file looks something like:
.DS
adiron Any ACU 1200 out0123456789- ogin-EOT-ogin uucp
cbosg Never Slave 300
cbosgd Never Slave 300
chico Never Slave 1200 out2010123456
.DE
The first field is the name of a site, the second indicates when
the machine may be called, the third field specifies how the
host is connected (through an ACU, a hardwired line, etc.),
then comes the phone number to use in connecting through an
auto-call unit, and finally a login sequence.  The phone number
may contain common abbreviations which are defined in
the L-dialcodes file.  The device specification should refer to
devices specified in the L-devices file.  Indicating only ACU
causes the \fIuucp\fP daemon, \fIuucico\fP, to search for any
available auto-call unit in L-devices.  Our L-dialcodes file
is of the form:
.DS
ucb 2
out 9%
.DE
while our L-devices file is:
.DS
ACU cul0 unused 1200 ventel
.DE
Refer to the README file in the \fIuucp\fP source directory
for more information about installation.
.PP
As \fIuucp\fP operates it creates (and removes) many small
files in the directories underneath /usr/spool/uucp.  Sometimes
files are left undeleted; these are most easily purged with
the \fIuuclean\fP program.  The log files can grow without 
bound unless trimmed back; \fIuulog\fP is used to maintain
these files.  Many useful aids in maintaining your \fIuucp\fP
installation are included in a subdirectory UUAIDS beneath
/usr/src/usr.bin/uucp.  Peruse this directory and read the
``setup'' instructions also located there.
