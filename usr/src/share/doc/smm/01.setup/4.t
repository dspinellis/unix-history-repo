.\" Copyright (c) 1980, 1986, 1988 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)4.t	6.10 (Berkeley) %G%
.\"
.ds LH "Installing/Operating \*(4B
.ds CF \*(Dy
.ds RH "System setup
.NH 1
System Setup
.PP
This section describes procedures used to set up a \*(4B UNIX system.
These procedures are used when a system is first installed
or when the system configuration changes.  Procedures for normal
system operation are described in the next section.
.NH 2
Kernel configuration
.PP
This section briefly describes the layout of the kernel code and
how files for devices are made.
For a full discussion of configuring
and building system images, consult the document ``Building
4.3BSD UNIX Systems with Config'' (SMM:2).
.NH 3
Kernel organization
.PP
As distributed, the kernel source is in a
separate tar image.  The source may be physically
located anywhere within any filesystem so long as
a symbolic link to the location is created for the file
.Pn /sys
(many files in
.Pn /usr/include
are normally symbolic links relative to
.Pn /sys ).
In further discussions of the system source all path names
will be given relative to
.Pn /sys .
.LP
The kernel is made up of several large generic parts:
.TS
l l l.
sys		main kernel header files
kern		kernel functions broken down as follows
	init	system startup, syscall dispatching, entry points
	kern	scheduling, descriptor handling and generic I/O
	sys	process management, signals
	tty	terminal handling and job control
	vfs	filesystem management
	uipc	interprocess communication (sockets)
	subr	miscellaneous support routines
vm		virtual memory management
ufs		local filesystems broken down as follows
	ufs	common local filesystem routines
	ffs	fast filesystem
	lfs	log-based filesystem
	mfs	memory based filesystem
nfs		Sun-compatible network filesystem
.TE
.LP
The networking code is organized by protocol
.TS
l l.
net	routing and generic interface drivers
netinet	Internet protocols (TCP, UDP, IP, etc)
netiso	ISO protocols (TP-4, CLNP, CLTP, etc)
netns	Xerox network systems protocols (IDP, SPP, etc)
netx25	CCITT X.25 protocols (X.25 Packet Level, HDLC/LAPB)
.TE
.LP
A separate subdirectory is provided for each machine architecture
.TS
l l.
hp300	HP 9000/300 series of Motorola 68000-based machines
i386	Intel 386/486-based PC machines
luna68k	Omron 68000-based workstations
news3400	Sony News MIPS-based workstations
pmax	Digital 3100/5000 MIPS-based workstations
sparc	Sun Microsystems SPARCstation 1, 1+, and 2
tahoe	(deprecated) CCI Power 6-series machines
vax	(deprecated) Digital VAX machines
.TE
.LP
Each machine directory is subdivided by function;
for example the hp300 directory contains
.TS
l l.
include	exported machine-dependent header files
hp300	machine-dependent support code and private header files
dev	device drivers
hpux	emulation for HP-UX system calls
conf	configuration files
stand	machine-dependent standalone code
.TE
.LP
Other kernel related directories
.TS
l l.
compile	area to compile kernels
conf	machine-independent configuration files
stand	machine-independent standalone code
.TE
.NH 3
Devices and device drivers
.PP
Devices supported by UNIX are implemented in the kernel
by drivers whose source is kept in
.Pn /sys/<architecture>/dev .
These drivers are loaded
into the system when included in a cpu specific configuration file
kept in the conf directory.  Devices are accessed through special
files in the filesystem, made by the
.Xr mknod (8)
program and normally kept in the
.Pn /dev
directory.
For all the devices supported by the distribution system, the
files in
.Pn /dev
are created by the
.Pn /dev/MAKEDEV
shell script.
.PP
Determine the set of devices that you have and create a new
.Pn /dev
directory by running the MAKEDEV script.
First create a new directory
.Pn /newdev ,
copy MAKEDEV into it, edit the file MAKEDEV.local
to provide an entry for local needs,
and run it to generate a
.Pn /newdev directory.
For instance,
.DS
\fB#\fP \fIcd /\fP
\fB#\fP \fImkdir newdev\fP
\fB#\fP \fIcp dev/MAKEDEV newdev/MAKEDEV\fP
\fB#\fP \fIcd newdev\fP
\fB#\fP \fIMAKEDEV \*(Dk0 pt0 std LOCAL\fP
.DE
Note the ``std'' argument causes standard devices such as
.Pn /dev/console ,
the machine console, to be created.
.PP
You can then do
.DS
\fB#\fP \fIcd /\fP
\fB#\fP \fImv dev olddev ; mv newdev dev\fP
\fB#\fP \fIsync\fP
.DE
to install the new device directory.
.NH 3
Building new system images
.PP
The kernel configuration of each UNIX system is described by
a single configuration file, stored in the
.Pn /sys/<architecture>/conf
directory.
To learn about the format of this file and the procedure used
to build system images,
start by reading ``Building 4.3BSD UNIX Systems with Config'' (SMM:2),
look at the manual pages in section 4
of the UNIX manual for the devices you have,
and look at the sample configuration files in the
.Pn /sys/<architecture>/conf
directory.
.PP
The configured system image
.Pn vmunix
should be copied to the root, and then booted to try it out.
It is best to name it
.Pn /newvmunix
so as not to destroy the working system until you are sure it does work:
.DS
\fB#\fP \fIcp vmunix /newvmunix\fP
\fB#\fP \fIsync\fP
.DE
It is also a good idea to keep the previous system around under some other
name.  In particular, we recommend that you save the generic distribution
version of the system permanently as
.Pn /genvmunix
for use in emergencies.
To boot the new version of the system you should follow the
bootstrap procedures outlined in section 6.1.
After having booted and tested the new system, it should be installed as
.Pn /vmunix
before going into multiuser operation.
A systematic scheme for numbering and saving old versions
of the system may be useful.
.NH 2
Disk configuration
.PP
This section describes how to layout filesystems to make use
of the available space and to balance disk load for better system
performance.
.NH 3
Disk naming and divisions
.PP
Each physical disk drive can be divided into up to 8 partitions;
UNIX typically uses only 3 or 4 partitions.
For instance, the first partition, \*(Dk0a,
is used for a root filesystem, a backup thereof,
or a small filesystem like,
.Pn /tmp ;
the second partition, \*(Dk0b,
is used for paging and swapping; and
the third partition, \*(Dk0\*(Pa,
holds a user filesystem.
.PP
The space available on a disk varies per device.
Each disk typically has a paging area of 30 to 100 megabytes
and a root filesystem of about 17 megabytes.
.\" XXX check
The distributed system binaries occupy about 150 (175 with X11R5) megabytes
.\" XXX check
while the major sources occupy another 250 (310 with X11R5) megabytes.
.PP
Be aware that the disks have their sizes
measured in disk sectors (usually 512 bytes), while the UNIX filesystem
blocks are variable sized.
If
.Sm BLOCKSIZE=1k
is set in the user's environment, all user programs report
disk space in kilobytes, otherwise,
disk sizes are always reported in units of 512-byte sectors\**.
.FS
You can thank System V intransigency and POSIX duplicity for
requiring that 512-byte blocks be the units that programs report.
.FE
The
.Pn /etc/disktab
file used in labelling disks and making filesystems
specifies disk partition sizes in sectors.
.NH 3
Layout considerations
.PP
There are several considerations in deciding how
to adjust the arrangement of things on your disks.
The most important is making sure that there is adequate space
for what is required; secondarily, throughput should be maximized.
Paging space is an important parameter.
The system, as distributed, sizes the configured
paging areas each time the system is booted.  Further,
multiple paging areas of different size may be interleaved.
.PP
Many common system programs (C, the editor, the assembler etc.)
create intermediate files in the
.Pn /tmp
directory, so the filesystem where this is stored also should be made
large enough to accommodate most high-water marks.
Typically,
.Pn /tmp
is constructed from a memory-based filesystem (see
.Xr mount_mfs (8)).
Programs that want their temporary files to persist
across system reboots (such as editors) should use
.Pn /var/tmp .
If you plan to use a disk-based
.Pn /tmp
filesystem to avoid loss across system reboots, it makes
sense to mount this in a ``root'' (i.e. first partition)
filesystem on another disk.
All the programs that create files in
.Pn /tmp
take care to delete them, but are not immune to rare events
and can leave dregs.
The directory should be examined every so often and the old
files deleted.
.PP
The efficiency with which UNIX is able to use the CPU
is often strongly affected by the configuration of disk controllers;
it is critical for good performance to balance disk load.
There are at least five components of the disk load that you can
divide between the available disks:
.IP 1)
The root filesystem.
.IP 2)
The
.Pn /tmp
and
.Pn /var/tmp
filesystems.
.IP 3)
The
.Pn /usr
filesystem.
.IP 4)
The user filesystems.
.IP 5)
The paging activity.
.LP
The following possibilities are ones we have used at times
when we had 2, 3 and 4 disks:
.TS
center doublebox;
l | c s s
l | lw(5) | lw(5) | lw(5).
	disks
what	2	3	4
_
root	0	0	0
tmp	1	2	3
usr	1	1	1
paging	0+1	0+2	0+2+3
users	0	0+2	0+2
archive	x	x	3
.TE
.PP
The most important things to consider are to
even out the disk load as much as possible, and to do this by
decoupling filesystems (on separate arms) between which heavy copying occurs.
Note that a long term average balanced load is not important; it is
much more important to have an instantaneously balanced
load when the system is busy.
.PP
Intelligent experimentation with a few filesystem arrangements can
pay off in much improved performance.  It is particularly easy to
move the root, the
.Pn /tmp
and
.Pn /var/tmp
filesystems and the paging areas.  Place the
user files and the
.Pn /usr
directory as space needs dictate and experiment
with the other, more easily moved filesystems.
.NH 3
Filesystem parameters
.PP
Each filesystem is parameterized according to its block size,
fragment size, and the disk geometry characteristics of the
medium on which it resides.  Inaccurate specification of the disk
characteristics or haphazard choice of the filesystem parameters
can result in substantial throughput degradation or significant
waste of disk space.  As distributed,
filesystems are configured according to the following table.
.DS
.TS
center;
l l l.
Filesystem	Block size	Fragment size
_
root	8 kbytes	1 kbytes
usr	8 kbytes	1 kbytes
users	4 kbytes	512 bytes
.TE
.DE
.PP
The root filesystem block size is
made large to optimize bandwidth to the associated disk.
The large block size is important as many of the most
heavily used programs are demand paged out of the
.Pn /bin
directory.
The fragment size of 1 kbyte is a ``nominal'' value to use
with a filesystem.  With a 1 kbyte fragment size
disk space utilization is about the same
as with the earlier versions of the filesystem.
.PP
The filesystems for users have a 4 kbyte block
size with 512 byte fragment size.  These parameters
have been selected based on observations of the
performance of our user filesystems.  The 4 kbyte
block size provides adequate bandwidth while the
512 byte fragment size provides acceptable space compaction
and disk fragmentation.
.PP
Other parameters may be chosen in constructing filesystems,
but the factors involved in choosing a block
size and fragment size are many and interact in complex
ways.  Larger block sizes result in better
throughput to large files in the filesystem as
larger I/O requests will then be performed by the
system.  However,
consideration must be given to the average file sizes
found in the filesystem and the performance of the
internal system buffer cache.   The system
currently provides space in the inode for
12 direct block pointers, 1 single indirect block
pointer, 1 double indirect block pointer,
and 1 double indirect block pointer.
If a file uses only direct blocks, access time to
it will be optimized by maximizing the block size.
If a file spills over into an indirect block,
increasing the block size of the filesystem may
decrease the amount of space used
by eliminating the need to allocate an indirect block.
However, if the block size is increased and an indirect
block is still required, then more disk space will be
used by the file because indirect blocks are allocated
according to the block size of the filesystem.
.PP
In selecting a fragment size for a filesystem, at least
two considerations should be given.  The major performance
tradeoffs observed are between an 8 kbyte block filesystem
and a 4 kbyte block filesystem.  Because of implementation
constraints, the block size versus fragment size ratio can not
be greater than 8.  This means that an 8 kbyte filesystem
will always have a fragment size of at least 1 kbytes.  If
a filesystem is created with a 4 kbyte block size and a
1 kbyte fragment size, then upgraded to an 8 kbyte block size
and 1 kbyte fragment size, identical space compaction will be
observed.  However, if a filesystem has a 4 kbyte block size
and 512 byte fragment size, converting it to an 8K/1K
filesystem will result in 4-8% more space being
used.  This implies that 4 kbyte block filesystems that
might be upgraded to 8 kbyte blocks for higher performance should
use fragment sizes of at least 1 kbytes to minimize the amount
of work required in conversion.
.PP
A second, more important, consideration when selecting the
fragment size for a filesystem is the level of fragmentation
on the disk.  With an 8:1 fragment to block ratio, storage fragmentation
occurs much sooner, particularly with a busy filesystem running
near full capacity.  By comparison, the level of fragmentation in a
4:1 fragment to block ratio filesystem is one tenth as severe.  This
means that on filesystems where many files are created and
deleted, the 512 byte fragment size is more likely to result in apparent
space exhaustion because of fragmentation.  That is, when the filesystem
is nearly full, file expansion that requires locating a
contiguous area of disk space is more likely to fail on a 512
byte filesystem than on a 1 kbyte filesystem.  To minimize
fragmentation problems of this sort, a parameter in the super
block specifies a minimum acceptable free space threshold.  When
normal users (i.e. anyone but the super-user) attempt to allocate
disk space and the free space threshold is exceeded, the user is
returned an error as if the filesystem were really full.  This
parameter is nominally set to 5%; it may be changed by supplying
a parameter to
.Xr newfs (8),
or by updating the super block of an existing filesystem using
.Xr tunefs (8).
.PP
In general, unless a filesystem is to be used
for a special purpose application (for example, storing
image processing data), we recommend using the
values supplied above.
Remember that the current
implementation limits the block size to at most 64 kbytes
and the ratio of block size versus fragment size must be 1, 2, 4, or 8.
.PP
The disk geometry information used by the filesystem
affects the block layout policies employed.  The file
.Pn /etc/disktab ,
as supplied, contains the data for most
all drives supported by the system.  Before constructing
a filesystem with
.Xr newfs (8)
you should label the disk (if it has not yet been labeled,
and the driver supports labels).
If labels cannot be used, you must instead
specify the type of disk on which the filesystem resides;
.Xr newfs
then reads
.Pn /etc/disktab
instead of the pack label.
This file also contains the default
filesystem partition
sizes, and default block and fragment sizes.  To
override any of the default values you can modify the file,
edit the disk label,
or use an option to
.Xr newfs .
.NH 3
Implementing a layout
.PP
To put a chosen disk layout into effect, you should use the
.Xr newfs (8)
command to create each new filesystem.
Each filesystem must also be added to the file
.Pn /etc/fstab
so that it will be checked and mounted when the system is bootstrapped.
.PP
As an example, consider a system with two disks.
On the first disk, \*(Dk0,
we will put the root filesystem in \*(Dk0a, and the
.Pn /usr
filesystem in \*(Dk0g, which has enough space to hold it and then some.
The
.Pn /tmp
directory will be a memory-based filesystem.
If we had only one disk we would put user files and
.Pn /var
in the \*(Dk0g partition with the system source and binaries.
.PP
If we had a second disk, we would place
.Pn /usr
in \*(Dk1g.
We would put user files in \*(Dk0g, calling the filesystem
.Pn /a .
We would put the
.Pn /var
filesystem in \*(Dk1a.
We would also interleave the paging
between the 2 disk's.
To do this we would build a system configuration that specified:
.DS
config	vmunix	root on \*(Dk0 swap on \*(Dk0 and \*(Dk1
.DE
to get the swap interleaved, and
.Pn /etc/fstab
would then contain
.TS
center;
l l l l n n.
/dev/\*(Dk0a	/	ufs	rw	1	1
/dev/\*(Dk0b	none	swap	sw	0	0
/dev/\*(Dk0b	/tmp	mfs	rw,-s=14000,-b=8192,-f=1024,-T=sd660	0	0
/dev/\*(Dk0g	/a	ufs	rw	1	2
/dev/\*(Dk1a	/var	ufs	rw	1	2
/dev/\*(Dk1b	none	swap	sw	0	0
/dev/\*(Dk1g	/usr	ufs	ro	1	2
.TE
We could set
.Pn /var
to be symbolic link into the user filesystem and
keep a backup copy of the root
filesystem in the \*(Dk1a disk partition.
.PP
To make the
.Pn /a
filesystem we would do:
.DS
\fB#\fP \fIcd /dev\fP
\fB#\fP \fIMAKEDEV \*(Dk1\fP
\fB#\fP \fIdisklabel -wr \*(Dk1 "disk type" "disk name"\fP
\fB#\fP \fInewfs \*(Dk1g\fP
(information about filesystem prints out)
\fB#\fP \fImkdir /a\fP
\fB#\fP \fImount /dev/\*(Dk1g /a\fP
.DE
.NH 2
Configuring terminals
.PP
If UNIX is to support simultaneous
access from directly-connected terminals other than the console,
the file
.Pn /etc/ttys
(see
.Xr ttys (5))
must be edited.
.PP
To add a new terminal device, be sure the device is configured into the system
and that the special files for the device have been made by
.Pn /dev/MAKEDEV .
Then, enable the appropriate lines of
.Pn /etc/ttys
by setting the ``status''
field to \fBon\fP (or add new lines).
Note that lines in
.Pn /etc/ttys
are one-for-one with entries in the file of current users
(see
.Pn /var/run/utmp ),
and therefore it is best to make changes
while running in single-user mode
and to add all of the entries for a new device at once.
.PP
Each line in the
.Pn /etc/ttys
file is broken into four tab separated
fields (comments are shown by a `#' character and extend to
the end of the line).  For each terminal line the four fields
are:
the device (without a leading
.Pn /dev ),
the program
.Pn /sbin/init
should startup to service the line
(or \fBnone\fP if the line is to be left alone),
the terminal type (found in
.Pn /usr/share/misc/termcap ),
and optional status information describing if the terminal is
enabled or not and if it is ``secure'' (i.e. the super user should
be allowed to login on the line).  All fields are character strings
with entries requiring embedded white space enclosed in double
quotes.
Thus a newly added terminal
.Pn /dev/tty00
could be added as
.DS
tty00 	"/usr/libexec/getty std.9600"	vt100	on secure	# mike's office
.DE
The std.9600 parameter provided to
.Pn /usr/libexec/getty
is used in searching the file
.Pn /etc/gettytab ;
it specifies a terminal's characteristics (such as baud rate).
To make custom terminal types, consult
.Xr gettytab (5)
before modifying
.Pn /etc/gettytab .
.PP
Dialup terminals should be wired so that carrier is asserted only when the
phone line is dialed up.
For non-dialup terminals, from which modem control is not available,
you must wire back the signals so that
the carrier appears to always be present.  For further details,
find your terminal driver in section 4 of the manual.
.PP
For network terminals (i.e. pseudo terminals), no program should
be started up on the lines.  Thus, the normal entry in
.Pn /etc/ttys
would look like
.DS
ttyp0 	none	network
.DE
(Note, the fourth field is not needed here.)
.PP
When the system is running multi-user, all terminals that are listed in
.Pn /etc/ttys
as \fBon\fP have their line enabled.
If, during normal operations, you wish
to disable a terminal line, you can edit the file
.Pn /etc/ttys
to change the terminal's status to \fBoff\fP and
then send a hangup signal to the
.Xr init
process, by doing
.DS
\fB#\fP \fIkill \-1 1\fP
.DE
Terminals can similarly be enabled by changing the status field
from \fBoff\fP to \fBon\fP and sending a hangup signal to
.Xr init .
.PP
Note that if a special file is inaccessible when
.Xr init
tries to create a process for it, init will log a message to the
system error logging process (see
.Xr syslogd (8))
and try to reopen the terminal every minute, reprinting the warning
message every 10 minutes.  Messages of this sort are normally
printed on the console, though other actions may occur depending
on the configuration information found in
.Pn /etc/syslog.conf .
.PP
Finally note that you should change the names of any dialup
terminals to ttyd?
where ? is in [0-9a-zA-Z], as some programs use this property of the
names to determine if a terminal is a dialup.
Shell commands to do this should be put in the
.Pn /dev/MAKEDEV.local
script.
.PP
While it is possible to use truly arbitrary strings for terminal names,
the accounting and noticeably the
.Xr ps (1)
command make good use of the convention that tty names
(by default, and also after dialups are named as suggested above)
are distinct in the last 2 characters.
Change this and you may be sorry later, as the heuristic
.Xr ps (1)
uses based on these conventions will then break down and
.Xr ps
will run MUCH slower.
.NH 2
Adding users
.PP
The procedure for adding a new user is described in
.Xr adduser (8).
You should add accounts for the initial user community, giving
each a directory and a password, and putting users who will wish
to share software in the same groups.
.PP
Several guest accounts have been provided on the distribution
system; these accounts are for people at Berkeley,
Bell Laboratories, and others
who have done major work on UNIX in the past.  You can delete these accounts,
or leave them on the system if you expect that these people would have
occasion to login as guests on your system.
.NH 2
Site tailoring
.PP
All programs that require the site's name, or some similar
characteristic, obtain the information through system calls
or from files located in
.Pn /etc .
Aside from parts of the
system related to the network, to tailor the system to your
site you must simply select a site name, then edit the file
.DS
/etc/netstart
.DE
The first lines in
.Pn /etc/netstart use a variable to set the hostname,
.DS
hostname=\fImysitename\fP
/bin/hostname $hostname
.DE
to define the value returned by the
.Xr gethostname (2)
system call.  If you are running the name server, your site
name should be your fully qualified domain name.  Programs such as
.Xr getty (8),
.Xr mail (1),
.Xr wall (1),
and
.Xr uucp (1)
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
/usr/bin/lpq	spooling queue examination program
/usr/bin/lprm	program to delete jobs from a queue
/usr/bin/lpr	program to enter a job in a printer queue
/etc/printcap	printer configuration and capability data base
/usr/sbin/lpd	line printer daemon, scans spooling queues
/usr/sbin/lpc	line printer control program
/etc/hosts.lpd	list of host allowed to use the printers
.TE
.DE
.PP
The file
.Pn /etc/printcap
is a master data base describing line
printers directly attached to a machine and, also, printers
accessible across a network.  The manual page
.Xr printcap (5)
describes the format of this data base and also
shows the default values for such things as the directory
in which spooling is performed.  The line printer system handles
multiple printers, multiple spooling queues, local and remote
printers, and also printers attached via serial lines that require
line initialization such as the baud rate.  Raster output devices
such as a Varian or Versatec, and laser printers such as an Imagen,
are also supported by the line printer system.
.PP
Remote spooling via the network is handled with two spooling
queues, one on the local machine and one on the remote machine.
When a remote printer job is started with
.Xr lpr ,
the job is queued locally and a daemon process created to oversee the
transfer of the job to the remote machine.  If the destination
machine is unreachable, the job will remain queued until it is
possible to transfer the files to the spooling queue on the
remote machine.  The
.Xr lpq
program shows the contents of spool
queues on both the local and remote machines.
.PP
To configure your line printers, consult the printcap manual page
and the accompanying document, ``4.3BSD Line Printer Spooler Manual'' (SMM:7).
A call to the
.Xr lpd
program should be present in
.Pn /etc/rc .
.NH 2
Setting up the mail system
.PP
The mail system consists of the following commands:
.DS
.TS
l l.
/usr/bin/mail	UCB mail program, described in \fImail\fP\|(1)
/usr/sbin/sendmail	mail routing program
/var/spool/mail	mail spooling directory
/var/spool/secretmail	secure mail directory
/usr/bin/xsend	secure mail sender
/usr/bin/xget	secure mail receiver
/etc/aliases	mail forwarding information
/usr/bin/newaliases	command to rebuild binary forwarding database
/usr/bin/biff	mail notification enabler
/usr/libexec/comsat	mail notification daemon
.TE
.DE
Mail is normally sent and received using the
.Xr mail (1)
command (found in
.Pn /usr/bin/mail ),
which provides a front-end to edit the messages sent
and received, and passes the messages to
.Xr sendmail (8)
for routing.
The routing algorithm uses knowledge of the network name syntax,
aliasing and forwarding information, and network topology, as
defined in the configuration file
.Pn /usr/lib/sendmail.cf ,
to process each piece of mail.
Local mail is delivered by giving it to the program
.Pn /usr/libexec/mail.local
that adds it to the mailboxes in the directory
.Pn /var/spool/mail/<username> ,
using a locking protocol to avoid problems with simultaneous updates.
After the mail is delivered, the local mail delivery daemon
.Pn /usr/libexec/comsat
is notified, which in turn notifies users who have issued a
``\fIbiff\fP y'' command that mail has arrived.
.PP
Mail queued in the directory
.Pn /var/spool/mail
is normally readable only by the recipient.
To send mail that is secure against perusal
(except by a code-breaker) you should use the secret mail facility,
which encrypts the mail.
.PP
To set up the mail facility you should read the instructions in the
file READ_ME in the directory
.Pn /usr/src/usr.sbin/sendmail
and then adjust the necessary configuration files.
You should also set up the file
.Pn /etc/aliases
for your installation, creating mail groups as appropriate.
For more informations see
``Sendmail Installation and Operation Guide'' (SMM:8) and
``Sendmail \- An Internetwork Mail Router'' (SMM:9).
.NH 3
Setting up a UUCP connection
.LP
The version of
.Xr uucp
included in \*(4B has the following features:
.IP \(bu 3
support for many auto call units and dialers
in addition to the DEC DN11,
.IP \(bu 3
breakup of the spooling area into multiple subdirectories,
.IP \(bu 3
addition of an
.Pn L.cmds
file to control the set
of commands that may be executed by a remote site,
.IP \(bu 3
enhanced ``expect-send'' sequence capabilities when
logging in to a remote site,
.IP \(bu 3
new commands to be used in polling sites and
obtaining snap shots of
.Xr uucp
activity,
.IP \(bu 3
additional protocols for different communication media.
.LP
This section gives a brief overview of
.Xr uucp
and points out the most important steps in its installation.
.PP
To connect two UNIX machines with a
.Xr uucp
network link using modems,
one site must have an automatic call unit
and the other must have a dialup port.
It is better if both sites have both.
.PP
You should first read the paper in the UNIX System Manager's Manual:
``Uucp Implementation Description'' (SMM:14).
It describes in detail the file formats and conventions,
and will give you a little context.
In addition,
the document ``setup.tblms'',
located in the directory
.Pn /usr/src/usr.bin/uucp/UUAIDS ,
may be of use in tailoring the software to your needs.
.PP
The
.Xr uucp
support is located in three major directories:
.Pn /usr/bin,
.Pn /usr/lib/uucp,
and
.Pn /var/spool/uucp .
User commands are kept in
.Pn /usr/bin,
operational commands in
.Pn /usr/lib/uucp ,
and
.Pn /var/spool/uucp
is used as a spooling area.
The commands in
.Pn /usr/bin
are:
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
/usr/bin/uuname	prints a list of known uucp hosts
/usr/bin/uuq	gives information about the queue
.TE
.DE
The important files and commands in
.Pn /usr/lib/uucp
are:
.DS
.TS
l l.
/usr/lib/uucp/L-devices	list of dialers and hard-wired lines
/usr/lib/uucp/L-dialcodes	dialcode abbreviations
/usr/lib/uucp/L.aliases	hostname aliases
/usr/lib/uucp/L.cmds	commands remote sites may execute
/usr/lib/uucp/L.sys	systems to communicate with, how to connect, and when
/usr/lib/uucp/SEQF	sequence numbering control file
/usr/lib/uucp/USERFILE	remote site pathname access specifications
/usr/lib/uucp/uucico	\fIuucp\fP protocol daemon
/usr/lib/uucp/uuclean	cleans up garbage files in spool area
/usr/lib/uucp/uuxqt	\fIuucp\fP remote execution server
.TE
.DE
while the spooling area contains the following important files and directories:
.DS
.TS
l l.
/var/spool/uucp/C.	directory for command, ``C.'' files
/var/spool/uucp/D.	directory for data, ``D.'', files
/var/spool/uucp/X.	directory for command execution, ``X.'', files
/var/spool/uucp/D.\fImachine\fP	directory for local ``D.'' files
/var/spool/uucp/D.\fImachine\fPX	directory for local ``X.'' files
/var/spool/uucp/TM.	directory for temporary, ``TM.'', files
/var/spool/uucp/LOGFILE	log file of \fIuucp\fP activity
/var/spool/uucp/SYSLOG	log file of \fIuucp\fP file transfers
.TE
.DE
.PP
To install
.Xr uucp
on your system,
start by selecting a site name
(shorter than 14 characters). 
A
.Xr uucp
account must be created in the password file and a password set up.
Then,
create the appropriate spooling directories with mode 755
and owned by user
.Xr uucp ,
group \fIdaemon\fP.
.PP
If you have an auto-call unit,
the L.sys, L-dialcodes, and L-devices files should be created.
The L.sys file should contain
the phone numbers and login sequences
required to establish a connection with a
.Xr uucp
daemon on another machine.
For example, our L.sys file looks something like:
.DS
adiron Any ACU 1200 out0123456789- ogin-EOT-ogin uucp
cbosg Never Slave 300
cbosgd Never Slave 300
chico Never Slave 1200 out2010123456
.DE
The first field is the name of a site,
the second shows when the machine may be called,
the third field specifies how the host is connected
(through an ACU, a hard-wired line, etc.),
then comes the phone number to use in connecting through an auto-call unit,
and finally a login sequence.
The phone number
may contain common abbreviations that are defined in the L-dialcodes file.
The device specification should refer to devices
specified in the L-devices file.
Listing only ACU causes the
.Xr uucp
daemon,
.Xr uucico ,
to search for any available auto-call unit in L-devices.
Our L-dialcodes file is of the form:
.DS
ucb 2
out 9%
.DE
while our L-devices file is:
.DS
ACU cul0 unused 1200 ventel
.DE
Refer to the README file in the
.Xr uucp
source directory for more information about installation.
.PP
As
.Xr uucp
operates it creates (and removes) many small
files in the directories underneath
.Pn /var/spool/uucp .
Sometimes files are left undeleted;
these are most easily purged with the
.Xr uuclean
program.
The log files can grow without bound unless trimmed back;
.Xr uulog
maintains these files.
Many useful aids in maintaining your
.Xr uucp
installation are included in a subdirectory UUAIDS beneath
.Pn /usr/src/usr.bin/uucp .
Peruse this directory and read the ``setup'' instructions also located there.
