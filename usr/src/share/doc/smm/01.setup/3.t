.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)3.t	5.1 (Berkeley) %G%
.\"
.ds lq ``
.ds rq ''
.ds LH "Installing/Operating 4.2BSD
.ds RH "Upgrading a 4BSD system
.ds CF \*(DY
.LP
.nr H1 3
.nr H2 0
.bp
.LG
.B
.ce
3. UPGRADING A 4BSD SYSTEM
.sp 2
.R
.NL
.PP
Begin by reading the other parts of this document to see what
has changed since the last time you bootstrapped the system.
Also read the ``Changes in 4.2BSD'' document, and look
at the new manual sections provided to you.
If you have local system modifications to the
kernel to install, look at the
document ``Kernel changes in 4.2BSD'' to get an idea of how
the system changes will affect your local mods.
.PP
If you are running a version of the system distributed
prior to 4.0BSD, you are pretty much on your own.  Sites running
3BSD or 32/V may be able to modify the restor program to understand
the old 512 byte block file system, but this has never been
tried.  This section assumes you are running 4.1BSD. 
.NH 2
Step 1: what to save
.PP
No matter what version of the system you may be running, you will
have to rebuild your root and usr file systems.  The easist way
to do this is to save the important files on your existing system,
perform a bootstrap as if you were installing 4.2BSD on a brand new
machine, then merge the saved files into the new system.  The following
list enumerates the standard set of files you will want to save and
indicates directories in which site specific files should be present.
This list will likely be augmented with non-standard files you
have added to your system;  be sure to do a tar of the
directories /etc, /lib, and /usr/lib to guard against your missing
something the first time around.
.DS
.TS
l l.
/.profile	root sh startup script
/.login	root csh startup script
/.cshrc	root csh startup script
/dev/MAKE	for the LOCAL case for making devices
/etc/fstab	disk configuration data
/etc/group	group data base
/etc/passwd	user data base
/etc/rc	for any local additions
/etc/ttys	terminal line configuration data
/etc/ttytype	terminal line to terminal type mapping data
/etc/termcap	for any local entries which may have been added
/lib	for any locally developed language processors
/usr/dict/*	for local additions to words and papers
/usr/include/*	for local additions
/usr/lib/aliases	mail forwarding data base
/usr/lib/crontab	cron daemon data base
/usr/lib/font/*	for locally developed font libraries
/usr/lib/lint/*	for locally developed lint libraries
/usr/lib/tabset/*	for locally developed tab setting files
/usr/lib/term/*	for locally developed nroff drive tables
/usr/lib/tmac/*	for locally developed troff/nroff macros
/usr/lib/uucp/*	for local uucp configuration files
/usr/man/manl	for manual pages for locally developed programs
/usr/msgs	for current msgs
/usr/spool/*	for current mail, news, uucp files, etc.
/usr/src/local	for source for locally developed programs
.TE
.DE
As 4.1BSD binary images will run unchanged under 4.2BSD
you should be certain to save any programs such as
compilers which you will need in
bootstrapping to 4.2BSD.*
.FS
* 4.2BSD can support a ``4.1BSD compatibility mode'' of system operation
whereby system calls from 4.1BSD are either emulated or safely ignored.
There are only two exceptions; programs which read directories or use
the old jobs library will not operate properly.  However, while 4.1BSD
binaries will execute under 4.2BSD
it is \fBSTRONGLY RECOMMENDED\fP that the programs be recompiled under
the new system.  Refer to the document ``Changes in 4.2BSD'' for elaboration
on this point.
.FE
.PP
Once you have saved the appropriate files in a convenient format,
the next step is to dump your file systems with \fIdump\fP\|(8).
For the utmost of safety this should be done to magtape.  However,
if you enjoy gambling with your life (or you have a VERY friendly
user community) and you have sufficient disk space, you can try
converting your file systems in-place
by using a disk partition.  If you select the latter tact,
a version of the 4.1BSD dump program which runs under 4.2 is
provided in /etc/dump.4.1;  be sure to
read through this entire document before beginning the conversion.
Beware that file systems created under 4.2BSD will
use about 5-10% more disk space for file system related information
than under 4.1BSD.  Thus, before dumping each file system it is
a good idea to remove any files which may be easily regenerated.
Since most all programs will likely be recompiled under the new
system your best bet is to remove any object files.  File
systems with at least 10% free space on them should restore into
an equivalently sized 4.2BSD file system without problem.
.PP
Once you have dumped the file systems you wish to convert to 4.2BSD,
install the system from the bootstrap tape as described in chapter 2,
then proceed to the next section.
.NH 2
Step 2: merging
.PP
When your system is booting reliably and you have the 4.2BSD
root and /usr file systems fully installed you will be ready
to proceed to the next
step in the conversion process:
merging your old files into the new system.
.PP
Using the tar tape, or tapes, you created in step 1 extract
the appropriate files into a scratch directory, say /usr/convert:
.DS
\fB#\fP mkdir /usr/convert
\fB#\fP cd /usr/convert
\fB#\fP tar x
.DE
.PP
Certain data files, such as those from the /etc directory,
may simply be copied into place.
.DS
\fB#\fP cp passwd group fstab ttys ttytype /etc
\fB#\fP cp crontab /usr/lib
.DE
Other files, however, must be merged into the distributed
versions by hand.  In particular, be careful with /etc/termcap.
.PP
The commands kept under the LOCAL entry in
/dev/MAKE should be placed in the new shell script /dev/MAKEDEV.local
so that saying ``MAKEDEV LOCAL'' will create the appropriate
local devices and device names.  If you have any homegrown device
drivers which use major device numbers reserved by the system you
will have to modify the commands used to create the devices or alter
the system device configuration tables in /sys/vax/conf.c.
.PP
The spooling directories saved on tape may be restored in their
eventual resting places without too much concern.  Be sure to
use the `p' option to tar so that files are recreated with the
same file modes:
.DS
\fB#\fP cd /usr
\fB#\fP tar xp msgs spool/mail spool/uucp spool/uucppublic spool/news
.DE
.PP
Whatever else is left is likely to be site specific or require
careful scrutiny before placing in its eventual resting place.
Refer to the documentation and source code 
before arbitrarily overwriting a file.
.NH 2
Step 3: converting file systems
.PP
The dump format used in 4.0 and 4.1BSD is upward
compatible with that
used in 4.2BSD.  That is, the 4.2BSD
.I restore
program understands
how to read old dump tapes, although 4.2BSD dump tapes may not
be properly restored under 4.0BSD or 4.1BSD.  To convert a file system
dumped to magtape, simply create the appropriate file system
and restore the data.  Note that the 4.2BSD
.I restore
program does
its work on a mounted file system using normal system operations
(unlike the older
.I restor
which accessed the raw file
system device and deposited inodes in the appropriate locations
on disk).  This means that file system dumps may be restored even
if the characteristics of the file system changed.  To restore
a dump tape for, say, the /a file system something like the following
would be used:
.DS
\fB#\fP mkdir /a
\fB#\fP newfs hp1g eagle
\fB#\fP mount /dev/hp1g /a
\fB#\fP cd /a
\fB#\fP restore r
.DE
If tar images were written instead of doing a dump, you should
be sure to use the `p' option when reading the files back.
No matter how you restore a file system, be sure and check its
integrity with fsck when the job is complete.
.NH 2
Bootstrapping language processors
.PP
To convert a compiler from 4.1BSD
to 4.2BSD you should simply have to recompile and relink the
various parts.  If the processor is written in itself, for instance
a PASCAL compiler written in PASCAL, the important step in
converting is to save a working copy of the 4.1BSD binary before
converting to 4.2BSD.  Then, once the system has been changed over,
the 4.1BSD binary should be used in the rebuilding process. 
In order to do this, you should enable the 4.1 compatibility
option when you configure the kernel (below).
.PP
If no working 4.1BSD binary exists, or the language processor
uses some nonstandard system call, you will likely have to compile
the language processor into an intermediate form, such as assembly
language, on a 4.1BSD system, then bring the intermediate form
to 4.2BSD for assembly and loading.
