.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)2.t	6.1 (Berkeley) 5/14/86
.\"
.ds lq ``
.ds rq ''
.ds LH "Installing/Operating \*(4B
.ds RH Bootstrapping
.ds CF \*(DY
.bp
.nr H1 2
.nr H2 0
.bp
.LG
.B
.ce
2. BOOTSTRAP PROCEDURE
.sp 2
.R
.NL
.PP
This section explains the bootstrap procedure that can be used
to get the kernel supplied with this distribution running on your machine.
If you are not currently running 4.2BSD you will
have to do a full bootstrap.
Chapter 3 describes how to upgrade an existing 4.2BSD system.
An understanding of the operations used in a full bootstrap
is very helpful in performing an upgrade as well.
In either case, it is highly desirable to read and understand
the remainder of this document before proceeding.
.NH 2
Booting from tape
.PP
The tape bootstrap procedure used to create a
working system involves the following major
steps:
.IP 1)
Format a disk pack with the \fIvdformat\fP program.
.IP 2)
Copy a ``mini root'' file system from the
tape onto the swap area of the disk.
.IP 3)
Boot the UNIX system on the ``mini root''.
.IP 4)
Restore the full root file system using \fIrestore\fP\|(8).
.IP 5)
Reboot the completed root file system.
.IP 6)
Label the disks with the \fIdisklabel\fP\|(8) program.
.IP 7)
Build and restore the /usr file system from tape
with \fItar\fP\|(1).
.IP 8)
Extract the system and utility files and contributed software
as desired.
.PP
The following sections describe the above steps in detail.
In these sections references to disk drives are of the
form \fIxx\fP\|(\fIn\fP,\fIm\fP)
and references to files on tape drives are of the form
\fIyy\fP\|(\fIn\fP,\fIm\fP) where \fIxx\fP and \fIyy\fP
are names described in section 1.4 and \fIn\fP
and \fIm\fP are the unit and offset numbers described in
section 1.4.  Commands
you are expected to type are shown in Roman, while that
information printed by the system is shown emboldened.
.PP
If you encounter problems while following the instructions in
this part of the document, refer to Appendix B for help in
troubleshooting.
.NH 3
Step 1: formatting the disk
.PP
All disks used with \*(4B should be formatted to insure
the proper handling of physically corrupted disk sectors.
The
.I vdformat
program included in the distribution, or a vendor supplied
formatting program, may be used to format disks if this has not
already been done.
The
.I vdformat
program is capable of formatting
any of the disk drives listed in \(sc1.1.
.PP
To load the \fIvdformat\fP program, perform the following steps.
.DS
.TS
lw(2i) l.
(machine powered up)
\fBMIB POC\fP
\fBType '#' to cancel boot\fP
#	(cancel automatic reboot)
\fBCP [a10.h0]#>\fP\|h	(halt the cpu)
\fB#>\|\fPy	(initialize the machine)
\fB#>\|\fPfd cyp(0,0)	(make cypher default device)
\fB#>\|\fPp23 3. \fB00000000\fP	(set boot flags)
\fB#>\|\fPfb	(boot machine)
\fBcyp(0,0)/etc/fstab\fP
\fBCP cold boot\fP
\fB4 way interleave set\fP
\fBCPU memory test\fP
\fBECC CPU memory test\fP
\fBcyp(0,0)/.\fP
\fBCPU POC1\fP
\fBcyp(0,0)/poc1\fP
\fBCPU POC2\fP
\fBcyp(0,0)/poc2\fP
\fBFPP POC\fR	(only if floating point processor present)
\fBcyp(0,0)/fppoc\fP
\fBFPP WCS\fR	(only if floating point processor present)
\fBcyp(0,0)/fppwcs\fP
\fBBOOT SYSTEM cyp(0,0)/boot\fP

\fBBoot\fP
\fB:\fRcy(0,0)stand/vdformat	(load and run from first tape file)
.TE
.DE
.PP
The \fIvdformat\fP program should now be running and awaiting your input:
.DS
\fB:\fPcy(0,0)stand/vdformat
\fB50176+14336+776780 start 0x1000\fP
\fBVDFORMAT	Version 3.0\fP

\fBcontroller 0: smd\fP
\fBcontroller 1: smd-e\fP

\fBType `Help' for help, `Start' to execute operations.\fP

\fBvdformat>\fP
.DE
If you made a mistake loading the program off the tape
you should get either the ``:'' prompt again from the
boot program or the ``#>'' prompt from the console
processor.  In either case you can retype the appropriate
command to try again.
If something else happened, you may have a bad distribution
tape, or your hardware may be broken; refer to
Appendix B for help in troubleshooting.
.PP
\fIVdformat\fP will create sector headers and verify
the integrity of each sector formatted.  
The program starts up by identifying the disk controllers
installed in the machine.  Old VDDC controllers which 
support only SMD drives are indicated
as ``smd'' while newer controllers capable of supporting both
SMD and extended-SMD drives are tagged as ``smd-e''. 
Remember \fIvdformat\fP works only with the drives listed above.
\fIVdformat\fP
will prompt for the information required as shown below.
If you err in answering questions,
``Delete'' erases the last character typed, and ``^U'' erases
the current input line.  At any point you can ask for
assistance by typing ``help''; \fIvdformat\fP will list
the possible answers to the current question.
.DS
\fBvdformat>\fP\|format
  \fBFormat on which controllers?\fP\|1
    \fBDrives on controller 1?\fP\|0
      \fBNumber of patterns to use while verifying?\fP\|1
      \fBDrive type for controller 1, drive 0?\fP\|egl
        \fBModule serial number for controller 1, drive 0?\fP\|1
\fBvdformat>\fP\|list
  \fBThe following operations will occur when Start is issued:\fP
    \fBFormat: Controller 1, drive 0, type EGL.\fP
\fBvdformat>\fP\|start
\fBStarting format on controller 1, drive 0, type EGL.\fP
(\fIbad sectors will be indicated\fP)
\fBvdformat>\fP
.DE
Once the root device has been formatted, \fIvdformat\fP
will prompt for another command.
Return to the bootstrap by typing
.DS
\fBvdformat>\fP\|exit
.DE
or halt the machine by
typing ``~h''.
.DS
\fBvdformat>\fP ~h
\fB#>\|\fP
.DE
.PP
It may be necessary to format other drives before constructing
file systems on them; this can be done at a later time with the
steps just performed, or \fIvdformat\fP may be brought in
off a disk drive as described in \(sc6.1.
.NH 3
Step 2: copying the mini-root file system
.PP
The second step is to run a simple program,
\fIcopy\fP, which copies a small root
file system into the second partition of the disk.
This file system will serve as the base for creating the actual root
file system to be restored.  The version of the operating
system maintained on the ``mini-root'' file system understands
that it should not swap on top of itself, thereby allowing double use
of the disk partition.
Disk 0 is normally used for this operation when doing an initial
boot.
Another disk may be substituted if necessary,
although several modifications to the procedure may be needed
to create special files for the alternate disk.
The actual disk number should be substituted for the \fIx\fP below.
\fICopy\fP is loaded just as the
\fIvdformat\fP program was loaded;
if you don't have the bootstrap running,
repeat the above instructions until you see the
prompt from Boot (a colon), and then:
.DS
.TS
lw(2i) l.
(copy mini root file system)
\fB:\|\fPcy(0,0)copy	(load and run copy program)
\fBFrom:\fP cy(0,1)	(unit 0, second tape file)
\fBTo:\fP dk(\fIx\fP,1)	(mini root is on drive \fIx\fP; second partition)
\fBCopy completed: 205 records copied\fP
\fBBoot\fP
\fB:\fP
.TE
(As above, `delete' erases characters and `^U' erases lines.)
.DE
.NH 3
Step 3: booting from the mini-root file system
.PP
You now have the minimal set of tools necessary to create a
root file system and restore the file system contents from tape.
To access this file system load the bootstrap program
and boot the version of unix that has been placed in the
``mini-root.''
As before, load the bootstrap if you do not already have
it running.  At the colon prompt:
.DS
.TS
lw(2i) l.
\fB: \fPdk(\fIx\fP,1)vmunix	(bring in \fIvmunix\fP off mini root)
.TE
.DE
The standalone boot program should then read the system from
the mini root file system you just created, and the system should boot:
.DS
.B
271944+78848+92812 start 0x12e8
4.3 BSD UNIX #1: Wed Apr  9 23:33:59 PST 1985
    sam@okeeffe.berkeley.edu:/usr/src/sys/GENERIC
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
identifying the release and
version of the system that is in use and the date that it was compiled.  
.PP
Next the
.I mem
messages give the
amount of real (physical) memory and the
memory available to user programs
in bytes.
For example, if your machine has only 512K bytes of memory, then
\fIxxx\fP will be 520192, 4096 bytes less than 512K.
The system reserves the last 4096 bytes of memory for use in
error logging and doesn't count it as part of real memory.
.PP
The messages that come out next show what devices were found on
the current processor.  These messages are described in
\fIautoconf\fP\|(4).
The distributed system may not have
found all the communications devices you have (vioc's),
or all the mass storage peripherals you have, especially
if you have more than
two of anything.  You will correct this soon, when you create
a description of your machine from which to configure UNIX.
The messages printed at boot here contain much of the information
that will be used in creating the configuration.
In a correctly configured system most of the information
present in the configuration description
is printed out at boot time as the system verifies that each device
is present.
.PP
The \*(lqroot device?\*(rq prompt was printed by the system 
and is now asking you for the name of the root file system to use.
This happens because the distribution system is a \fIgeneric\fP
system, i.e. it can be bootstrapped on a Tahoe cpu with its root device
and paging area on any available disk drive.  You should respond
to the root device question with \fIxx\fP0*.  This response
supplies two pieces of information:
first, \fIxx\fP0 shows that the disk it is running on is drive
0 of type \fIxx\fP, secondly the \*(lq*\*(rq shows that the system is
running \*(lqatop\*(rq the paging area.  The latter is most important,
otherwise the system will attempt to page on top of itself and
chaos will ensue.
You will later build a system tailored to your configuration that
will not ask this question when it is bootstrapped.
.DS
\fBroot device?\fP \fIxx\fP0*
WARNING: preposterous time in file system \-\- CHECK AND RESET THE DATE!
\fBerase ^?, kill ^U, intr ^C\fP
\fB#\fP
.DE
.PP
The \*(lqerase ...\*(rq message is part of the /.profile
that was executed by the root shell when it started.  This message
is present to inform you as to what values the character erase,
line erase, and interrupt characters have been set.
.NH 3
Step 4: restoring the root file system
.PP
UNIX is now running,
and the `UNIX Programmer's manual' applies.
The `#' is the prompt from the shell,
and lets you know that you are the super-user,
whose login name is \*(lqroot\*(rq.  To complete installation
of the bootstrap system one step remains:  the root
file system must be created.
.PP
If the root file system is to reside on a disk other than
unit 0 (as the information printed out
during autoconfiguration shows), you will
have to create the necessary special files in /dev and use
the appropriate value. For example, if the root should be
placed on dk1, you must create /dev/rdk1a and /dev/dk1a using 
\fImknod\fP(8) or the MAKEDEV script in /dev.
.PP
To create the root file system the shell script \*(lqxtr\*(rq
should be run:
.DS
\fB#\|\fPdisk=dk\fIx\fP tape=cy xtr
.DE
This will generate many messages regarding the construction
of the file system and the restoration of the tape contents,
but should eventually stop with the messages:
.DS
 ...
\fBRoot filesystem extracted\fP
\fB#\fP
.DE
.NH 3
Step 5: rebooting the completed root file system
.PP
With the above work completed, all that is left is to reboot:
.DS
.ta 3.5i
\fB#\|\fPsync	(synchronize file system state)
\fB#\|\fP~h	(halt cpu)
\fB#>\|\fPy	(initialize machine)
\fB#>\|\fPp23 2	(set boot flags)
\fB#>\|\fPfr boot
\fI\&...(boot program is eventually loaded)...\fP
\fBBoot\fP
\fB:\fP dk(\fIx\fP,0)vmunix	(\fIvmunix\fP brought in off root)
\fB271944+78848+92812 start 0x12e8
\fB4.3 BSD UNIX #1: Wed Apr  9 23:33:59 PST 1985
\fB    karels@okeeffe.berkeley.edu:/usr/src/sys/GENERIC
\fBreal mem  = \fIxxx\fR
\fBavail mem = \fIyyy\fR
\fI\&... information about available devices ...\fP
\fBroot on xx0\fP
WARNING: preposterous time in file system \-\- CHECK AND RESET THE DATE!
\fBerase ^?, kill ^U, intr ^C\fP
\fB#\fP
.DE
.PP
If the root device selected by the kernel is not correct,
it is necessary to reboot again using the option to ask for the root
device.  On the Tahoe use ``p23 3''.
At the prompt from the bootstrap, use the same device specification
above: dk(\fIx\fP,0)vmunix.
Then, to the question ``root device?,''
respond with dk0.
See section 6.1 and appendix C if the system does not reboot properly.
.PP
The system is now running single user on the installed
root file system.  The next section tells how to complete
the installation of distributed software on the /usr file system.
.NH 3
Step 6: placing labels on the disks
.PP
4.3BSD uses disk labels in the first sector of each disk to contain
information about the geometry of the drive and the partition layout.
This information is written with \fIdisklabel\fP\|(8).
Note that recent CCI releases, and apparently Harris releases,
may use a different form of disk label, also in the first sector.
As the formats of these labels are incompatible,
skip this step if your machine is using disk labels already.
Recent firmware for the console processor (CP) may use these labels,
and thus the labels must be retained.
Eventually, it will be possible to use both formats simultaneously.
.PP
For each disk that you wish to label, run the following command:
.DS
\fB#\|\fPdisklabel -w dk\fIx\fP \fItype\fP "\fIoptional_pack_name\fP"
.DE
The \fItype\fP is the CCI disk device name as listed in section 1.3,
or any other name listed in /etc/disktab.
The optional information may contain any descriptive name for the
contents of a disk, and may be up to 16 characters long.
This procedure will place the label on the disk using the information
found in /etc/disktab for the disk type named.
The default disk partitions in \*(4B are the mostly
the same as those in the CCI 1.21 release,
except for CDC 340Mb xfd drives;
see section 4.3.2 for details.
If you have changed the disk partition sizes,
you may add entries for the modified configuration in /etc/disktab
before labeling the affected disks.
.NH 3
Step 7: setting up the /usr file system
.PP
The next thing to do is to extract the rest of the data from
the tape.
You might wish to review the disk configuration information in section 4.4
before continuing; the partitions used below are those most appropriate
in size.
.PP
For the Cypher tape drive, execute the following commands:
.DS
\fB#\fP cd /dev; MAKEDEV cy0; sync
.DE
Then perform the following:
.br
.ne 5
.sp
.DS
.TS
lw(2i) l.
\fB#\fP date \fIyymmddhhmm\fP	(set date, see \fIdate\fP\|(1))
\&....
\fB#\fP passwd root	(set password for super-user)
\fBNew password:\fP	(password will not echo)
\fBRetype new password:\fP
\fB#\fP hostname \fImysitename\fP	(set your hostname)
\fB#\fP newfs dk0c	(create empty user file system)
(this takes a few minutes)
\fB#\fP mount /dev/dk0c /usr	(mount the usr file system)
\fB#\fP cd /usr	(make /usr the current directory)
\fB#\fP mt fsf
\fB#\fP tar xpbf 20 /dev/rmt12 	(extract all of usr except usr/src)
(this takes about 15-20 minutes)
.TE
.DE
If the tape had been rewound or positioned incorrectly before the \fItar\fP,
it may be repositioned by the following commands.
.DS
\fB#\fP mt rew
\fB#\fP mt fsf 3
.DE
The data on the fourth tape file has now been extracted.
If you are using 1600bpi tapes,
the first reel of the distribution is no longer needed;
the remainder of the installation procedure uses the second
reel of tape that should be mounted in place of the first.
The first instruction below should be ignored if using 1600bpi tapes.
The installation procedure continues from this point on the 6250bpi tape.
.DS
.TS
lw(2i) l.
\fB#\fP mt fsf		(do not do on 1600bpi tapes)
\fB#\fP mkdir src	(make directory for source)
\fB#\fP mkdir src/sys	(make directory for system source)
\fB#\fP cd src/sys	(make /usr/sys the current directory)
\fB#\fP tar xpbf 20 /dev/rmt12 	(extract the system source)
(this takes about 5-10 minutes)
\fB#\fP cd /	(back to root)
\fB#\fP chmod 755  /  /usr  /usr/src /usr/src/sys
\fB#\fP rm \-f sys
\fB#\fP ln \-s usr/src/sys sys	(make a symbolic link to the system source)
\fB#\fP umount /dev/dk0c	(unmount /usr)
.TE
.DE
.PP
You can check the consistency of the /usr file system by doing
.DS
\fB#\fP fsck /dev/rdk0c
.DE
The output from
.I fsck
should look something like:
.DS
.B
** /dev/rdk0c
** Last Mounted on /usr
** Phase 1 - Check Blocks and Sizes
** Phase 2 - Check Pathnames
** Phase 3 - Check Connectivity
** Phase 4 - Check Reference Counts
** Phase 5 - Check Cyl groups
671 files, 3497 used, 137067 free (75 frags, 34248 blocks)
.R
.DE
.PP
If there are inconsistencies in the file system, you may be prompted
to apply corrective action; see the document describing
.I fsck
for information.
.PP
To use the /usr file system, you should now remount it by
saying
.DS
\fB#\fP /etc/mount /dev/dk0c /usr
.DE
You can then extract the source code for the commands:
.DS
\fB#\fP cd /usr/src
\fB#\fP mt fsf
\fB#\fP tar xpb 20
.DE
If you get an error at this point, most likely it was
a problem with tape positioning.
You can reposition the tape by rewinding it and
then skipping over the files already read (see \fImt\fP\|(1)).
.NH 3
Additional software
.PP
There are three extra tape files on the distribution tape(s)
which have not been installed to this point.  They are
a font library for use with Varian and Versatec printers,
the Ingres database system, and user contributed software.
All three tape files are in \fItar\fP\|(1) format and
can be installed by positioning the tape 
using \fImt\fP\|(1) and reading
in the files as was done for /usr/src above.  As distributed,
the fonts should be placed in a directory /usr/lib/vfont, the
Ingres system should be placed in /usr/ingres, and the user
contributed software should be placed in /usr/src/new.  The
exact contents of the user contributed software is given in
a separate document.
.NH 2
Additional conversion information
.PP
After setting up the new \*(4B filesystems,
you may restore the user files that were saved on tape before beginning
the conversion.
Note that the \*(4B \fIrestore\fP program does
its work on a mounted file system using normal system operations
(unlike the older \fIrestor\fP that accessed the raw file
system device and deposited inodes in the appropriate locations
on disk).  This means that file system dumps may be restored even
if the characteristics of the file system changed.  To restore
a dump tape for, say, the /a file system something like the following
would be used:
.DS
\fB#\fP mkdir /a
\fB#\fP newfs dk1c eagle
\fB#\fP mount /dev/dk1c /a
\fB#\fP cd /a
\fB#\fP restore x
.DE
.PP
If \fItar\fP images were written instead of doing a dump, you should
be sure to use the `p' option when reading the files back.
No matter how you restore a file system, be sure and check its
integrity with \fIfsck\fP when the job is complete.
