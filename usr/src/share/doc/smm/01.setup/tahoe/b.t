.\" Copyright (c) 1988 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" Redistribution and use in source and binary forms are permitted
.\" provided that the above copyright notice and this paragraph are
.\" duplicated in all such forms and that any documentation,
.\" advertising materials, and other materials related to such
.\" distribution and use acknowledge that the software was developed
.\" by the University of California, Berkeley.  The name of the
.\" University may not be used to endorse or promote products derived
.\" from this software without specific prior written permission.
.\" THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
.\" IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
.\" WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
.\"
.\"	@(#)b.t	1.4 (Berkeley) 3/7/89
.\"
.de IR
\fI\\$1\fP\|\\$2
..
.ds LH "Installing/Operating \*(4B
.nr H1 6
.nr H2 0
.ds RH "Appendix B \- installation troubleshooting
.ds CF \*(DY
.bp
.LG
.B
.ce
APPENDIX B \- INSTALLATION TROUBLESHOOTING
.sp 2
.R
.NL
.PP
This appendix lists and explains certain problems
that might be encountered while trying to install the \*(4B
distribution.  The information provided here is
limited to the early steps in the installation process;
i.e. up to the point where the root file system is installed.
If you have a problem installing
the release consult this section before contacting our group.
.SH
\fBBooting the generic system\fP
.LP
\fIcopy does not copy 205 records\fP
.LP
If a tape read error occurred,
clean your tape drive heads.  If a disk write error occurred,
the disk formatting may have failed.  If the disk pack is
removable, try another one.  If you are currently running
UNIX, you can reboot your old system and use \fIdd\fP to
copy the mini-root file system into a disk partition
(assuming the destination is not in use by the running
system).
.LP
\fIboot prints ``not a directory''\fP
.LP
The
.I boot
program was unable to find the requested program because
it encountered something other than a directory while
searching the file system.
This usually suggests that
no file system is present on the disk partition supplied,
or the file system has been corrupted.  First check to
make sure you typed the correct line to boot.  If this
is the case and you are booting from the mini-root file
system, the mini-root was probably not copied correctly off the
tape (perhaps it was not placed in the correct disk partition).
Try reinstalling the mini-root file system or, if trying
to boot the true root file system, try booting from the
mini-root file system and run \fIfsck\fP on the restored
root file system to insure its integrity.  Finally, as
a last resort, copy the \fIboot\fP program from the 
mini-root file system to the newly installed root file system.
.LP
\fIboot prints ``bad format''\fP
.LP
The program you requested 
.I boot
to load did not have a valid magic number in its header.  This should
never happen on a distribution system.  If you were trying to boot off
the root file system, reboot the system on the mini-root file system
and look at the program on the root file system.  Try copying the copy
of vmunix on the mini-root to the root file system also.
.LP
\fIboot prints ``read short''\fP
.LP
The file header for the program contained a size larger
than the actual size of the file located on disk.  This
is probably the result of file system corruption (or a 
disk I/O error).  Try booting again or creating a new
copy of the program to be loaded (see above).
.LP
This section contains common problems encountered when booting
the generic version of the system.
.LP
\fIsystem panics with ``panic: iinit''\fP
.LP
This occurred because the system was unable to mount
the root file system.
The root file system supplied at the ``root device?''
prompt was probably incorrect.  Remember that when running on the
mini-root file system, this question must be answered
with something of the form \fIxx#\fP*, where \fIxx\fP is one of the
disk types listed in section 1.4, and \fI#\fP is the unit number.
If the answer had been ``dk0'', the system would have used the ``a''
partition on unit 0 of the ``dk'' drive, where presumably no file
system exists.
.LP
Alternatively, the file system on which you were trying
to run is corrupted.  Try
reinstalling the appropriate file system.
.LP
\fIsystem crashes during autoconfiguration\fP
.LP
This is almost always caused by an
unsupported  device being present at a
location where a supported device was expected.
You must disable the device in some way, either
by pulling it off the bus, or by moving the location
of the console status register (consult Appendix A
for a complete list of VERSAbus CSR's used in the generic system).
