.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)5.t	6.4 (Berkeley) 4/14/86
.\"
.SH
.LG
.ce
Section 5
.SM
.sp
.PP
A new subdirectory, \fI/usr/include/protocols\fP, has been created to
keep header files that are shared between user programs and daemons.
Several header files have been moved here, including those for \fIrwhod\fP,
\fIrouted\fP, \fItimed\fP, \fIdump\fP, \fItalk\fP, and \fIrestore\fP.
.PP
Two new header files, <\fIstring.h\fP> and <\fImemory.h\fP>, have
been added for System V compatibility.
.BP disktab
Two new fields have been added to specify that the disk supports
\fIbad144\fP-style bad sector forwarding,
and that offsets should be specified by sectors rather than cylinders.
.BP dump
The header file <\fIdumprestor.h\fP> has moved
to <\fIprotocols/dumprestore.h\fP>.
.BP gettytab
New entries have been added, including a 2400 baud
dial-in rotation for modems, a 19200 baud
standard line, and an entry for the \fIxterm\fP terminal emulator
of the \fIX\fP window system.  New capabilities for automatic speed
selection and setting strict xoff/xon flow control (\fBdecctlq\fP) were added.
.BP termcap
Many new entries were added and older entries fixed.
.BP ttys
The format of the ttys file, \fI/etc/ttys\fP,
reflects the merger of information previously kept in \fI/etc/ttys\fP,
\fI/etc/securetty\fP, and \fI/etc/ttytype\fP.
The new format permits arbitrary programs, not just \fI/etc/getty\fP,
to be spawned by \fIinit\fP.
A special \fBwindow\fP field can be used to
set up a window server before spawning a terminal emulator program.
