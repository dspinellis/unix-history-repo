.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)sys.tty.t	1.7 (Berkeley) 4/11/86
.\"
.NH 2
Changes in the terminal line disciplines
.XP tty.c
The kernel maintains the terminal or window size in the tty structure
and provides \fIioctl\fP\^s to set and get these values.
The window size is cleared on final close.
The sizes include rows and columns in characters
and may include X and Y dimensions in pixels where that is meaningful.
The kernel makes no use of these values, but they are stored here
to provide a consistent way to determine the current size.
When a new value is set, a SIGWINCH signal is sent to the process group
associated with the terminal.
.XP
The notions of line discipline exit and final close have been separated.
\fITtyclose\fP is used only at final close, while \fIttylclose\fP
is provided for closing down a discipline.
Modem control transitions are handled more cleanly
by moving the common code from the terminal hardware drivers
into the line disciplines; the \fIl_modem\fP entry in the \fIlinesw\fP
is now used for this purpose.
\fITtymodem\fP handles carrier transitions for the standard disciplines;
\fInullmodem\fP is provided for disciplines with minimal requirements.
.XP
A new mode, LPASS8, was added to support 8-bit input in normal modes;
it is the input analog of LLITOUT.
An entry point, \fIcheckoutq\fP, has been added to enable internal
output operations (\fIuprintf\fP, \fItprintf\fP) to check for output overflow
and optionally to block to wait for space.
Certain operations are handled more carefully than before:
the use of the TIOCSTI \fIioctl\fP requires read permission on the terminal,
and SPGRP is disallowed if the group corresponds with another user's
process.
\fITtread\fP and \fIttwrite\fP both check for carrier drop when
restarting after a sleep.
An off-by-one consistency check of \fIuio_iovcnt\fP
in \fIttwrite\fP was corrected.
A bug was fixed that caused data to be flushed
when opening a terminal that was already open when using the ``old''
line discipline.
\fISelect\fP now returns true for reading if carrier has been lost.
While changing line disciplines, interrupts must be disabled until
the change is complete or is backed out.
If changing to the same discipline, the close and reopen (and probable
data flush) are avoided.
The \fIt_delct\fP field in the tty structure was not used and has been deleted.
.XP tty_conf.c
The line discipline close entries that used \fIttyclose\fP
now use \fIttylclose\fP.
The two tablet disciplines have been combined.
A new entry was added for a Serial-Line link-layer encapsulation for the
Internet Protocol, SLIPDISC.
.XP tty_pty.c
Large sections of the pseudo-tty driver have been reworked to improve
performance and to avoid
races when one side closed, which subsequently hung pseudo-terminals.
The line-discipline modem control routine is called to clean up
when the master closes.
Problems with REMOTE mode and non-blocking I/O were fixed by using the 
raw queue
rather than the cannonicalized queue.
A new mode was added to allow a small set of commands to be passed
to the pty master from the slave as a rudimentary type of \fIioctl\fP,
in a manner analogous to that of PKT mode.
Using this mode or PKT mode, a \fIselect\fP for exceptional
conditions on the master
side of a pty returns true when a command operation is available to be read.
\fISelect\fP for writing on the master side has been corrected,
and now uses the same criteria as \fIptcwrite\fP.
As the pty driver depends on normal operation of the tty queues,
it no longer permits changes to non-tty line disciplines.
.XP tty_subr.c
The \fIclist\fP support routines have been modified to use block moves
instead of \fIgetc/putc\fP wherever possible.
.XP tty_tablet.c
The two line disciplines have been merged and a number of new
tablet types are supported.
Tablet type and operating mode are now set by \fIioctl\fP\^s.
Tablets that continuously stream data are now told to stop
sending on last close.
