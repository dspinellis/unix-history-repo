.SH
Terminal Emulation
.PP
The current terminal emulator for X (\fIxterm\fP) is a client application,
in principle similar to any other application.
In practice, \fIxterm\fP is
probably the most complex and least graceful part of the package.
Pseudo teletypes (hereafter called pty's) are used to
implement this in 4.2BSD.
As currently implemented, ptys consist of a device driver which presents
a terminal on one side and a master controlling device on the other side.
Data is looped back from one side to the other, with full terminal
processing occurring (tab expansion, cooked/raw mode, etc.)
.PP
These present a number of problems:
1) pty's are a limited resource.  
Typical systems have 16 or 32 ptys.
On a single user machine,
this limit is seldom reached,
but on a timesharing machine it can be inconvenient.
2) Since they appear statically in the file system, protection
on the tty/pty pairs can be a problem.
A previous process that terminates unexpectedly can leave the pty
in an incorrect state.
\fIXterm\fP is the only application that must  run set user id to root 
to guarantee it can make the
tty/pty pair properly accessible
and to set ownership on the slave to the user.
.PP
The net result is that \fIxterm\fP is the most 
.UX
dependent
(and least likely to
port between 
.UX
implementations) of any of the X clients currently existing.
Dennis Richie's [3] stream mechanism appears to eliminate
most of these problems by allowing stacking of terminal processing on
IPC.
