.SH
Window System Initialization
.PP
Most displays capable of running  a window system bear
little resemblance to 
.UX \'s 
model of a terminal connected by
a serial line.
Current display hardware may require involved initialization before it
is usable as a terminal, and may have an interface that looks nothing like
the conventional view of a serial device.
As soon as the window system is running, however, it is easy to
provide a terminal emulator to a user.
.PP
Unix currently realizes someone has logged out
by the eventual termination of the process started by \fIinit\fP(8).
\fIInit\fP is also the only process which can detect when an orphan process
terminates,
so the restart of a terminal line (or window system)
after logout can only be performed by
\fIinit\fP.
.PP
The solution taken to support X (or Andrew, which has a similar structure)
was to generalize \fIinit\fP.
\fIGetty\fP(8) or (in X's case, \fIxterm\fP) now opens and revokes access to
a terminal or pty rather than \fIinit\fP.
The format of the /etc/ttys file, already extended at Berkeley, was further
extended to allow the specification of an arbitrary command to be
run as \fIgetty\fP.
For X, this would normally be the terminal emulator.
\fIInit\fP will also restart an optional window system server
process associated with the pty.
\fIInit\fP must start this process,
since \fIinit\fP is the only process
in 
.UX
that can detect its exit.
The initial \fIxterm\fP can not be started from a window system server,
since the server must exist all the time, and \fIinit\fP has to know the
process id in order for it to detect the \fIlogin\fP process has terminated.
The X server process itself opens the display device and performs
whatever initialization may be required (for example, the Vs100 requires
loading with firmware stored in a file).
.PP
Once \fIxterm\fP starts execution, it exec's \fIgetty\fP on the slave side of the
pty, and a user can log in normally.
When the user's shell exits, \fIxterm\fP exits, and \fIinit\fP can then
detect the user has logged out normally.
.PP
\fIInit\fP can now be used to guarantee that a process will be kept running
despite failures as long as the system is multi-user.
Another approach not seriously examined would have made it possible for
an orphan process to have a parent other than \fIinit\fP.

