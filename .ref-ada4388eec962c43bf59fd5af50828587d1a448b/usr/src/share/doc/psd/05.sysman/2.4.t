.\" Copyright (c) 1983, 1993, 1994
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)2.4.t	8.5 (Berkeley) %G%
.\"
.Sh 2 "Terminals and Devices
.Sh 3 "Terminals
.PP
Terminals support
.Fn read
and
.Fn write
I/O operations,
as well as a collection of terminal specific
.Fn ioctl
operations,
to control input character interpretation and editing,
and output format and delays.
.PP
A terminal may be used as a controlling terminal (login terminal)
for a login session.
A controlling terminal is associated with a session (see section
.Xr 1.1.4 ).
A controlling terminal has a foreground process group, which must be
a member of the session with which the terminal is associated (see section
.Xr 1.1.5 ).
Members of the foreground process group are allowed to read from and write to
the terminal and change the terminal settings; other process groups from
the session may be stopped upon attempts to do these operations.
.PP
A session leader allocates a terminal
as the controlling terminal for its session using the ioctl
.DS
ioctl(fd, TIOCSCTTY, NULL);
int fd;
.DE
Only a session leader may acquire a controlling terminal.
.ne 1i
.Sh 4 "Terminal input
.PP
Terminals are handled according to the underlying communication
characteristics such as baud rate and required delays,
and a set of software parameters.
These parameters are described in the \fItermios\fP structure
maintained by the kernel for each terminal line:
.DS
.TS
l s s s
l l l l.
struct termios {
	tcflag_t	c_iflag;	/* input flags */
	tcflag_t	c_oflag;	/* output flags */
	tcflag_t	c_cflag;	/* control flags */
	tcflag_t	c_lflag;	/* local flags */
	cc_t	c_cc[NCCS];	/* control chars */
	long	c_ispeed;	/* input speed */
	long	c_ospeed;	/* output speed */
};
.TE
.DE
The \fItermios\fP structure is set and retrieved using the
.Fn tcsetattr
and
.Fn tcgetattr
functions.
.PP
Two general kinds of input processing are available, determined by
whether the terminal device file is in canonical mode or noncanonical
mode. Additionally, input characters are processed according to the
\fIc_iflag\fP and \fIc_lflag\fP fields.
Such processing can include echoing, which
in general means transmitting input characters immediately back to the
terminal when they are received from the terminal.
Non-graphic ASCII input characters may be echoed as a two-character
printable representation, ``^character.''
.PP
In canonical mode input processing,
terminal input is processed in units of lines.
A line is delimited by a newline character (NL),
an end-of-file (EOF) character, or an end-of-line (EOL) character.
Input is presented on a line-by-line basis.
Using this mode means that a read request will not return
until an entire line has been typed,
or a signal has been received.
Also, no matter how many bytes are requested
in the read call, at most one line is returned.
It is not, however, necessary to read a whole line at once;
any number of bytes, even one, may
be requested in a read without losing information.
.PP
When the terminal is in canonical mode, editing of an input line
is performed.  Editing facilities allow deletion of the previous
character or word, or deletion of the current input line. 
In addition,
a special character may be used to reprint the current input line.
Certain other characters are also interpreted specially.
Flow control is provided by the \fIstop output\fP
and \fIstart output\fP control characters.
Output may be flushed with the \fIflush output\fP character;
and the \fIliteral character\fP may be used to force the following
character into the input line, regardless of any special meaning
it may have.
.PP
In noncanonical mode input processing, input bytes are not assembled into
lines, and erase and kill processing does not occur.
All input is passed through to the
reading process immediately and without interpretation.
Signals and flow control may be enabled; here
the handler interprets input only by looking
for characters that cause interrupts or output flow control;
all other characters are made available.
.PP
When interrupt characters are being interpreted
by the terminal handler they
cause a software interrupt to be sent to all processes in the process
group associated with the terminal.
Interrupt characters exist to send SIGINT
and SIGQUIT signals,
and to stop a process group
with the SIGTSTP signal either immediately, or when
all input up to the stop character has been read.
.Sh 4 "Terminal output
.PP
On output, the terminal handler provides some simple formatting services.
These include converting the carriage return character to the
two character return-linefeed sequence,
inserting delays after certain standard control characters,
and expanding tabs.
.pl +1
.Sh 3 "Structured devices
.PP
Structured devices are typified by disks and magnetic
tapes, but may represent any random-access device.
The system performs read-modify-write type buffering actions on block
devices to allow them to be read and written in random access
fashion like ordinary files.
Filesystems are normally mounted on block devices.
.pl -1
.Sh 3 "Unstructured devices
.PP
Unstructured devices are those devices which
do not support block structure.  Familiar unstructured devices
are raw communications lines (with
no terminal handler), raster plotters, magnetic tape and disks unfettered
by buffering and permitting large block input/output and positioning
and formatting commands.
