.\" Copyright (c) 1986 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.proprietary.roff%
.\"
.\"	@(#)implement.ms	6.5 (Berkeley) %G%
.\"
.ds UU UUCP
.ds Uu U\s-2UCP\s+2
.ds uu \s-2UUCP\s+2
.TL
Installation and Operation of \*(UU
.br
4.3BSD Edition
.AU
D. A. Nowitz
.AI
AT&T Bell Laboratories
Murray Hill, New Jersey 07974
.AU
Carl S. Gutekunst
.AI
Communications Software Research and Development
Pyramid Technology Corporation
Mountain View, California 94039
.AB
\*(Uu is a series of programs designed to permit communication
between
.UX
systems using a variety of communications links.
\*(Uu provides batched, error free file transfers and remote command
execution.
It is well suited for tasks such as electronic mail, public news networks,
and software distribution, particularly when only slow,
low-cost communication links are available (e.g., 1200 baud dial-up).
.PP
This document describes the 4.3BSD version of \*(Uu.
This is a distant but direct descendent of the ``second implementation''
of \*(Uu developed by D. A. Nowitz at AT&T Bell Laboratories.
A number of other \*(Uu versions are in common usage; these are discussed
only to the extent that they affect administration of 4.3BSD systems.
.LP
Revised August 24, 1986
.AE
.LP
.OH 'Installation and Operation of UUCP''SMM:9-%'
.EH 'SMM:9-%''Installation and Operation of UUCP'
.ne 5
.NH
\*(UU OVERVIEW
.PP
\*(Uu is a batch-type operation.
Users issue commands that are queued in a spool directory for processing
by background daemons.
.LP
\fIUucp\fP (UNIX-to-UNIX Copy) and \fIuux\fP (UNIX-to-UNIX Execution)
provide the user interface to \*(Uu.
.I Uucp
has syntax and semantics similar to the standard
.UX
utility \fIcp\fP(1), with the added ability to prefix filenames with
system names.
Similarly, \fIuux\fP mimics the conventions of \fIsh\fP(1), and allows
commands to be prefixed with system names.
.LP
\fIUucico\fP (Copy-In, Copy-Out) is the primary \*(Uu daemon.
It processes the requests queued by \fIuucp\fP and \fIuux\fP,
initiates calls to remote systems, transfers files, and forks other
daemons to execute \fIuux\fP-requested commands.
\fIUucico\fP also acts as the \*(Uu ``shell'' when remote systems call in
to requests transfers.
.LP
Three types of files are used in \*(Uu operation.
\fIControl files\fP describe the \*(Uu environment, including
known remote hosts, available devices, and remote file access permissions.
Control files are relatively static; they are generally changed only by the
system administrator.
\fISpool files\fP (also called \fIQueue files\fP) contain transfer requests
and data; they are created and deleted as necessary
by \fIuucp\fP, \fIuux\fP, and \fIuucico\fP.
\fILog files\fP accumulate a history of \*(Uu activity; these tend to
grow forever if not periodically cleaned up.
.LP
Spool files are further divided into three types:
\fIWork files\fP
contain directions for file transfers between systems.
Every invocation of \fIuucp\fP or \fIuux\fP creates one or more work files.
\fIData files\fP contain data for transfer to or from remote systems.
\fIExecution files\fP contain directions for
.UX
command executions which
involve the resources of one or more systems.
Execution files are created only by \fIuux\fP.
.\"===========================================================================
.\" SECTION 2: USER UTILITIES
.\"===========================================================================
.ne 5
.NH
USER UTILITIES
.PP
\*(Uu includes a total of ten ``primary'' utilities, that is, ten
utilities for general users.
All reside in the \fB/usr/bin\fP directory, where they are easily accessible.
This section provides detailed implementation descriptions for the more
important commands; see the corresponding \fIman\fP pages for additional
information.
.LP
The following two commands queue transfer requests:
.RS
.IP uucp(1C) 15
UNIX-to-UNIX File Copy.
One of more \fIcontrol files\fP are created, containing names of files to be
transferred.
When necessary, local files are copied into \fIdata files\fP for
transmission.
.IP uux(1C)
Execute command.
An \fIexecute file\fP is created, containing a
.UX
command to be executed and its arguments.
A \fIcontrol file\fP is created that includes all files that must be
transferred to execute the command, including the \fIexecute file\fP itself.
When necessary, local files are copied into \fIdata files\fP for
transmission.
Any output from the command will also be written to \fIdata files\fP.
.RE
.LP
The following four commands provide \*(Uu status information:
.RS
.IP uulog(1C) 15
Display selected information from the \*(Uu log.
.IP uuname(1C)
Display the names of all remote hosts that are directly accessible via \*(Uu.
.IP uusnap(8C)
Provide a snapshot of the current queue, including
the number of work files, data files, and execute files for each site.
.IP uuq(1C)
A variant of \fIuusnap\fP, lists files and \fIuux\fP commands
queued for each site.
\fIUuq\fP also permits the \*(Uu administrator to delete jobs.
.RE
.LP
The following four commands provide miscellaneous support services:
.RS
.IP uudecode(1C) 15
The decoder for files created by \fIuuencode\fP, below.
.IP uuencode(1C)
A filter to convert binary files into printable ASCII.
This is useful when transferring object files over communications links
that do not support 8-bit transfers.
.IP uupoll(8C)
A user utility to conveniently fork the \*(Uu daemon, \fIuucico\fP.
.IP uusend(1C)
A utility to send files to remote sites more than one ``hop'' distant.
.RE
.\"===========================================================================
.NH 2
Uucp - UNIX to UNIX File Copy
.LP
The \fIuucp\fP
command is the user's primary interface with the system.
The
.I uucp
command was designed to look like
.I cp
to the user.
The syntax is
.IP
.I uucp\ \ 
.B [
option
.B ]
\ ...\ \ source\ ...\ \ destination
.LP
where the source and destination
may contain the prefix \fIsystem-name\fP\fB!\fP
which indicates the system on which the file
or files reside
or where they will be copied.
.LP
The options interpreted by
.I uucp
are:
.RS
.IP \-f 10
Don't make directories when copying the file.
The default is to make the necessary directories.
.IP \-C
Copy source files to the spool directory.
The default is to use the specified source when the actual
transfer takes place.
.IP \-g\fIgrade\fR
Put
.I grade
in as the grade in the name of the work file.
This is a single character in the range \fB[0-9][A-Z][a-z]\fP.
The \fIgrade\fP will be used by \fIuucico\fP to establish the
priority of requests.
\fB0\fP is the highest (best) grade; \fBz\fP is the lowest (worst).
The default
.I grade
for
.I uucp
is \fBn\fP.
.IP \-m
Send mail on completion of the work.
.IP \-n\fIuser\fR
Notify \fIuser\fR on the destination system that a file was sent.
.LP
The following options are used primarily for debugging, or when
.I uucp
is invoked from other programs:
.IP \-r 10
Queue the job but do not start \fIuucico\fP.
The assumption is that \fIuucico\fP will be started at a later time, perhaps
by \fIcron\fP(8) or \fIuupoll\fP.
.IP \-s\fIdir\fR
Use directory
.I dir
for the top level spool directory.
.IP \-x\fInum\fR
.I Num
is the level of debugging output desired.
This option requires the user to have read permission to the \*(Uu
control file \fIL.sys\fP.
.RE
.LP
The destination may be a directory name,
in which case the file name is taken from the last part of the
source's name.
The source
name may contain special shell characters
such as ``\fB?*[]\fR'';
these and other shell characters such as ``\fB!<>\fP'' will need to be quoted
or escaped.
If a source argument has a
\fIsystem-name\fP\fB!\fP
prefix for a remote system,
the file name expansion will be done on the remote system.
.LP
The command
.IP "" 12
uucp\ \ *.c\ \ usg!/usr/dan
.LP
will set up the transfer of all files whose names end with ``.c''
to the ``/usr/dan'' directory on the ``usg'' machine.
.LP
The source and/or destination names may also contain a \fB~\fP\fIuser\fP
prefix.
This translates to the login directory on
the specified system.
A lone \fB~\fP prefix is expanded to the name of the specified system's
public access directory, usually 
\fB/usr/spool/uucppublic\fP.
For names with partial path-names,
the current directory is prepended to the file name.
File names with
.I ../
are not permitted.
.LP
The command
.IP "" 12
uucp\ \ usg!~dan/*.h\ \ ~dan
.LP
will set up the transfer of files whose names end with ``.h''
in dan's login
directory on system ``usg'' to dan's local
login directory.
.LP
For each source file,
the program will check the source and destination
file-names
and the system-part of each to
classify the work into one of five types:
.RS
.IP [1]
Copy source to destination on local system.
.IP [2]
Receive files from a remote system.
.IP [3]
Send files to a remote system.
.IP [4]
Send files from remote system
to another remote system.
.IP [5]
Receive files from remote system when the source pathname
contains special shell characters as
mentioned above.
.RE
.LP
After the work has been set up in the spool directories,
the \*(Uu daemon \fIuucico\fP is started to try to contact the other
machine to execute the work (unless the \-r option
was specified).
.SH
Type 1
.LP
.I Uucp
makes a copy of the file.
The
.I \-m
option is not honored in this case.
.SH
Type 2
.LP
A one line
.I "work file"
is created for each file requested and put in the \fBC.\fP spool directory
with the following fields, each separated by a blank.
(All
.I "work files"
and
.I "execute files"
use a blank as the field separator.)
.RS
.IP [1]
R
.IP [2]
The full path-name of the source or a ~user/path-name.
The
.I ~user
part will be expanded on the remote system.
.IP [3]
The full path-name of the local destination file.
If the
.I ~user
notation is used, it will be immediately
expanded to be the login directory for the user.
.IP [4]
The user's login name.
.IP [5]
A `\-' followed by an option list.
.RE
.KS
.SH
Type 3
.LP
For each source file, a
.I "work file"
is created.
A \fB\-C\fP option on the
.I uucp
command will cause the
.I "data file"
to be copied into the spool directory
and the file to be transmitted from
the copy; the copy is deleted when the transfer completes.
The fields of each entry are given below.
.RS
.IP [1]
S
.IP [2]
The full-path name of the source file.
.IP [3]
The full-path name of the destination or
~user/file-name.
.IP [4]
The user's login name.
.IP [5]
A `\-' followed by an option list.
.IP [6]
The full path-name of the local source file.
If the
.I ~user
notation is used, it will be immediately
expanded to be the login directory for the user.
If the \fB\-C\fP option was used, this will be the name of a
.I "data file"
in the spool directory.
.IP [7]
The file mode bits of the source file
in octal print format
(e.g. 0666).
.IP [8]
The user to notify on the remote system that the transfer has completed.
.RE
.KE
.SH
Type 4 and Type 5
.LP
.I Uucp
generates a
.I uucp
command and sends it to the remote machine;
the remote
.I uucico
executes the
.I uucp
command.
.\"===========================================================================
.NH 2
Uux - UNIX To UNIX Execution
.LP
The \fIuux\fP
command is used to set up the execution of a
.UX
command
where the execution machine and/or some of the
files are remote.
The syntax of the uux command is
.IP
.I uux\ \ 
.B [
\-
.B "] ["
option
.B ]
\ ...\ \ command-string
.LP
where the command-string is made up of one or more arguments.
All special shell characters such as ``<>|*?!'' must be quoted
either by quoting the entire command-string
or quoting the character as a separate argument.
Within the command-string, the command and file names may
contain a
\fIsystem-name\fP\fB!\fP
prefix.
All arguments which do not contain a ``!'' will not
be treated as files.
(They will not be copied to the execution machine.)
The `\-' is used to indicate that the standard input
for
.I command-string
should be inherited from the standard input
of the
.I uux
command.
.LP
The options, used mostly for debugging and by other programs, are:
.RS
.IP \-a\fIname\fP 10
Use \fIname\fP as the requestor of the \fIuux\fP command, instead of the
real system and login names.
Unlike most other \*(Uu arguments, \fIname\fP may consist of a chain of
system names separated by `!' characters, as in:
.IP \  15
uux\ \ \-\ \ \-r\ \ \-aihnp4!decwrl!pyramid!csg\ \ seismo!rmail\ \ rick
.IP \-C 10
Copy source files to the spool directory.
Same as for \fIuucp\fP.
.IP \-g\fIgrade\fR
Put
.I grade
in as the grade in the name of the work file.
Same as for \fIuucp\fP.
The default
.I grade
for
.I uux
is \fBA\fP.
.IP \-n
Do not mail an acknowledgement to the requestor of the command.
Normally the execution daemon, \fIuuxqt\fP, will mail a message
to the user who entered the \fIuux\fP command.
This message includes the status return value that the program exited with.
The \fB\-n\fP option requests that this message not be sent.
.IP \-r
Do not start the \*(Uucp daemons \fIuucico\fP(8C) or \fIuuxqt\fP(8C)
after queuing the job.
.IP \-x\fInum\fR
Num is the level of debugging output desired.
.IP \-z
Mail an acknowledgement to the requestor only if the command fails, that
is, the command exits with a non-zero status.
.RE
.LP
The command
.IP "" 12
pr\ \ abc\ \ |\ \ uux\ \ \-\ \ usg!lpr
.LP
will set up the output of ``pr abc''
as standard input to an lpr command
to be executed on system ``usg''.
.LP
.I Uux
generates an
.I "execute file"
which contains the
names of the files required
for execution (including standard input),
the user's login name, the destination
of the standard output, and the command to be executed.
This file is either put in the \fBX.\fP spool directory
for local execution,
or in the \fBD.\fP\fIhostname\fP\fBX\fP directory
for transfer to the remote system.
.LP
For required files which are not on the execution machine,
.I uux
will generate receive command files (type 2 above).
These command-files will be put on the execution machine and
executed by 
\fIuucico\fP.
(This will work only if the local system has permission
to put files in the remote spool directory as controlled
by the remote
\fIUSERFILE\fP.)
.LP
The
.I "execute file"
will be processed
by the
\fIuuxqt\fP(8C)
program on the execution machine.
It is made up of several lines,
each of which contains an identification character
and one or more arguments.
The order of the lines in the file is not relevant
and some of the lines may not be present.
Each line is described below.
.RS
.SH
User Line
.IP
U\ \ user\ \ system
.LP
where the
.I user
and
.I system
are the requestor's login name and system.
.SH
Required File Line
.IP
F file-name real-name
.LP
where the
.I file-name
is the generated name of a file for the execute machine
and
.I real-name
is the last part of the actual file name (contains no
path information).
Zero or more of these lines may be present in the
.I "execute file" .
The
.I uuxqt
program will check for the existence of all required
files before the command is executed.
.SH
Standard Input Line
.IP
I\ \ file-name
.LP
The standard input is either specified by a `<' in the
command-string or inherited from the standard input of the
.I uux
command if the `\-' option is used.
If a standard input is not specified,
.B /dev/null
is used.
.SH
Standard Output Line
.IP
O\ \ file-name\ \ system-name
.LP
The standard output is specified by a `>' within the
command-string.
If a standard output is not specified,
.B /dev/null
is used.
(Note \- the use of ``>>'' is not implemented.)
.SH
Status Return Line
.IP
N
.LP
Normally \fIuuxqt\fP mails an acknowledgement message to the
requestor after the command completes.
The message includes the status return value that the program exited with.
This line inhibits mailing of the acknowledgement message.
It is generated by the \fB-n\fP option of \fIuux\fP;
it is also quietly assumed by \fIuuxqt\fP on the command \fBrmail\fP.
.SH
Error Status Return Line
.IP
Z
.LP
A variant of the \fIStatus Return\fP line, this line indicates that
an acknowledgement should be mailed only if the command's status
return is non-zero, i.e., the program exited with an error.
This line is generated by the \fB-z\fP option of \fIuux\fP.
It is also quietly assumed by \fIuuxqt\fP on the command \fBrnews\fP.
If both the \fBZ\fP and \fBN\fP lines appear, the \fBZ\fP line has
precedence.
.SH
Requestor Line
.IP
R\ \ requestor
.LP
where
.I requestor
is a complete return mailing address to the original requestor.
This line is generated by the \fB-a\fP option of \fIuux\fP, and is used
to override the mail return address implied by the \fIUser\fP line.
This is commonly used by mailers and programs like \fIuusend\fP that
know how to ``hop'' a file from system to system.
.SH
Command Line
.IP
C\ \ command\ \ 
.B [
arguments
.B ]
\ ...
.LP
The arguments are those specified in the command-string.
The standard input and standard output will not appear on this
line.
All
.I "required files"
will be moved to the execution directory (a subdirectory
of the spool directory)
and the
.UX
command is executed using the Shell specified in the
.I uucp.h
header file (usually \fI/bin/sh\fP).
In addition, a shell ``PATH'' statement is prepended
to the command line.
.LP
After execution, the temporary standard output file is copied to
or set up to be
sent to the proper place.
.RE
.\"===========================================================================
.\" SECTION 3: SYSTEM UTILITIES
.\"===========================================================================
.ne 5
.NH
SYSTEM AND ADMINISTRATIVE UTILITIES
.LP
\*(Uu includes four system utilities;
these are not normally referenced by users.
All except \fIuucpd\fP reside in the \*(Uu administrative directory,
\fB/usr/lib/uucp\fP.
These include:
.RS
.IP uucico(8C) 15
Copy In, Copy Out. This is the primary \*(Uu daemon.
.IP uuclean(8C)
A handy utility to clean up the \*(Uu spool directories.
.IP uucpd
The \*(Uu TCP/IP daemon.
This daemon ``answers'' the connection request from a remote \fIuucico\fP
to a TCP/IP socket.
It is functionally a stripped-down version of \fIrlogind\fP(8) that provides
full 8-bit communication.
(Note: this utility does not have a \fIman\fP page.)
.IP uuxqt(8C)
Execution Daemon.
This is forked by \fIuucico\fP to interpret execution files
transferred from a remote system.
.RE
.\"===========================================================================
.NH 2
Uucico - Copy In, Copy Out (\*(Uu Daemon)
.LP
.I Uucico
is the ``heart'' of the \*(Uu system.
The program performs the following major functions:
.RS
.IP -\ \  3
Scan the spool directory for work.
.IP -\ \  
Place a call to a remote system.
.IP -\ \ 
Negotiate a line protocol to be used.
.IP -\ \ 
Execute all requests from both systems.
.IP -\ \ 
Log work requests and work completions.
.RE
.LP
.I Uucico
may be started in several ways;
.RS
.IP a) 5
by a system daemon (such as \fIcron\fP(8)),
.IP b)
by one of the
.I
uucp, uux, uuxqt
.R
or
.I uupoll
programs,
.IP c)
directly by the user (this is usually for testing),
.IP d)
by a remote system.
(The \fIuucico\fP program should be specified as the ``shell''
field in the \fB/etc/passwd\fP file for the \*(Uu logins.)
.RE
.LP
When started by method a, b or c, the program is considered to
be in
.I MASTER
mode.
In this mode, a connection will be made to a remote system.
If started by a remote system (method d),
the program is considered to be in
.I SLAVE
mode.
.LP
The
.I MASTER
mode will operate in one of two ways.
If no system name is specified
(\-s option not specified)
the program will scan the spool directory for
systems to call.
If a system name is specified, that system will be called,
and work will only be done for that system.
.LP
The
.I uucico
program is generally started by another program.
There are several options used for execution:
.RS
.IP \-g\fIgrade\fP 10
Set the minimum grade of this \fIuucico\fP run to \fIgrade\fP.
Only files of this grade or better will be transferred.
.IP \-r1
Start the program in
.I MASTER
mode.
This is used when
.I uucico
is started by a program or \fIcron\fP shell.
.IP \-s\fIsys\fR
Do work only for system
.I sys.
If
.B \-s
is specified,
a call to the specified system
will be made even if there is no work for system
.I sys
in the spool directory.
This is useful for polling systems which do not have
the hardware to initiate a connection.
.LP
The following options are used primarily for debugging:
.IP \-d\fIdir\fR
Use directory
.I dir
for the top level spool directory.
.IP \-x\fInum\fR
.I Num
is the level of debugging output desired.
.RE
.LP
The next part of this section will describe the major steps within
the
.I uucico
program.
.SH
Scan For Work
.LP
The names of the work related files in a spool subdirectory have format
.IP
type \fB.\fP system-name grade number
.LP
where:
.IP
.I Type
is an upper case letter,
(
.B C
-\ work (copy command) file,
.B D
-\ data file,
.B X
-\ execute file);
.IP
.I System-name
is the remote system;
.IP
.I Grade
is a character in the range \fB[0-9][A-Z][a-z]\fP;
.IP
.I Number
is a four digit, padded sequence number.
.LP
The file
.IP "" 12
C.res45n0031
.LP
would be a
.I "work file"
for a file transfer between the local
machine and the ``res45'' machine.
.LP
The scan for work is done by looking through the
appropriate spool directory for
\fIwork files\fP
(files with prefix \fBC.\fP).
A list is made of all systems to be called.
.I Uucico
will then call each system and process all
.I "work files" .
.SH
Call Remote System
.LP
The call is made using information from the \fIcontrol\fP files
that reside in the \fB/usr/lib/uucp\fP directory.
At the start of the call process, a lock is
set to forbid multiple conversations 
between the same two systems.
.LP
The system name is found in the
.I L.sys
control file.
The information contained for each system is;
.RS
.IP [1]
system name,
.IP [2]
times to call the system
(days-of-week and times-of-day),
.IP [3]
the \fIcaller\fP, that is, the type of device to be used for the call,
.IP [4]
the line speed or network number (as appropriate),
.IP [5]
telephone number or device name (as appropriate),
.IP [6]
login information (multiple fields).
.RE
.LP
The time field is checked against the present time to see
if the call should be made.
.LP
The
.I
phone number
.R
may contain abbreviations (e.g. mh, py, boston) which get translated into dial
sequences using the
.I L-dialcodes
file.
.LP
The
.I L-devices
file is scanned using fields [3] and [4] from the
.I L.sys
file to find an available device for the call.
The program will try all devices which satisfy
[3] and [4] until the call is made or no more
devices can be tried.
If a device is successfully opened, a lock file
is created so that another copy of
.I uucico
will not try to use it.
If the call is complete, the
.I
login information
.R
(field [6] of
\fIL.sys\fP)
is used to login.
.LP
The conversation between the two
.I uucico
programs begins with a handshake started by the called,
.I SLAVE ,
system.
The
.I SLAVE
sends a message to let the
.I MASTER
know it is ready to receive the system
identification and conversation sequence number.
The response from the
.I MASTER
is
verified by the
.I SLAVE
and if acceptable, protocol selection begins.
The
.I SLAVE
can also reply with a ``call-back required''
message in which case, the current conversation
is terminated.
.SH
Line Protocol Selection
.LP
The remote system sends a message
.IP "" 12
P\fIproto-list\fR
.LP
where proto-list is a string of characters, each
representing a line protocol.
.LP
The calling program checks the proto-list
for a letter corresponding to an available line
protocol and returns a
.I use-protocol
message.
The
.I use-protocol
message is
.IP "" 12
U\fIcode\fR
.LP
where 
.I code
is either a one character
protocol letter or
.B N
which means there is no common protocol.
.LP
The following protocols are implemented in 4.3BSD \*(Uu:
.RS
.IP \fBg\fP
General.
Default for dialup or hardwired lines, supported by all versions of \*(Uu.
This protocol employs small (64 byte) data packets with
checksums and packet-by-packet retransmission.
This ensures reliable and efficient transfers over slow and noisy links
like 1200-baud dial-up lines.
These same characteristics make the \fBg\fP protocol bulky and slow over
error free links, and very expensive on public data-switched networks.
.IP \fBf\fP
Optimized for use on X.25 PAD public data-switched networks.
The protocol employs larger (256 byte) data packets,
passes no control characters except CR,
and uses only a 7-bit data path.
(Note that the files transferred may still contain full 8-bit data.)
It assumes that the link is ``mostly'' error-free, calculating a checksum
for the entire file only. 
When an error is detected, the entire file is retransmitted.
.IP \fBt\fP
Optimized for use on TCP/IP networks and other completely error free links.
It employs large (1024 byte) packets, and uses the full 8-bit data path.
.RE
.LP
Note: AT&T System VR2 \*(UU supports the \fBx\fP (\fIX.25\fP) and \fBe\fP
(\fIError Free\fP) protocols, which provide functionality similar to the
4.3BSD \fBf\fP and \fBt\fP protocols, respectively. 
They are incompatible, however.
Thus when attempting to connect two systems via X.25 or an local area network,
it is not adequate for both systems to simply ``support X.25'' or ``support
error free transfers.''
Both must support the same \*(Uu protocols.
.SH
Work Processing
.LP
The initial roles (
.I MASTER
or
.I SLAVE
) for the work processing are
the mode in which each program starts.
(The 
.I MASTER
has been specified by the \fB\-r1\fP \fIuucico\fP option.)
The
.I MASTER
program does a work search similar to the
one used in the ``Scan For Work'' section.
.LP
There are five messages used during the
work processing, each specified by the first
character of the message.
They are;
.IP "" 12
.RS
.IP S 3
send a file,
.IP R
receive a file,
.IP C
copy complete,
.IP X
execute a
.I uucp
command, and
.IP H
hangup.
.RE
.LP
The
.I MASTER
will send
.I R ,
.I S
or
.I X
messages until all work from the spool directory is
complete, at which point an
.I H
message will be sent.
The
.I SLAVE
will reply with
\fISY\fR, \fISN\fR, \fIRY\fR, \fIRN\fR, \fIHY\fR, \fIHN\fR,
\fIXY\fR, \fIXN\fR,
corresponding to
.I yes
or
.I no
for each request.
.LP
The send and receive replies are
based on permission to access the
requested file/directory using
.I USERFILE
and read/write permissions of the file/directory.
After each file is copied into the spool directory
of the receiving system,
a copy-complete message is sent by the receiver of the file.
The message
.I CY
will be sent if the
file has successfully been moved from the
temporary spool file to the actual destination.
Otherwise, a
.I CN
message is sent.
(In the case of
.I CN ,
the transferred file will be in the \fBTM.\fP spool subdirectory.)
The requests and results are logged on both systems.
.LP
The hangup response is determined by the
.I SLAVE
program by a work scan of its spool directory.
If work for the \fIMASTER\fP\|'s system exists in the
\fISLAVE\fP\|'s
spool directory, an
.I HN
message is sent and the programs switch roles.
If no work exists, an
.I HY
response is sent.
.SH
Conversation Termination
.LP
When a
.I HY
message is received by the
.I MASTER
it is echoed back to the
.I SLAVE
and the protocols are turned off.
Each program sends a final ``OO'' message to the
other.
The original
.I SLAVE
program will clean up and terminate.
The
.I MASTER
will proceed to call other systems
and process work as long as possible
or terminate if a
.B \-s
option was specified.
.LP
.\"===========================================================================
.NH 2
Uuxqt - Uucp Command Execution
.LP
The
.I uuxqt
program is used to execute
.I
execute files
.R
generated by
.I uux.
The
.I uuxqt
program may be started by either the
.I uucico
or
.I uux
programs.
The program scans the \fBX.\fP spool directory for
\fIexecute files\fP.
Each one is checked to see if all the required files are available and
if so, the command line or send line is executed.
.LP
The
.I
execute file
.R
is described in the
.I uux
section above.
.SH
Command Execution
.LP
The execution is accomplished by executing a
.I
sh \-c
.R
of the command line after appropriate
standard input and standard output have been opened.
If a standard output is specified, the program
will create a send command or copy the output
file as appropriate.
.\"===========================================================================
.NH 2
Uuclean - Uucp Spool Directory Cleanup
.LP
This program is typically started by the
\fIcron\fP(8)
daemon, once a day.
Its function is to remove files from the spool directories which
are more than 3 days old.
These are usually files for work which can not be completed.
.LP
.LP
The options available are:
.RS
.IP \-d\fIdir\fR 10
The directory to be scanned is
.I dir .
.IP \-m
Send mail to the owner of each file being removed.
(Note that most files put into the spool directory
will be owned by the owner of the
uucp programs since the setuid bit will be set on these
programs.
The mail will therefore most often go to the owner
of the uucp programs.)
.IP \-n\fIhours\fR
Change the aging time from 72 hours to
.I hours
hours.
.IP \-p\fIpre\fR
Examine files with prefix
.I pre
for deletion.
(Up to 10 file prefixes may be specified.)
.IP \-x\fInum\fR
This is the level of debugging output desired.
.RE
.\"===========================================================================
.\" SECTION 4: CONTROL FILES
.\"===========================================================================
.ne 5
.NH
SYSTEM CONTROL FILES
.PP
Seven \fIControl Files\fP are referenced by the \*(Uu utilities.
All live in the \*(Uu administrative directory, \fB/usr/lib/uucp\fP.
These are ASCII files, and can be modified using standard text editors such
as \fIvi\fP and \fIex\fP.
Lines beginning with a `#' character are comments;
lines ending with a `\e' are continued on the next input line.
.RS
.IP L-devices(5) 15
Declares all devices that are available to \fIuucico\fP for calling out.
.IP L-dialcodes(5)
Phone number prefixes.
Used to map alphabetic prefixes on phone numbers from \fIL.sys\fP to
real phone numbers.
Also useful to keep a phone number database outside of \fIL.sys\fP.
.IP L.sys(5)
Systems.
Declares all ``adjacent'' \*(Uu hosts, with directions on how to reach them.
.IP L.aliases(5)
Contains aliases used to map obsolete or truncated host names to the
correct names.
.IP L.cmds(5)
Commands Permissions.
Declares those commands for which remote \fIuux\fP execution is permitted.
.IP SQFILE
Sequence-number check file. (Optional)
.IP USERFILE(5)
Directory Tree Permissions.
Specifies the set of directory trees that a particular user or host may
reference.
.RE
.LP
A general description of each file follows; see the \fIman\fP pages for
complete information.
Examples of the six standard files are included in the distribution in the
\fB/usr/lib/uucp/UUAIDS\fP directory.
.NH 2
L-devices \- \*(UU Devices File
.LP
This file declares all devices that are available to
\fIuucico\fP
for calling out.
The special device files are assumed to be in the
.I /dev
directory.
The format for each entry is
.IP "" 12
caller  line  call-unit  class  dialer  [chat....]
.LP
where;
.RS
.IP caller 12
is the caller mechanism, that is, the type of device to be used.
This can be one of  \fBACU\fP (for Automatic
Call Units (modem)), \fBDIR\fP (direct hardwired), \fBPAD\fP
(X.25/PAD), and others.
.IP line
is the device for the link.
For example, \fBcul0\fP for a modem, \fBtty10\fP for a hardwired line.
.IP call-unit
is the automatic call unit associated with
\fIdevice\fP.
This is used on autodialers such as the Racal-Vadic MACS and the
DEC DN-11 that use one device for data, and a second device for dialing.
If unused, this field must contain a placeholder such as ``unused'' or ``0''.
Some modems use this field to specify tone or pulse dialing.
.IP class
is the line speed, plus an optional alphabetic prefix.
The prefix can be used to distinguish among different devices that
have identical \fIcaller\fP and line speed.
.IP dialer
applies to \fBACU\fP devices only;
this is the type or brand name of the modem.
Supported modems include \fBDN11\fP (DEC DN-11),
\fBhayes\fP (Hayes Smartmodem),
\fBvadic\fP (Racal-Vadic 3451),
\fBventel\fP (VenTel 212A), and others.
.IP chat
refers to an \fIexpect/send\fP script, similar to that provided in
\fIL.sys\fP.
The difference is that the script in
.I L-devices
is executed before the connection is established, while the script in
.I L.sys
is executed afterwards.
.RE
.LP
The line
.IP "" 12
ACU\ \ tty47\ \ unused\ \ 1200\ \ hayes
.LP
would be set up for a system which
had device tty47 wired to a 
Hayes ``Smartmodem 1200''
for use at 1200 baud.
.NH 2
L-dialcodes \- Phone Number Prefix File
.LP
This file contains entries with location abbreviations used
in the
.I L.sys
file (e.g. py, mh, boston).
The entry format is
.IP "" 12
abb\ \ dial-seq
.LP
where;
.RS
.IP abb 12
is the abbreviation,
.IP dial-seq
is the dial sequence to call that location.
.RE
.LP
The line
.IP "" 12
py\ \ 165\-
.LP
would be set up so that entry py7777 would
send 165\-7777 to the dial-unit.
.NH 2
L.aliases \- Hostname Aliases File
.LP
This file defines mapping (aliasing) of remote host names.
This is intended for compensating for systems that have
changed names, or do not provide their entire machine name
(like most USG systems).
It is also useful when a machine's name is not obvious or commonly misspelled.
.LP
Each line is of the form
.IP
real-name\ \ alias-name
.LP
where
.I real-name
is the full, correct name for the host, and
.I alias-name
is the old or truncated name.
.NH 2
L.sys \- \*(Uu Systems File
.LP
Each entry in this file represents one system
which can be called by the local uucp programs.
The format for each entry is
.IP
system  times  caller  class  device/phone-number  [login]
.LP
where;
.RS
.IP "system" 12
is the hostname of the remote system.
.IP times
is a keyword-encoded string that indicates the days-of-the-week
and times-of-day when the system may
be called.
For example \fBMoTuTh0800\-1730\fP would denote Monday, Tuesday,
and Thursday, between 8 a.m. and 5:30p.m.
.IP
The day portion may be a list containing
any of
\fBSu\fP, \fBMo\fP, \fBTu\fP, \fBWe\fP, \fBTh\fP, \fBFr\fP, \fBSa\fP,
or 
.B Wk
for any week-day or
.B Any
for any day.
.IP
The time should be a range of times (as in \fB0800\-1230\fP).
If no time portion is specified, any time
of day is assumed to be acceptable for the call.
.IP caller
is one of the caller device-types listed in \fIL-devices\fP.
.IP class
is the line speed for the call (e.g., 300, 1200, 9600),
plus an optional alphabetic prefix.
Network devices use this field for the network port number.
.IP phone
is the the phone number to call (for \fBACU\fP devices) or the
device filename. 
A phone number is made up of an optional
alphabetic abbreviation and a numeric part.
The abbreviation is one which appears in the
.I L-dialcodes
file (e.g. mh5900, boston995\-9980).
.IP login
is a script describing how to log in to the remote host.
It is expressed as a series of
fields and subfields in the format
.IP "" 17
expect\ \ send\ 
[
expect\ \ send
]
\ ...
.IP "" 12
where;
.I expect
is the string expected to be read and
.I send
is the string to be sent when the
.I expect
string is received.
The
.I send
string is normally terminated with carriage-return;
an empty
.I send
string will send only a carriage-return.
.IP
The expect field may be made up of subfields
of the form
.IP "" 17
expect\fB[\fR\-send\-expect\fB]\fR...
.IP "" 12
where the
.I send
is sent if the prior
.I expect
is not successfully read
and the
.I expect
following the
.I send
is the next expected string.
.RE
.LP
A typical entry in the L.sys file would be
.IP "" 6
sys\ Any\ ACU\ 1200\ \ mh7654\ login:--login:\ uucp\ ssword:\ word
.LP
The expect algorithm looks at the last part of the
string as illustrated in the password field.
.NH 2
L.cmds \- Commands Permissions File
.LP
This file contains a list of commands, one per line, that are permitted
for remote execution via \fIuux\fP.
This list should be chosen with great care, since commands that take filenames
as arguments will allow users to easily circumvent \*(Uu's security.
For most sites,
.I L.cmds
should only include the lines:
.DS
rmail
ruusend
.DE
.NH 2
SQFILE \- Sequence Check File (Optional)
.LP
This file contains an entry for each remote
system with which this site agrees to perform conversation
sequence checks.
The initial entry is just the system name of
the remote system.
The first conversation will add two items to the line,
the conversation count, and the date/time of the most
resent conversation.
These items will be updated with each conversation.
If a sequence check fails, which could indicate that an unauthorized
connection has been attempted, the entry will have to
be adjusted.
.LP
This facility is technically no longer supported in 4.3BSD \*(Uu,
since it was hardly ever used and consumed precious memory space on PDP-11
systems.
The compile-time #define GNXSEQ can be set to enable sequence checking
should it be needed.
.NH 2
USERFILE \- Pathnames Permissions File
.LP
This file contains user accessibility information.
It specifies four types of constraint;
.RS
.IP [1]
which files can be accessed by a normal user of the local machine,
.IP [2]
which files can be accessed from a remote computer,
.IP [3]
which login name is used by a particular remote computer,
.IP [4]
whether a remote computer should be called back in order to confirm
its identity.
.RE
.LP
Each line in the file has the following format
.IP "" 6
login,sys\ \ 
.B [
c
.B ]
\ \ path-name\ \ 
.B [
path-name
.B ]
\ ...
.LP
where;
.RS
.IP login 12
is the login name for a user or the remote computer,
.IP  sys
is the system name for a remote computer,
.IP c
is the optional
.I
call-back required
.R
flag,
.IP path-name
is a path-name prefix that is acceptable for
.I user.
.LP
.RE
.LP
The constraints are implemented as follows.
.RS
.IP [1]
When the program is obeying a command stored on the local machine,
.I MASTER
mode,
the path-names allowed are those given for the first line in the
.I USERFILE
that has a login name that matches the login name of the user
who entered the command.
If no such line is found, the first line with a
.I null
login name is used.
.IP [2]
When the program is responding to a command from a remote machine,
.I SLAVE
mode, the path-names allowed are those given for the first line
in the file that has the system name that matches the system name
of the remote machine.
If no such line is found, the first one with a
.I null
system name is used.
.IP [3]
When a remote computer logs in, the login name that
it uses must appear in the
.I USERFILE .
There may be several lines with the same login name but one of them must
either have the name of the remote system or must contain a
.I null
system name.
.B Note:
This constraint, although stated in the original Nowitz \*(Uu document,
was not implemented in Version 7 \*(Uu.
For all practical purposes,
a remote computer's login was not validated by \*(Uu.
This is still the case in 4.3BSD.
Remote login checking \fIis\fP implemented in AT&T's System VR2.2 release,
and in the \*(Uu provided with Digital Equipment Corporation's ULTRIX.
HoneyDanBer analogously requires all remote logins to be listed in
its \fIPermissions\fP file.
.IP [4]
If the line matched in ([3]) contains a ``c'',
the remote machine is called back before any transactions take place.
.RE
.LP
The line
.IP "" 12
u,m  /usr/xyz
.LP
allows machine
.I m
to login with name
.I u
and request the transfer of files whose names start with
``/usr/xyz''.
.LP
The line
.IP "" 12
dan,  /usr/dan
.LP
allows the ordinary user
.I dan
to issue commands for files whose name starts with
``/usr/dan''.
.LP
The lines
.IP "" 12
u,m /usr/xyz  /usr/spool
.br
u,  /usr/spool
.LP
allows any remote machine to login with name
.I u ,
but if its system name is not
.I m ,
it can only ask to transfer files whose names start with
``/usr/spool''.
.LP
The lines
.IP "" 12
root,  /
.br
,   /usr
.LP
allows any user to transfer files beginning with ``/usr''
but the user with login
.I root
can transfer any file.
.PP
.\"===========================================================================
.\" SECTION 5: SPOOL FILES
.\"===========================================================================
.ne 5
.NH
SPOOL FILES
.PP
Spool Files contain \*(Uu transfer requests and data.
Most have been described in detail earlier in this document.
.LP
All spool files live in the
.B /usr/spool/uucp
directory tree.
To keep the spool directory from becoming hopelessly cluttered,
each type of spool file is kept in its own subdirectory.
The name of the directory is the same as the common prefix of the
filename.
For example, \fIwork files\fP (files beginning with \fBC.\fP) are kept
in the \fBC.\fP directory; \fIexecute files\fP (which begin with \fBX.\fP)
are kept in the \fBX.\fP directory, and so on.
.LP
A total of ten spool subdirectories are used, one of which is optional:
.RS
.IP C. 15
\fIWork\fP files.
.IP CORRUPT
Corrupted \fIwork\fP and \fIexecute\fP files.
\fIUucico\fP and \fIuuxqt\fP will deposit \fBC.\fP and \fBX.\fP files here
when they are unable to parse them.
A notice will also be placed in the \*(Uu log.
.IP D.
\fIData files\fP received from remote hosts.
.IP D.\fIhostname\fP
\fIData files\fP to be sent to remote hosts.
.IP D.\fIhostname\fPX
\fIExecution files\fP to be sent to remote hosts.
.IP LCK
Per-device and per-site lock (\fBLCK.\fP) files. (Optional)
.IP STST
Per-site system status files.
.IP TM.
Temporary files used in data transfer.
When the transfer is complete, the file is typically
\fImv\fP'ed to the \fBD.\fP or \fBX.\fP directory.
.IP X.
\fIExecution files\fP received from remote sites. 
.IP XTMP
Temporary files and home directory for \fIuuxqt\fP.
.RE
.LP
The following sections describe only those spool files that were not
discussed earlier.
.NH 2
LCK \- lock files
.LP
Lock files are created for each device in use (except for TCP/IP sockets)
and each system conversing.
This prevents duplicate conversations and multiple attempts to use the
same devices.
The form of the lock file name is
.IP "" 12
\fBLCK..\fRstr
.LP
where
.I str
is either a device or system name.
The files may be left in the spool directory if
.I uucico
aborts.
They will be ignored (reused) after 90 minutes.
When runs abort and calls are desired before the time limit expires,
the lock files should be removed.
If the \fBLCK.\fP subdirectory is used, it's access mode can be set to 777,
thus allowing normal users to remove dead lock files when necessary.
.NH 2
STST \- system status files
.LP
These files are created in the \fBSTST\fP subdirectory by
\fIuucico\fP.
They contain information of failures such as login, dialup, or
sequence check, and will contain a
\fITALKING\fP, \fIRECEIVING\fP, or \fPSENDING\fP
status when two machines are conversing.
The file name is
\fBSTST/\fP\fIsystem\fP,
where \fIsystem\fP is the host name of the remote machine.
.LP
For ordinary failures (dialup, login), the file indicates the time
of the last failure;
this allows \fIuucico\fP to avoid retrying the failed call too soon.
For sequence check failures, the file must be removed before
any future attempts to converse with that remote system.
.LP
If the file is left due to an aborted run, it may contain a
.I TALKING
status.
In this case, the file must be removed before a conversation
is attempted.
The easiest way to do this is to use the \fIuupoll\fP command to
force \fIuucico\fP to start up.
.NH 2
TM \- temporary data files
.LP
These files are created in the
.B /usr/spool/uucp/TM.
directory while files are being copied
from a remote machine.
Their names have the form
.IP "" 12
\fBTM\fR.pid.ddd
.LP
where
.I pid
is a process-id and
.I ddd
is a sequential three digit number starting at zero
for each invocation of
.I uucico
and incremented for each file received.
After the entire remote file is received, the
.B TM
file is moved to the requested destination,
often the \fBX.\fP or \fBD.\fP directory.
If processing is abnormally terminated or the
move fails, the file will remain in the
\fBTM.\fP directory.
.LP
The stranded files should be periodically removed;
the
.I uuclean
program is useful in this regard.
The command
.IP "" 12
uuclean\ \ -d/usr/spool/uucp/TM.\ \ \-pTM.
.LP
will remove all
.B TM
files older than three days.
.\"===========================================================================
.\" SECTION 6: LOG FILES
.\"===========================================================================
.ne 5
.NH
LOG FILES
.LP
The following files provide a history of \*(Uu activities.
All live in the spool directory, \fB/usr/spool/uucp\fP.
They grow forever, and must be periodically trimmed or deleted;
this is usually done weekly (or daily) via \fIcron\fP.
.RS
.IP AUDIT 12
This is a directory of audit trail files, one file per site.
.I Uucico
uses an audit file for debugging output
whenever it is run with debug enabled (via the \fB-x\fP option or a
\fBSIGFPE\fP signal), but the standard message output file \fBstderr\fP is
not available.
.IP ERRLOG
This is an oft-forgotten log of \*(Uu ``Assert'' errors.
An Assert error is a catastrophic and unrecoverable failure of the \*(Uu
system.
These include spool directories or control files that cannot be opened, 
an unexpected error return from a system call, or an ``impossible case''
in a utility's control flow.
.IP
Utilities that abort with an Assert error return a status code of -1.
If a user reports \fIuucp\fP or \fIuux\fP dying with a message like
``uux failed, status -1,'' then the ERRLOG file should be checked.
.IP LOGFILE
This is the primary \*(Uu log. All \*(Uu activity is recorded here,
including queue requests from \fIuucp\fP and \fIuux\fP, attempted
connections, file transfers, and communications failures.
.IP SYSLOG
This is a log of file transfer statistics: number of bytes, time required,
and number of packet retries.
The effective data rate can be calculated simply by dividing the number of
bytes by the time;
low data rates or a large number of retries implies that the communication
link may marginal.
.RE
.LP
Optionally, one \fILOGFILE\fP per site may be maintained in the \fBLOG\fP
subdirectory.
This option can be selected at \*(Uu compile time via the LOGBYSITE #define
in \fBuucp.h\fP.
.\"===========================================================================
.\" SECTION 7: ADMINSTRATION AND SYSTEM SECURITY
.\"===========================================================================
.ne 5
.NH
ADMINISTRATION AND SYSTEM SECURITY
.NH 2
.UX 
System Files
.SH
/etc/passwd
.LP
\*(Uu requires a login in \fB/etc/passwd\fP;
at its simplest the entry would be
.DS
uucp::66:1:UNIX-to-UNIX Copy:/usr/spool/uucppublic:/usr/lib/uucp/uucico
.DE
.LP
This user should own all the \*(Uu files and utilities.
Remote sites wishing to call in for \*(Uu transfers would login to
\fBuucp\fP (with the correct password, if any), and get
.I uucico
as their ``shell.''
Since \fIuucico\fP would be called without any options, it would run in
.I SLAVE
mode, thus responding correctly to the remote system, which
would be in
.I MASTER
mode.
.LP
The directory
.B /usr/spool/uucppublic
should be created with 777 access modes, owned by \fBuucp\fP.
In addition to serving as the home directory for \*(Uu remote logins,
.B uucppublic
provides a ``public-access'' directory where any user can read, write,
or transfer files.
.LP
There are a number of security problems with using a single login, not
the least of which is that superuser permission would be necessary to
edit the \fIcontrol\fP files.
A better arrangement would be:
.DS
uucp::66:1:UUCP Administrator:/usr/lib/uucp:
nuucp::67:1:UNIX-to-UNIX Copy:/usr/spool/uucppublic:/usr/lib/uucp/uucico
.DE
This provides one login for the \*(Uu administrator (which must be kept
secure!) and a second for remote machines to use for login.
A still more elaborate setup would use a separate login for each remote
site, and possibly provide the administrator with a choice of shells:
.DS
uucp::66:1:UUCP Administrator:/usr/lib/uucp:
UUCP::66:1:UUCP Administrator:/usr/lib/uucp:/bin/csh
Uhosta::6001:1:UNIX-to-UNIX Copy:/usr/spool/uucppublic:/usr/lib/uucp/uucico
Uhostb::6002:1:UNIX-to-UNIX Copy:/usr/spool/uucppublic:/usr/lib/uucp/uucico
Uhostc::6003:1:UNIX-to-UNIX Copy:/usr/spool/uucppublic:/usr/lib/uucp/uucico
.DE
.LP
It is assumed that the login name
used by a remote computer to dial in
is not the same as the login name of a normal user
of the machine.
However, several remote computers may employ the same
login name.
.LP
Note that
.B uucppublic
is
.I not
used as the home directory for
.B uucp
when it logs into a regular shell.
This would be an extreme security hazard, since anyone could slip a
``Trojan horse'' into a
.B .profile
or
.B .cshrc
file, which would be automatically executed when the \*(Uu administrator
logged in.
.SH
/etc/rc
.LP
The system startup file, \fB/etc/rc\fP, should clean up any stray lock
files with the line
.IP
rm -f /usr/spool/uucp/LCK.*
.LP
or, if the LCK subdirectory is being used,
.IP
rm -f /usr/spool/uucp/LCK/LCK.*
.SH
/etc/services
.LP
If \*(Uu is to be used over TCP/IP links, then an entry for \*(Uu's port
number should be added to \fB/etc/services\fP:
.IP
uucp\ \ 540/tcp\ \ uucpd\ \ # UUCP TCP/IP
.\"===========================================================================
.NH 2
Shell Scripts For Periodic Cleanup
.LP
The \*(Uu system has a fairly large number of activities that must
occur periodically.
These include starting \fIuucico\fP to process queued requests,
running \fIuuclean\fP to remove old spool files, and
shuffling the boundlessly-growing log files.
Some sites will also want to poll other sites periodically.
.LP
While it's possible to put all the necessary commands into \fIcron\fP's
control file \fB/usr/lib/crontab\fP, this would be extremely awkward.
The usual technique is to use three separate shells scripts, one each
for hourly, daily, and weekly operations.
Examples are provided in the
.B UUAIDS
directory; the following sections provide some specific recommendations.
.SH
Hourly
.LP
Activities that should occur hourly include:
.RS
.IP - 2
Polling of selected sites.
Sites that have no dial-out capability will need to be periodically polled.
The \fIuupoll\fP command works well for this.
.IP -
Start \fIuucico\fP to complete all unfinished work.
This can be as simple as:
.IP "" 7
uucico -r1 &
.RE
.SH
Daily
.LP
The daily script should be started by \fIcron\fP in the wee hours, around
4 a.m.
Activities that should occur daily include:
.RS
.IP - 2
Call
.I uuclean
to remove old spool files.
The preferred technique is something like the following:
.DS
cd /usr/lib/uucp
deadtime=`expr 24 * 7`
uuclean -d/usr/spool/uucp/AUDIT -n72
uuclean -d/usr/spool/uucp/LCK -pLCK. -pLTMP. -n24
uuclean -d/usr/spool/uucp/STST -n72
uuclean -d/usr/spool/uucp/TM. -pTM. -n72
uuclean -d/usr/spool/uucp/XTMP -n72
uuclean -d/usr/spool/uucp/X. -pX. -n$deadtime
uuclean -d/usr/spool/uucp/C. -pC. -n$deadtime
uuclean -d/usr/spool/uucp/D. -pD. -n$deadtime
uuclean -d/usr/spool/uucp/D.`uuname -l` -pD. -n$deadtime
uuclean -d/usr/spool/uucp/D.`uuname -l`X -pD. -n$deadtime
.DE
.IP
In this example,
Audit files, Lock files, System Status files, temp files, and \fIuuxqt\fP
output files are cleaned up every 72 hours (3 days).
(\fBLTMP.\fP files are temporary files created by the lock mechanism;
they are rarely around for more than a few seconds.
Note, the above assumes that the
.B LCK
subdirectory is being used.)
All normal data files are cleaned up every 24 * 7 hours, or every 7 days.
.IP -
Shuffle the log files.
At the very least, LOGFILE should be moved to LOGFILE.old, and SYSLOG moved
to SYSLOG.old.
Busy sites may want to use \fIcompress\fP(1) to squeeze down the old files.
.IP -
Use \fIfind\fP(1) to clean up the
.B /usr/spool/uucppublic
directory.
If left unattended, garbage will gradually accumulate there until it fills
the file system.
.RE
.SH
Weekly
.LP
Small sites with very little traffic may chose to shuffle the log files once
per week, instead of once per day.
The weekly script should, like the daily script, be run early in the morning.
.\"===========================================================================
.NH 2
Connecting new systems
.LP
When first connecting a new machine to a \*(Uu network,
it is useful to try and establish a connection with
\fItip\fR or \fIcu\fR first.
The \*(Uu administrator will quickly become aware of any special facilities
that are going to be required,
for example:
What lines and modems are to be used?
Is the connection through different hardware and carriers?
Does the remote system care about parity?
What speed lines are being used and do they cycle through several speeds?
Is there a line switch front end that will require special login dialogue in
\fBL.sys\fP?
.LP
Once a successful login is achieved ``by hand,'' the administrator should
have enough information to allow the correct setup of the \fIcontrol\fR files
in
.B /usr/lib/uucp.
.LP
The \*(Uu administrator should then
negotiate with the remote site's \*(Uu administrator
as to who (if anyone) will do polling and when.
Both administrators must set up the relevant accounts and passwords.
The local administrator should
decide on what permissions and security precautions are to be observed.
Testing time and facilities will need to be arranged
to complete initial connection testing between the systems.
.\"============================================================================
.NH 2
Miscellaneous Security Issues
.LP
The \*(Uu system, left unrestricted,
will let any outside user execute any commands
and copy any files that are accessible
to the
.B uucp
login user.
It is up to the individual sites to be aware of this and
apply the protections that they feel are necessary.
.PP
There are several security features available aside from the
normal file mode protections.
These must be set up by the installer of the
\*(Uu
system.
.IP - 3
The login for uucp does not get a standard shell.
Instead, 
.I uucico
is started.
Therefore, the only work that can be done is through
.I uucico .
.IP -
A path check is done on file names that are to be sent
or received.
.I USERFILE
supplies the information for these checks.
.I USERFILE
can also be set up to require call-back
for certain login-ids.
(See the description of 
.I USERFILE
above.)
.IP -
A conversation sequence count can be set up so
that the called system
can be more confident that the caller
is who he says he is.
.IP -
.I Uuxqt
is restricted via the
.I L.cmds
file to a small list of commands that it
will execute.
A ``PATH'' shell statement is prepended to the command
line as specified in the
.I L.cmds
file.
The administrator may modify the list or remove the
restrictions as desired.
.IP -
All the \*(Uu utilities except \fIuudecode\fP, \fIuuencode\fP,
and \fIuusend\fP should be owned by the
.B uucp
login with the ``setuid'' bit set and only execute
permissions (e.g. mode 04111).
This will prevent outsiders from modifying the programs
to get at a standard shell with a
.B uucp
login.
Optionally, the utilities may belong to group \fBdaemon\fP and be given
``setgid'' permissions (mode 06111).
\fIUuxqt\fP should only permit other \*(Uu programs to execute it;
its mode should be 04100 or 06110.
.IP -
The \fIcontrol\fP files \fIL.sys\fP, \fIUSERFILE\fP, and \fISQFILE\fP
contain highly sensitive information.
They should be owned by the
.B uucp
login, with read and write permission granted only to the owner (mode 0600).
.\"===========================================================================
.\" SECTION 7: UUCP SOURCE INSTALLATION
.\"===========================================================================
.ne 5
.NH
INSTALLING THE \*(UU SYSTEM
.PP
The source for the \*(UU system resides in the
.B /usr/src/usr.bin/uucp
directory.
The README file includes complete instructions on how to rebuild the
\*(Uu system from source.
.LP
For most environments, only two files will need to be modified:
.B uucp.h
includes a large number of tunable system-dependent parameters,
including operating system type, devices to be supported,
and a variety of optional features.
The
.B Makefile
may also have to be modified,
particularly if you chose to keep certain files in different
directories from usual.
.\"===========================================================================
.\" SECTION 8: ACKNOWLEDGMENTS
.\"===========================================================================
.ne 5
.NH
ACKNOWLEDGEMENTS
.PP
4.3BSD UUCP was a group development effort, involving the contributed work
of over one hundred members of the USENET community.
We're extremely grateful to them all.
.LP
Special thanks go to the following individuals, whose contributions were
especially valuable:
.RS
.IP - 2
Rick Adams (Center for Seismic Studies) coordinated the 4.3BSD UUCP release,
incorporating (and often correcting) hundreds of bug fixes that
were posted on the USENET and mailed to him directly.
Rick also managed to find time to add many enhancements
and corrections of his own.
.IP -
Tom Truscott (Research Triangle Institute) and Bob Gray (then with
PAR Tech Corp, now at Univ of Colorado)
coordinated the 4.2BSD UUCP release, which was also a group effort.
Tom has continued to provide enhancements and fixes in 4.3BSD.
.IP -
Guy Harris (then with Computer Consoles, Inc., now with Sun Microsystems)
contributed many general bug fixes;
in particular, he was the first to isolate the infamous 4.2BSD ``TIMEOUT'' bug.
.IP -
Lou Salkind (New York University) wrote the \fIuuq\fP utility.
.IP -
James Bloom (U.C. Berkeley) isolated a major
day-one bug in the \fBg\fP-protocol driver
that had eluded many people's attempts to squash it.
.IP -
Piet Beertema (Centrum voor Wiskunde en Informatica, Amsterdam) wrote the
\fBf\fP-protocol to support ``mostly error-free links'';
Robert Elz (University of Melbourne) modified the protocol
specifically for X.25/PAD.
.IP -
Peter Honeyman (Princeton) assisted Rick by providing information on the
facilities provided in HoneyDanBer UUCP;
Rick then added many HDB-compatibility features and HDB-like
extensions to 4.3BSD UUCP.
.IP -
Ross Green (U.C. Berkeley) produced the first revision of this chapter,
updating the aging Nowitz document to more closely reflect reality.
.RE
.LP
Thanks again to everyone who contributed.
Berkeley UUCP continues to be a product of its own users,
and would not exist as it does today without them.
