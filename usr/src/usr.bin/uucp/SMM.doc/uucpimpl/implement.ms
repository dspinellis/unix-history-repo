.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)implement.ms	6.2 (Berkeley) %G%
.\"
.TL
Installation and Operation of UUCP
.br
4.3BSD Edition
.AU
D. A. Nowitz
.AI
.MH
.AU
Ross Green
.AI
Computer Systems Research Group
Computer Science Division
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, CA  94720
.AB
Uucp is a series of programs designed to permit communication between
.UX
systems using either dial-up or
hardwired communication lines.
It is used for file transfers and remote command execution.
The first version of the system was designed and implemented
by M. E. Lesk (SMM:21).
.LP
There have been many changes to the implementation of UUCP
since the release of 4.2BSD.
Many problems been fixed,
and several improvements to provide greater throughput have been incorporated.
A number of new features and facilites have been added,
these include:
.IP \(bu 3
Improved administration.
.IP \(bu 3
Extended modem support.
.IP \(bu 3
New transfer protocols
.IP \(bu 3
Security enhancements.
.PP
The first part of this document gives a detailed 
description of the use of UUCP.
The rest of the document indicates the changes that have been made to UUCP,
and provides an update on the installation and implementation details.
It is for use by an administrator/installer of the system;
it is not meant as a user's guide.
.sp
.LP
Revised 
.AE
.LP
.OH 'Installation and Operation of UUCP''SMM:9-%'
.EH 'SMM:9-%''Installation and Operation of UUCP'
.NH
Uucp Implementation Description
.PP
Uucp is a batch type operation.
Files are created in a spool directory for processing
by the uucp demons.
There are three types of files used for the execution
of work.
.I Data\ files
contain data for transfer to remote systems.
.I Work\ files
contain directions for file transfers between systems.
.I Execution\ files
are directions for
.UX
command executions which
involve the resources of one or more systems.
.LP
The uucp system consists of ten primary and four
secondary programs.
These programs are summaried in section 9.
The three most important primary programs are:
.RS
.IP uucp 10
This program creates work and gathers data files in the spool directory
for the transmission of files.
.IP uux
This program creates work files, execute files and gathers data files for the remote execution of
.UX
commands.
.IP uulog
This program updates the log file with new entries
and reports on the status of uucp requests.
.RE
.ne 10
.LP
The three most important secondary programs are:
.RS
.IP uucico 10
This program executes the work files for data transmission.
.IP uuxqt
This program executes the execution files for
.UX
command execution.
.IP uuclean
This program removes old files from the spool directory.
.RE
.LP
The next six sections of this paper will describe the operation
of each program.
the installation of the system,
the security aspects of the system,
the files required for execution,
and the administration of the system.
.NH
Uucp - UNIX to UNIX File Copy
.LP
The
.I uucp
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
may contain the prefix
.I system-name!
which indicates the system on which the file
or files reside
or where they will be copied.
.LP
The options interpreted by
.I uucp
are:
.RS
.IP \-d 10
Make directories when necessary for copying the file.
.IP \-c
Don't copy source files to the spool directory,
but use the specified source when the actual
transfer takes place.
.IP \-g\fIletter\fR
Put
.I letter
in as the grade in the name of the work file.
(This can be used to change the order of work for a particular
machine.)
.IP \-m
Send mail on completion of the work.
.LP
The following options are used primarily for debugging:
.IP \-r 10
Queue the job but do not start
.I uucico
program.
.IP \-s\fIdir\fR
Use directory
.I dir
for the spool directory.
.IP \-x\fInum\fR
.I Num
is the level of debugging output desired.
.RE
.LP
The destination may be a directory name,
in which case the file name is taken from the last part of the
source's name.
The source
name may contain special shell characters
such as ``\fI?*[]\fR''.
If a source argument has a
.I system-name!
prefix for a remote system,
the file name expansion will be done on the remote system.
.LP
The command
.IP "" 12
uucp\ \ *.c\ \ usg!/usr/dan
.LP
will set up the transfer of all files whose names end with ``.c''
to the ``/usr/dan'' directory on the``usg'' machine.
.LP
The source and/or destination names may also contain a
.I ~user
prefix.
This translates to the login directory on
the specified system.
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
Receive files from other systems.
.IP [3]
Send files to a remote systems.
.IP [4]
Send files from remote systems
to another remote system.
.IP [5]
Receive files from remote systems when the source
contains special shell characters as
mentioned above.
.RE
.LP
After the work has been set up in the spool directory,
the
.I uucico
program is started to try to contact the other
machine to execute the work (unless the \-r option
was specified).
.SH
Type 1
.LP
A
.I cp
command is used to do the work.
The
.I \-d
and the
.I \-m
options are not honored in this case.
.SH
Type 2
.LP
A one line
.I "work file"
is created for each file requested and put in the spool directory
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
The full path-name of the destination file.
If the
.I ~user
notation is used, it will be immediately
expanded to be the login directory for the user.
.IP [4]
The user's login name.
.IP [5]
A ``\-'' followed by an option list.
(Only the \-m and \-d options will appear in this list.)
.RE
.KS
.SH
Type 3
.LP
For each source file, a
.I "work file"
is created and the source file is copied into a
.I "data file"
in the spool directory.
(A ``\-c'' option on the
.I uucp
command will prevent the
.I "data file"
from being made.)
In this case, the file will be transmitted from
the indicated source.)
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
A ``\-'' followed by an option list.
.IP [6]
The name of the
.I "data file"
in the spool directory.
.IP [7]
The file mode bits of the source file
in octal print format
(e.g. 0666).
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
.NH
Uux - UNIX To UNIX Execution
.LP
The
.I uux
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
All special shell characters such as ``<>|^'' must be quoted
either by quoting the entire command-string
or quoting the character as a separate argument.
Within the command-string, the command and file names may
contain a
.I system-name!
prefix.
All arguments which do not contain a ``!'' will not
be treated as files.
(They will not be copied to the execution machine.)
The ``\-'' is used to indicate that the standard input
for
.I command-string
should be inherited from the standard input
of the
.I uux
command.
The options, essentially for debugging, are:
.RS
.IP \-r 10
Don't start
.I uucico
or
.I uuxqt
after queuing the job;
.IP \-x\fInum\fR
Num is the level of debugging output desired.
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
This file is either put in the spool directory
for local execution or sent to the remote system using
a generated send command (type 3 above).
.LP
For required files which are not on the execution machine,
.I uux
will generate receive command files (type 2 above).
These command-files will be put on the execution machine and
executed by the
.I uucico
program.
(This will work only if the local system has permission
to put files in the remote spool directory as controlled
by the remote
.I USERFILE .
)
.LP
The
.I "execute file"
will be processed
by the
.I uuxqt
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
are the requester's login name and system.
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
The standard input is either specified by a ``<'' in the
command-string or inherited from the standard input of the
.I uux
command if the ``\-'' option is used.
If a standard input is not specified,
``/dev/null'' is used.
.SH
Standard Output Line
.IP
O\ \ file-name\ \ system-name
.LP
The standard output is specified by a ``>'' within the
command-string.
If a standard output is not specified,
``/dev/null'' is used.
(Note \- the use of ``>>'' is not implemented.)
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
header file.
In addition, a shell ``PATH'' statement is prepended
to the command line as specified in the
.I uuxqt
program.
.LP
After execution, the standard output is copied or set up to be
sent to the proper place.
.RE
.NH
Uulog - Uucp Log Inquiry
.LP
The
.I uucp
programs create individual
log files for each program invocation.
Periodically,
.I uulog
may be executed to prepend these files to the
system logfile.
This method of logging was chosen to minimize file
locking of the logfile during program execution.
.LP
The
.I uulog
program merges the individual
log files and outputs specified log entries.
The output request is specified by the use of the
following options:
.RS
.IP \-s\fIsys\fR 9
Print entries where
.I sys
is the remote system name;
.IP \-u\fIuser\fR
Print entries for user
.I user.
.RE
.LP
The intersection of lines satisfying the two options is output.
A null
.I sys
or
.I user
means all system names or users respectively.
.NH
Uucico - Copy In, Copy Out
.LP
The
.I uucico
program will perform the following major functions:
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
by a system daemon,
.IP b)
by one of the
.I
uucp, uux, uuxqt
.R
or
.I uucico
programs,
.IP c)
directly by the user (this is usually for testing),
.IP d)
by a remote system.
(The uucico program should be specified as the ``shell''
field in the ``/etc/passwd'' file for the ``uucp'' logins.)
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
.IP \-r1 10
Start the program in
.I MASTER
mode.
This is used when
.I uucico
is started by a program or ``cron'' shell.
.IP \-s\fIsys\fR
Do work only for system
.I sys.
If
.I \-s
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
for the spool directory.
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
The names of the work related files in the spool directory have format
.IP
type . system-name grade number
.LP
where:
.IP
.I Type
is an upper case letter,
(
.I C
-\ copy command file,
.I D
-\ data file,
.I X
-\ execute file);
.IP
.I System-name
is the remote system;
.IP
.I Grade
is a character;
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
spool directory for
.I "work files"
(files with prefix ``C.'').
A list is made of all systems to be called.
.I Uucico
will then call each system and process all
.I "work files" .
.SH
Call Remote System
.LP
The call is made using information from several
files which reside in the uucp program directory.
At the start of the call process, a lock is
set to forbid multiple conversations 
between the same two systems.
.LP
The system name is found in the
.I L.sys
file.
The precise format of the
.I L.sys
file is described in section 10, ``System File Details''.
The information contained for each system is;
.RS
.IP [1]
system name,
.IP [2]
times to call the system
(days-of-week and times-of-day),
.IP [3]
device or device type to be used for call,
.IP [4]
line speed,
.IP [5]
phone number if field [3] is
.I ACU
or the device name (same as field [3])
if not
.I ACU,
.IP [6]
login information (multiple fields),
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
[3] and [4] until the call is made, or no more
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
.I L.sys )
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
where code is either a one character
protocol letter or
.I N
which means there is no common protocol.
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
has been specified by the ``\-r1'' uucico option.)
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
command,
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
requested file/directory using the
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
the transferred file will be in the spool
directory with a name beginning with ``TM'.)
The requests and results are logged on both systems.
.LP
The hangup response is determined by the
.I SLAVE
program by a work scan of the spool directory.
If work for the remote system exists in the
.I SLAVE's
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
.I \-s
option was specified.
.LP
.NH
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
The program scans the spool directory for
.I
execute files
.R
(prefix ``X.'').
Each one is checked to see if all the required files are available and
if so, the command line or send line is executed.
.LP
The
.I
execute file
.R
is described in the ``Uux''
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
.NH
Uuclean - Uucp Spool Directory Cleanup
.LP
This program is typically started by the daemon, once a day.
Its function is to remove files from the spool directory which
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
.NH
Changes to the UUCP Implementation
.PP
The demands placed on UUCP networking,
and new technology,
have prompted several changes and improvements to the UUCP software.
Such things as low cost, autodial, autoanswer, high speed modems,
and the availability of X.25 and TCP/IP as carriers,
have encouraged new facilities in UUCP to be developed.
.PP
The following areas have been changed between the 4.2 and 4.3 BSD releases.
.IP \(bu 3
General fixes and performance improvements.
.IP \(bu 3
Administration control facilites.
.IP \(bu 3
Modem and autodialer support has been extended.
.IP \(bu 3
New protocols for different transport media.
.IP \(bu 3
Security enhancements.
.SH
Fixes and performance improvements.
.PP
Many of the fixes related to portability considerations,
and improvements as provided by the USENET community.
.PP
The \fIsitename\fR truncation length as been extended to 14 characters from
the original 7 characters.
This maintains compatibility with the current System V version of UUCP.
.PP
An effort as been made to improve the overall performance of the UUCP system
by organizing its workload in a more sensible way.
For example the program \fIuucico\fR will not resend files it has already sent,
when the files are specified in one ``C.'' file.
.SH
Administration and control facilities.
.PP
There is a new program, \fIuuq\fR,
to give more descriptive information on status of jobs in the UUCP spool queue.
It also allows the user to delete spooled requests,
still in the queue.
.PP
In the past, on large UUCP sites,
the spool directory could grow large with many files
within the ``/usr/spool/uucp'' directory.
To help the UUCP administrator control the system,
a number of subdirectories have been created to easy this congestion.
.PP
The system status ``STST'' files are kept in a subdirectory.
.PP
Corrupted ``C.'' and ``X.'' files,
that could not be processed,
are placed in the ``CORRUPT'' subdirectory,
instead of just exiting.
.PP
Lock files may be kept in a subdirectory,
``LCK'',
if desired.
.PP
If an ``X.'' request fails,
the notification is returned to the originator of the request,
instead of ``uucp'' on the previous system. 
.PP
There is a new \fIsystem\fR file, ``L.aliases'',
that may be used when a site changes it's name.
All the utilities,
\fIuucp\fR, \fIuux\fR, \fIuucico\fR, etc.,
all check ``L.aliases''.
.SH
Modem and autodialer support:
.PP
In a short period of time,
there has been a big increase in the transfer rates and capabilites
that are being provided with modern modems.
Most modems will allow several combinations of baud rate,
and provide autodial and autoanswer facilites as well.
.PP
Most sites will have but a few modems,
they are therefore a precious resource an effort has been made to use them
to maximum potential.
The \fIuucico\fR program now has code to dial in and out on the same device,
if that modem as both autodial and autoanswer provision.
.PP
There is a new dialing facility \fIacucntrl\fR that has been built to handle
some of the changes in modem technology.
There are a number of new modems and autodialers that can now be handled.
Here is a list of some of the new devices:
Racal Vadic 212,
Racal Vadic 811 dialer with 831 adaptor,
Racal Vadic 820 dialer with 831 adaptor,
Racal Vadic MACS 811 dialer with 831 adaptor,
Racal Vadic MACS 820 dialer with 831 adaptor,
Dec DF112,
Novation,
Penril,
Hayes 2400 Smartmodem,
Concord Data Systems CDS 224,
ATT 2224 2400 baud modem.
.PP
It now correctly does the closing hangup sequence.
.PP
It will try up to TRYCALLS to dial a site
instead of one try for each dialer.
.SH
New protocols for different transportation mediums
.PP
The UUCP software has had provision for different protocols
to be used in the sending and receiving of data,
but originaly only one was implemented
and this is the one that is largely used throughout the UUCP community.
The standard `g' protocol,
has a maxium throughput around 9000 baud,
regardless of the physical medium.
The use of checksums and the like are of little use when the protocol
is bounded within another protocol such as TCP or X.25.
It is a waste of time and cpu resources to calculate CRC's
when the carrier already provides error free transmissions.
The UUCP system did not utilize LAN's and high speed carriers well.
Two new protocols have been added
to provide for efficient use of new carrier facilities.
The protocols now available to UUCP are:
.DS
`t' protocol, optimised for use on TCP/IP carriers.
`f' protocol, optimised for use on X.25 PAD carriers.
`g' protocol, standard UUCP protocol used for dialup or hardwired lines.
.DE
.PP
The `t' protocol presumes an error free channel,
and is essentially the `g' protocol without the checksumming and packetizing.
Other changes where necessary to run UUCP on top of a TCP carrier,
the code could not do ioctl's on sockets that are connections to remote hosts.
.PP
The `f' protocol relies on flow control of the data stream.
It is meant for working over links that can be guaranteed to be errorfree,
specifically X.25/PAD links.
A checksum is calculated over a whole file only.
If a transport fails the receiver can request retransmissions.
This protocol uses a 7-bit datapath only,
so it may be used on carriers that are not 8-bit transparent.
.PP
As well as adding new protocols,
the existing `g' protocol code has been cleaned up.
.SH
Changes to \fIuucico\fR
.PP
You can specify a maximum grade to send either
on the command line using (-gX)
or in the ``L.sys'' file
.DS
Any/C|Evening will only send grade C,
.DE
usually mail,
or higher during the day and will send everything in the evening
See Figure 5.
.SH
UUCP System files.
.PP
The \fIsystem\fR files in the ``/usr/lib/uucp'' directory can contain comments,
by putting a `#' as the first character on a line.
Lines may be continued by placing a `\\' as the last character of a line.
This is helpful in making an understandable \fIL.sys\fR file.
.PP
Some new options are available to \fIuucico\fR, these include:
.RS
.IP -R
This flag reverses \fIuucico\fR role.
Let the remote system be master first instead of being the slave.
.IP -L
\fIuucico\fR will only call ``local'' sites.
Local sites are those sites having one of
\fBLOCAL\fR, \fBTCP\fR or \fBDIRECT\fR in the CALLER field of ``L.sys''.
.RE
.PP
If ``/etc/nologin'' is present,
usually created by \fIshutdown\fR,
\fIuucico\fR and \fIuuxqt\fR will gracefully exit,
instead of getting killed off when the system goes down.
.PP
Does an exponential backoff on retry time if call fails
instead of always waiting the default 5 minutes.
The default may be overridden by adding ",TIME" to the time field in ``L.sys''.
.DS
ucbvax Any,2
.DE
will use a default retry time of 2 minutes.
.PP
If \fIuucico\fR receives a SIGFPE while running,
it will toggle debugging on and off.
.PP
It will not send files to remote system if the remote system
is returning an out of temp space error.
.PP
Add ABORT sequence to the expect/send sequence so it does not have
to wait for timeout if cannot get through a dataswitch.
See figure X.
This example will only call noao in the evening.
It will expect nothing,
then wait 1 second (\ed),
and then send a carriage return.
Look for CLASS, then send NOAOUUCP.
From then on,
if it sees the word Down before finishing logging in,
it will hang up immediately.
In the mean time,
it looks for GO,
when received,
delay 1 second and then send a CR.
Look for ``ogin:'', etc.
This abort sequence is useful
if you must go through a dataswitch to get to the computer.
.PP
The time field in the ``L.sys'' file now handles
"Evening" and "Night" in addition to:
.DS
Any, Mo, Tu, We, Th, Fr, Sa, Su.
.DE
Evening and Night are defined to be:
.DS
Evening = Wk1700-0800|Sa|Su
Night = Any2300-0800|Sa|Su0800-1700
.DE
.PP
The expect/send code now handles '\e' sequences:
.RS
.IP "s"
indicates a space
.IP "d"
indicates a delay 1 second
.IP "r"
indicates a carriage return with no linefeed
.IP "b"
indicates a break
.IP "c"
indicates do not send a CR after these characters
.IP "xxx"
the octal character xxx (e.g. \es == \e040
.RE
.PP
The file \fIL-devices\fR now handles "chat" scripts,
to help get through local port selectors and smart modems.
This helps keep \fIL.sys\fR readable but increases functionality.
See figure 4.
.PP
For compatibility with System V UUCP system,
in the Date fields of ``L.sys'':
.DS
`|' was changed to `,' (| is supported, but not encouraged)
`,' was changed to `;' (to allow , to be the date seperator)
.DE
.PP
For Honey DanBer compatibility,
the Grade flag is now passed as:
.DS
-vgrade=X
.DE
instead of the old -pX
.PP
You can specify a time for the expect send sequences with ~ instead
of getting the default MAXMSGTIME.
For example:
.DS
system Any ACU 1200 1234567 ogin~20--ogin~10--ogin user password pw
.DE
will look for ``ogin'' for 20 seconds,
then send CR,
look for ``ogin'' for 10 seconds,
send a CR and look for ``ogin'' for MAXMSGTIME seconds
.PP
Added code to support GTEs PC Pursuit service.
It's mainly the handling of the dialback they use.
.PP
Added time "NonPeak" for Tymnet/Telenet services that charge lower rates
from 6pm-7am M-F and Sat & Sun.
.SH
Security enhancements.
.PP
Running \fIuucico\fR with debugging turned on,
requires ``L.sys'' to have read access.
.NH
The UUCP system.
.SH
Names
.PP
The naming of a site is important since it provides a means of
identifying a machine,
and consequently,
that machine's users.
There are two names used within the UUCP system,
these are the \fIloginname\fR and the \fIsitename\fR.
.PP
It is important that the \fIloginname\fR used by a remote machine
to call into a local machine
is not the same as that of a normal user of the local machine.
It is common for many remote machines to have the same \fIloginname\fR,
such as uucp or nuucp.
.PP
Each machine in a UUCP network is given a unique \fIsitename\fR.
The \fIsitename\fR identifies the calling machine to the called machine.
A \fIsitename\fR can be up to 14 characters in length.
It is useful to have a \fIsitename\fR that is unique in the first 7 characters,
to be compatible with earlier implementaions of UUCP.
It is desirable that the \fIsitename\fR will convey this uniqueness
and perhaps a real world identity to the rest of the network.
.PP
The \fIsitename\fR and the \fIloginname\fR are not the same thing!
They may have the same values,
but this is purely a decision of the UUCP administrators
when the network connections are set up.
.SH
The UUCP system organization.
.PP
There are several directories that are used by the UUCP system as distributed.
These are:
.RS
.IP src 12
(/usr/src/usr.bin/uucp)
This directory contains the source files for generating the UUCP system.
.IP system 12
(/usr/lib/uucp)
This is the directory where system binaries,
and system control files reside.
.IP spool 12
(/usr/spool/uucp)
This is the spool directory used to store transfer requests and data.
.IP commands 12
(/usr/bin)
This is the directory where the programs used by users will be kept.
.RE
.SH
The system directory
.PP
The following files are required for execution,
and should reside in the \fIsystem\fR directory,
/usr/lib/uucp.
.RS
.IP L-devices 15
This file contains entries for all devices that are to be used by UUCP.
.IP L-dialcodes 15
This file contains dialing abbreviations.
.IP L.aliases 15
Used to provide sitename aliases.
.IP L.cmds 15
This file contains commands that can be used by a remote site.
.IP L.sys 15
Contains site connection information for each system that can be called.
.IP SEQF 15
The sequence numbering and check file.
.IP USERFILE 15
Remote system access rights.
.IP acucntrl 15
The program used to control calling remote systems.
.IP uucico 15
The actual transfer program.
.IP uuclean 15
A utility to cleanup after UUCP.
.IP uuxqt 15
Allows UUCP to execute commands.
.RE
.SH
The command directory
.PP
The \fIcommand\fR directory, /usr/bin,
contains the following user available commands:
.RS
.IP uucp 15
spools a UNIX to UNIX file-copy request.
.IP uux 15
spools a request for remote execution.
.IP uusend 15
provides a facility to transfer binary files using mail.
.IP uuencode 15
binary file encoder (for \fIuusend\fR)
.IP uudecode 15
binary file decoder (for \fIuusend\fR)
.IP uulog 15
reports from log files.
.IP uusnap 15
provides a snap-shot of \fIuucp\fP activity.
.IP uupoll 15
polls remote system until an answer is received.
.IP uuname 15
prints a list of known remote UUCP hosts.
.IP uuq 15
reports information from the UUCP spool queue.
.RE
.SH
The spool directory
.PP
The \fIspool\fR directory,
/usr/spool/uucp,
contains the following files and directories:
.RS
.IP C. 15
A directory for command, ``C.'' files.
.IP D. 15
A directory for data, ``D.'', files.
.IP X. 15
A directory for command execution, ``X.'', files.
.IP D.\fImachine\fP 15
A directory for local ``D.'' files.
.IP D.\fImachine\fPX 15
A directory for local ``X.'' files.
.IP CORRUPT 15
A directory for corrupted ``C.'' and ``X.'' files.
.IP ERRLOG 15
A file where internal error messages are collected.
.IP LCK 15
A directory for device and site lock files.
.IP LOG 15
A directory for individual site LOGFILE's.
.IP LOGFILE 15
the log file of UUCP activity.
.IP STST 15
A directory for site STST files.
.IP SYSLOG 15
the log file of UUCP file transfers.
.IP TM. 15
A directory for temporary, ``TM.'', files.
.RE
.PP
There is an additional directory,
/usr/spool/uucppublic,
that is used as a general Public accecs directory for UUCP.
It is not used by UUCP directly
but it is normally the home directory for the UUCP system owner.
Most importantly this directory is owned by uucp,
and the access permissions are 0777,
this usually garrantees a place that files can be sent to,
and retrieved from,
on any site.
.NH
System file details.
.SH
L-devices
.PP
This file contains entries for the call-unit devices and
hardwired connections which are to be used by UUCP.
The special device files are assumed to be in the /dev directory.
.PP
The format for each entry is:
.DS
\fIType Device Useful Class Dialer\fR [\fIChat\fR ...]
.DE
.LP
where;
.IP \fIType\fR 10
is the type of calling to use.
There are several different calling types.
.RS
.br
\fBACU\fR indicates a dialing devices.
.br
\fBDIR\fR is used when a direct connection is used.
.br
\fBDK\fR an AT&T Datakit.
.br
\fBMICOM\fR Micom terminal switch.
.br
\fBPAD\fR X.25 PAD connection.
.br
\fBPCP\fR GTE Telenet PC Pursuit.
.br
\fBSYTEK\fR Sytek high-speed dedicated modem port.
.br
\fBTCP\fR TCP/IP connection.
.RE
.IP \fIDevice\fR 10
is the entry in ``/dev'' corresponding to a real device,
the owner should be uucp.
.IP \fIUseful\fR 10
information to be used by the device,
such as the name of the callunit to be used,
or maybe ``unused''.
.IP \fIClass\fR 10
is the line speed.
.IP \fIDialer\fR 10
is either \fBdirect\fR,
or from the list of available dialers.
The list of available dialers includes:
.RS
.IP \fBDF02\fR 10
DEC DF02 or DF03 modems.
.IP \fBDF112\fR 10
Dec DF112 modems.
Use a \fIDialer\fR field of \fBDF112T\fR to use tone dialing,
or \fBDF112P\fR for pulse dialing.
.IP \fBatt\fR 10
AT&T 2224 2400 baud modem.
.IP \fBcds224\fR 10
Concord Data Systems 224 2400 baud modem.
.IP \fBdn11\fR 10
DEC DN11 Unibus dialer.
.IP \fBhayes\fR 10
Hayes Smartmodem 1200 and compatible autodialing modems.
Use a \fIDialer\fR field of \fBhayestone\fR to use tone dialing,
or \fBhayespulse\fR for pulse dialing.
It is also permissible to include the letters `T' and `P' in the phone number
(in \fIL.sys\fR)
to change to tone or pulse midway through dialing.
(Note that a leading `T' or `P' will be interpreted as a dialcode!)
.IP \fBhayes2400\fR 10
Hayes Smartmodem 2400 and compatible modems.
Use a \fIDialer\fR field of \fBhayes2400tone\fR to use tone dialing,
or \fBhayes2400pulse\fR for pulse dialing.
.IP \fBnovation\fR 10
Novation ``Smart Cat'' autodialing modem.
.IP \fBpenril\fR 10
Penril Corp ``Hayes compatible'' modems.
.IP \fBrvmacs\fR 10
Racal-Vadic 820 dialer with 831 adapter in a MACS configuration.
.IP \fBva212\fR 10
Racal-Vadic 212 autodialing modem.
.IP \fBva811s\fR 10
Racal-Vadic 811s dialer with 831 adapter.
.IP \fBva820\fR 10
Racal-Vadic 820 dialer with 831 adapter.
.IP \fBvadic\fR 10
Racal-Vadic 3450 and 3451 series autodialing modems.
.IP \fBventel\fR 10
Ventel 212+ autodialing modem.
.IP \fBvmacs\fR 10
Racal-Vadic 811 dialer with 831 adapter in a MACS configuration.
.RE
.IP \fIChat\fR
is a send/expect sequence that can be used to talk through dataswitches,
or issue special comands to a device such as a modem.
The syntax is identical to that of the Expect/Send script of \fIL.sys\fR.
The difference is that,
the \fIL-devices\fR script is used before the connection is made,
while the \fIL.sys\fR script is used after.
.SH
L-dialcodes
.PP
This file contains entries with location abbreviations used
in the ``L.sys'' file (e.g. py, mh, boston).
The entry format is:
.DS
abb\ \ dial-seq
.DE
.LP
where;
.RS
.IP abb 12
is the abbreviation,
.IP dial-seq
is the dial sequence to call that location.
.RE
.PP
The line
.IP "" 12
py\ \ 165\-
.LP
would be set up so that entry py7777 would
send 165\-7777 to the dial-unit.
.SH
L.aliases.
.PP
The \fIL.aliases\fR file provides a mapping facility of \fIsitenames\fR.
This facility is useful when \fIsitenames\fR are changed,
and the change is only temporary,
or the change is permanent but it is not widely known by the users of the net.
The format of the file is:
.DS
real_name alias_name
.DE
The ``L.aliases'' file may be used to map hosts with longer names in ``L.sys''
to 7 character names that some hosts send.
This provides a mechanism to handle those sites,
entries should be:
.DS
	fullname 7-char-name
.DE
.SH
L.cmds
.PP
The L.cmds file contains a list of commands that are permitted
for remote execution with \fIuux\fR.
The commands are listed one per line.
Most sites L.cmds will be something like:
.DS
rmail
ruusend
.DE
A line of the form:
.DS
PATH=/bin:/usr/bin:/usr/ucb:/usr/local/bin
.DE
can be used to set a search path.
.SH
L.sys
.PP
Each entry in this file represents one system
which can be called by the local UUCP programs.
The fields are described below.
.DS
\fISitename  Times  Caller  Class  Device\fR  [\fIExpect  Send\fR]....
.DE
.IP \fISitename\fR 10
is the name of the remote system.
Every machine with which this system communicates via UUCP should be listed,
regardless of who calls whom.
Systems not listed in ``L.sys'' will not be permitted a connection.
.PP
.IP \fITimes\fR 10
is a comma-separated list of the times of the day and week that 
calls are permitted to this site.
This can be used to restrict long distance telephone calls
to those times when rates are lower.
List items are constructed as:
.DS
\fIkeyword\fPhhmm\fB-\fPhhmm\fB/\fP\fIgrade\fP\fB;\fP\fIretry_time\fP
.DE
\fIKeyword\fR is required, and must be one of:
.RS
.IP \fBAny\fR 8
Any time, any day of the week.
.IP \fBWk\fR 8
Any weekday. In addition,
\fBMo, Tu, We, Th, Fr, Sa,\fR and \fBSu\fR
can be used for Monday through Sunday, respectively.
.IP \fBEvening\fR 8
When evening telephone rates are in effect,
from 1700 to 0800 Monday through Friday,
and all day Saturday and Sunday.
.B Evening
is the same as
.B Wk1700-0800,Sa,Su .
.IP \fBNight\fR 8
When nighttime telephone rates are in effect,
from 2300 to 0800 Monday through Friday,
all day Saturday, and from 2300 to 1700 Sunday.
.B Night
is the same as
.B Any2300-0800,Sa,Su0800-1700 .
.IP \fBNonPeak\fR 8
This is a slight modification of 
.B Evening .
It matches when the USA X.25 carriers have their lower rate period. This
is 1800 to 0700 Monday through Friday, and all day Saturday and Sunday.
.B NonPeak
is the same as
.B Any1800-0700,Sa,Su .
.IP \fBNever\fR 8
Never call;
calling into this site is forbidden or impossible.
This is intended for polled connections,
where the remote system calls into the local machine periodically.
This is necessary when one of the machines is lacking
either dial-in or dial-out modems.
.PP
The optional \fIhhmm-hhmm\fR
subfield provides a time range that modifies the keyword.
.I hhmm
refers to
.I hours
and
.I minutes
in 24-hour time (from 0000 to 2359).
The time range is permitted to "wrap" around midnight,
and will behave in the obvious way.
It is invalid to follow the \fBEvening\fR, \fBNonPeak\fR,
and \fBNight\fR keywords with a time range.
.PP
The \fIgrade\fR subfield is optional; if present,
it is composed of a `/' (slash) and single character denoting the \fIgrade\fR
of the connection.
Grades are in the range [\fB0-9A-Za-z\fR].
This specifies that only requests of grade \fIgrade\fR
or better will be transferred during this time.
(The grade of a request or job is specified
when it is queued by \fIuucp\fR or \fIuux\fR).
By convention, mail is sent at grade \fBC\fR,
news is sent at grade \fBd\fR,
and uucp copies are sent at grade \fBn\fR.
Unfortunately, some sites do not follow these conventions,
so it is not 100% reliable.
.PP
The \fIretry_time\fR subfield is optional;
it must be preceded by a `;' (semicolon) and
specifies the time, in minutes,
before a failed connection may be tried again.
By default,
the retry time starts at 10 minutes and gradually increases at each failure,
until after 26 tries \fIuucico\fR gives up completely (MAX RETRIES).
If the retry time is too small,
\fIuucico\fR may run into MAX RETRIES too soon.
.RE
.IP \fICaller\fR 10
is the type of device used.
It may be one of the following:
.DS
\fBACU DIR MICOM PAD PCP SYTEK TCP\fR
.DE
the derscriptions in L-devices apply here.
If several alternate ports or network connections should be tried,
use multiple
.I L.sys
entries.
.IP \fIClass\fR 10
is usually the speed (baud) of the device,
typically 300, 1200, or 2400 for \fRACU\fR devices and 9600 for direct lines.
Valid values are device dependent,
and are specified in the ``L-devices'' file.
.PP
On some devices,
the speed may be preceded by a non-numeric prefix.
This is used in ``L-devices''
to distinguish among devices that have identical \fICaller\fR and baud,
but yet are distinctly different.
For example,
1200 could refer to all Bell 212-compatible modems,
V1200 to Racal-Vadic modems,
and C1200 to CCITT modems,
all at 1200 baud.
.PP
On TCP connections,
.I Class
is the port number (an integer number) or a port name from ``/etc/services''
that is used to make the connection.
For standard Berkeley TCP/IP,
UUCP normally uses port number 540.
.IP \fIDevice\fR 10
varies based on the \fICaller\fR field.
For \fBACU\fR devices,
this is the phone number to dial.
The number may include: digits \fB0\fR through \fB9\fR;
.B #
and
.B *
for dialing those symbols on tone telephone lines;
.B -
(hyphen) to pause for a moment, typically two to four seconds;
.B =
(equal sign) to wait for a second dial tone
(implemented as a pause on many modems).
Other characters are modem dependent;
generally standard telephone punctuation characters
(such as the slash and parentheses) are ignored, although
.I uucico
does not guarantee this.
.PP
The phone number can be preceded by an alphabetic
string; the string is indexed and converted through the ``L-dialcodes'' file.
.PP
For \fBDIR\fR devices, the
.I Device
field contains the name of the device in /dev
that is used to make the connection.
There must be a corresponding line in ``L-devices'' with identical
\fICaller, Class\fR, and \fIDevice\fR fields.
.PP
For \fBTCP\fR and other network devices,
.I Device
holds the true network name of the remote system,
which may be different from its UUCP name
(although one would hope not).
.PP
The
.I Expect
and
.I Send
refer to an arbitrarily long set of strings that
alternately specify what to
.I expect
and what to
.I send
to login to the remote system once a physical connection has
been established.  A complete set of expect/send strings is referred
to as an
.IR "expect/send script" .
The same syntax is used in the
.I L\-devices
file to interact with the dialer prior to making a connection;
there it is referred to as a \fIchat script\fR.
The complete format for one
.I expect/send
pair is:
.PP
.DS
\fIexpect\fP\fB~\fP\fItimeout\fP\fB-\fP\fIsend\fP\fB-\fP\fI\
expect\fP\fB~\fP\fItimeout   send\fP
.DE
.PP
.I Expect
and
.I Send
are character strings.
.I Expect
is compared against in coming text from the remote host;
.I send
is sent back when
.I expect
is matched.  By default, the
.I send
is followed by a `\er' (carriage return). If the
.I expect
string is not matched within
.I timeout
seconds (default 45), then it is assumed that the match failed.
The `\fIexpect\fP\fB-\fP\fIsend\fP\fB-\fP\fIexpect\fP' notation 
provides a limited loop mechanism; if the first
.I expect
string fails to match, then the
.I send
string between the hyphens is transmitted, and
.I uucico
waits for the second
.I expect
string. This can be repeated indefinitely. When the last
.I expect
string fails,
.I uucico
hangs up and logs that the connection failed. 
.PP
The timeout can (optionally) be specified by appending the parameter
`\fB~\fP\fInn\fP' to the expect string, when \fInn\fR is the timeout
time in seconds.
.PP
Backslash escapes that may be imbedded in the
.I expect
or
.I send
strings include:
.PP
.DS
\eb	Generate a 3/10 second BREAK.
\eb\fIn\fP	Where \fIn\fP is a single-digit number;
	generate an \fIn\fP/10 second BREAK.
\ec	Suppress the \er at the end of a \fIsend\fP string.
\ed	Delay; pause for 1 second. (\fISend\fR only.)
\er	Carriage Return.
\es	Space.
\en	Newline.
\exxx	Where \fIxxx\fP is an octal constant;
	denotes the corresponding ASCII character.
.DE
.PP
As a special case, an empty pair of double-quotes \fB""\fP in the
.I expect
string is interpreted as ``expect nothing'';
that is, transmit the \fIsend\fR string regardless of what is received.
Empty double-quotes in the \fIsend\fR string
cause a lone `\er' (carriage return) to be sent.
.PP
One of the following keywords may be substituted for the
.I send
string:
.DS
BREAK	Generate a 3/10 second BREAK
BREAK\fIn\fP	Generate an \fIn\fP/10 second BREAK
CR	Send a Carriage Return (same as "").
EOT	Send an End-Of-Transmission character, ASCII \e004.
	Note that this will cause most hosts to hang up.
NL	Send a Newline.
PAUSE	Pause for 3 seconds.
PAUSE\fIn\fP	Pause for \fIn\fR seconds.
P_ODD	Use odd parity on future send strings.
P_ONE	Use parity one on future send strings.
P_EVEN	Use even parity on future send strings. (Default)
P_ZERO	Use parity zero on future send strings.
.DE
.PP
Finally, if the
.I expect
string consists of the keyword
.BR ABORT ,
then the string following is used to arm an abort trap. If that string
is subsequently received any time prior to the completion of the entire
.I expect/send
script, then
.I uucico
will abort, just as if the
script had timed out. This is useful for trapping error messages from
port selectors or front-end processors such as ``Host Unavailable'' or
``System is Down.''
.PP
For example:
.PP
.DS
""  ""  ogin:--ogin:  nuucp  ssword:  ufeedme
.DE
.PP
This is executed as, ``When the remote system answers,
.I expect
nothing.
.I Send
a carriage return.
.I Expect
the remote to transmit the string `ogin:'. If it doesn't
within 45 seconds, send another carriage return.  When it finally does,
.I send
it the string `nuucp'.  Then
.I expect
the string `ssword:'; when that is received,
.I send
`ufeedme'.''
.SH
USERFILE
.PP
This file contains user accessibility information.
It specifies the file system directory trees that are accessible to
local users and to remote systems via UUCP
.PP
Each line in
.I USERFILE
is of the form:
.DS
[\fIloginname\fP]\fB,\fP[\fIsitename\fP] [ \fBc\fP ] \fIpathname\fP \c
[\fIpathname\fP] [\fIpathname\fP]
.DE
.PP
The first two items are separated by a comma;
any number of spaces or tabs may separate the remaining items.
.PP
The \fIloginname\fR
is a login name (from ``/etc/passwd'')
on the local machine.
.PP
The \fIsitename\fR
is the name of a remote machine,
this is the same name used in ``L.sys''.
.PP
The \fIc\fR denotes the optional \fIcallback\fR field.
If a \fBc\fP appears here,
a remote machine that calls in will be told that callback is requested,
and the conversation will be terminated.
The local system will then immediately call the remote host back.
.PP
The \fIpathname\fR
is a pathname prefix that is permissible for this \fIloginname\fR
and/or \fIsitename\fR.
.PP
It accepts the pathnames on the first line that has a null
.I system 
field.
.PP
A line with both \fIloginname\fR and \fIsitename\fR are null,
for example
.DS
,  /usr/spool/uucppublic
.DE
can be used to conveniently specify the paths for both "no match" cases
if lines earlier in ``USERFILE'' did not define them.
.NH
Installing the UUCP system.
.PP
There are several source modifications that may be required
before the system programs are compiled.
.PP
Two files which may require modification,
the ``Makefile'' file and the ``uucp.h'' file.
The following paragraphs describe some of the options
available at build time.
.SH
Uucp.h modifications
.PP
The installer of UUCP may wish to change some of the defines in ``uucp.h'',
some of the interesting defines are mentioned below.
.PP
if \fBDIALINOUT\fR is defined then \fIacucntrl\fR will allow modems to be
used in both directions.
.PP
If \fBDONTCOPY\fR is defined in ``uucp.h'',
\fIuucp\fR will not make a copy of the source file by default.
.PP
if \fBLOCKDIR\fR is defined then lock files
will be stored in the ``/usr/spool/uucp/LCK'' directory.
.PP
If \fBLOGBYSITE\fR is defined,
\fIuucp\fR logging is done with a log file per site,
instead of one LOGFILE.
.PP
If \fBNOSTRANGERS\fR is defined in ``uucp.h'',
the remote site must be in your ``L.sys'' or the call will be rejected.
.SH
Makefile modification
.PP
There are several
.I make
variable definitions which may need modification.
.RS
.IP DESTDIR 12
where all the system will end up,
normal from the root.
.IP LIBDIR 12
${DESTDIR}/usr/lib/uucp, low level binaries, site information, and dialing
information resides here.
.IP BIN 12
${DESTDIR}/usr/bin, the user utilities reside here.
.IP ETC 12
${DESTDIR}/etc
.IP PUBDIR 12
${DESTDIR}/usr/spool/uucppublic, uucp owns this directory.
A place where files can almost always be sent.
.IP SPOOL 12
${DESTDIR}/usr/spool/uucp,
where all commands and data is held until transfers can take place.
.IP XQTDIR 12
${SPOOL}/XTMP, a place where UUCP executeable commands will be kept.
.IP CORRUPT 12
${SPOOL}/CORRUPT, where corrupted ``C.'' and ``D.'' files end up.
.IP AUDIT
${SPOOL}/AUDIT, an audit trail.
.IP LCK 12
${SPOOL}/LCK, a place to keep lock files.
LOG= ${SPOOL}/LOG
.IP STST 12
${SPOOL}/STST, a place to keep ``STST'' files for different systems.
.IP HOSTNAME 12
the machines name.
.IP SUBDIRS 12
There are six subdirs,
not counting XTMP, that may be built under SPOOL,
these are,
``C.'', ``D.\fIHOSTNAME\fRX'', ``D.\fIHOSTNAME\fR'', ``D.'', ``X.'', and ``TM.''.
.RE
.SH
Building the system
.PP
The command
.DS
make
.DE
will compile the entire system.
.PP
The command
.DS
make makedirs
.DE
will build all the directories needed for the system,
giving them appropriate owners and permissions.
.PP
The command
.DS
make install
.DE
.PP
will install the commands in the correct directories,
setting ownership and permissions.
.NH
Connecting new systems to the network.
.PP
When first connecting a new machine to a UUCP network,
it is advisable to try and establish a connection with
\fItip\fR or \fIcu\fR first.
The administrator should then be aware of any special facilities
that are going to be required,
things like;
What lines and modems are to be used?
or
is the connection through different hardware and carriers?
Does the remote system care about parity?
What speed lines are being used and do they cycle through several speeds?
Is there a line switch front end that will require special Chat dialogue in
``L.sys''?
.PP
Once a \fIlogin\fR connection can be completed the Admisitrator should
have enough information to allow the correct setup of the \fIsystem\fR files
in /usr/lib/uucp.
.PP
The UUCP administrator should then
negociate with the remote site UUCP administrator
as to when and who will do polling.
The relevant accounts and passwords must be set up.
Decide on what permissions and security precausions are to be observed.
Arrange testing time and facilites to complete initial connection to the system.
.NH
Security
.PP
The uucp system, left unrestricted,
will let any outside user execute any commands
and copy in/out any file which is readable/writable
by the uucp login user.
It is up to the individual sites to be aware of this and
apply the protections that they feel are necessary.
.PP
There are several security features available aside from the
normal file mode protections.
These must be set up by the installer of the
.I uucp
system.
.IP - 3
The login for uucp does not get a standard shell.
Instead, the
.I uucico
program is started.
Therefore, the only work that can be done is through
.I uucico .
.IP -
A path check is done on file names that are to be sent
or received.
The
.I USERFILE
supplies the information for these checks.
The
.I USERFILE
can also be set up to require call-back
for certain login-ids.
(See the ``Files required for execution'' section
for the file description.)
.IP -
A conversation sequence count can be set up so
that the called system
can be more confident that the caller
is who he says he is.
.IP -
The
.I uuxqt
program comes with a list of commands that it
will execute.
A ``PATH'' shell statement is prepended to the command
line as specifed in the
.I uuxqt
program.
The installer may modify the list or remove the
restrictions as desired.
.IP -
The
.I L.sys
file should be owned by uucp and have mode 0400
to protect the phone numbers and login information
for remote sites.
(Programs uucp, uucico, uux, uuxqt should be also
owned by uucp and have the setuid bit set.)
.NH
Administration
.PP
This section indicates some events and files which must be
administered for the
.I uucp
system.
Some administration can be accomplished by 
.I "shell files"
which can be initiated by
.I crontab
entries.
Others will require manual intervention.
Some sample 
.I "shell files"
are given toward the end of this section.
.SH
SQFILE \- sequence check file
.PP
This file is set up in the
.I program
directory and contains an entry for each remote
system with which you agree to perform conversation
sequence checks.
The initial entry is just the system name of
the remote system.
The first conversation will add two items to the line,
the conversation count, and the date/time of the most
resent conversation.
These items will be updated with each conversation.
If a sequence check fails, the entry will have to
be adjusted.
.SH
TM \- temporary data files
.PP
These files are created in the
.I spool
directory while files are being copied
from a remote machine.
Their names have the form
.IP "" 12
\fBTM\fR.pid.ddd
.PP
where
.I pid
is a process-id and
.I ddd
is a sequential three digit number starting at zero
for each invocation of
.I uucico
and incremented for each file received.

After the entire remote file is received, the
.I TM
file is moved/copied to the requested destination.
If processing is abnormally terminated or the
move/copy fails, the file will remain in the
spool directory.
.PP
The leftover files should be periodically removed;
the
.I uuclean
program is useful in this regard.
The command
.IP "" 12
uuclean\ \ \-pTM
.LP
will remove all
.I TM
files older than three days.
.SH
LOG \- log entry files
.PP
During execution of programs, individual
.I LOG
files are created in the
.I spool
directory with information about
queued requests, calls to remote systems,
execution of
.I uux
commands and file copy results.
These files should be combined into the
.I LOGFILE
by using the
.I uulog
program.
This program will put the new
.I LOG
files at the beginning of the existing
.I LOGFILE.
The command
.IP "" 12
uulog
.LP
will accomplish the merge.
Options are available to print some or all the
log entries after the files are merged.
The
.I LOGFILE
should be removed periodically
since it is copied each time new LOG
entries are put into the file.
.PP
The
.I LOG
files are created initially with mode 0222.
If the program which creates the file terminates normally,
it changes the
mode to 0666.
Aborted runs may leave the files with mode
0222 and the
.I uulog
program will not read or remove them.
To remove them, either use
.I rm ,
.I uuclean ,
or change the mode to 0666 and let
.I uulog
merge them with the
.I LOGFILE .
.SH
STST \- system status files
.PP
These files are created in the spool directory by the
.I uucico
program.
They contain information of failures such as login, dialup or
sequence check and will contain a
.I TALKING
status when to machines are conversing.
The form of the file name is
.IP
\fBSTST\fR.sys
.PP
where
.I sys
is the remote system name.
.PP
For ordinary failures (dialup, login), the file will prevent
repeated tries for about one hour.
For sequence check failures, the file must be removed before
any future attempts to converse with that remote system.
.PP
If the file is left due to an aborted run, it may contain a
.I TALKING
status.
In this case, the file must be removed before a conversation
is attempted.
.SH
LCK \- lock files
.LP
Lock files are created for each device in use (e.g. automatic calling
unit) and each system conversing.
This prevents duplicate conversations and multiple attempts to use the
same devices.
The form of the lock file name is
.IP "" 12
\fBLCK..\fRstr
.LP
where
.I str
is either a device or system name.
The files may be left in the spool directory if runs abort.
They will be ignored (reused) after a time of about 24 hours.
When runs abort and calls are desired before the time limit,
the lock files should be removed.
.SH
Shell Files
.PP
The
.I uucp
program will spool work and attempt to start the
.I uucico
program, but the starting of
.I uucico
will sometimes fail.
(No devices available, login failures etc.).
Therefore, the
.I uucico
program should be periodically started.
The command to start
.I uucico
can be put in a ``shell'' file with a command to merge
.I LOG
files and started by a crontab entry on an hourly basis.
The file could contain the commands
.IP
.I program /uulog
.br
.I program /uucico
\ \ \-r1
.PP
Note that the ``\-r1'' option is required to start the
.I uucico
program in
.I MASTER
mode.
.PP
Another shell file may be set up on a daily basis to remove
.I TM ,
.I ST
and
.I LCK
files
and
.I C.
or
.I D.
files for work which can not be accomplished for
reasons like bad phone number, login changes etc.
A shell file containing commands like
.IP
.I program /uuclean
\ \ \-pTM \-pC. \-pD.
.br
.I program /uuclean
\ \ \-pST \-pLCK \-n12
.LP
can be used.
Note the ``\-n12'' option causes the
.I ST
and
.I LCK
files older than 12 hours to be deleted.
The absence of the ``\-n'' option will use a three day time
limit.
.PP
A daily or weekly shell should also be created
to remove or save old
.I LOGFILE s.
One can use a command like
.IP
mv spool/LOGFILE spool/o.LOGFILE
.SH
Login Entry
.PP
One or more logins should be set up for
.I uucp .
Each of the ``/etc/passwd'' entries should
have the
``\fIprogram\fR/uucico''
as the shell to be executed.
The login directory is normally ``/usr/spool/uucppublic''.
The various logins are used in conjunction with the
.I USERFILE
to restrict file access.
Specifying the
.I shell
argument limits the login to the use of UUCP (\fIuucico\fP) only.
.SH
File Modes
.PP
It is suggested that the owner and file modes of various
programs and files be set as follows.
.PP
The programs
.I uucp ,
.I uux ,
.I uucico
and
.I uuxqt
should be owned by the
.I uucp
login with the ``setuid'' bit set and only execute
permissions (e.g. mode 04111).
This will prevent outsiders from modifying the programs
to get at a standard
.I shell
for the
.I uucp
logins.
.PP
The
.I L.sys ,
.I SQFILE
and the
.I USERFILE
which are put in the
.I program
directory should be owned by
the
.I uucp
login and set with mode 0400.
