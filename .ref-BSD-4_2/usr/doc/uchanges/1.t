.SH
.ce
.LG
Notable improvements
.SM
.sp
.IP \s+1\(bu\s0
The file system organization has been redesigned to provide at least
an order of magnitude improvement in disk bandwidth.
.IP \s+1\(bu\s0
The system now provides full support for the DOD Standard TCP/IP
network communication protocols.   This support has been integrated
into the system in a manner which allows the development and
concurrent use of other communication protocols.  Hardware support
and routing have been isolated from the protocols to allow sharing
between varying network architectures.  Software support is
provided for 10
different hardware devices including 3 different 10 Mb/s Ethernet
modules.
.IP \s+1\(bu\s0
A new set of interprocess communication facilities has
replaced the old multiplexed file mechanism.
These new facilities allow unrelated processes
to exchange messages in either a connection-oriented
or connection-less
manner.  The interprocess communication facilities have been integrated
with the networking facilities (described above) to provide a single user
interface which may be used in constructing applications which operate
on one or more machines.
.IP \s+1\(bu\s0
A new signal package which closely models the hardware interrupt facilities
found on the VAX replaces the old signals and jobs library of 4.1BSD.
The new signal package provides for automatic masking of signals,
sophisticated signal stack management, and reliable protection of critical
regions.
.IP \s+1\(bu\s0
File names are now almost arbitrary length (up to 255 characters) and
a new file type, symbolic link, has been added.  Symbolic
links provide a ``symbolic referencing'' mechanism similar to that found
in Multics.  They are interpolated during pathname expansion and allow users
to create links to files and directories which span file systems.  
.IP \s+1\(bu\s0
The system supports advisory locking on files.  Files can have
``shared'' or ``exclusive'' locks applied by processes.  Multiple
processes may apply shared locks, but only one process at any
time may have an exclusive lock on a file.  Further, when an exclusive
lock is present on a file, shared locks are disallowed.  Locking
requests normally block a process until they can be completed, or they
may be indicated as ``non-blocking'' in which case an error is returned
if the lock can not be immediately obtained.
.IP \s+1\(bu\s0
The group identifier notion has been extended to a ``group set''.  When
users log in to the system they are placed in all their groups.  Access
control is now done based on the group set rather than just a single group
id.  This has obviated the need for the newgrp command.
.IP \s+1\(bu\s0
Per-user, per-filesystem disk quotas are now part of the system.  Soft and
hard limits may be specified on a per user and per filesystem basis to control
the number of files and amount of disk space allocated to a user.  Users
who exceed a soft limit are warned and if, after three login sessions, their
disk usage has not dropped below the soft limit, their soft limit is treated
as a hard limit.  Utilities exist for the creation, maintenance, and reporting
of disk quotas.
.IP \s+1\(bu\s0
System time is now available in microsecond precision and millisecond
accuracy.  Users are
provided with 3 high-resolution timers which may be set up to automatically
reload on expiration.  The timers operate in real time, user time, and
process virtual time (for profiling).  All statistics and times returned
to users are now given in a standard format with seconds and microseconds
separated.  This eliminates program dependence on the line clock frequency.
.IP \s+1\(bu\s0
A new system call to rename files in the same file system has been
added.  This call eliminates many of the anomalies which could occur
in older versions of the system due to lack of atomicity in removing
and renaming files.
.IP \s+1\(bu\s0
A new system call to truncate files to a specific length has been added.
This call improves the performance of the Fortran I/O library.
.IP \s+1\(bu\s0
Swap space configuration has been improved by allowing multiple swap
partition of varying sizes to be interleaved.  These partitions are
sized at boot time to minimize configuration dependencies.
.IP \s+1\(bu\s0
The Fortran 77 compiler and associated I/O library have undergone
extensive changes to improve reliability and performance.  Compilation may,
optionally, include optimization phases to improve code density and
decrease execution time.
.IP \s+1\(bu\s0
A new symbolic debugger, dbx, replaces the old symbolic debugger
sdb.  Dbx works on both C and Fortran 77 programs and allows users
to set break points and trace execution by source code line numbers,
references to memory locations, procedure entry, etc.  Dbx allows
users to reference structured and local variables using
the program's programming language syntax.
.IP \s+1\(bu\s0
The delivermail program has been replaced by sendmail.  Sendmail
provides full internetwork routing, domain style naming 
as defined in the DARPA Request For Comments document #833,
and eliminates the compiled in
configuration database previously used by delivermail.
Further, sendmail
uses the DARPA standard Simple Mail Transfer Protocol (SMTP)
for mail delivery.
.IP \s+1\(bu\s0
The system contains a new line printer system.  Multiple
line printers and spooling queues are supported through a printer
database file.  Printers on serial lines, raster printing devices,
and laser printers are supported through a series of filter programs
which interface to the standard line printer ``core programs''.
A line printer control program, lpc, allows printers and printer queues
to be manipulated.  Spooling to remote printers is supported in a
transparent fashion.
.IP \s+1\(bu\s0
Cu has been replaced by a new program tip.  Tip
supports a number of auto-call units and allows destination
sites to be specified by name rather than phone number.  Tip also
supports file transfer to non-UNIX machines and can be used with
sites which require half-duplex and/or odd-even parity.
.IP \s+1\(bu\s0
Uucp now supports many auto-call units other than the
DN11.  Spooling has been reorganized into multiple directories
to cut down on system overhead.  Several new utilities and
shell scripts exist for use in adminstrating uucp traffic.
Operation has been greatly improved by numerous bug fixes. 
.br
.ne 10
.LP
.LP
