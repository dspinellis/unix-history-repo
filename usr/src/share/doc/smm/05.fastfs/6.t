.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)6.t	5.1 (Berkeley) %G%
.\"
.ds RH Software engineering
.NH
Software engineering
.PP
The preliminary design was done by Bill Joy in late 1980;
he presented the design at The USENIX Conference
held in San Francisco in January 1981.
The implementation of his design was done by Kirk McKusick
in the summer of 1981.
Most of the new system calls were implemented by Sam Leffler.
The code for enforcing quotas was
implemented by Robert Elz at the University of Melbourne.
.PP
To understand how the project was done it is necessary
to understand the interfaces that the UNIX system provides to
the hardware mass storage systems.
At the lowest level is a
.I "raw disk."
This interface provides access to the disk as a linear
array of sectors.
Normally this interface is only used by programs that need to
do disk to disk copies or that wish to dump file systems.
However, user programs with proper access rights can also access
this interface.
A disk is usually formated with a file system that is
interpreted by the UNIX system to
provide a directory hierarchy and files.
The UNIX system interprets and multiplexes requests from user programs
to create, read, write, and delete files by allocating and freeing
inodes and data blocks.
The interpretation of the data on the disk could be done by the
user programs themselves.
The reason that it is done by the UNIX system is to synchronize the user
requests, so that two processes do not attempt
to allocate or modify the same resource simultaneously.
It also allows access to be restricted at the file level rather than 
at the disk level and allows the common file system
routines to be shared between processes.
.PP
The implementation of the new file system amounted to 
using a different scheme for formating and interpreting the disk.
Since the synchronization and disk access routines themselves
were not being changed,
the changes to the file system could be developed by moving the 
file system interpretation routines out of the kernel and into a
user program.
Thus, the first step was to extract the file system code for
the old file system from the UNIX kernel and
change its requests to the disk driver to accesses to a raw disk.
This produced a library of routines that
mapped what would normally be system calls
into read or write operations on the raw disk.
This library was then debugged by linking it into
the system utilities that copy, remove, archive, and restore files.
.PP
A new cross file system utility was written that copied files from
the simulated file system to the one implemented by the kernel.
This was accomplished by calling the simulation library to do a read,
and then writing the resultant data by using the conventional
write system call.
A similar utility copied data from the kernel to the simulated file
system by doing a conventional read system call and then writing
the resultant data using the simulated file system library.
.PP
The second step was to rewrite the file system simulation library to
interpret the new file system.
By linking the new simulation library into the cross file system
copying utility,
it was possible to easily copy files from the old file system
into the new one and from the new one to the old one.
Having the file system interpretation implemented
in user code had several major benefits.
These included being able to use the standard system tools
such as the debuggers to set breakpoints and single step through the code.
When bugs were discovered,
the offending problem could be fixed and
tested without the need to reboot the machine.
There was never a period where it was necessary to 
maintain two concurrent file systems in the kernel.
Finally it was not necessary to dedicate a machine
entirely to file system development,
except for a brief period while the new file system was boot strapped.
.PP
The final step was to merge the new file system back into the UNIX kernel.
This was done in less than two weeks, 
since the only bugs remaining were those that involved interfacing
to the synchronization routines
that could not be tested in the simulated system.
Again the simulation system proved useful since it enabled
files to be easily copied between old and new file systems
regardless of which file system was running in the kernel.
This greatly reduced the number of times that the system had
to be rebooted.
.PP
The total design and debug time took about one man year.
Most of the work was done on the file system utilities,
and changing all the user programs to use the new facilities.
The code changes in the kernel were minor, involving the
addition of only about 800 lines of code (including comments).
.bp
