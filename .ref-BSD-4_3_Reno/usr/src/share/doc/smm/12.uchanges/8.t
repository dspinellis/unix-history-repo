.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)8.t	6.7 (Berkeley) 4/14/86
.\"
.SH
.LG
.ce
Section 8
.SM
.sp
.LP
Major changes affecting system operations include:
.IP \(bu 3
The format of the ttys file, \fI/etc/ttys\fP, has been changed
to include information about terminal type.
.IP \(bu 3
The \fIcrontab\fP file used by \fIcron\fP has a new field in each line
to specify the user ID to be used.
.IP \(bu 3
A new Internet server-server, \fIinetd\fP,
listens for service requests on a number of ports
and spawns the appropriate server upon demand.
Fewer of the Internet services now require long-lived daemon processes.
.IP \(bu 3
The \fIbad144\fP program
can now be used to add new bad sectors to the bad sector file.
Replacement sectors are rearranged as needed to sort the new sectors
into the bad sector list.
Reformat operations to mark bad sectors
to the bad sector table should still be done only
with the system running single user.
.IP \(bu 3
\fIGetty\fP's description file, \fI/etc/gettytab\fP, now describes what
program should be run in addition to the other information that
it used to include.
.sp
.PP
.BP arff
Has been extended to understand multiple directory segments.  
This allows it
to handle the console RL02 pack on the VAX 8600.
.BP arp
A new program for examining and modifying the kernel Address
Resolution Protocol tables.
.BP bad144
\fIBad144\fP has new options to add sectors to the bad sector table and
to attempt to copy sectors to their replacements before marking them bad.
It verifies that the file is properly sorted.
Verbose and no-write options allow dry runs.
.BP catman
Now allows a list of manual directories.  Links are properly set up so
that the manual source need not be kept on line on all machines.
.BP checkquota
Runs multiple filesystems in parallel.  Quotas for users with zero blocks
are left around but they are deleted if the user-id no longer exists.
.BP chown
Was modified to be recursive.
\fIChown\fP accepts an \fIowner.group\fP syntax
to change owner and group simultaneously.
The group-id will be set correctly when dealing
with symbolic links.
.BP comsat
\fIComsat\fP is now invoked by \fIinetd\fP.
It reaps its child processes correctly.
Large systems with many terminal lines are now handled.
.BP config
Swap size may be specified.  \fBMaxusers\fP is no longer truncated.
The name of the generated
.I Makefile
is now capitalized.
Object files may now be listed for inclusion in the \fIfiles\fP file
and will be added to the compilation
properly.  Optional files may be listed multiple times
if different options require their inclusion.
\fBSwapconf\fP supports larger unit numbers.
\fIConfig\fP builds a new file containing definitions for counting
device interrupts.
.BP cron
.I /usr/lib/crontab
has a new format to specify the user-id under which the process should be run.
.BP diskpart
Handles disks with either cylinder or sector offsets and that do not use
.I bad144
bad block forwarding.
.BP dump
When dumping at 6250 bpi, the tape is written in 32Kb records instead
of 10Kb records.
Efforts have been made to improve the consistency of dumps made
on active file systems (though the practice is still NOT recommended).
The Caltech streaming dump modifications using a ring of slave
processes have been incorporated.
.I Dump
makes a better estimate of the size of the dump by attempting
to account for files with holes.
The error messages have been made less condescending.
.BP edquota
Can edit quotas on filesystems where a user does not have any usage.
.BP fingerd
A new daemon to return user information; it runs under \fIinetd\fP.
.BP fsck
\fIFsck\fP has been sped up considerably by eliminating one of
the two passes across the inodes.
It has also been taught to create and grow directories
so that it can now rebuild the root of a file system
as well as create and enlarge the \fIlost+found\fP
directory as necessary.
.BP ftpd
Among the new facilities supported by the FTP server are:
the ABOR command for transfer abort,
the PASV command for third party transfers,
and the new RFC959 FTP commands (such as STOU, ``store unique'').
\fIFtpd\fP now uses \fIsyslog\fP to log errors, and is invoked by \fIinetd\fP.
.BP gettable
Now has a flag for checking the version without retrieving the whole host table.
.BP getty
\fIGetty\fP supports automatic baud rate detection based on carriage
return.  Support for window system startup has been added.  
The login banner
can now include the terminal name.  The environment is set up now and passed
to 
.I login .
.BP htable
Some byte ordering problems have been fixed.
It is more intelligent about gateway handling.
A looping problem with single character host names has been fixed.
.BP ifconfig
\fIIfconfig\fP has been augmented to allow different address families.
The current families understood are \fIinet\fP and \fIns\fP.
\fIIfconfig\fP has additions to set up subnets of Internet networks,
change Internet broadcast addresses, and
set destination addresses of point-to-point links.
.BP implog
Handles class B and class C networks.
.BP inetd
A new program to spawn network servers on demand.
\fIInetd\fP listens on each port listed in its configuration file
\fI/etc/inetd.conf\fP.
When service requests arrive, it passes the original socket
or a newly accepted socket to the designated server for the service.
Several trivial services are implemented internally.
.BP init
May run commands other than \fIgetty\fP.  
Large systems are no longer a problem.
Window systems may be started.
.BP lpc
A new command, \fBdown\fP, disables queueing and printing, and, optionally,
creates a status message displayed by the \fIlpq\fP program.  The
\fBup\fP command reverses the effect of the \fBdown\fP command.  The
\fBstatus\fP command now displays the contents of the print queue in
addition to the status of the daemon process.  The \fBclean\fP command
does a better job of removing incomplete queue entries.
.BP lpd
A new capability, \fBhl\fP, may be used to print a job's banner after
the contents of the job.
Error logging is now done with \fIsyslog\fP\|(3).
Hosts permitting remote access may now be
specified in the file \fI/etc/hosts.lpd\fP (in addition to
\fI/etc/hosts.equiv\fP).  A master lock file is now used so
that \fI/dev/printer\fP can be automatically removed.  Symbolic
links to spool files are now checked carefully to close a
security hole.  All printing parameters are now
properly reset for each job.  Remote spooling connections
now time out if the server crashes.  Errors in spooling
filters are now reported to users via mail.  When servicing
a remote job, files are not transferred unless enough disk
space is available.
.BP mkfs
Will print the filesystem information without creating the filesystem.
Filesystem optimization may be specified.
.BP mkhosts
A new program to rebuild the \fI/etc/hosts\fP dbm database.
Note that this database is not used with the default name server
configuration.
.BP mkpasswd
A new program to rebuild the \fI/etc/passwd\fP dbm database.
.BP mount
Better error messages are returned when \fImount\fP fails.
When checking \fI/etc/fstab\fP to find the device name of a file system
when only the mount point is specified,
it also checks the \fItype\fP field to insure that the entry
is \fBrw\fP, \fBro\fP, or \fBrq\fP.
.BP named
Is a new program implementing the Internet domain naming system.
It is used to perform hostname and
address mapping functions for the standard C library
functions, \fIgethostbyname\fP and \fIgethostbyaddr\fP if
\fInamed\fP is running.
.BP newfs
Supports new options to
.I mkfs .
.BP pac
Has a new option, \fB\-m\fP, to cause machine names to be disregarded
in merging accounting information.  The per-page cost is now
taken from the printer description if it is not specified on the command line
with the \fB\-p\fP option.
.BP ping
Is a new program for sending ICMP echo requests.
.BP pstat
Can handle kernel crash dumps and new terminal multiplexers.  Core dumps
should be less frequent.
.BP repquota
Only prints entries for users that have files (or blocks) allocated.
.BP restore
The interactive mode of \fIrestore\fP now understands globbing.
Interrupting interactive mode returns to the prompt.
A new input path name may be specified on each volume change.
The tape block size is calculated dynamically
unless it is specified with the \fB\-b\fP flag on the command line.
.BP rexecd
Now runs under
.I inetd .
.BP rlogind
Propagates window size changes in a backward compatible way.
This is negotiated at startup time.
.I Inetd
now starts up the server.
.BP rmt
Uses large network buffers for better performance.
.BP route
Will handle subnets.
Flags were added to specify whether a name is a host or a network.
Multiple addresses are tried until an operation is successful
or there are
no more addresses to try.
.BP routed
Is more strict about received packets' formats and values.
Subnet routing is handled.
Point to point links are handled.
Gateways to external networks advertise a default route instead of all networks.
The loopback network number is no longer compiled in.
When a process is terminated,
it tells its peers that its routes are no longer valid.
.BP rshd
Is started by
.I inetd .
The address is passed through if the host name for the address
cannot be determined.
.BP rwhod
Should be less expensive to run.
Broadcasts are done less frequently and path lookups are shorter.
Large systems are handled better.
.BP rxformat
Will now operate if the standard input is not a terminal.
.BP sa
Supports alternate accounting files.  The units of CPU time have changed.
.BP savecore
Works correctly when given an alternate system name.
Dump partitions smaller than the memory size are handled more gracefully.
.BP sendmail
Several bugs have been fixed.
Upper case letters are allowed in file names
and program arguments in the alias file.
Multiple recipients sharing a receive program
are not collapsed into one delivery.
List owners on queued jobs have been fixed.
Commas in quoted aliases work.
Dollar signs in headers are no longer interpreted as macro expansions.
Underscores are allowed in login names.
.IP
Substantial performance enhancements have been made
for large queues.
If the
.B Y
option is not set,
all jobs in the queue will be run in one process,
with host statuses cached;
this uses more memory but generally improves performance.
The job priority now includes creation time
and number of recipients
(the
.B y
option)
as well as the message size
(the
.B q
option)
and the job precedence
(the
.B z
option);
this priority is modified by the
.B Z
option whenever it fails to complete.
No attempt is made to run large jobs
if the load average is too high.
.IP
The
.B $[
\&...
.B $]
syntax can be used on the RHS of a rewriting rule
to canonicalize a host name using \fIgethostbyname\fP.
This is especially useful when running the version
of \fIgethostbyname\fP that calls the name server.
.IP
Error reporting has been improved.
Some limits have been increased.
Security holes have been plugged.
.I Syslogd
and
.I vacation
are now part of the standard system.
.IP
Minor changes have been made to the configuration file.
The RHS of aliases are no longer checked while the alias file is rebuilt
unless the
.B n
option is set
to improve performance. 
The character substituted for blanks in addresses
is settable by the
.B B
option.
The default network name
(formerly hardwired ``ARPA'')
is settable with the
.B N
option.
The
.B E
mailer option escapes ``From'' lines with a `>' on delivery
(formerly the default to the local mailer).
.BP shutdown
Has flags to specify that it should not sync the disks and that
it should skip the disk checks after rebooting.
.BP swapon
Error messages have been cleaned up and now specify the device to which
they correspond.
.BP syslogd
Formerly
.I syslog ,
allows the classification of messages based on
.I facilities .
The configuration file has been restructured.
.BP talkd
Now runs under
.I inetd .
New version, new protocol.
.BP telnetd
Handles pty allocation better.
.I Inetd
now starts the server.
Interpretation of carriage return-newline
now conforms with the standard, but is compatible with the 4.2BSD \fItelnet\fP
client.
.BP tftpd
Now works with other clients and is started by
.I inetd .
.BP timed
A new program for maintaining time synchronization between machines
on a local network.
.BP trpt
The \fItrpt\fP program to examine TCP traces
now prints the traces in the correct order.
It has been extended to follow traces as a connection runs.
.BP tunefs
Supports the new filesystem optimization preferences.
.BP uucpd
A new server, invoked by \fIinetd\fP, for running
uucp over network connections.
.BP vipw
Builds the new hashed lookup table.
.I /etc/passwd
will not be left unreadable if root has a restrictive umask.
.BP XNSrouted
A new daemon, similar to \fIrouted\fP, that implements the
Xerox NS routing protocol.
