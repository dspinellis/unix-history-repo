.SH
.LG
.ce
Section 8
.SM
.NL
.PP
Major changes affecting system operations include:
.IP \(bu 3
The system now supports disk quotas.  These allow system
administrators to control users' disk space and file 
allocation on a per-file system basis.  Utilities in this
section exist for fixing, summarizing, and editing disk
quota summary files.
.IP \(bu 3
File systems are now made with a new program, newfs,
which acts as front end to the old mkfs program.  There
no longer is a need to remember disk partition sizes, as
newfs gets this information automatically from the /etc/disktab
file.  In addition, newfs attempts to lay out file systems
according to the characteristics of the underlying disk
drive (taking into account disk geometry information).
.IP \(bu 3
DEC standard bad block forwarding is now supported on the RP06
and second vendor UNIBUS storage module disks.  The bad144 program
can now be used to mark sectors bad on many disks, though
inclusion in the bad sector table is still somewhat risky
due to requirements in the ordering of entries in the table.
.IP \(bu 3
A new program, format, should be used to initialize all
non-DEC storage modules before creating file systems.
Format formats the sector headers and creates a bad sector
table which is used in normal system operation.  Format runs
in a standalone mode.
.IP \(bu 3
Getty has been rewritten to use a description file, /etc/gettytab.
This allows sites to tailor terminal operation and configuration
without making modifications to getty.
.IP \(bu 3
The line printer system is totally new.  A program to administer
the operation of printers, lpc, is supplied, and printer accounting
has been consolidated into a single program, pac.
.IP \(bu 3
The program used to restore files from dump tapes is now called
\fIrestore\fP.  This name change was done to reinforce the fact that it is
completely rewritten and operates in a very different way than
the old restor program.  Restore operates on mounted file systems
and uses only normal file system operations to restore files.
Versions of both dump and restore which operate across a network
are included as rdump and rrestore.  Dump and restore (and their
network oriented counterparts) now perform so efficiently (mostly
because of the new file system), that disk to
disk backups should no longer be an attractive alternative.
.sp
.PP
.BP arff
No longer asks if you want to clobber the floppy when manipulating
archives which are not on the floppy.
.BP bad144
Has been modified to use the /etc/disktab file.  Can be used
to create bad sector tables for the DEC RP06 and several
new Winchester disk drives.  Consult the source code for
details and use with extreme care.
.BP badsect
Has been modified to work with the new file system and now
must interact with fsck to perform its duties.  Consult
the manual page for more information.
.BP bugfiler
Is a new program for automatic filing and acknowledgement
of bug reports submitted by the sendbug program.  Intended
to operate with the Rand MH software which is part of the
user contributed software.  Used at Berkeley to process
bug reports on 4.2BSD.
.BP chgrp
Has been moved to section 1.
.BP comsat
Has been changed to filter the noise lines in message headers
when displaying incoming mail.  No longer uses a
second process watchdog as it uses the more reliable socket facilities
instead of the old mpx facilities.
.BP config
Has been extensively modified to handle the new root and
swap device specification syntax.  A new document,
\&``Configuring 4.2BSD UNIX Systems with Config'', describes
its use, as well as other important information needed
in configuring system images; this is part of Volume 2C
of the programmer's manual.
.BP diskpart
Is a new program which may be used to generate disk
partition tables according to the rules used at Berkeley.
Can automatically generate entries required for device
drivers and for the /etc/diskpart file.  (Does not handle
the new DEC DSA style drives properly because it tries
to reserve space for the bad sector table.)
.BP drtest
Is a new standalone program which is useful in testing
standalone disk device drivers and for pinpointing
bad sectors on a disk.
.BP dump
Has been modified for the new file system organization.
Mainly due to the new file system, it runs virtually
at tape speed.  Properly handles locking on the dumpdates
file when multiple dumps are performed concurrently on
the same machine.
.BP dumpfs
Is a new program for dumping out information about a file
system such as the block size and disk layout information.
.BP edquota
Is a new program for editing user quotas.  Operates by
invoking your favorite editor on an ASCII representation
of the information stored in the binary quota files.
Edquota also has a ``replication'' mode whereby a
quota template may be used to create quotas for a 
group of users.
.BP fastboot
Is a new shell script which reboots the system without
checking the file systems; should be used with extreme
care.
.BP fasthalt
Is a new script which is similar to fastboot.
.BP format
Is a new standalone program for formatting non-DEC
storage modules and creating the appropriate bad
sector table on the disk.
.BP fsck
Has been changed for the new file system.  Fsck
is more paranoid then ever in checking the disks,
and has been sped up significantly.  The accompanying
Volume 2C document has been updated to reflect the
new file system organization.
.BP ftpd
Is the DARPA File Transfer Protocol server program.
It supports C shell style globbing of arguments
and a large set of the commands in the specification (except
the ABORT command!).
.BP gettable
Is a new program which can be used in aquiring up to
date DARPA Internet host database files.
.BP getty
Has been rewritten to use a terminal description database,
/etc/gettytab.  Consult the manual entries for \fIgetty\fP\|(8)
and \fIgettytab\fP\|(5) for more information.
.BP icheck
Has been modified for the new file system.
.BP init
Has been significantly modified to use the new signal
facilities.  In doing so, several race conditions related
to signal delivery have been fixed.
.BP kgmon
Is a new program for controlling running systems which have
been created with kernel profiling.  Using kgmon, profiling
can be turned on or off and internal profiling buffers can
dumped into a gmon.out file suiitable for interpretation
by gprof.
.BP lpc
Is a new program controlling line printers and their associated
spooling queues.  Lpc can be used to enable and disable printers
and/or their spooling queues.  Lpc can also be used to rearrange
existing jobs in a queue.
.BP lpd
Has been rewritten and now runs as a ``server'', using the
interprocess communication facilities to service print
requests.  A supplementary document describing the line
printer system is now part of Volume 2C of the programmer's
manual.
.BP MAKEDEV
.br
Is a new shell script which resides in /dev and is used
to create special files there.  MAKEDEV keeps commands for
creating and manipulating local devices in a separate file
MAKEDEV.local.
.BP mkfs
Has been virtually rewritten for the new file system.
The arguments supplied are very different.  For the
most part, users now use the newfs program when
creating file systems.  Mkfs now automatically creates the
lost+found directory.
.BP mount
Now indicates file systems which are
mounted read-only or have disk quotas enabled.
.BP newfs
Is a new front-end to the mkfs program.  Newfs figures
out the appropriate parameters to supply to mkfs,
invokes it, and then, if necessary, installs the
boot blocks necessary to bootstrap UNIX on 11/750's.
.BP pac
Is a new program which can be used to do printer accounting on
any printer.  It subsumes the vpac program.
.BP quot
Now uses the information in the inode of each file to
find out how many blocks are allocated to it.
.BP quotacheck
.br
Is a new program which performs consistency checks
on disk quota files.
Quotacheck is normally run from the /etc/rc.local
file after a system is rebooted, though it can also be run
on mounted on file systems which are not in use.
.BP quotaon
Is a new program which enables disk quotas on
file systems.  A link to quotaon, named quotaoff,
is used to disable disk quotas on file systems.
.BP pstat
Has been modified to understand new kernel data structures.
.BP rc
Has had system dependent startup commands moved to /etc/rc.local.
.BP rdump
Is a new program to dump file systems across a network.
.BP renice
Has been rewritten to use the new setpriority system call.
As a result, you can now renice users and process groups.
.BP repquota
Is a new program which summarizes disk quotas on one or
more file systems.
.BP restor
No longer exists.  A new program, restore, is its successor.
.BP restore
Replaces restor.  Restore operates on mounted file systems; 
it contains an interactive mode and can be used to restore
files by name.  Restore has become almost as flexible to use
as tar in retrieving files from tape.
.BP rexecd
Is a network server for the \fIrexec\fP\|(3X) library routine.
Supports remote command execution where authentication is
performed using user accounts and passwords.
.BP rlogind
Is a network server for the \fIrlogin\fP\|(1C) command.
Supports remote login sessions where authentication is
performed using privileged port numbers and two files,
/etc/hosts.equiv and .rhosts (in each users home directory).
.BP rmt
Is a program used by rrestore and rdump for doing remote
tape operations.
.BP route
Is a program for manually manipulating network routing
tables.
.BP routed
Is a routing daemon which uses a variant of the Xerox
Routing Information Protocol to automatically maintain
up to date routing tables.
.BP rrestore
Is a version of restore which works across a network.
.BP rshd
Is a server for the \fIrsh\fP\|(1C) command.  It
supports remote command execution using privileged
port numbers and the /etc/hosts.equiv and .rhosts
files in users' home directories.
.BP rwhod
Is a server which generates and listens for host status
information on local networks.  The information stored
by rwhod is used by the \fIrwho\fP\|(1C) and\fIruptime\fP\|(1C)
programs.
.BP rxformat
Is a program for formatting floppy disks (this uses
the \fIrx\fP device driver, not the console floppy interface).
.BP savecore
Has been modified to get many pieces of information
from the running system and crash dump to avoid compiled
in constants.
.BP sendmail
Is a new program replacing delivermail; it provides fully
internetwork mail forwarding capabilities.  Sendmail uses
the DARPA standard SMTP protocol to send and receive mail.
Sendmail uses a configuration file to control its operation,
eliminating the compiled in description used in delivermail.
.BP setifaddr
Is a new program used to set a network interface's address.
Calls to this program are normally placed in the /etc/rc.local
file to configure the network hardware present on a machine.
.BP syslog
Is a server which receives system logging messages.  Currently,
only the sendmail program uses this server.
.BP telnetd
Is a server for the DARPA standard TELNET protocol.
.BP tftpd
Is a server for the DARPA Trivial File Transfer Protocol.
.BP trpt
Is a program used in debugging TCP.  Trpt transliterates
protocol trace information recorded by TCP in a circular buffer in
kernel memory.
.BP  tunefs
Is a program for modifying certain parameters in the
super block of file systems.
.BP vipw
Is no longer a shell script and properly interacts
with passwd, chsh, and chfn in locking the password file.
