.SH
.LG
.ce
Section 2.
.SM
.sp
.PP
The most important change in section 2 is that the documentation
has been significantly improved.  Manual page entries now indicate
the possible error codes which may be returned and how to interpret
them.  The introduction to section 2 now includes a glossary of
terms used throughout the section.  The terminology and formatting
have been made consistent.  Many manual pages now have ``NOTES''
or ``CAVEATS'' providing useful information heretofore left out
for the sake of brevity.  As always the manual pages are still for
the programmer; they are terse and extremely concise.  The ``4.2BSD
System Manual'' is likewise concise, but a bit more verbose in
providing an overall picture of the system facilities.
.PP
With regard to changes in the facilities, these fall into three
major categories: interprocess communication, signals, and file
system related calls.  The interprocess communication facilities
center around the \fIsocket\fP mechanism described in the
``A 4.2BSD Interprocess Communication Primer''.  The new signals
do not have an accompanying document, so the manual pages should
be studied carefully.  The new file system calls pretty much stand
on their own, with a late section of the document
``A Fast File System for UNIX'' supplying a quick overview of the
most important new file system facilities.  Finally, it should
be noted that the job control facilities introduced in 4.1BSD have
been adopted as a standard part of 4.2BSD.  No special distinction
is given to these calls (in 4.1BSD they were earmarked ``2J'').
.PP
Many of the new system calls have both a ``set'' and a ``get''
form.  Only the ``get'' forms are indicated below.  Consult
the manual for details on the ``set'' form.
.BP intro
Has been updated to reflect the new list of possible error
codes.  Now includes a glossary of terminology used in section 2.
.BP access
Now has symbolic definitions for the \fImode\fP parameter defined
in <\fIsys/file.h\fP>.
.BP bind
Is a new interprocess communication system call for binding
names to sockets.
.BP connect
Is a new interprocess communication system call for establishing
a connection between two sockets.
.BP creat
Has been obsoleted by the new \fIopen\fP interface.
.BP fchmod
Is a new system call which does a \fIchmod\fP operation given
a file descriptor; useful in conjunction with the new advisory
locking facilities.
.BP fchown
Is a new system call which does a \fIchown\fP operation given
a file descriptor; useful in conjunction with the new advisory
locking facilities.
.BP fcntl
Is a new system call which is useful in controlling how i/o
is performed on a file descriptor (non-blocking i/o, signal
drive i/o).  This
interface is compatible with the System III fcntl interface.
.BP flock
Is a new system call for manipulating advisory locks on
files.  Locks may be shared or exclusive and locking operations
may be indicated as being non-blocking, in which case a
process is not blocked if the requested lock is currently
in use.
.BP fstat
Now returns a larger stat buffer; see below under stat.
.BP fsync
Is a new system call for synchronizing a file's in-core
state with that on disk.  Its intended use is in building
transaction oriented facilities.
.BP ftruncate
Is a new system call which does a \fItruncate\fP operation given
a file descriptor; useful in conjunction with the new advisory
locking facilities.
.BP getdtablesize
Is a new system call which returns the size of the
descriptor table.
.BP getgroups
Is a new system call which returns the group access list
for the caller. 
.BP gethostid
Is a new system call which returns the unique (hopefully)
identifier for the current host.
.BP gethostname
Is a new system call which returns the name of the current host.
.BP getitimer
Is a new system call which gets the current value for an
interval timer.
.BP getpagesize
Is a new system call which returns the system page size.
.BP getpriority
Is a new system call which returns the current scheduling
priority for a specific process, a group of processes, or
all processes owned by a user.  In the latter two cases,
the priority returned is the highest (lowest numerical
value) enjoyed by any of the specified processes.
.BP getrlimit
Is a new system call which returns information about a
resource limit.  The getrlimit and setrlimit calls replace
the old vlimit call from 4.1BSD.
.BP getrusage
Is a new system call which returns information about
resource utilitization of a child process or the caller.
This call replaces the vtimes call of 4.1BSD.
.BP getsockopt
Is a new interprocess communication system call which
returns the current options present on a socket.
.BP gettimeofday
Is a new system call which returns the current Greenwich
date and time, and the current timezone in which the machine is
operating.  Time is returned in seconds and microseconds
since January 1, 1970.
.BP ioctl
Has been changed to encode the size of parameters and
whether they are to be copied in, out, or in and out of
the user address space in the \fIrequest\fP.  The symbolic
names for the various ioctl requests remain the same,
only the numeric values have changed.  A number of new ioctls exist
for use with sockets and the network facilities.  The old
LINTRUP request has been replaced by a call to fcntl and
the SIGIO signal.
.BP killpg
Has now been made a system call; in 4.1BSD it was a
library routine.
.BP listen
Is a new interprocess communication system call used
to indicate a socket will be used to listen for incoming
connection requests. 
.BP lseek
Now has symbolic definitions for its \fIwhence\fP
parameter defined in <\fIsys/file.h\fP>.
.BP mkdir
Is a new system call which creates a directory.
.BP mpx
The multiplexed file facilities are no longer part
of the system.  They have been replaced by the
socket, and related, system calls.
.BP open
Is different, now taking an optional third parameter
and supporting file creation, automatic truncation,
automatic append on write, and ``exclusive'' opens.
The open interface has been made compatible with
System III with the exception that non-blocking
opens on terminal lines requiring carrier are not
supported.
.BP profil
Now returns statistical information based on a 100 hz
clock rate.
.BP quota
Is a new system call which is part of the disk quota
facilities.  Quota is used to manipulate disk quotas
for a specific user, as well as perform certain random
chores such as syncing quotas to disk.
.BP read
Now automatically restarts when a read on a terminal
is interrupted by a signal before any data is read.
.BP readv
Is a new system call which supports scattering of
read data into (possibly) disjoint areas of memory.
.BP readlink
Is a new system call for reading the value of a
symbolic link.
.BP recv
Is a new interprocess communication system call
used to receive a message on a connected socket.
.BP recvfrom
Is a new interprocess communication system call
used to receive a message on a (possibly) unconnected socket.
.BP recvmsg
Is a new interprocess communication system call
used to receive a message on a (possibly) unconnected socket
which may have access rights included.  When using on-machine
communication, recvmsg and sendmsg may be used to pass
file descriptors between processes.
.BP rename
Is a new system call which changes the name of an entry
in the file system (plain file, directory, character
special file, etc.).  Rename has an important property
in that it guarantees the target will always exist, even
if the system crashes in the middle of the operation.
Rename only works with source and destination in the
same file system.
.BP rmdir
Is a new system call for removing a directory.
.BP select
Is a new system call (mainly for interprocess
communication) which provides facility for synchronous i/o
multiplexing.  Sets of file descriptors may be queried
for readability, writability, and if any exceptional 
conditions are present (such as out of band data on a
socket).  An optional timeout may also be supplied in
which case the select operation will return after a
specified period of time should no descriptor satisfy the
requests.
.BP send
Is a new interprocess communication system call for
sending a message on a connected socket.
.BP sendto
Is a new interprocess communication system call for
sending a message on a (possibly) unconnected socket.
.BP sendmsg
Is a new interprocess communication system call for
sending a message on a (possibly) unconnected socket
which may included access rights.
.BP setquota
Is a new system call for enabling or disabling disk
quotas on a file system.
.BP setregid
Is a new system call which replaces the 4.1BSD setgid
system call.  Setregid allows the real and effective
group ID's of a process to be set separately. 
.BP setreuid
Is a new system call which replaces the 4.1BSD setuid
system call.  Setreuid allows the real and effective
user ID's of a process to be set separately. 
.BP shutdown
Is a new interprocess communication system call for
shutting down part or all of full-duplex connection.
.BP sigblock
Is a new system call for blocking signals during a
critical section of code.
.BP sigpause
Is a new system call for blocking a set of signals
and then pausing indefinitely for a signal to arrive.
.BP sigsetmask
Is a new system call for setting the set of signals
which are currently blocked from delivery to a process.
.BP sigstack
Is a new system call for defining an alternate stack
on which signals are to be processed.
.BP sigsys
Is no longer supported.  The new signal facilities are
a superset of those which sigsys provided.
.BP sigvec
Is the new system call interface for defining signal
actions.  For each signal (except SIGSTOP and SIGKILL),
sigvec allows a ``signal vector'' to be defined.  The
signal vector is comprised of a handler, a mask of signals
to be blocked while the handler executes, and an indication
of whether or not the handler should execute on a signal
stack defined by a sigstack call.  The old signal interface
is provided as a library routine with several important
caveats.  First, signal actions are no longer reset to
their default value after a signal is delivered to a process.
Second, while a signal handler is executing the signal which
is being processed is blocked until the handler returns.
To simulate the old signal interface, the user must explicitly
reset the signal action to be the default value and unblock
the signal being processed.
.IP
Four new signals have been added for the interprocess
communication and interval timer facilities.  SIGIO is
delivered to a process when an fcntl call enables
signal driven i/o and input is present on a terminal (and
a signal handler is defined).
SIGURG is delivered when an urgent condition arises on a
socket (and a handler is defined).  SIGPROF and SIGVTALRM
are associated with the ITIMER_PROF and ITIMER_VIRTUAL
interval timers, and delivered to a process when such
a timer expires (the SIGALRM signal is used for the ITIMER_REAL
interval timer).  The old SIGTINT signal is replaced by
SIGIO.
.BP socket
Is a new interprocess communication system call for
creating a socket.
.BP socketpair
Is a new interprocess communication system call for
creating a pair of connected and unnamed sockets.
.BP stat
Now returns a larger structure.  New fields are present
indicating the optimal blocking factor in which i/o
should be performed (for disk files the block size of
the underlying file system) and the actual number of
disk blocks allocated to the file.  Inode numbers are
now 32-bit quantities.  Several spare fields have been
allocated for future expansion.  These include space
for 64-bit file sizes and 64-bit time stamps.  Two new
file types may be returned, S_IFLNK for symbolic
links, and S_IFSOCK for sockets residing in the file system.
.BP swapon
Has been renamed from the vswapon call of 4.1BSD.
.BP symlink
Is a new system call for creating a symbolic link.
.BP truncate
Is a new system call for truncating a file to a
specific size.
.BP unlink
Should no longer be used for removing directories.
Directories should only be created with mkdir and
removed with rmdir.  Creating hard links to directories
can cause disastrous results.
.BP utime
Is defunct, replaced by utimes.
.BP utimes
Is a new system call which uses the new time format
in setting the accessed and updated times on a file.
.BP vfork
Is still present, but definitely on its way out.
Future plans include implementing fork with a scheme
in which pages are initially shared read-only.  On
the first attempt by a process to write on a page
the parent and child would receive separate writable
copies of the page.
.BP vlimit
Is no longer supported.  Vlimit is replaced by the
getrlimit and setrlimit calls.
.BP vread
Is no longer supported in the system.
.BP vswapon
Has been renamed swapon.
.BP vtimes
Is no longer supported.  Vtimes is replaced by the
getrusage call.
.BP vwrite
Is no longer supported in the system.
.BP wait
Now is automatically restarted when interrupted
by a signal before status could be returned.
.BP wait3
Returns resource usage in a different format than
that which was returned in 4.1BSD.  This structure
is compatible with the new getrusage system call.
Wait3 is now automatically restarted when interrupted
by a signal before status could be returned.
.BP write
Now is automatically restarted when writing on a
terminal and interrupted by a signal before any
i/o was completed.
.BP writev
Is a new version of the write system call which
supports gathering of data in (possibly)
discontiguous areas of memory
