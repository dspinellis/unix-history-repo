.\" Copyright (c) 1983, 1993, 1994
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)1.5.t	8.6 (Berkeley) %G%
.\"
.Sh 2 Descriptors
.Sh 3 "The reference table
.PP
Each process has access to resources through
\fIdescriptors\fP.  Each descriptor is a handle allowing
processes to reference objects such as files, devices
and communications links.
.PP
Rather than allowing processes direct access to descriptors, the system
introduces a level of indirection, so that descriptors may be shared
between processes.  Each process has a \fIdescriptor reference table\fP,
containing pointers to the actual descriptors.
The descriptors themselves therefore may have multiple references,
and are reference counted by the system.
.PP
Each process has a limited size descriptor reference table, where
the current size is returned by the
.Fn getdtablesize
call:
.DS
.Fd getdtablesize 0 "get descriptor table size
nds = getdtablesize();
result int nds;
.DE
and guaranteed to be at least 64.
The maximum number of descriptors is a resource limit (see section
.Xr 1.6.3 ).
The entries in the descriptor reference
table are referred to by small integers; for example if there
are 64 slots they are numbered 0 to 63.
.Sh 3 "Descriptor properties
.PP
Each descriptor has a logical set of properties maintained
by the system and defined by its \fItype\fP.
Each type supports a set of operations;
some operations, such as reading and writing, are common to several
abstractions, while others are unique.
For those types that support random access, the current file offset
is stored in the descriptor.
The generic operations applying to many of these types are described
in section
.Xr 2.1 .
Naming contexts, files and directories are described in section
.Xr 2.2 .
Section
.Xr 2.3
describes communications domains and sockets.
Terminals and (structured and unstructured) devices are described in section
.Xr 2.4 .
.Sh 3 "Managing descriptor references
.LP
A duplicate of a descriptor reference may be made by doing:
.DS
.Fd dup 1 "duplicate an existing file descriptor
new = dup(old);
result int new; int old;
.DE
returning a copy of descriptor reference \fIold\fP which is
indistinguishable from the original.
The value of \fInew\fP chosen by the system will be the
smallest unused descriptor reference slot.
A copy of a descriptor reference may be made in a specific slot
by doing:
.DS
.Fd dup2 2 "duplicate an existing file descriptor
dup2(old, new);
int old, new;
.DE
The
.Fn dup2
call causes the system to deallocate the descriptor reference
current occupying slot \fInew\fP, if any, replacing it with a reference
to the same descriptor as old.
.LP
Descriptors are deallocated by:
.DS
.Fd close 1 "delete a descriptor
close(old);
int old;
.DE
.Sh 3 "Multiplexing requests
.PP
The system provides a
standard way to do
synchronous and asynchronous multiplexing of operations.
Synchronous multiplexing is performed by using the
.Fn select
call to examine the state of multiple descriptors simultaneously,
and to wait for state changes on those descriptors.
Sets of descriptors of interest are specified as bit masks,
as follows:
.DS
.Fd select 5 "synchronous I/O multiplexing
nds = select(nd, in, out, except, tvp);
result int nds; int nd; result fd_set *in, *out, *except;
struct timeval *tvp;

FD_CLR(fd, &fdset);
FD_COPY(&fdset, &fdset2);
FD_ISSET(fd, &fdset);
FD_SET(fd, &fdset);
FD_ZERO(&fdset);
int fd; fs_set fdset, fdset2;
.DE
The
.Fn select
call examines the descriptors specified by the
sets \fIin\fP, \fIout\fP and \fIexcept\fP, replacing
the specified bit masks by the subsets that select true for input,
output, and exceptional conditions respectively (\fInd\fP
indicates the number of file descriptors specified by the bit masks).
If any descriptors meet the following criteria,
then the number of such descriptors is returned in \fInds\fP and the
bit masks are updated.
.if n .ds bu *
.if t .ds bu \(bu
.IP \*(bu
A descriptor selects for input if an input oriented operation
such as
.Fn read
or
.Fn receive
is possible, or if a connection request may be accepted (see sections
.Xr 2.1.3
and
.Xr 2.3.1.4 ).
.IP \*(bu
A descriptor selects for output if an output oriented operation
such as
.Fn write
or
.Fn send
is possible, or if an operation
that was ``in progress'', such as connection establishment,
has completed (see sections
.Xr 2.1.3
and
.Xr 2.3.1.5 ).
.IP \*(bu
A descriptor selects for an exceptional condition if a condition
that would cause a SIGURG signal to be generated exists (see section
.Xr 1.3.2 ),
or other device-specific events have occurred.
.LP
For these tests, an operation is considered to be possible if a call
to the operation would return without blocking (even if the O_NONBLOCK
flag were not set).
For example, a descriptor would test as ready for reading if a read
call would return immediately with data, an end-of-file indication,
or an error other than EWOULDBLOCK.
.LP
If none of the specified conditions is true, the operation
waits for one of the conditions to arise,
blocking at most the amount of time specified by \fItvp\fP.
If \fItvp\fP is given as NULL, the
.Fn select
waits indefinitely.
.LP
Options affecting I/O on a descriptor
may be read and set by the call:
.DS
.Fd fcntl 3 "file control
dopt = fcntl(d, cmd, arg);
result int dopt; int d, cmd, arg;
.DE
.DS
.TS
l s
l l.
/* command values */

F_DUPFD	/* return a new descriptor */
F_GETFD	/* get file descriptor flags */
F_SETFD	/* set file descriptor flags */
F_GETFL	/* get file status flags */
F_SETFL	/* set file status flags */
F_GETOWN	/* get SIGIO/SIGURG proc/pgrp */
F_SETOWN	/* set SIGIO/SIGURG proc/pgrp */
F_GETLK	/* get blocking lock */
F_SETLK	/* set or clear lock */
F_SETLKW	/* set lock with wait */
.TE
.DE
The F_DUPFD \fIcmd\fP provides identical functionality to
.Fn dup2 ;
it is provided solely for POSIX compatibility.
The F_SETFD \fIcmd\fP can be used to set the close-on-exec
flag for a file descriptor.
The F_SETFL \fIcmd\fP may be used to set a descriptor in 
non-blocking I/O mode and/or enable signaling when I/O is possible.
F_SETOWN may be used to specify a process or process
group to be signaled when using the latter mode of operation
or when urgent indications arise.
The
.Fn fcntl
system call also provides POSIX-compliant byte-range locking on files.
However the semantics of unlocking on every 
.Fn close
rather than last close makes them useless.
Much better semantics and faster locking are provided by the
.Fn flock
system call (see section
.Xr 2.2.7 ).
The
.Fn fcntl
and
.Fn flock
locks can be used concurrently;
they will serialize against each other properly.
.PP
Operations on non-blocking descriptors will
either complete immediately,
return the error EWOULDBLOCK,
partially complete an input or output operation returning a partial count,
or return an error EINPROGRESS noting that the requested operation is
in progress.
A descriptor which has signalling enabled will cause the specified process
and/or process group
be signaled, with a SIGIO for input, output, or in-progress
operation complete, or
a SIGURG for exceptional conditions.
.PP
For example, when writing to a terminal
using non-blocking output,
the system will accept only as much data as there is buffer space,
then return.
When making a connection on a \fIsocket\fP, the operation may
return indicating that the connection establishment is ``in progress''.
The
.Fn select
facility can be used to determine when further
output is possible on the terminal, or when the connection establishment
attempt is complete.
