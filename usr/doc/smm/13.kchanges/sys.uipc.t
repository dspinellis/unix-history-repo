.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)sys.uipc.t	1.7 (Berkeley) 4/11/86
.\"
.NH 2
Changes in Interprocess Communication support
.XP uipc_domain.c
The skeletal support for the PUP-1 protocol has been removed.
A domain for Xerox NS is now in use.
The per-domain data structure allows a per-domain initialization routine
to be called at boot time.
.XP
The \fIpffindproto\fP routine, used in creating a socket to support
a specified protocol,
takes an additional argument, the type of the socket.
It checks both the protocol and type, useful when the same protocol
implements multiple socket types.
If the type is SOCK_RAW and no exact match is found,
a \fIprotosw\fP entry for raw support and a wildcard protocol (number zero)
will be used.
This allows for a generic raw socket that passes
through packets for any given protocol.
.XP
The second argument to \fIpfctlinput\fP, the generic error-reporting
routine, is now declared as a \fIsockaddr\fP pointer.
.XP  uipc_mbuf.c
The mbuf support routines now use the \fIwait\fP flag passed to \fIm_get\fP
or MGET.
If M_WAIT is specified, the allocator may wait for free memory,
and the allocation is guaranteed to return an mbuf if it returns.
In order to prevent the system from slowly going to sleep after
exhausting the mbuf pool by losing the mbufs to a leak,
the allocator will panic after creating the maximum allocation of mbufs
(by default, 256K).
Redundant \fIspl\fP's have been removed; most internal routines must
be called at \fIsplimp\fP, the highest priority at which mbuf and memory
allocation occur.
.XP
When copying mbuf chains \fIm_copy\fP now preserves the type of each mbuf.
There were problems in \fIm_adj\fP, in particular assumptions
that there would be no zero-length mbufs within the chain;
this was corrected by changing its \fIn\fP-pass algorithm for trimming
from the tail of the chain
to either one- or two-pass, depending on whether the correction was entirely
within the last mbuf.
In order to avoid return business, \fIm_pullup\fP was changed
to pull additional data (MPULL_EXTRA, defined in \fImbuf.h\fP)
into the contiguous area in the first mbuf, if convenient.
\fIm_pullup\fP will use the first mbuf of the chain rather then a new one
if it can avoid copying.
.XP uipc_pipe.c
This ``temporary'' file has been removed;
pipe now uses \fIsocketpair\fP.
.XP uipc_proto.c
New entries in the protocol switch for externalization and disposal
of access rights are initialized for the Unix domain protocols.
.XP uipc_socket.c
The \fIsocreate\fP function uses the new interface to \fIpffindproto\fP
described above if the protocol is specified by the caller.
The \fIsoconnect\fP routine will now try to disconnect a connected socket
before reconnecting.
This is only allowed if the protocol itself is not connection oriented.
Datagram sockets may connect to specify
a default destination, then later connect to another destination
or to a null destination to disconnect.
The \fIsodisconnect\fP routine never used its second argument, and it has
been removed.
.XP
The \fIsosend\fP routine, which implements write and send on sockets,
has been restructured for clarity.
The old routine had the main loop upside down, first emptying and then filling
the buffers.
The new implementation also makes it possible to send zero-length datagrams.
The maximum length calculation was simplified to avoid problems
trying to account for both mbufs and characters of buffer space used.
Because of the large improvement in speed of data handling when large
buffers are used, \fIsosend\fP will use page clusters if it can use
at least half of the cluster.
Also, if not using nonblocking I/O,
it will wait for output to drain if it has enough data
to fill an mbuf cluster but not enough space in the output queue for one,
instead of fragmenting the write into small mbufs.
A bug allowing access rights to be sent more than once when using scatter-gather
I/O (\fIsendmsg\fP) was fixed.
A race that occurred when \fIuiomove\fP blocked during a page fault
was corrected by allowing the protocol send routines to report disconnection
errors; as with disconnection detected earlier, \fIsosend\fP returns
EPIPE and sends a SIGPIPE signal to the process.
.XP
The receive side of socket operations, \fIsoreceive\fP, has also been reworked.
The major changes are a reflection of the way that datagrams are now queued;
see uipc_socket2.c for further information.
The MSG_PEEK flag is passed to the protocol's \fIusrreq\fP routine
when requesting out-of-band data so that the protocol may know
when the out-of-band data has been consumed.
Another bug in access-rights passing was corrected here; the protocol
is not called to externalize the data when PEEKing.
.XP
The \fIsosetopt\fP and \fIsogetopt\fP functions have been expanded
considerably.
The options that existed in 4.2BSD all set some flag at the socket level.
The corresponding options in 4.3BSD use the value argument as a boolean,
turning the flag off or on as appropriate.
There are a number of additional options at the socket level.
Most importantly, it is possible to adjust the send or receive buffer
allocation so that higher throughput may be achieved, or that temporary
peaks in datagram arrival are less likely to result in datagram loss.
The linger option is now set with a structure including a boolean
(whether or not to linger) and a time to linger if the boolean is true.
Other options have been added to determine the type of a socket
(eg, SOCK_STREAM, SOCK_DGRAM), and to collect any outstanding error status.
If an option is not destined for the socket level itself,
the option is passed to the protocol using the \fIctloutput\fP entry.
\fIGetopt\fP's last argument was changed from \fImbuf *\fP to \fImbuf **\fP
for consistency with \fIsetopt\fP and the 
new \fIctloutput\fP calling convention.
.XP
\fISelect\fP for exceptional conditions on sockets is now possible,
and this returns true when out-of-band data is pending.
This is true from the time that the socket layer is notified
that the OOB data is on its way until the OOB data has been consumed.
The interpretation of socket process groups in 4.2BSD was inconsistent
with that of ttys and with the \fIfcntl\fP documentation.
This was corrected; positive numbers refer to processes, negative numbers
to process groups.
The socket process group is used when posting a SIGURG to notify
processes of pending out-of-band data.
.XP uipc_socket2.c
Signal-driven I/O now works with sockets as well as with ttys;
\fIsorwakeup\fP and \fIsowwakeup\fP call the new routine \fIsowakeup\fP
which calls \fIsbwakeup\fP as before and also sends SIGIO as appropriate.
Process groups are interpreted in the same manner as for SIGURG.
.XP
Larger socket buffers may be used with 4.3BSD than with 4.2BSD;
socket buffers (\fIsockbuf\fPs) have been modified to use unsigned short
rather than short integers for character counts and mbuf counts.
This increases the maximum buffer size to 64K\-1.  These fields
should really be unsigned longs, but a socket would no longer fit
in an mbuf.
So that as much as possible of the allotment may be used,
\fIsbreserve\fP allows the high-water mark for data to be set as high as 80%
of the maximum value (64K), and sets the high-water mark on mbuf allocation
to the smaller of twice the character limit and 64K.
.XP
In 4.2BSD, datagrams queued in sockbufs were linked through the mbuf
\fIm_next\fP field, with \fIm_act\fP set to 1 in the last mbuf
of each datagram.
Also, each datagram was required to have one mbuf to contain an address,
another to contain access rights, and at least one additional mbuf of data.
In 4.3BSD, the mbufs comprising a datagram are linked through \fIm_next\fP,
and different datagrams are linked through the \fIm_act\fP field of the first
mbuf in each.
No mbuf is used to represent missing components of a datagram,
but the ordering of the mbufs remains important.
The components are distinguished by the mbuf type.
Any address must be in the first mbuf.
Access rights follow the address if present, otherwise they may be first.
Data mbufs follow; at least one data buffer will be present
if there is no address or access rights.
The routines \fIsbappend\fP, \fIsbappendaddr\fP, \fIsbappendrights\fP
and \fIsbappendrecord\fP are used to add new data to a sockbuf.
The first of these appends to an existing record, and is commonly
used for stream sockets.
The other three begin new records with address, optional rights, and data
(\fIsbappendaddr\fP), with rights and data (\fIsbappendrights\fP),
or data only (\fIsbappendrecord\fP).
A new internal routine, \fIsbcompress\fP, is used by these functions
to compress and append data mbufs to a record.
These changes improve the functionality of this layer
and in addition make it faster to find the end of a queue.
.XP
An occasional ``panic: sbdrop'' was due to zero-length mbufs at the end
of a chain.
Although these should no longer be found in a sockbuf queue,
\fIsbdrop\fP was fixed to free empty buffers at the end of the last
record.
Similarly, \fIsbfree\fP continues to empty a sockbuf as long as mbufs
remain, as zero-length packets might be present.
\fISbdroprecord\fP was added to free exactly one record from the front
of a sockbuf queue.
.XP uipc_syscalls.c
Errors reported during an \fIaccept\fP call are cleared so that
subsequent \fIaccept\fP calls may succeed.
A failed attempt to \fIconnect\fP returns the error once only,
and SOISCONNECTING is cleared,
so that additional connect calls may be attempted.
(Lower level protocols may or may not allow this, depending
on the nature of the failure.)
The \fIsocketpair\fP system call has been fixed to work
with datagram sockets as well as with streams,
and to clean up properly upon failure.
Pipes are now created using \fIconnect2\fP.
An additional argument, the type of the data to be fetched,
is passed to \fIsockargs\fP. 
.XP uipc_usrreq.c
The binding and connection of Unix domain sockets has
been cleaned up so that \fIrecvfrom\fP and \fIaccept\fP get the address 
of the peer (if bound) rather than their own.
The Unix-domain connection block records the bound address of a socket,
not the address of the socket to which it is connected.
For stream sockets, back pressure to implement flow control
is now handled by adjusting the limits in the send buffer
without overloading the normal count fields; the flow control
information was moved to the connection block.
Access rights are checked now when connecting; the connected-to socket
must be writable by the caller, or the connection request is denied.
In order to test one previously unused
routine, the Unix domain stream support was modified
to support the passage of access rights.
Problems with access-rights garbage collection were also noted and fixed,
and a count is kept of rights outstanding so that garbage collection
is done only when needed.
Garbage collection is triggered by socket shutdown now
rather than file close; in 4.2BSD, it happened prematurely.
The PRU_SENSE \fIusrreq\fP entry, used by \fIstat\fP, has been added.
It returns the write buffer size as the ``blocksize,'' and generates
a fake inode number and device for the benefit of those programs
that use \fIfstat\fP information to determine whether file descriptors refer
to the same file.
Unimplemented requests have been carefully checked to see that they properly
free mbufs when required and never otherwise.
Larger buffers are allocated for both stream and datagram sockets.
A number of minor bugs have been corrected: the back pointer from an inode
to a socket needed to be cleared before release of the inode when detaching;
sockets can only be bound once, rather than losing inodes; datagram
sockets are correctly marked as connected and disconnected; several mbuf
leaks were plugged.
A serious problem was corrected in \fIunp_drop\fP: it did not properly
abort pending connections, with the result that closing a socket with
unaccepted connections would cause an infinite loop trying to drop them.
