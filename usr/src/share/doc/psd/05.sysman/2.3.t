.\" Copyright (c) 1983, 1993, 1994
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)2.3.t	8.6 (Berkeley) %G%
.\"
.Sh 2 "Interprocess communications
.Sh 3 "Interprocess communication primitives
.Sh 4 "Communication domains
.PP
The system provides access to an extensible set of 
communication \fIdomains\fP.  A communication domain (or protocol family)
is identified by a manifest constant defined in the
file \fI<sys/socket.h>\fP.
Important standard domains supported by the system are the local (``UNIX'')
domain (PF_LOCAL or PF_UNIX) for communication within the system,
the ``Internet'' domain (PF_INET) for communication in the DARPA Internet,
the ISO family of protocols (PF_ISO and PF_CCITT) for providing
a check-off box on the list of your system capabilities,
and the ``NS'' domain (PF_NS) for communication
using the Xerox Network Systems protocols.
Other domains can be added to the system.
.Sh 4 "Socket types and protocols
.PP
Within a domain, communication takes place between communication endpoints
known as \fIsockets\fP.  Each socket has the potential to exchange
information with other sockets of an appropriate type within the domain.
.PP
Each socket has an associated
abstract type, which describes the semantics of communication using that
socket.  Properties such as reliability, ordering, and prevention
of duplication of messages are determined by the type.
The basic set of socket types is defined in \fI<sys/socket.h>\fP:
.DS
.TS
l s
l l.
Standard socket types
_
SOCK_DGRAM	/* datagram */
SOCK_STREAM	/* virtual circuit */
SOCK_RAW	/* raw socket */
SOCK_RDM	/* reliably-delivered message */
SOCK_SEQPACKET	/* sequenced packets */
.TE
.DE
The SOCK_DGRAM type models the semantics of datagrams in network communication:
messages may be lost or duplicated and may arrive out-of-order.
A datagram socket may send messages to and receive messages from multiple
peers.
The SOCK_RDM type models the semantics of reliable datagrams: messages
arrive unduplicated and in-order, the sender is notified if
messages are lost.
The
.Fn send
and
.Fn receive
operations (described below)
generate reliable or unreliable datagrams.
The SOCK_STREAM type models connection-based virtual circuits: two-way
byte streams with no record boundaries.
Connection setup is required before data communication may begin.
The SOCK_SEQPACKET type models a connection-based,
full-duplex, reliable, exchange preserving message boundaries;
the sender is notified if messages are lost, and messages are never
duplicated or presented out-of-order.
Users of the last two abstractions may use the facilities for
out-of-band transmission to send out-of-band data.
.PP
SOCK_RAW is used for unprocessed access to internal network layers
and interfaces; it has no specific semantics.
Other socket types can be defined.
.PP
Each socket may have a specific \fIprotocol\fP associated with it.
This protocol is used within the domain to provide the semantics
required by the socket type.
Not all socket types are supported by each domain;
support depends on the existence and the implementation
of a suitable protocol within the domain.
For example, within the ``Internet'' domain, the SOCK_DGRAM type may be
implemented by the UDP user datagram protocol, and the SOCK_STREAM
type may be implemented by the TCP transmission control protocol, while
no standard protocols to provide SOCK_RDM or SOCK_SEQPACKET sockets exist.
.Sh 4 "Socket creation, naming and service establishment
.PP
Sockets may be \fIconnected\fP or \fIunconnected\fP.  An unconnected
socket descriptor is obtained by the
.Fn socket
call:
.DS
.Fd socket 3 "create an endpoint for communication
s = socket(domain, type, protocol);
result int s; int domain, type, protocol;
.DE
The socket domain and type are as described above,
and are specified using the definitions from \fI<sys/socket.h>\fP.
The protocol may be given as 0, meaning any suitable protocol.
One of several possible protocols may be selected using identifiers
obtained from a library routine,
.Fn getprotobyname .
.PP
An unconnected socket descriptor of a connection-oriented type
may yield a connected socket descriptor
in one of two ways: either by actively connecting to another socket,
or by becoming associated with a name in the communications domain and
\fIaccepting\fP a connection from another socket.
Datagram sockets need not establish connections before use.
.PP
To accept connections or to receive datagrams,
a socket must first have a binding
to a name (or address) within the communications domain.
Such a binding may be established by a
.Fn bind
call:
.DS
.Fd bind 3 "bind a name to a socket
bind(s, name, namelen);
int s; struct sockaddr *name; int namelen;
.DE
Datagram sockets may have default bindings established when first
sending data if not explicitly bound earlier.
In either case,
a socket's bound name may be retrieved with a
.Fn getsockname
call:
.DS
.Fd getsockname 3 "get socket name
getsockname(s, name, namelen);
int s; result struct sockaddr *name; result int *namelen;
.DE
while the peer's name can be retrieved with
.Fn getpeername :
.DS
.Fd getpeername 3 "get name of connected peer
getpeername(s, name, namelen);
int s; result struct sockaddr *name; result int *namelen;
.DE
Domains may support sockets with several names.
.Sh 4 "Accepting connections
.LP
Once a binding is made to a connection-oriented socket,
it is possible to
.Fn listen
for connections:
.DS
.Fd listen 2 "listen for connections on a socket
listen(s, backlog);
int s, backlog;
.DE
The \fIbacklog\fP specifies the maximum count of connections
that can be simultaneously queued awaiting acceptance.
.LP
An
.Fn accept
call:
.DS
.Fd accept 3 "accept a connection on a socket
t = accept(s, name, anamelen);
result int t; int s; result struct sockaddr *name; result int *anamelen;
.DE
returns a descriptor for a new, connected, socket
from the queue of pending connections on \fIs\fP.
If no new connections are queued for acceptance,
the call will wait for a connection unless
non-blocking I/O has been enabled (see section
.Xr 1.5.4 ).
.Sh 4 "Making connections
.LP
An active connection to a named socket is made by the
.Fn connect
call:
.DS
.Fd connect 3 "initiate a connection on a socket
connect(s, name, namelen);
int s; struct sockaddr *name; int namelen;
.DE
Although datagram sockets do not establish connections,
the
.Fn connect
call may be used with such sockets
to create an \fIassociation\fP with the foreign address.
The address is recorded for use in future
.Fn send
calls, which then need not supply destination addresses.
Datagrams will be received only from that peer,
and asynchronous error reports may be received.
.PP
It is also possible to create connected pairs of sockets without
using the domain's name space to rendezvous; this is done with the
.Fn socketpair
call\(dg:
.FS
\(dg 4.4BSD supports
.Fn socketpair
creation only in the PF_LOCAL communication domain.
.FE
.DS
.Fd socketpair 4 "create a pair of connected sockets
socketpair(domain, type, protocol, sv);
int domain, type, protocol; result int sv[2];
.DE
Here the returned \fIsv\fP descriptors correspond to those obtained with
.Fn accept
and
.Fn connect .
.LP
The call:
.DS
.Fd pipe 1 "create descriptor pair for interprocess communication
pipe(pv);
result int pv[2];
.DE
creates a pair of SOCK_STREAM sockets in the PF_LOCAL domain,
with pv[0] only writable and pv[1] only readable.
.Sh 4 "Sending and receiving data
.LP
Messages may be sent from a socket by:
.DS
.Fd sendto 6 "send a message from a socket
cc = sendto(s, msg, len, flags, to, tolen);
result int cc; int s; void *msg; size_t len;
int flags; struct sockaddr *to; int tolen;
.DE
if the socket is not connected or:
.DS
.Fd send 4 "send a message from a socket
cc = send(s, msg, len, flags);
result int cc; int s; void *msg; size_t len; int flags;
.DE
if the socket is connected.
The corresponding receive primitives are:
.DS
.Fd recvfrom 6 "receive a message from a socket
msglen = recvfrom(s, buf, len, flags, from, fromlenaddr);
result int msglen; int s; result void *buf; size_t len; int flags;
result struct sockaddr *from; result int *fromlenaddr;
.DE
and:
.DS
.Fd recv 4 "receive a message from a socket
msglen = recv(s, buf, len, flags);
result int msglen; int s; result void *buf; size_t len; int flags;
.DE
.PP
In the unconnected case,
the parameters \fIto\fP and \fItolen\fP
specify the destination or source of the message, while
the \fIfrom\fP parameter stores the source of the message,
and \fI*fromlenaddr\fP initially gives the size of the \fIfrom\fP
buffer and is updated to reflect the true length of the \fIfrom\fP
address.
.PP
All calls cause the message to be received in or sent from
the message buffer of length \fIlen\fP bytes, starting at address \fIbuf\fP.
The \fIflags\fP specify
peeking at a message without reading it, sending or receiving
high-priority out-of-band messages, or other
special requests as follows:
.DS
.TS
l l.
MSG_OOB	/* process out-of-band data */
MSG_PEEK	/* peek at incoming message */
MSG_DONTROUTE	/* send without using routing tables */
MSG_EOR	/* data completes record */
MSG_TRUNC	/* data discarded before delivery */
MSG_CTRUNC	/* control data lost before delivery */
MSG_WAITALL	/* wait for full request or error */
MSG_DONTWAIT	/* this message should be nonblocking */
.TE
.DE
.Sh 4 "Scatter/gather and exchanging access rights
.PP
It is possible to scatter and gather data and to exchange access rights
with messages.  When either of these operations is involved,
the number of parameters to the call becomes large.
Thus, the system defines a message header structure, in \fI<sys/socket.h>\fP,
which can be
used to conveniently contain the parameters to the calls:
.DS
.TS
l s s s
l l l l.
struct msghdr {
	caddr_t	msg_name;	/* optional address */
	u_int	msg_namelen;	/* size of address */
	struct	iovec *msg_iov;	/* scatter/gather array */
	u_int	msg_iovlen;	/* # elements in msg_iov */
	caddr_t	msg_control;	/* ancillary data */
	u_int	msg_controllen;	/* ancillary data buffer len */
	int	msg_flags;	/* flags on received message */
};
.TE
.DE
Here \fImsg_name\fP and \fImsg_namelen\fP specify the source or destination
address if the socket is unconnected; \fImsg_name\fP may be given as
a null pointer if no names are desired or required.
The \fImsg_iov\fP and \fImsg_iovlen\fP describe the scatter/gather
locations, as described in section
.Xr 2.1.1 .
The data in the \fImsg_control\fP buffer is composed of
an array of variable length messages
used for additional information with or about a datagram
not expressible by flags.  The format is a sequence
of message elements headed by \fIcmsghdr\fP structures:
.DS
.TS
l s s s
l l l l.
struct cmsghdr {
	u_int	cmsg_len;	/* data byte count, including hdr */
	int	cmsg_level;	/* originating protocol */
	int	cmsg_type;	/* protocol-specific type */
	u_char	cmsg_data[\|];	/* variable length type specific data */
};
.TE
.DE
The following macros are provided for use with the \fImsg_control\fP buffer:
.DS
.TS
l l.
CMSG_FIRSTHDR(mhdr)	/* given msghdr, return first cmsghdr */
CMSG_NXTHDR(mhdr, cmsg)	/* given msghdr and cmsghdr, return next cmsghdr */
CMSG_DATA(cmsg)	/* given cmsghdr, return associated data pointer */
.TE
.DE
Access rights to be sent along with the message are specified
in one of these
\fIcmsghdr\fP structures, with level SOL_SOCKET and type SCM_RIGHTS.
In the PF_LOCAL domain these are an array of integer descriptors,
copied from the sending process and duplicated in the receiver.
.ne 1i
.LP
This structure is used in the operations
.Fn sendmsg
and
.Fn recvmsg :
.DS
.Fd sendmsg 3 "send a message from a socket
sendmsg(s, msg, flags);
int s; struct msghdr *msg; int flags;
.DE
.DS
.Fd recvmsg 3 "receive a message from a socket
msglen = recvmsg(s, msg, flags);
result int msglen; int s; result struct msghdr *msg; int flags;
.DE
.Sh 4 "Using read and write with sockets
.PP
The normal
.Fn read
and
.Fn write
calls may be applied to connected sockets and translated into
.Fn send
and
.Fn receive
calls from or to a single area of memory and discarding any rights
received.  A process may operate on a virtual circuit socket, a terminal
or a file with blocking or non-blocking input/output
operations without distinguishing the descriptor type.
.Sh 4 "Shutting down halves of full-duplex connections
.PP
A process that has a full-duplex socket such as a virtual circuit
and no longer wishes to read from or write to this socket can
give the call:
.DS
.Fd shutdown 2 "shut down part of a full-duplex connection
shutdown(s, direction);
int s, direction;
.DE
where \fIdirection\fP is 0 to not read further, 1 to not
write further, or 2 to completely shut the connection down.
If the underlying protocol supports unidirectional or bidirectional shutdown,
this indication will be passed to the peer.
For example, a shutdown for writing might produce an end-of-file
condition at the remote end.
.Sh 4 "Socket and protocol options
.PP
Sockets, and their underlying communication protocols, may
support \fIoptions\fP.  These options may be used to manipulate
implementation- or protocol-specific facilities. 
The
.Fn getsockopt
and
.Fn setsockopt
calls are used to control options:
.DS
.Fd getsockopt 5 "get options on socket
getsockopt(s, level, optname, optval, optlen);
int s, level, optname; result void *optval; result int *optlen;
.DE
.DS
.Fd setsockopt 5 "set options on socket
setsockopt(s, level, optname, optval, optlen);
int s, level, optname; void *optval; int optlen;
.DE
The option \fIoptname\fP is interpreted at the indicated
protocol \fIlevel\fP for socket \fIs\fP.  If a value is specified
with \fIoptval\fP and \fIoptlen\fP, it is interpreted by
the software operating at the specified \fIlevel\fP.  The \fIlevel\fP
SOL_SOCKET is reserved to indicate options maintained
by the socket facilities.  Other \fIlevel\fP values indicate
a particular protocol which is to act on the option request;
these values are normally interpreted as a ``protocol number''
within the protocol family.
.Sh 3 "PF_LOCAL domain
.PP
This section describes briefly the properties of the PF_LOCAL (``UNIX'')
communications domain.
.Sh 4 "Types of sockets
.PP
In the local domain,
the SOCK_STREAM abstraction provides pipe-like
facilities, while SOCK_DGRAM provides (usually)
reliable message-style communications.
.Sh 4 "Naming
.PP
Socket names are strings and may appear in the filesystem
name space.
.Sh 4 "Access rights transmission
.PP
The ability to pass descriptors with messages in this domain
allows migration of service within the system and allows
user processes to be used in building system facilities.
.Sh 3 "INTERNET domain
.PP
This section describes briefly how the Internet domain is
mapped to the model described in this section.  More
information will be found in the document describing the
network implementation in 4.4BSD (SMM:18).
.Sh 4 "Socket types and protocols
.PP
SOCK_STREAM is supported by the Internet TCP protocol;
SOCK_DGRAM by the UDP protocol.
Each is layered atop the transport-level Internet Protocol (IP).
The Internet Control Message Protocol is implemented atop/beside IP
and is accessible via a raw socket.
The SOCK_SEQPACKET
has no direct Internet family analogue; a protocol
based on one from the XEROX NS family and layered on
top of IP could be implemented to fill this gap.
.Sh 4 "Socket naming
.PP
Sockets in the Internet domain have names composed of a 32-bit
Internet address and a 16-bit port number.
Options may be used to
provide IP source routing or security options.
The 32-bit address is composed of network and host parts;
the network part is variable in size and is frequency encoded.
The host part may optionally be interpreted as a subnet field
plus the host on the subnet; this is is enabled by setting a network address
mask at boot time.
.Sh 4 "Access rights transmission
.PP
No access rights transmission facilities are provided in the Internet domain.
.Sh 4 "Raw access
.PP
The Internet domain allows the super-user access to the raw facilities
of IP.
These interfaces are modeled as SOCK_RAW sockets.
Each raw socket is associated with one IP protocol number,
and receives all traffic received for that protocol.
This approach allows administrative and debugging
functions to occur,
and enables user-level implementations of special-purpose protocols
such as inter-gateway routing protocols.
