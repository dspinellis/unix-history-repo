.\" Copyright (c) 1983 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)6.t	5.1 (Berkeley) %G%
.\"
.nr H2 1
.ds RH "Internal layering
.NH
\s+2Internal layering\s0
.PP
The internal structure of the network system is divided into
three layers.  These
layers correspond to the services provided by the socket
abstraction, those provided by the communication protocols,
and those provided by the hardware interfaces.  The communication
protocols are normally layered into two or more individual
cooperating layers, though they are collectively viewed
in the system as one layer providing services supportive
of the appropriate socket abstraction.
.PP
The following sections describe the properties of each layer
in the system and the interfaces each must conform to.
.NH 2
Socket layer
.PP
The socket layer deals with the interprocess communications
facilities provided by the system.  A socket is a bidirectional
endpoint of communication which is ``typed'' by the semantics
of communication it supports.  The system calls described in
the \fI4.2BSD System Manual\fP are used to manipulate sockets.
.PP
A socket consists of the following data structure:
.DS
._f
struct socket {
	short	so_type;		/* generic type */
	short	so_options;		/* from socket call */
	short	so_linger;		/* time to linger while closing */
	short	so_state;		/* internal state flags */
	caddr_t	so_pcb;			/* protocol control block */
	struct	protosw *so_proto;	/* protocol handle */
	struct	socket *so_head;	/* back pointer to accept socket */
	struct	socket *so_q0;		/* queue of partial connections */
	short	so_q0len;		/* partials on so_q0 */
	struct	socket *so_q;		/* queue of incoming connections */
	short	so_qlen;		/* number of connections on so_q */
	short	so_qlimit;		/* max number queued connections */
	struct	sockbuf so_snd;		/* send queue */
	struct	sockbuf so_rcv;		/* receive queue */
	short	so_timeo;		/* connection timeout */
	u_short	so_error;		/* error affecting connection */
	short	so_oobmark;		/* chars to oob mark */
	short	so_pgrp;		/* pgrp for signals */
};
.DE
.PP
Each socket contains two data queues, \fIso_rcv\fP and \fIso_snd\fP,
and a pointer to routines which provide supporting services. 
The type of the socket,
\fIso_type\fP is defined at socket creation time and used in selecting
those services which are appropriate to support it.  The supporting
protocol is selected at socket creation time and recorded in
the socket data structure for later use.  Protocols are defined
by a table of procedures, the \fIprotosw\fP structure, which will
be described in detail later.  A pointer to a protocol specific
data structure,
the ``protocol control block'' is also present in the socket structure.
Protocols control this data structure and it normally includes a
back pointer to the parent socket structure(s) to allow easy
lookup when returning information to a user 
(for example, placing an error number in the \fIso_error\fP
field).  The other entries in the socket structure are used in
queueing connection requests, validating user requests, storing
socket characteristics (e.g.
options supplied at the time a socket is created), and maintaining
a socket's state.
.PP
Processes ``rendezvous at a socket'' in many instances.  For instance,
when a process wishes to extract data from a socket's receive queue
and it is empty, or lacks sufficient data to satisfy the request,
the process blocks, supplying the address of the receive queue as
an ``wait channel' to be used in notification.  When data arrives
for the process and is placed in the socket's queue, the blocked
process is identified by the fact it is waiting ``on the queue''.
.NH 3
Socket state
.PP
A socket's state is defined from the following:
.DS
.if t .ta .6i 2.3i 3.0i
.if n .ta .84i 2.5i 3.20i
#define	SS_NOFDREF	0x001	/* no file table ref any more */
#define	SS_ISCONNECTED	0x002	/* socket connected to a peer */
#define	SS_ISCONNECTING	0x004	/* in process of connecting to peer */
#define	SS_ISDISCONNECTING	0x008	/* in process of disconnecting */
#define	SS_CANTSENDMORE	0x010	/* can't send more data to peer */
#define	SS_CANTRCVMORE	0x020	/* can't receive more data from peer */
#define	SS_CONNAWAITING	0x040	/* connections awaiting acceptance */
#define	SS_RCVATMARK	0x080	/* at mark on input */

#define	SS_PRIV	0x100	/* privileged */
#define	SS_NBIO	0x200	/* non-blocking ops */
#define	SS_ASYNC	0x400	/* async i/o notify */
.DE
.PP
The state of a socket is manipulated both by the protocols
and the user (through system calls).
When a socket is created the state is defined based on the type of
input/output the user wishes to perform.  ``Non-blocking'' I/O  implies
a process should never be blocked to await resources.  Instead, any
call which would block returns prematurely
with the error EWOULDBLOCK (the service request may be partially
fulfilled, e.g. a request for more data than is present).
.PP
If a process requested ``asynchronous'' notification of events
related to the socket the SIGIO signal is posted to the process.
An event is a change in the socket's state,
examples of such occurances are: space
becoming available in the send queue, new data available in the
receive queue, connection establishment or disestablishment, etc. 
.PP
A socket may be marked ``priviledged'' if it was created by the
super-user.  Only priviledged sockets may
send broadcast packets, or bind
addresses in priviledged portions of an address space.
.NH 3
Socket data queues
.PP
A socket's data queue contains a pointer to the data stored in
the queue and other entries related to the management of
the data.  The following structure defines a data queue:
.DS
._f
struct sockbuf {
	short	sb_cc;		/* actual chars in buffer */
	short	sb_hiwat;	/* max actual char count */
	short	sb_mbcnt;	/* chars of mbufs used */
	short	sb_mbmax;	/* max chars of mbufs to use */
	short	sb_lowat;	/* low water mark */
	short	sb_timeo;	/* timeout */
	struct	mbuf *sb_mb;	/* the mbuf chain */
	struct	proc *sb_sel;	/* process selecting read/write */
	short	sb_flags;	/* flags, see below */
};
.DE
.PP
Data is stored in a queue as a chain of mbufs.  The actual
count of characters as well as high and low water marks are
used by the protocols in controlling the flow of data.
The socket routines cooperate in implementing the flow control
policy by blocking a process when it requests to send data and
the high water mark has been reached, or when it requests to
receive data and less than the low water mark is present
(assuming non-blocking I/O has not been specified).
.PP
When a socket is created, the supporting protocol ``reserves'' space
for the send and receive queues of the socket.
The actual storage associated with a
socket queue may fluctuate during a socket's lifetime, but is assumed
this reservation will always allow a protocol to acquire enough memory
to satisfy the high water marks.
.PP
The timeout and select values are manipulated by the socket routines
in implementing various portions of the interprocess communications
facilities and will not be described here.
.PP
A socket queue has a number of flags used in synchronizing access
to the data and in acquiring resources;
.DS
._d
#define	SB_LOCK	0x01	/* lock on data queue (so_rcv only) */
#define	SB_WANT	0x02	/* someone is waiting to lock */
#define	SB_WAIT	0x04	/* someone is waiting for data/space */
#define	SB_SEL	0x08	/* buffer is selected */
#define	SB_COLL	0x10	/* collision selecting */
.DE
The last two flags are manipulated by the system in implementing
the select mechanism.
.NH 3
Socket connection queueing
.PP
In dealing with connection oriented sockets (e.g. SOCK_STREAM)
the two sides are considered distinct.  One side is termed
\fIactive\fP, and generates connection requests.  The other
side is called \fIpassive\fP and accepts connection requests.
.PP
From the passive side, a socket is created with the option
SO_ACCEPTCONN specified, 
creating two queues of sockets: \fIso_q0\fP for connections
in progress and \fIso_q\fP for connections already made and
awaiting user acceptance.
As a protocol is preparing incoming connections, it creates
a socket structure queued on \fIso_q0\fP by calling the routine
\fIsonewconn\fP().  When the connection
is established, the socket structure is then transfered
to \fIso_q\fP, making it available for an accept.
.PP
If an SO_ACCEPTCONN socket is closed with sockets on either
\fIso_q0\fP or \fIso_q\fP, these sockets are dropped.
.NH 2
Protocol layer(s)
.PP
Protocols are described by a set of entry points and certain
socket visible characteristics, some of which are used in
deciding which socket type(s) they may support.  
.PP
An entry in the ``protocol switch'' table exists for each
protocol module configured into the system.  It has the following form:
.DS
._f
struct protosw {
	short	pr_type;		/* socket type used for */
	short	pr_family;		/* protocol family */
	short	pr_protocol;		/* protocol number */
	short	pr_flags;		/* socket visible attributes */
/* protocol-protocol hooks */
	int	(*pr_input)();		/* input to protocol (from below) */
	int	(*pr_output)();		/* output to protocol (from above) */
	int	(*pr_ctlinput)();	/* control input (from below) */
	int	(*pr_ctloutput)();	/* control output (from above) */
/* user-protocol hook */
	int	(*pr_usrreq)();		/* user request */
/* utility hooks */
	int	(*pr_init)();		/* initialization routine */
	int	(*pr_fasttimo)();	/* fast timeout (200ms) */
	int	(*pr_slowtimo)();	/* slow timeout (500ms) */
	int	(*pr_drain)();		/* flush any excess space possible */
};
.DE
.PP
A protocol is called through the \fIpr_init\fP entry before any other.
Thereafter it is called every 200 milliseconds through the
\fIpr_fasttimo\fP entry and
every 500 milliseconds through the \fIpr_slowtimo\fP for timer based actions.
The system will call the \fIpr_drain\fP entry if it is low on space and
this should throw away any non-critical data.
.PP
Protocols pass data between themselves as chains of mbufs using
the \fIpr_input\fP and \fIpr_output\fP routines.  \fIPr_input\fP
passes data up (towards
the user) and \fIpr_output\fP passes it down (towards the network); control
information passes up and down on \fIpr_ctlinput\fP and \fIpr_ctloutput\fP.
The protocol is responsible for the space occupied by any the
arguments to these entries and must dispose of it.
.PP
The \fIpr_userreq\fP routine interfaces protocols to the socket
code and is described below.
.PP
The \fIpr_flags\fP field is constructed from the following values:
.DS
._d
#define	PR_ATOMIC	0x01		/* exchange atomic messages only */
#define	PR_ADDR	0x02		/* addresses given with messages */
#define	PR_CONNREQUIRED	0x04		/* connection required by protocol */
#define	PR_WANTRCVD	0x08		/* want PRU_RCVD calls */
#define	PR_RIGHTS	0x10		/* passes capabilities */
.DE
Protocols which are connection-based specify the PR_CONNREQUIRED
flag so that the socket routines will never attempt to send data
before a connection has been established.  If the PR_WANTRCVD flag
is set, the socket routines will notfiy the protocol when the user
has removed data from the socket's receive queue.  This allows
the protocol to implement acknowledgement on user receipt, and
also update windowing information based on the amount of space
available in the receive queue.  The PR_ADDR field indicates any
data placed in the socket's receive queue will be preceded by the
address of the sender.  The PR_ATOMIC flag specifies each \fIuser\fP
request to send data must be performed in a single \fIprotocol\fP send
request; it is the protocol's responsibility to maintain record
boundaries on data to be sent.  The PR_RIGHTS flag indicates the
protocol supports the passing of capabilities;  this is currently
used only the protocols in the UNIX protocol family.
.PP
When a socket is created, the socket routines scan the protocol
table looking for an appropriate protocol to support the type of
socket being created.  The \fIpr_type\fP field contains one of the
possible socket types (e.g. SOCK_STREAM), while the \fIpr_family\fP
field indicates which protocol family the protocol belongs to.
The \fIpr_protocol\fP field contains the protocol number of the
protocol, normally a well known value.
.NH 2
Network-interface layer
.PP
Each network-interface configured into a system defines a
path through which packets may be sent and received.
Normally a hardware device is associated with this interface,
though there is no requirement for this (for example, all
systems have a software ``loopback'' interface used for 
debugging and performance analysis).
In addition to manipulating the hardware device, an interface
module is responsible
for encapsulation and deencapsulation of any low level header
information required to deliver a message to it's destination.
The selection of which interface to use in delivering packets
is a routing decision carried out at a
higher level than the network-interface layer.  Each interface
normally identifies itself at boot time to the routing module
so that it may be selected for packet delivery.
.PP
An interface is defined by the following structure,
.DS
._f
struct ifnet {
	char	*if_name;		/* name, e.g. ``en'' or ``lo'' */
	short	if_unit;		/* sub-unit for lower level driver */
	short	if_mtu;			/* maximum transmission unit */
	int	if_net;			/* network number of interface */
	short	if_flags;		/* up/down, broadcast, etc. */
	short	if_timer;		/* time 'til if_watchdog called */
	int	if_host[2];		/* local net host number */
	struct	sockaddr if_addr;	/* address of interface */
	union {
		struct	sockaddr ifu_broadaddr;
		struct	sockaddr ifu_dstaddr;
	} if_ifu;
	struct	ifqueue if_snd;		/* output queue */
	int	(*if_init)();		/* init routine */
	int	(*if_output)();		/* output routine */
	int	(*if_ioctl)();		/* ioctl routine */
	int	(*if_reset)();		/* bus reset routine */
	int	(*if_watchdog)();	/* timer routine */
	int	if_ipackets;		/* packets received on interface */
	int	if_ierrors;		/* input errors on interface */
	int	if_opackets;		/* packets sent on interface */
	int	if_oerrors;		/* output errors on interface */
	int	if_collisions;		/* collisions on csma interfaces */
	struct	ifnet *if_next;
};
.DE
.PP
Each interface has a send queue and routines used for 
initialization, \fIif_init\fP, and output, \fIif_output\fP.
If the interface resides on a system bus, the routine \fIif_reset\fP
will be called after a bus reset has been performed. 
An interface may also
specify a timer routine, \fIif_watchdog\fP, which should be called
every \fIif_timer\fP seconds (if non-zero).
.PP
The state of an interface and certain characteristics are stored in
the \fIif_flags\fP field.  The following values are possible:
.DS
._d
#define	IFF_UP	0x1	/* interface is up */
#define	IFF_BROADCAST	0x2	/* broadcast address valid */
#define	IFF_DEBUG	0x4	/* turn on debugging */
#define	IFF_ROUTE	0x8	/* routing entry installed */
#define	IFF_POINTOPOINT	0x10	/* interface is point-to-point link */
#define	IFF_NOTRAILERS	0x20	/* avoid use of trailers */
#define	IFF_RUNNING	0x40	/* resources allocated */
#define	IFF_NOARP	0x80	/* no address resolution protocol */
.DE
If the interface is connected to a network which supports transmission
of \fIbroadcast\fP packets, the IFF_BROADCAST flag will be set and
the \fIif_broadaddr\fP field will contain the address to be used in
sending or accepting a broadcast packet.  If the interface is associated
with a point to point hardware link (for example, a DEC DMR-11), the
IFF_POINTOPOINT flag will be set and \fIif_dstaddr\fP will contain the
address of the host on the other side of the connection.  These addresses
and the local address of the interface, \fIif_addr\fP, are used in
filtering incoming packets.  The interface sets IFF_RUNNING after
it has allocated system resources and posted an initial read on the
device it manages.  This state bit is used to avoid multiple allocation
requests when an interface's address is changed.  The IFF_NOTRAILERS
flag indicates the interface should refrain from using a \fItrailer\fP
encapsulation on outgoing packets; \fItrailer\fP protocols are described
in section 14.  The IFF_NOARP flag indicates the interface should not
use an ``address resolution protocol'' in mapping internetwork addresses
to local network addresses.
.PP
The information stored in an \fIifnet\fP structure for point to point
communication devices is not currently used by the system internally.
Rather, it is used by the user level routing process in determining
host network connections and in initially devising routes (refer to
chapter 10 for more information).
.PP
Various statistics are also stored in the interface structure.  These
may be viewed by users using the \fInetstat\fP(1) program.
.PP
The interface address and flags may be set with the SIOCSIFADDR and
SIOCSIFFLAGS ioctls.  SIOCSIFADDR is used to initially define each
interface's address; SIOGSIFFLAGS can be used to mark
an interface down and perform site-specific configuration.
.NH 3
UNIBUS interfaces
.PP
All hardware related interfaces currently reside on the UNIBUS.
Consequently a common set of utility routines for dealing
with the UNIBUS has been developed.  Each UNIBUS interface
utilizes a structure of the following form:
.DS
.if t .ta .5i 1.25i 2.8i
.if n .ta .7i 1.75i 3.8i
struct	ifuba {
	short	ifu_uban;			/* uba number */
	short	ifu_hlen;			/* local net header length */
	struct	uba_regs *ifu_uba;		/* uba regs, in vm */
	struct ifrw {
.if t .ta .5i 1.25i 2.0i 2.8i
.if n .ta .7i 1.75i 2.75i 3.8i
		caddr_t	ifrw_addr;		/* virt addr of header */
		int	ifrw_bdp;		/* unibus bdp */
		int	ifrw_info;		/* value from ubaalloc */
		int	ifrw_proto;		/* map register prototype */
		struct	pte *ifrw_mr;		/* base of map registers */
	} ifu_r, ifu_w;
.if t .ta .5i 1.25i 2.8i
.if n .ta .7i 1.75i 3.8i
	struct	pte ifu_wmap[IF_MAXNUBAMR];	/* base pages for output */
	short	ifu_xswapd;			/* mask of clusters swapped */
	short	ifu_flags;			/* used during uballoc's */
	struct	mbuf *ifu_xtofree;		/* pages being dma'd out */
};
.DE
.PP
The \fIif_uba\fP structure describes UNIBUS resources held by
an interface.
IF_NUBAMR map registers are held for datagram data, starting
at \fIifr_mr\fP.  UNIBUS map register \fIifr_mr\fP[\-1]
maps the local network header
ending on a page boundary.  UNIBUS data paths are
reserved for read and for
write, given by \fIifr_bdp\fP.  The prototype of the map
registers for read and for write is saved in \fIifr_proto\fP.
.PP
When write transfers are not full pages on page boundaries
the data is just copied into the pages mapped on the UNIBUS
and the transfer is started.
If a write transfer is of a (1024 byte) page size and on a page
boundary, UNIBUS page table entries are swapped to reference
the pages, and then the initial pages are
remapped from \fIifu_wmap\fP when the transfer completes.
.PP
When read transfers give whole pages of data to be input, page
frames are allocated from a network page list and traded
with the pages already containing the data, mapping the allocated
pages to replace the input pages for the next UNIBUS data input.
.PP
The following utility routines are available for use in
writing network interface drivers, all use the \fIifuba\fP
structure described above.
.IP "if_ubainit(ifu, uban, hlen, nmr);"
.br
\fIif_ubainit\fP allocates resources on UNIBUS adaptor \fIuban\fP
and stores the resultant information
in the \fIifuba\fP structure pointed to by \fIifu\fP. 
It is called only at boot time or after a UNIBUS reset. 
Two data paths (buffered or unbuffered,
depending on the \fIifu_flags\fP field) are allocated, one for
reading and one for writing.  The \fInmr\fP parameter indicates
the number of UNIBUS mapping registers required to map a maximal
sized packet onto the UNIBUS, while \fIhlen\fP specifies the size
of a local network header, if any, which should be mapped separately
from the data (see the description of trailer protocols in chapter 14).
Sufficient UNIBUS mapping registers and pages of memory are allocated
to initialize the input data path for an initial read.  For the output
data path, mapping registers and pages of memory are also allocated
and mapped onto the UNIBUS.  The pages associated with the output
data path are held in reserve in the event a write requires copying
non-page-aligned data (see \fIif_wubaput\fP below).
If \fIif_ubainit\fP is called with resources already allocated,
they will be used instead of allocating new ones (this normally
occurs after a UNIBUS reset).
A 1 is returned when allocation and initialization is successful,
0 otherwise.
.IP "m = if_rubaget(ifu, totlen, off0);"
.br
\fIif_rubaget\fP pulls read data off an interface.  \fItotlen\fP
specifies the length of data to be obtained, not counting the
local network header.  If \fIoff0\fP is non-zero, it indicates
a byte offset to a trailing local network header which should be
copied into a
separate mbuf and prepended to the front of the resultant mbuf
chain.  When page sized units of data are present and are
page-aligned, the previously mapped data pages are remapped
into the mbufs and swapped with fresh pages; thus avoiding
any copying.  A 0 return value indicates a failure to allocate
resources.
.IP "if_wubaput(ifu, m);"
.br
\fIif_wubaput\fP maps a chain of mbufs onto a network interface
in preparation for output.  The chain includes any local network
header, which is copied so that it resides in the mapped and
aligned I/O space.  Any other mbufs which contained non page
sized data portions are also copied to the I/O space.
Pages mapped from a previous output operation (no longer needed)
are unmapped and returned to the network page pool.
.ds RH "Socket/protocol interface
.bp
