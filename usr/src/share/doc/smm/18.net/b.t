.\" Copyright (c) 1983 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)b.t	6.1 (Berkeley) %G%
.\"
.nr H2 1
.ds RH "Raw sockets
.NH
\s+2Raw sockets\s0
.PP
A raw socket is a mechanism which allows users direct access
to a lower level protocol.  Raw sockets are intended for knowledgeable
processes which wish to take advantage of some protocol
feature not directly accessible through the normal interface, or 
for the development of new protocols built atop existing lower level
protocols.  For example, a new version of TCP might be developed at the
user level by utilizing a raw IP socket for delivery of packets.
The raw IP socket interface attempts to provide an identical interface
to the one a protocol would have if it were resident in the kernel.
.PP
The raw socket support is built around a generic raw socket interface,
and (possibly) augmented by protocol-specific processing routines.
This section will describe the core of the raw socket interface.
.NH 2
Control blocks
.PP
Every raw socket has a protocol control block of the following form,
.DS
.if t .ta .5i 1.25i 2.9i
.if n .ta .7i 1.75i 4.0i
struct rawcb {
	struct	rawcb *rcb_next;	/* doubly linked list */
	struct	rawcb *rcb_prev;
	struct	socket *rcb_socket;	/* back pointer to socket */
	struct	sockaddr rcb_faddr;	/* destination address */
	struct	sockaddr rcb_laddr;	/* socket's address */
	caddr_t	rcb_pcb;		/* protocol specific stuff */
	short	rcb_flags;
};
.DE
All the control blocks are kept on a doubly linked list for
performing lookups during packet dispatch.  Associations may
be recorded in the control block and used by the output routine
in preparing packets for transmission.  The addresses are also
used to filter packets on input; this will be described in more
detail shortly.  If any protocol specific information is required,
it may be attached to the control block using the \fIrcb_pcb\fP
field. 
.PP
A raw socket interface is datagram oriented.  That is, each send
or receive on the socket requires a destination address.  This
address may be supplied by the user or stored in the control block
and automatically installed in the outgoing packet by the output
routine.  Since it is not possible to determine whether an address
is present or not in the control block, two flags, RAW_LADDR and
RAW_FADDR, indicate if a local and foreign address are present.
Another flag, RAW_DONTROUTE, indicates if routing should be performed
on outgoing packets.  If it is, a route is expected to be 
allocated for each
``new'' destination address.  That is, the first time a packet is
transmitted a route is determined, and thereafter each time the
destination address stored in \fIrcb_route\fP differs from
\fIrcb_faddr\fP, or \fIrcb_route.ro_rt\fP is zero, the old
route is discarded and a new one allocated. 
.NH 2
Input processing
.PP
Input packets are ``assigned'' to raw sockets based on a simple
pattern matching scheme.  Each network interface or protocol
gives packets
to the raw input routine with the call:
.DS
raw_input(m, proto, src, dst)
struct mbuf *m; struct sockproto *proto, struct sockaddr *src, *dst;
.DE
The data packet then has a generic header prepended to it of the
form
.DS
._f
struct raw_header {
	struct	sockproto raw_proto;
	struct	sockaddr raw_dst;
	struct	sockaddr raw_src;
};
.DE
and it is placed in a packet queue for the ``raw input protocol'' module.
Packets taken from this queue are copied into any raw sockets that
match the header according to the following rules,
.IP 1)
The protocol family of the socket and header agree.
.IP 2)
If the protocol number in the socket is non-zero, then it agrees
with that found in the packet header.
.IP 3)
If a local address is defined for the socket, the address format
of the local address is the same as the destination address's and
the two addresses agree bit for bit.
.IP 4)
The rules of 3) are applied to the socket's foreign address and the packet's
source address.
.LP
A basic assumption is that addresses present in the
control block and packet header (as constructed by the network
interface and any raw input protocol module) are in a canonical
form which may be ``block compared''.
.NH 2
Output processing
.PP
On output the raw \fIpr_usrreq\fP routine 
passes the packet and raw control block to the
raw protocol output routine for any processing required before
it is delivered to the appropriate network interface.  The
output routine is normally the only code required to implement
a raw socket interface.
.ds RH "Buffering and congestion control
.bp
