.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)net.t	1.6 (Berkeley) 4/11/86
.\"
.NH
Network
.PP
There have been many changes in the kernel network support.
A major change is the addition of the Xerox NS protocols.
During the course of the integration of a second major protocol family
to the kernel, a number of Internet dependencies were removed from
common network code, and structural changes were made to accommodate
multiple protocol and address families simultaneously.
In addition, there were a large number of bug fixes and other cleanups
in the general networking code and in the Internet protocols.
The skeletal support for PUP that was in 4.2BSD has been removed.
.PP
The link layer drivers were changed to save an indication
of the incoming interface with each packet received, and this
information was made available to the protocol layer.
There were several problems that could be corrected
by taking advantage of this change.
The IMP code needed
to save error packets for software interrupt-level processing
in order to fix a race condition, but it needed to know which
interface had received the packet when decoding the addresses.
ICMP needed this information to support information requests
and (newly added) network mask requests properly, as these
request information about a specific network.
IP was able to take advantage of this change to implement redirect
generation when the incoming and outgoing interfaces are
the same.
.NH 2
Network common code
.PP
The changes in the common support routines for networking,
located in \fB/sys/net\fP, are described here.
.XP if_arp.h
This new file contains the definitions for the Address Resolution
Protocol (ARP) that are independent of the protocols using ARP.
.XP if.c
Most of the \fIif_ifwith*\fP functions that returned pointers
to \fIifnet\fP structures were converted to \fIifa_with*\fP equivalents
that return pointers to \fIifaddr\fP structures.
The old \fIif_ifonnetof\fP function is no longer provided,
as there is no concept of network number that is independent of address family. 
A new routine, \fIifa_ifwithdstaddr\fP, is provided
for use with point-to-point interfaces.
Interface \fIioctl\fPs that set interface addresses
are now passed to the appropriate protocol
using the PRU_CONTROL request of the \fIpr_usrreq\fP entry.
Additional \fIioctl\fP operations were added to get and set interface metrics
and to manipulate the ARP table (see \fInetinet/if_ether.c\fP).
.XP if.h
In 4.2BSD, the per-interface structure \fIifnet\fP held the address
of the interface, as well as the host and network numbers.
These have all been moved into a new structure, \fIifaddr\fP,
that is managed by the address family.
The \fIifnet\fP structure for an interface includes a pointer
to a linked list of addresses for the interface.
The IFF_ROUTE flag was also removed.
The software loopback interface is distinguished with a new flag.
Each interface now has a routing metric that is stored by the kernel
but only interpreted by user-level routing processes.
Additional interface \fIioctl\fP operations allow the metric
or the broadcast address to be read or set.
When received packets are passed to the receiving protocol,
they include a reference to the incoming interface;
a variant of the IF_DEQUEUE macro, IF_DEQUEUEIFP,
dequeues a packet and extracts the information about the receiving interface.
.XP if_loop.c
The software loopback driver now supports Xerox NS and Internet protocols.
It was modified to provide information on the incoming interface to the
receiving protocol.
The loopback driver's address(es) must now be set with \fIifconfig\fP.
.XP if_sl.c
This file was added to support a customized line discipline for
the use of an asynchronous serial line as a network interface.
Until the encapsulation is changed the interface supports only IP traffic.
.XP raw_cb.c
Raw sockets record the socket's protocol number and address family
in a \fIsockproto\fP structure in the raw connection block.
This allows a wildcard raw protocol entry to support
raw sockets using any single protocol.
.XP raw_cb.h
A \fIsockproto\fP description and a hook for protocol-specific options
were added to the raw protocol control block.
.XP raw_usrreq.c
A bug was fixed that caused received packet return addresses
to be corrupted periodically;
an mbuf was being used after it was freed.
Routing is no longer done here, although the raw socket protocol control block
includes a routing entry for use by the transport protocol.
The SO_DONTROUTE flag now works correctly with raw sockets.
.XP route.c
The routing algorithm was changed to use the first route found
in the table instead of the one with the lowest use count.
This reduces routing overhead and makes response more predictable.
The load-sharing effect of the old algorithm was minimal under
most circumstances.
Several races were fixed.
The hash indexes have been declared as unsigned; negative indices
worked for the network route hash table but not for the host hash table.
(This fix was included on most 4.2BSD tapes.)
New routes are placed at the front of the hash chains
instead of at the end.
The redirect handling is more robust;
redirects are only accepted from the current router,
and are not used if the new gateway is the local host.
The route allocated while checking a redirect is freed
even if the redirect is disbelieved.
Host redirects cause a new route to be created if the previous route
was to the network.
Routes created dynamically by redirects are marked as such.
When adding new routes, the gateway address is checked against
the addresses of point-to-point links for exact matches before using 
another interface on the appropriate network.
\fIRtinit\fP takes arguments for flags and operation separately,
allowing point-to-point interfaces to delete old routes.
.XP route.h
The size of the routing hash table has been changed to a power of two,
allowing unsigned modulus operations to be performed with a mask.
The size of the table is expanded if the GATEWAY option is configured.
