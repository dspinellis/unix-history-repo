.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)netinet.t	1.8 (Berkeley) 4/11/86
.\"
.hw SUBNETSARELOCAL
.NH
Internet network protocols
.PP
There are numerous bug fixes and extensions in the Internet
protocol support (\fB/sys/netinet\fP).
This section describes some of the more important changes
with very little detail.
As many of the changes span several source files,
and as it is very difficult to merge this code with earlier versions
of these protocols,
it is strongly recommended that the 4.3BSD network be adopted
intact, with local hacks merged into it only if necessary.
.NH 2
Internet common code
.PP
By far, the most important change in IP and the shared Internet support
layer is the addition of subnetwork addressing.
This facility is used (and required) by a number of large university
and other networks that include multiple physical networks
as well as connections with the DARPA Internet.
Subnet support allows a collection of interconnected local networks
to share a single network number,
hiding the complexity of the local environment and routing
from external hosts and gateways.
The subnet support in 4.3BSD conforms with the Internet standard
for subnet addressing, RFC-950.
For each network interface, a network mask is set along with the address.
This mask determines which portion of the address is the network number,
including the subnet, and by default is set according to the network
class (A, B, or C, with 8, 16, or 24 bits of network part, respectively).
Within a subnetted network each subnet appears as a distinct network;
externally, the entire network appears to be a single entity.
.PP
Another important change in IP addressing
is a change to the default IP broadcast address.
The default broadcast address is the address with a host part of all ones
(using the definition INADDR_BROADCAST),
in conformance with RFC-919.
In 4.2BSD, the broadcast address was the address with a host part
of all zeros (INADDR_ANY).
To facilitate the conversion process,
and to help avoid breaking networks with forwarded broadcasts,
4.3BSD allows the broadcast address to be set for each interface.
IP recognizes and accepts network broadcasts
as well as subnet broadcasts when subnets are enabled.
Such broadcasts normally originate from hosts that do not know about subnets.
IP also accepts old-style (4.2) broadcasts using a host part of all
zeros, either as a network or subnet broadcast.
An address of all ones
is recognized as ``broadcast on this network,'' and an address of all
zeros is accepted as well.
The latter two are sometimes used in
broadcast information requests or network mask requests in the course
of starting a diskless workstation.
ICMP includes support for the Network Mask Request and Response.
A new routine, \fIin_broadcast\fP,
was added for the use of link layer output routines
to determine whether an IP packet should be broadcast.
.PP
Network numbers are now stored and used unshifted to
minimize conversions and reduce the overhead associated with comparisons.
4.2BSD shifted network numbers to the low-order part of the word.
The structure defining Internet addresses no longer includes
the old IMP-host fields, but only a featureless 32-bit address.
.XP in.h
The definitions of Internet port numbers in this file
were deleted, as they have been superceded by the \fIgetservicebyname\fP
interface.
A definition was added for the single
option at the IP level accessible through \fIsetsockopt\fP,
IP_OPTIONS.
.XP in_pcb.h
The Internet protocol control block includes a pointer to an optional
mbuf containing IP options.
.XP in_var.h
This new header file contains the declaration of the Internet
variety of the per-interface address information.
The \fIin_ifaddr\fP structure includes the network, subnet, network mask
and broadcast information.
.XP in.c
The \fIif_*\fP routines which manipulate Internet addresses
were renamed to \fIin_*\fP.
\fIin_netof\fP and \fIin_lnaof\fP check whether the address
is for a directly-connected network, and if so they use the local
network mask to return the subnet/net and host portions, respectively.
\fIin_localaddr\fP determines whether an address corresponds
to a directly-connected network.
By default, this includes any subnet of a local network;
a configuration option, SUBNETSARELOCAL=0, changes this to return
true only for a directly-connected subnet or non-subnetted network.
Interface \fIioctl\fPs that get or set addresses or related status information
are forwarded to \fIin_control\fP, which implements them.
\fIin_iaonnetof\fP replaces \fIif_ifonnetof\fP for Internet addresses only.
.XP in_pcb.c
The destination address of a \fIconnect\fP may be given as INADDR_ANY (0)
as a shorthand notation for ``this host.''
This simplifies the process of connecting to local servers
such as the name-domain server that translates host names to addresses.
Also, the short-hand address INADDR_BROADCAST is converted to the broadcast
address for the primary local network; it fails if that network
is incapable of broadcast.
The source address for a connection or datagram
is selected according to the outgoing interface;
the initial route is allocated at this time and stored
in the protocol control block, so that it may be used again
when actually sending the packet(s).
The \fIin_pcbnotify\fP routine was generalized to apply any function
and/or report an error to all connections to a destination;
it is used to notify connections of routing changes and other
non-error situations as well as errors.
New entries have been added to this level to invalidate cached
routes when routing changes occur,
as well as to report possible routing failures detected by
higher levels.
.XP in_proto.c
The protocol switch table for Internet protocols includes entries
for the \fIctloutput\fP routines.
ICMP may be used with raw sockets.
A raw wildcard entry allows raw sockets to use any protocol
not already implemented in the kernel (e.g., EGP).
.NH 2
IP
.PP
Support was added for IP source routing and other IP options
(partly derived from BBN's implementation).
On output, IP options such as strict or loose source route and record
may be set by a client process using TCP, UDP or raw IP sockets.
IP properly updates source-route and record-route options
when forwarding (and leaves them in the packet, unlike 4.2 which
stripped them out after updating).
IP input preserves any source-routing information in an incoming packet
and passes it up to the receiving protocol upon request,
reversing it and arranging it in the same way as user-supplied options.
Both TCP and ICMP retrieve incoming source routes for use in replies.
Most of the option-handling code has been converted to use
\fIbcopy\fP instead of structure assignments when copying addresses,
as the alignment in the incoming packet may not be correct for the host.
This is not required on the VAX, but is needed on most other machines
running 4.2BSD.
.XP ip.h
The IP time-to-live field is decremented by one when forwarding;
in 4.2BSD this value was five.
.XP ip_var.h
Data structures and definitions were added for storing
IP options.
New fields have been added to the structure containing IP statistics.
.XP ip_input.c
The changes to save and present incoming IP source-routing information
to higher level protocols are in this file.
The identity of the interface that received the packet is also
determined by \fIip_input\fP and passed to the next protocol
receiving the packet.
To avoid using uninitialized data structures,
IP must not begin receiving packets until at least one Internet address
has been set.
A bug in the reassembly of IP packets with options has been corrected.
Machines with only a single network interface (in addition to the loopback
interface) no longer attempt to forward received IP packets that are
not destined for them;
they also do not respond with ICMP errors unless configured with
the GATEWAY option.
This change prevents large increases in network activity which used to result
when an IP packet that was broadcast was not understood as a broadcast.
A one-element route cache was added to the IP forwarding routine.
When a packet is forwarded using the same interface on which it arrived,
if the source host is on the directly-attached network,
an ICMP redirect is sent to the source.
If the route used for forwarding was a route to a host
or a route to a subnet,
a host redirect is used, otherwise a network redirect is sent.
The generation of redirects may be disabled by a configuration option,
IPSENDREDIRECTS=0.
More statistics are collected, in particular on traffic and fragmentation.
The \fIip_ctlinput\fP routine was moved to each of the upper-level
protocols, as they each have somewhat different requirements.
.XP ip_output.c
The IP output routine manages a cached route in the protocol
control block for each TCP, UDP or raw IP socket.
If the destination has changed, the route has been marked down,
or the route was freed because of a routing change, a new route
is obtained.
The route is not used if the IP_ROUTETOIF (aka SO_DONTROUTE or MSG_DONTROUTE)
option is present.
Preformed IP options passed to \fIip_output\fP are inserted,
changing the destination address as required.
The \fIip_ctloutput\fP routine allows options to be set for an individual
socket, validating and internalizing them as appropriate.
.XP raw_ip.c
The type-of-service and offset fields in the IP header
are set to zero on output.
The SO_DONTROUTE flag is handled properly.
.NH 2
ICMP
.PP
There have been numerous fixes and corrections to ICMP.
Length calculations have been corrected, allowing
most ICMP packet lengths to be received and allowing errors
to be sent about smaller input packets.
ICMP now uses information about the interface on which a message
was received to determine the
correct source address on returned error packets
and replies to information requests.
Support was added for the Network Mask Request.
Responses to source-routed requests use the reversed source route
for the return trip.
Timestamps are created with \fImicrotime\fP, allowing 1-millisecond
resolution.
The \fIicmp_error\fP routine is capable of sending ICMP redirects.
When processing network redirects, the returned source address is converted
to a network address before passing it to the routing redirect handler.
The translation of ICMP errors to Unix error returns was updated.
.NH 2
TCP
.PP
In addition to bug fixes, several performance changes have been
made to TCP.
Several of these address overall network performance and congestion
avoidance, while others address performance of an individual connection.
The most important changes concern the TCP send policy.
First, the sender silly-window syndrome avoidance strategy was fixed.
In 4.2BSD, the amount that could be sent was compared to the offered window,
and thus small amounts could still be sent if the receiver offered
a silly window.
Once this was fixed, there were problems with peers that never offered
windows large enough for a maximum segment, or at least 512 bytes
(e.g., the peer is a TAC or an IBM PC).
Code was then added to maintain estimates of the peer's receive and send
buffer sizes.
The send policy will now send if the offered
window is at least one-half of the receiver's buffer, as well as when
the window is at least a full-sized segment.
(When the window is large enough for all data that is queued,
the data will also be sent.)
The send buffer size estimate is not yet used, but is desired for a new
delayed-acknowledgement scheme that has yet to be tested.
Another problem that was exposed when the silly-window avoidance was fixed
was that the persist code didn't expect to be used with a non-zero window.
The persist now lasts only until the first timeout, at which time
a packet is sent of the largest size allowed by the window.
If this packet is not acknowledged, the output routine must begin retransmission
rather than returning to the persist state.
.PP
Another change related to the send policy is a strategy designed to minimize
the number of small packets outstanding on slow links.
This is an implementation of an algorithm proposed by John Nagle
in RFC-896.
The algorithm is very simple:
when there is outstanding, unacknowledged data pending
on a connection, new data are not sent unless they fill a maximum-sized
segment.
This allows bulk data transfers to proceed,
but causes small-packet traffic such as remote login to bundle together
data received during a single round-trip time.
On high-bandwidth, low-delay networks such as a local Ethernet,
this change seldom causes delay, but over slow links or across the Internet,
the number of small packets can be reduced considerably.
This algorithm does interact poorly with one type of usage, however,
as demonstrated by the X window system.
When small packets are sent in a stream, such as when doing rubber-banding
to position a new window, and when no echo or other acknowledgement
is being received from the other end of the connection,
the round-trip delay becomes as large as the delayed-acknowledgement timer
on the remote end.
For such clients, a TCP option may be set with \fIsetsockopt\fP
to defeat this part of the send policy.
.PP
For bulk-data transfers, the largest single change to improve performance
is to increase the size of the send and receive buffers.
The default buffer size in 4.3BSD is 4096 bytes, double the value in 4.2BSD.
These values allow more outstanding data and reduce the amount of time
waiting for a window update from the receiver.
They also improve the utility of the delayed-acknowledgement strategy.
The delayed acknowledgment strategy withholds acknowledgements
until a window update would uncover at least 35% of the window;
in 4.2BSD, with 1024-byte packets on an Ethernet and 2048-byte windows,
this took only a single packet.
With 4096-byte windows, up to 50% of the acknowledgements may be avoided.
.PP
The use of larger buffers might cause problems when bulk-data transfers
must traverse several networks and gateways with limited buffering capacity.
The source-quench ICMP message was provided to allow gateways in such
circumstances to cause source hosts to slow their rate of packet injection
into the network.
While 4.2BSD ignored such messages, the 4.3BSD TCP includes a mechanism
for throttling back the sender when a source quench is received.
This is done by creating an artificially small window (one which is 80%
of the outstanding data at the time the quench is received, but no less than
one segment).
This artificial congestion window is slowly opened as acknowledgements
are received.
The result under most circumstances is a slow fluctuation around the buffering
limit of the intermediate gateways, depending on the other traffic flowing
at the same time.
.PP
A final set of changes designed to improve network throughput
concerns the retransmission policy.
The retransmission timer is set according to the current round-trip
time estimate.
Unfortunately, the round-trip timing code in 4.2BSD had several bugs
which caused retransmissions to begin much too early.
These bugs in round trip timing have been corrected.
Also, the retransmission code has been tuned, using a faster
backoff after the first retransmission.
On an initial connection request where there is no round-trip time estimate,
a much more conservative policy is used.
When a slow link intervenes between the sender and the destination,
this policy avoids queuing large numbers of retransmitted connection requests
before a reply can be received. It also avoids saturation when
the destination host
is down or nonexistent.
During a connection, when the retransmission timer expires,
only a single packet is sent.
When only a single packet has been lost, this avoids resending
data that was successfully received;
when a host has gone down or become unreachable, it avoids sending
multiple packets at each timeout.
Once another acknowledgement is received, the transmission policy
returns to normal.
.PP
4.2BSD offered a maximum receive segment size of 1024 for all connections,
and accepted such offers whenever made.
However, that size was especially poor for the Arpanet
and other 1822-based IMP networks (sorry, make that PSN networks)
where the maximum packet size is 1007 bytes.
This was compounded by a bug in the LH/DH driver that did not allow
space for an end-of-packet bit in the receive buffer,
and thus maximum size packets that were received were split across buffers.
This, in turn, aggravated a hardware
problem causing small packets following a segmented packet to be concatenated
with the previous packet.
The result of this set of conditions was that performance across
the Arpanet was sometimes abominably slow.
The maximum size segment selected by 4.3BSD is chosen according
to the destination and the interface to be used.
The segment size chosen is somewhat less than the maximum transmission unit
of the outgoing interface.
If the destination is not local,
the segment size is a convenient small size near
the default maximum size (512 bytes).
This value is both the maximum segment size
offered to the sender by the receive side,
and the maximum size segment that will be sent.
Of course, the send size is also limited
to be no more than the receiver has indicated it is willing to receive.
.PP
The initial sequence number prototype for TCP is now
incremented much more quickly; this has exposed two bugs.
Both the window-update receiving code and the urgent data receiving
code compared sequence numbers to 0 the first time they were called
on a connection.  This fails if the initial sequence number has
wrapped around to negative numbers.  Both are now initialized
when the connection is set up.  This still remains a problem
in maintaining compatibility with 4.2BSD systems;
thus an option, TCP_COMPAT_42, was added to avoid using such sequence numbers
until 4.2 systems have been upgraded.
.PP
Additional changes in TCP are listed by source file:
.XP tcp_input.c
The common case of TCP data input, the arrival of the next
expected data segment with an empty reassembly queue, was made
into a simplified macro for efficiency.
\fITcp_input\fP was modified to know when it needed to call the output side,
reducing unnecessary tests for most acknowledgement-only packets.
The receive window size calculation on input was modified
to avoid shrinking the offered window;
this change was needed due to a change in input data
packaging by the link layer.
A bug in handling TCP packets received with both data and options
(that are not supposed to be used) has been corrected.
If data is received on a connection after the process has closed,
the other end is sent a reset, preventing connections from
hanging in CLOSE_WAIT on one end and FIN_WAIT_2 on the other.
(4.2BSD contained code to do this, but it was never executed
because such input packets had already been dropped
as being outside of the receive window.)
A timer is now started upon entering
FIN_WAIT_2 state if the local user has closed, closing the connection
if the final FIN is not received within a reasonable time.
Half-open connections are now reset more reliably; there were circumstances
under which one end could be rebooted, and new connection requests
that used the same port number might not receive a reset.
The urgent-data code was modified to remember which data had
already been read by the user, avoiding possible confusion if two
urgent-data signals were received close together.
Another change was made specifically for connections with a TAC.
The TAC doesn't fill in the window field on its initial packet (SYN),
and the apparent window is random.
There is some question as to the validity of the window field
if the packet does not have ACK set,
and therefore TCP was changed to ignore the window information
on those packets.
.XP tcp_output.c
The advertised window is never allowed to shrink,
in correspondence with the earlier change in the input handler.
The retransmit code was changed to check for shrinking windows,
updating the connection state rather than timing out
while waiting for acknowledgement.
The modifications to the send policy described above are largely
within this file.
.XP tcp_timer.c
The timer routines were changed to allow a longer wait for acknowledgements.
(TCP would generally time out before the routing protocol
had changed routes.)
.NH 2
UDP
.PP
An error in the checksumming of output UDP packets was corrected.
Checksums are now checked by default, unless the COMPAT_42 configuration
option is specified; it is provided to allow communication with the 4.2BSD UDP
implementation, which generates incorrect checksums.
When UDP datagrams are received for a port at which no process is listening,
ICMP unreachable messages are sent in response unless the input packet
was a broadcast.
The size of the receive buffer was increased, as several large datagrams
and their attached addresses could otherwise fill the buffer.
The time-to-live of output datagrams was reduced from 255
to 30.
UDP uses its own \fIctlinput\fP routine for handling of ICMP errors,
so that errors may be reported to the sender without closing the socket.
.NH 2
Address Resolution Protocol
.PP
The address resolution protocol has been generalized somewhat.
It was specific for IP on 10\  Mb/s Ethernet; it now handles multiple
protocols on 10 Mb/s Ethernet and could easily be adapted to other
hardware as well.
This change was made while adding ARP resolution
of trailer protocol addresses.
Hosts desiring to receive trailer
encapsulations must now indicate that by the use of ARP.  This allows
trailers to be used between cooperating 4.3 machines while using
non-trailer encapsulations with other hosts.
The negotiation need not be symmetrical: a VAX may request trailers,
for example, and a SUN may note this and send trailer packets
to the VAX without itself requesting trailers.
This change requires modifications to the 10 Mb/s Ethernet drivers,
which must provide an additional argument to \fIarpresolve\fP,
a pointer for the additional return value indicating whether trailer
encapsulations may be sent.
With this change, the IFF_NOTRAILERS flag on each interface is interpreted
to mean that trailers should not be requested.
Modifications to ARP from SUN Microsystems add \fIioctl\fP operations
to examine and modify entries in the ARP address translation table, 
and to allow ARP translations to be ``published.''
When future requests are received for Ethernet address translations,
if the translation is in the table and is marked as published,
they will be answered for that host.
Those modifications superceded the ``oldmap'' algorithmic translation
from IP addresses, which has been removed.
Packets are not forwarded to the loopback interface if it is not marked
up, and a bug causing an mbuf to be freed twice
if the loopback output fails was corrected.
ARP complains if a host lists the broadcast address as its Ethernet address.
The ARP tables were enlarged to reflect larger network configurations
now in use.
A new function for use in driver messages, \fIether_sprintf\fP,
formats a 48-bit Ethernet address and returns a pointer to the resulting string.
.NH 2
IMP support
.PP
The support facilities for connections to an 1822 (or X.25) IMP port
(\fB/sys/netimp\fP)
have had several bug fixes and one extension.
Unit numbers are now checked more carefully during autoconfiguration.
Code from BRL was installed to support class B and C networks.
Error packets received from the IMP such as Host Dead are queued
in the interrupt handler for reprocessing from a software interrupt,
avoiding state transitions in the protocols at priorities above \fIsplnet\fP.
The host-dead timer is no longer restarted when attempting new output,
as a persistent sender could otherwise prevent new output from being attempted
once a host was reported down.
The network number is always taken from the address
configured for the interface at boot time;
network 10 is no longer assumed.
A timer is used to prevent blocking if RFNM messages from the IMP are lost.
A race was fixed when freeing mbufs containing host table entries,
as the mbuf had been used after it was freed.
