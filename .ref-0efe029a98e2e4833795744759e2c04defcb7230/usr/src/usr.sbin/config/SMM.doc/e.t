.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)e.t	6.2 (Berkeley) %G%
.\"
.\".ds RH "Network configuration options
.bp
.LG
.B
.ce
APPENDIX E. NETWORK CONFIGURATION OPTIONS
.sp
.R
.NL
.PP
The network support in the kernel is self-configuring
according to the protocol support options (INET and NS) and the network
hardware discovered during autoconfiguration.
There are several changes that may be made to customize network behavior
due to local restrictions.
Within the Internet protocol routines, the following options
set in the system configuration file are supported:
.IP \fBGATEWAY\fP
.br
The machine is to be used as a gateway.
This option currently makes only minor changes.
First, the size of the network routing hash table is increased.
Secondly, machines that have only a single hardware network interface
will not forward IP packets; without this option, they will also refrain
from sending any error indication to the source of unforwardable packets.
Gateways with only a single interface are assumed to have missing
or broken interfaces, and will return ICMP unreachable errors to hosts
sending them packets to be forwarded.
.IP \fBTCP_COMPAT_42\fP
.br
This option forces the system to limit its initial TCP sequence numbers
to positive numbers.
Without this option, 4.3BSD systems may have problems with TCP connections
to 4.2BSD systems that connect but never transfer data.
The problem is a bug in the 4.2BSD TCP; this option should be used
during the period of conversion to 4.3BSD.
.IP \fBIPFORWARDING\fP
.br
Normally, 4.3BSD machines with multiple network interfaces
will forward IP packets received that should be resent to another host.
If the line ``options IPFORWARDING="0"'' is in the system configuration
file, IP packet forwarding will be disabled.
.IP \fBIPSENDREDIRECTS\fP
.br
When forwarding IP packets, 4.3BSD IP will note when a packet is forwarded
using the same interface on which it arrived.
When this is noted, if the source machine is on the directly-attached
network, an ICMP redirect is sent to the source host.
If the packet was forwarded using a route to a host or to a subnet,
a host redirect is sent, otherwise a network redirect is sent.
The generation of redirects may be inhibited with the configuration
option ``options IPSENDREDIRECTS="0".''
.br
.IP \fBSUBNETSARELOCAL\fP
TCP calculates a maximum segment size to use for each connection,
and sends no datagrams larger than that size.
This size will be no larger than that supported on the outgoing
interface.
Furthermore, if the destination is not on the local network,
the size will be no larger than 576 bytes.
For this test, other subnets of a directly-connected subnetted
network are considered to be local unless the line
``options SUBNETSARELOCAL="0"'' is used in the system configuration file.
.IP \fBCOMPAT_42\fP
.br
This option, intended as a catchall for 4.2BSD compatibility options,
has only a single function thus far.
It disables the checking of UDP input packet checksums.
As the calculation of UDP packet checksums was incorrect in 4.2BSD,
this option allows a 4.3BSD system to receive UDP packets from
a 4.2BSD system.
.LP
The following options are supported by the Xerox NS protocols:
.IP \fBNSIP\fP
.br
This option allows NS IDP datagrams to be encapsulated in Internet IP
packets for transmission to a collaborating NSIP host.
This may be used to pass IDP packets through IP-only link layer networks.
See
.IR nsip (4P)
for details.
.IP \fBTHREEWAYSHAKE\fP
.br
The NS Sequenced Packet Protocol does not require a three-way handshake
before considering a connection to be in the established state.
(A three-way handshake consists of a connection request, an acknowledgement
of the request along with a symmetrical opening indication,
and then an acknowledgement of the reciprocal opening packet.)
This option forces a three-way handshake before data may be transmitted
on Sequenced Packet sockets.
