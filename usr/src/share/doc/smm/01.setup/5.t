.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)5.t	5.1 (Berkeley) %G%
.\"
.ds lq ``
.ds rq ''
.ds LH "Installing/Operating 4.2BSD
.ds RH Network setup
.ds CF \*(DY
.LP
.nr H1 5
.nr H2 0
.bp
.LG
.B
.ce
5. NETWORK SETUP
.sp 2
.R
.NL
.PP
4.2BSD provides support for the DARPA standard Internet
protocols IP, ICMP, TCP, and UDP.  These protocols may be used
on top of a variety of hardware devices ranging from the
IMP's used in the ARPANET to local area network controllers
for the Ethernet.  Network services are split between the
kernel (communication protocols) and user programs (user
services such as TELNET and FTP).  This section describes
how to configure your system to use the networking support.
.NH 2
System configuration
.PP
To configure the kernel to include the Internet communication
protocols, define the INET option and include the pseudo-devices
``inet'', ``pty'', and ``loop'' in your machine's configuration
file.  
The ``pty'' pseudo-device forces the pseudo terminal device driver
to be configured into the system, see \fIpty\fP\|(4), while
the ``loop'' pseudo-device forces inclusion of the software loopback
interface driver.  The loop driver
is used in network testing and also by the mail system.
.PP
If you are planning to use the network facilities on a 10Mb/s
Ethernet, the pseudo-device ``ether'' should also be included
in the configuration; this forces inclusion of the Address Resolution
Protocol module used in mapping between 48-bit Ethernet
and 32-bit Internet addresses.
Also, if you have an imp,
you will need to include the pseudo-device ``imp.''
.PP
Before configuring the appropriate networking hardware, you should
consult the manual pages in section 4 of the programmer's manual.
The following table lists the devices for which software support
exists.
.DS
.TS
l l.
Device name	Manufacturer and product
_
acc	ACC LH/DH interface to IMP
css	DEC IMP-11A interface to IMP
dmc	DEC DMC-11 (also works with DMR-11)
ec	3Com 10Mb/s Ethernet
en	Xerox 3Mb/s prototype Ethernet (not a product)
hy	NSC Hyperchannel, w/ DR-11B and PI-13 interfaces
il	Interlan 10Mb/s Ethernet
pcl	DEC PCL-11
un	Ungermann-Bass network w/ DR-11W interface
vv	Proteon ring network (V2LNI)
.TE
.DE
.PP
All network interface drivers require some or all of their
host address be defined at boot time.  This is accomplished
with
.IR ifconfig (8C)
commands included in the /etc/rc.local file.
Interfaces which are able to dynamically deduce the host
part of an address, but not the network number, take the
network number from the address specified with
.IR ifconfig .
Hosts which use a more complex address mapping scheme, such
as the Address Resolution Protocol,
.IR arp (4),
require the full address.
The manual page for each network interface
describes the method used to establish a host's address.
.IR Ifconfig (8)
can also be used to set options for the interface at boot time.
These options include disabling the use of the Address Resolution Protocol
and/or the use of trailer encapsulation; this is useful if a network
is shared with hosts running software which is unable to perform these
functions.
Options are set independently for each interface, and
apply to all packets sent using that interface.
An alternative approach to ARP is to divide the address range,
using ARP only for those addresses below the cutoff and using
another mapping above this constant address; see the source
(/sys/netinet/if_ether.c) for more information.
.PP
In order to use the pseudo terminals just configured, device
entries must be created in the /dev directory.  To create 16
pseudo terminals (plenty, unless you have a heavy network load)
perform the following commands.
.DS
\fB#\fP cd /dev
\fB#\fP MAKEDEV pty0
.DE
More pseudo terminals may be made by specifying \fIpty1\fP, \fIpty2\fP,
etc.  The kernel normally includes support for 32 pseudo terminals
unless the configuration file specifies a different number.
Each pseudo terminal actually consists of two files in /dev:
a master and a slave.  The master pseudo terminal file is named
/dev/pty?, while the slave side is /dev/ttyp?.  Pseudo terminals
are also used by the \fIscript\fP\|(1) program.  In addition to
creating the pseudo terminals, be sure to install them in the
.I /etc/ttys
file (with a `0' in the first column so no
.I getty
is started), and in the 
.I /etc/ttytype
file (with type ``network'').
.PP
When configuring multiple networks some thought must be given
to the ordering of the devices in the configuration file.  The first
network interface configured in the system is used as the default
network when the system is forced to assign a local address to
a socket.  This means that your most widely known network should
always be placed first in the configuration file.  For example,
hosts attached to both the ARPANET and our local area
network have devices configured in the order show below.
.DS
.ta 1.0i 1.5i 2.0i 2.5i 3.0i 3.5i
device	acc0	at uba? csr 0167600 vector accrint accxint
device	en0	at uba? csr 0161000 vector enxint enrint encollide
.DE
.NH 2
Network data bases
.PP
A number of data files are used by the network library routines
and server programs.  Most of these files are host independent
and updated only rarely.
.DS
.TS
l l l.
File	Manual reference	Use
_
/etc/hosts	\fIhosts\fP\|(5)	host names
/etc/networks	\fInetworks\fP\|(5)	network names
/etc/services	\fIservices\fP\|(5)	list of known services
/etc/protocols	\fIprotocols\fP\|(5)	protocol names
/etc/hosts.equiv	\fIrshd\fP\|(8C)	list of ``trusted'' hosts
/etc/rc.local	\fIrc\fP\|(8)	command script for starting servers
/etc/ftpusers	\fIftpd\fP\|(8C)	list of ``unwelcome'' ftp users
.TE
.DE
The files distributed are set up for ARPANET or other Internet hosts.
Local networks and hosts should be added to describe the local
configuration; the Berkeley entries may serve as examples
(see also the next section).
Network numbers will have to be chosen for each ethernet.
For sites not connected to the Internet,
these can be chosen more or less arbitrarily,
otherwise the normal channels should be used for allocation of network
numbers.
.NH 3
Regenerating /etc/hosts and /etc/networks
.PP
The host and network name data bases are normally derived from
a file retrieved from the Internet Network Information Center at
SRI.  To do this you should use the program /etc/gettable
to retrieve the NIC host data base, and the program /etc/htable
to convert it to the format used by the libraries.
.DS
\fB#\fP cd /usr/src/ucb/netser/htable
\fB#\fP /etc/gettable sri-nic
\fBConnection to sri-nic opened.\fP
\fBHost table received.\fP
\fBConnection to sri-nic closed.\fP
\fB#\fP /etc/htable hosts.txt
\fBWarning, no localgateways file.\fP
\fB#\fP
.DE
The \fIhtable\fP program generates two files of interest
in the local directory: \fIhosts\fP and \fInetworks\fP.  If
a file ``localhosts'' is present in the working directory its
contents are first copied to the output file.  Similarly, a
``localnetworks'' file may be prepended to the output created
by \fIhtable\fP.  It is usually wise to run \fIdiff\fP\|(1) on
the new host and network data bases before installing them in
/etc.
.NH 3
/etc/hosts.equiv
.PP
The remote login and shell servers use an
authentication scheme based on trusted hosts.  The hosts.equiv
file contains a list of hosts which are considered trusted
and/or, under a single administrative control.  When a user
contacts a remote login or shell server requesting service,
the client process passes the user's name and the official
name of the host on which the client is located.  In the simple
case, if the hosts's name is located in hosts.equiv and
the user has an account on the server's machine, then service
is rendered (i.e. the user is allowed to log in, or the command
is executed).  Users may constrain this ``equivalence'' of
machines by installing a .rhosts file in their login directory.
The root login is handled specially, bypassing the hosts.equiv
file, and using only the /.rhosts file.
.PP
Thus, to create a class of equivalent machines, the hosts.equiv
file should contain the \fIofficial\fP names for those machines.
For example, most machines on our major local
network are considered trusted, so the hosts.equiv file is
of the form:
.DS
ucbarpa
ucbcalder
ucbdali
ucbernie
ucbkim
ucbmatisse
ucbmonet
ucbvax
ucbmiro
ucbdegas
.DE
.NH 3
/etc/rc.local
.PP
Most network servers are automatically started up at boot time
by the command file /etc/rc (if they are installed in their
presumed locations).  These include the following:
.DS
.TS
l l.
/etc/rshd	shell server
/etc/rexecd	exec server
/etc/rlogind	login server
/etc/rwhod	system status daemon
.TE
.DE
To have other network servers started up as well, commands
of the following sort should be placed in the site dependent
file /etc/rc.local.
.DS
if [ -f /etc/telnetd ]; then
	/etc/telnetd & echo -n ' telnetd'			>/dev/console
f\&i
.DE
The following servers are included with the system and should
be installed in /etc/rc.local as the need arises.
.DS
.TS
l l.
/etc/telnetd	TELNET server
/etc/ftpd	FTP server
/etc/tftpd	TFTP server
/etc/syslog	error logging server
/etc/sendmail	SMTP server
/etc/courierd	Courier remote procedure call server
/etc/routed	routing table management daemon
.TE
.DE
Consult the manual pages and accompanying documentation (particularly
for sendmail) for details about their operation.
.NH 3
/etc/ftpusers
.PP
The FTP server included in the system provides support for an
anonymous FTP account.  Due to the inherent security problems
with such a facility you should read this section carefully if
you consider providing such a service.
.PP
An anonymous account is enabled by creating a user \fIftp\fP.
When a client uses the anonymous account a \fIchroot\fP\|(2)
system call is performed by the server to restrict the client
from moving outside that part of the file system where the
user ftp home directory is located.  Because a chroot call
is used, certain programs and files must be supplied the server
process for it to execute properly.  Further, one must be
sure that all directories and executable images are unwritable.
The following directory setup is recommended.
.DS
\fB#\fP cd ~ftp
\fB#\fP chmod 555 .; chown ftp .; chgrp ftp .
\fB#\fP mkdir bin etc pub
\fB#\fP chown root bin etc
\fB#\fP chmod 555 bin etc
\fB#\fP chown ftp pub
\fB#\fP chmod 777 pub
\fB#\fP cd bin
\fB#\fP cp /bin/sh /bin/ls .
\fB#\fP chmod 111 sh ls
\fB#\fP cd ../etc
\fB#\fP cp /etc/passwd /etc/group .
\fB#\fP chmod 444 passwd group
.DE
When local users wish to place files in the anonymous
area, they must be placed in a subdirectory.  In the
setup here, the directory ~ftp/pub is used.
.PP
Aside from the problems of directory modes and such,
the ftp server may provide a loophole for interlopers
if certain user accounts are allowed.  The
file /etc/ftpusers is checked on each connection.  If
the requested user name is located in the file, the
request for service is denied.  This file normally has
the following names on our systems.
.DS
uucp
root
.DE
Accounts with nonstandard shells and no passwords (e.g., who or finger)
should also be listed in this file to prevent their use as anonymous
accounts with ftp.
.NH 2
Routing and gateways/bridges
.PP
If your environment allows access to networks not directly
attached to your host you will need to set up routing information
to allow packets to be properly routed.  Two schemes are
supported by the system.  The first scheme
employs the routing table management daemon /etc/routed
to maintain the system routing tables.  The routing daemon
uses a variant of the Xerox Routing Information Protocol
to maintain up to date routing tables in a cluster of local
area networks.  By using the /etc/gateways
file created by /etc/htable,
the routing daemon can also be used to initialize static routes
to distant networks.  When the routing daemon is started up
(usually from /etc/rc.local) it reads /etc/gateways and installs
those routes defined there, then broadcasts on each local network
to which the host is attached to find other instances of the routing
daemon.  If any responses are received, the routing daemons
cooperate in maintaining a globally consistent view of routing
in the local environment.  This view can be extended to include
remote sites also running the routing daemon by setting up suitable
entries in /etc/gateways; consult
.IR routed (8C)
for a more thorough discussion.
.PP
The second approach is to define a wildcard route to a smart
gateway and depend on the gateway to provide ICMP routing
redirect information to dynamically create a routing data
base.  This is done by adding an entry of the form
.DS
/etc/route add 0 \fIsmart-gateway\fP 1
.DE
to /etc/rc.local; see
.IR route (8C)
for more information.  The wildcard route, indicated by a 0 valued
destination, will be used by the system as a ``last resort''
in routing packets to their destination.  Assuming the gateway
to which packets are directed is able to generate the proper
routing redirect messages, the system will then add routing
table entries based on the information supplied.  This approach
has certain advantages over the routing daemon, but is
unsuitable in an environment where their are only bridges (i.e.
pseudo gateways which, for instance, do not generate routing
redirect messages).  Further, if the
smart gateway goes down there is no alternative, save manual
alteration of the routing table entry, to maintaining service.
.PP
The system always listens, and processes, routing table redirect
information, so it is possible to combine both the above
facilities.  For example, the routing table management process
might be used to maintain up to date information about routes
to geographically local networks, while employing the wildcard
routing techniques for ``distant'' networks.  The
.IR netstat (1)
program may be used to display routing table contents as well
as various routing oriented statistics.  For example,
.DS
\fB#\fP\|netstat \-r
.DE
will display the contents of the routing tables, while
.DS
\fB#\fP\|netstat \-r \-s
.DE
will show the number of routing table entries dynamically
created as a result of routing redirect messages, etc.
