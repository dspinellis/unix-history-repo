.ds RH "Advanced Topics
.bp
.nr H1 5
.nr H2 0
.bp
.LG
.B
.ce
5. ADVANCED TOPICS
.sp 2
.R
.NL
.PP
A number of facilities have yet to be discussed.  For most users
of the ipc the mechanisms already
described will suffice in constructing distributed
applications.  However, others will find need to utilize some
of the features which we consider in this section.
.NH 2
Out of band data
.PP
The stream socket abstraction includes the notion of \*(lqout
of band\*(rq data.  Out of band data is a logically independent 
transmission channel associated with each pair of connected
stream sockets.  Out of band data is delivered to the user
independently of normal data along with the SIGURG signal.
In addition to the information passed, a logical mark is placed in
the data stream to indicate the point at which the out
of band data was sent.  The remote login and remote shell
applications use this facility to propagate signals from between
client and server processes.  When a signal is expected to
flush any pending output from the remote process(es), all
data up to the mark in the data stream is discarded.
.PP
The
stream abstraction defines that the out of band data facilities
must support the reliable delivery of at least one
out of band message at a time.  This message may contain at least one
byte of data, and at least one message may be pending delivery
to the user at any one time.  For communications protocols which
support only in-band signaling (i.e. the urgent data is
delivered in sequence with the normal data) the system extracts
the data from the normal data stream and stores it separately.
This allows users to choose between receiving the urgent data
in order and receiving it out of sequence without having to
buffer all the intervening data.
.PP
To send an out of band message the SOF_OOB flag is supplied to
a \fIsend\fP or \fIsendto\fP calls,
while to receive out of band data SOF_OOB should be indicated
when performing a \fIrecvfrom\fP or \fIrecv\fP call.
To find out if the read pointer is currently pointing at
the mark in the data stream, the SIOCATMARK ioctl is provided:
.DS
ioctl(s, SIOCATMARK, &yes);
.DE
If \fIyes\fP is a 1 on return, the next read will return data
after the mark.  Otherwise (assuming out of band data has arrived), 
the next read will provide data sent by the client prior
to transmission of the out of band signal.  The routine used
in the remote login process to flush output on receipt of an
interrupt or quit signal is shown in Figure 5.
.KF
.DS
oob()
{
	int out = 1+1;
	char waste[BUFSIZ], mark;

	signal(SIGURG, oob);
	/* flush local terminal input and output */
	ioctl(1, TIOCFLUSH, (char *)&out);
	for (;;) {
		if (ioctl(rem, SIOCATMARK, &mark) < 0) {
			perror("ioctl");
			break;
		}
		if (mark)
			break;
		(void) read(rem, waste, sizeof (waste));
	}
	recv(rem, &mark, 1, SOF_OOB);
	...
}
.DE
.ce
Figure 5.  Flushing terminal i/o on receipt of out of band data.
.sp
.KE
.NH 2
Signals and process groups
.PP
Due to the existence of the SIGURG and SIGIO signals each socket has an
associated process group (just as is done for terminals).
This process group is initialized to the process group of its
creator, but may be redefined at a later time with the SIOCSPGRP
ioctl:
.DS
ioctl(s, SIOCSPGRP, &pgrp);
.DE
A similar ioctl, SIOCGPGRP, is available for determining the
current process group of a socket.
.NH 2
Pseudo terminals
.PP
Many programs will not function properly without a terminal
for standard input and output.  Since a socket is not a terminal,
it is often necessary to have a process communicating over
the network do so through a \fIpseudo terminal\fP.  A pseudo
terminal is actually a pair of devices, master and slave,
which allow a process to serve as an active agent in communication
between processes and users.  Data written on the slave side
of a pseudo terminal is supplied as input to a process reading
from the master side.  Data written on the master side is
given the slave as input.  In this way, the process manipulating
the master side of the pseudo terminal has control over the
information read and written on the slave side.  The remote
login server uses pseudo terminals for remote login sessions.
A user logging in to a machine across the network is provided
a shell with a slave pseudo terminal as standard input, output,
and error.  The server process then handles the communication
between the programs invoked by the remote shell and the user's
local client process.  When a user sends an interrupt or quit
signal to a process executing on a remote machine, the client
login program traps the signal, sends an out of band message
to the server process who then uses the signal number, sent
as the data value in the out of band message, to perform a
\fIkillpg\fP(2) on the appropriate process group.  
.NH 2
Internet address binding
.PP
Binding addresses to sockets in the Internet domain can be
fairly complex.  Communicating processes are bound
by an \fIassociation\fP.  An association 
is composed of local and foreign
addresses, and local and foreign ports.  Port numbers are
allocated out of separate spaces, one for each Internet
protocol.  Associations are always unique.  That is, there
may never be duplicate <protocol, local address, local port, foreign
address, foreign port> tuples. 
.PP
The bind system call allows a process to specify half of
an association, <local address, local port>, while the connect
and accept primitives are used to complete a socket's association.
Since the association is created in two steps the association
uniqueness requirement indicated above could be violated unless
care is taken.  Further, it is unrealistic to expect user
programs to always know proper values to use for the local address
and local port since a host may reside on multiple networks and
the set of allocated port numbers is not directly accessible
to a user.
.PP
To simplify local address binding the notion of a
\*(lqwildcard\*(rq address has been provided.  When an address
is specified as INADDR_ANY (a manifest constant defined in
<netinet/in.h>), the system interprets the address as 
\*(lqany valid address\*(rq.  For example, to bind a specific
port number to a socket, but leave the local address unspecified,
the following code might be used:
.DS
#include <sys/types.h>
#include <netinet/in.h>
 ...
struct sockaddr_in sin;
 ...
s = socket(AF_INET, SOCK_STREAM, 0);
sin.sin_family = AF_INET;
sin.sin_addr.s_addr = INADDR_ANY;
sin.sin_port = MYPORT;
bind(s, (char *)&sin, sizeof (sin));
.DE
Sockets with wildcarded local addresses may receive messages
directed to the specified port number, and addressed to any
of the possible addresses assigned a host.  For example,
if a host is on a networks 46 and 10 and a socket is bound as
above, then an accept call is performed, the process will be
able to accept connection requests which arrive either from
network 46 or network 10.
.PP
In a similar fashion, a local port may be left unspecified
(specified as zero), in which case the system will select an
appropriate port number for it.  For example:
.DS
sin.sin_addr.s_addr = MYADDRESS;
sin.sin_port = 0;
bind(s, (char *)&sin, sizeof (sin));
.DE
The system selects the port number based on two criteria.
The first is that ports numbered 0 through 1023 are reserved
for privileged users (i.e. the super user).  The second is
that the port number is not currently bound to some other
socket.  In order to find a free port number in the privileged
range the following code is used by the remote shell server:
.DS
struct sockaddr_in sin;
 ...
lport = IPPORT_RESERVED \- 1;
sin.sin_addr.s_addr = INADDR_ANY;
 ...
for (;;) {
	sin.sin_port = htons((u_short)lport);
	if (bind(s, (caddr_t)&sin, sizeof (sin)) >= 0)
		break;
	if (errno != EADDRINUSE && errno != EADDRNOTAVAIL) {
		perror("socket");
		break;
	}
	lport--;
	if (lport == IPPORT_RESERVED/2) {
		fprintf(stderr, "socket: All ports in use\en");
		break;
	}
}
.DE
The restriction on allocating ports was done to allow processes
executing in a \*(lqsecure\*(rq environment to perform authentication
based on the originating address and port number.
.PP
In certain cases the algorithm used by the system in selecting
port numbers is unsuitable for an application.  This is due to
associations being created in a two step process.  For example,
the Internet file transfer protocol, FTP, specifies that data
connections must always originate from the same local port.  However,
duplicate associations are avoided by connecting to different foreign
ports.  In this situation the system would disallow binding the
same local address and port number to a socket if a previous data
connection's socket were around.  To override the default port
selection algorithm then an option call must be performed prior
to address binding:
.DS
setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)0, 0);
bind(s, (char *)&sin, sizeof (sin));
.DE
With the above call, local addresses may be bound which
are already in use.  This does not violate the uniqueness
requirement as the system still checks at connect time to
be sure any other sockets with the same local address and
port do not have the same foreign address and port (if an
association already exists, the error EADDRINUSE is returned).
.PP
Local address binding by the system is currently
done somewhat haphazardly when a host is on multiple
networks.  Logically, one would expect
the system to bind the local address associated with
the network through which a peer was communicating.
For instance, if the local host is connected to networks
46 and 10 and the foreign host is on network 32, and
traffic from network 32 were arriving via network
10, the local address to be bound would be the host's address
on network 10, not network 46.  This unfortunately, is
not always the case.  For reasons too complicated to discuss
here, the local address bound may be appear to be chosen
at random.  This property of local address binding
will normally be invisible to users unless the foreign
host does not understand how to reach the address
selected*.
.FS
* For example, if network 46 were unknown to the host on
network 32, and the local address were bound to that located
on network 46, then even though a route between the two hosts
existed through network 10, a connection would fail.
.FE
.NH 2
Broadcasting and datagram sockets
.PP
By using a datagram socket it is possible to send broadcast
packets on many networks supported by the system (the network
itself must support the notion of broadcasting; the system
provides no broadcast simulation in software).  Broadcast
messages can place a high load on a network since they force
every host on the network to service them.  Consequently,
the ability to send broadcast packets has been limited to
the super user.
.PP
To send a broadcast message, an Internet datagram socket 
should be created:
.DS
s = socket(AF_INET, SOCK_DGRAM, 0);
.DE
and at least a port number should be bound to the socket:
.DS
sin.sin_family = AF_INET;
sin.sin_addr.s_addr = INADDR_ANY;
sin.sin_port = MYPORT;
bind(s, (char *)&sin, sizeof (sin));
.DE
Then the message should be addressed as:
.DS
dst.sin_family = AF_INET;
dst.sin_addr.s_addr = INADDR_ANY;
dst.sin_port = DESTPORT;
.DE
and, finally, a sendto call may be used:
.DS
sendto(s, buf, buflen, 0, &dst, sizeof (dst));
.DE
.PP
Received broadcast messages contain the senders address
and port (datagram sockets are anchored before
a message is allowed to go out).  
.NH 2
Signals
.PP
Two new signals have been added to the system which may
be used in conjunction with the interprocess communication
facilities.  The SIGURG signal is associated with the existence
of an \*(lqurgent condition\*(rq.  The SIGIO signal is used
with \*(lqinterrupt driven i/o\*(rq (not presently implemented).
SIGURG is currently supplied a process when out of band data
is present at a socket.  If multiple sockets have out of band
data awaiting delivery, a select call may be used to determine
those sockets with such data.
.PP
An old signal which is useful when constructing server processes
is SIGCHLD.  This signal is delivered to a process when any
children processes have changed state.  Normally servers use
the signal to \*(lqreap\*(rq child processes after exiting.
For example, the remote login server loop shown in Figure 2
may be augmented as follows:
.DS
int reaper();
 ...
sigset(SIGCHLD, reaper);
listen(f, 10);
for (;;) {
	int g, len = sizeof (from);

	g = accept(f, &from, &len, 0);
	if (g < 0) {
		if (errno != EINTR)
			perror("rlogind: accept");
		continue;
	}
	...
}
 ...
#include <wait.h>
reaper()
{
	union wait status;

	while (wait3(&status, WNOHANG, 0) > 0)
		;
}
.DE
.PP
If the parent server process fails to reap its children,
a large number of \*(lqzombie\*(rq processes may be created.
