.ds RH "Client/Server Model
.bp
.nr H1 4
.nr H2 0
.bp
.LG
.B
.ce
4. CLIENT/SERVER MODEL
.sp 2
.R
.NL
.PP
The most commonly used paradigm in constructing distributed applications
is the client/server model.  In this scheme client applications request
services from a server process.  This implies an asymmetry in establishing
communication between the client and server which has been examined
in section 2.  In this section we will look more closely at the interactions
between client and server, and consider some of the problems in developing
client and server applications.
.PP
Client and server require a well known set of conventions before
service may be rendered (and accepted).  This set of conventions
comprises a protocol which must be implemented at both ends of a
connection.  Depending on the situation, the protocol may be symmetric
or asymmetric.  In a symmetric protocol, either side may play the 
master or slave roles.  In an asymmetric protocol, one side is
immutably recognized as the master, with the other the slave.  
An example of a symmetric protocol is the TELNET protocol used in
the Internet for remote terminal emulation.  An example
of an asymmetric protocol is the Internet file transfer protocol,
FTP.  No matter whether the specific protocol used in obtaining
a service is symmetric or asymmetric, when accessing a service there
is a \*(lqclient process\*(rq and a \*(lqserver process\*(rq.  We
will first consider the properties of server processes, then
client processes.
.PP
A server process normally listens at a well know address for
service requests.  Alternative schemes which use a service server
may be used to eliminate a flock of server processes clogging the
system while remaining dormant most of the time.  The Xerox
Courier protocol uses the latter scheme.  When using Courier, a
Courier client process contacts a Courier server at the remote
host and identifies the service it requires.  The Courier server
process then creates the appropriate server process based on a
data base and \*(lqsplices\*(rq the client and server together,
voiding its part in the transaction.  This scheme is attractive
in that the Courier server process may provide a single contact
point for all services, as well as carrying out the initial steps
in authentication.  However, while this is an attractive possibility
for standardizing access to services, it does introduce a certain
amount of overhead due to the intermediate process involved.
Implementations which provide this type of service within the
system can minimize the cost of client server
rendezvous.  The \fIportal\fP notion described
in the \*(lq4.2BSD System Manual\*(rq embodies many of the ideas
found in Courier, with the rendezvous mechanism implemented internal
to the system.
.NH 2
Servers
.PP
In 4.2BSD most servers are accessed at well known Internet addresses
or UNIX domain names.  When a server is started at boot time it
advertises it services by listening at a well know location.  For
example, the remote login server's main loop is of the form shown
in Figure 2.
.KF
.if t .ta .5i 1.0i 1.5i 2.0i
.if n .ta .7i 1.4i 2.1i 2.8i
.DS
main(argc, argv)
	int argc;
	char **argv;
{
	int f;
	struct sockaddr_in from;
	struct servent *sp;

	sp = getservbyname("login", "tcp");
	if (sp == NULL) {
		fprintf(stderr, "rlogind: tcp/login: unknown service\en");
		exit(1);
	}
	...
#ifndef DEBUG
	<<disassociate server from controlling terminal>>
#endif
	...
	sin.sin_port = sp->s_port;
	...
	f = socket(AF_INET, SOCK_STREAM, 0);
	...
	if (bind(f, (caddr_t)&sin, sizeof (sin)) < 0) {
		...
	}
	...
	listen(f, 5);
	for (;;) {
		int g, len = sizeof (from);

		g = accept(f, &from, &len);
		if (g < 0) {
			if (errno != EINTR)
				perror("rlogind: accept");
			continue;
		}
		if (fork() == 0) {
			close(f);
			doit(g, &from);
		}
		close(g);
	}
}
.DE
.ce
Figure 2.  Remote login server.
.sp
.KE
.PP
The first step taken by the server is look up its service
definition:
.sp 1
.nf
.in +5
.if t .ta .5i 1.0i 1.5i 2.0i
.if n .ta .7i 1.4i 2.1i 2.8i
sp = getservbyname("login", "tcp");
if (sp == NULL) {
	fprintf(stderr, "rlogind: tcp/login: unknown service\en");
	exit(1);
}
.sp 1
.in -5
.fi
This definition is used in later portions of the code to
define the Internet port at which it listens for service
requests (indicated by a connection).
.PP
Step two is to disassociate the server from the controlling
terminal of its invoker.  This is important as the server will
likely not want to receive signals delivered to the process
group of the controlling terminal. 
.PP
Once a server has established a pristine environment, it
creates a socket and begins accepting service requests.
The \fIbind\fP call is required to insure the server listens
at its expected location.  The main body of the loop is
fairly simple:
.DS
.if t .ta .5i 1.0i 1.5i 2.0i
.if n .ta .7i 1.4i 2.1i 2.8i
for (;;) {
	int g, len = sizeof (from);

	g = accept(f, &from, &len);
	if (g < 0) {
		if (errno != EINTR)
			perror("rlogind: accept");
		continue;
	}
	if (fork() == 0) {
		close(f);
		doit(g, &from);
	}
	close(g);
}
.DE
An \fIaccept\fP call blocks the server until
a client requests service.  This call could return a
failure status if the call is interrupted by a signal
such as SIGCHLD (to be discussed in section 5).  Therefore,
the return value from \fIaccept\fP is checked to insure
a connection has actually been established.  With a connection
in hand, the server then forks a child process and invokes
the main body of the remote login protocol processing.  Note
how the socket used by the parent for queueing connection
requests is closed in the child, while the socket created as
a result of the accept is closed in the parent.  The
address of the client is also handed the \fIdoit\fP routine
because it requires it in authenticating clients.
.NH 2
Clients
.PP
The client side of the remote login service was shown
earlier in Figure 1.
One can see the separate, asymmetric roles of the client
and server clearly in the code.  The server is a passive entity,
listening for client connections, while the client process is
an active entity, initiating a connection when invoked.  
.PP
Let us consider more closely the steps taken
by the client remote login process.  As in the server process
the first step is to locate the service definition for a remote
login:
.DS
sp = getservbyname("login", "tcp");
if (sp == NULL) {
	fprintf(stderr, "rlogin: tcp/login: unknown service\en");
	exit(1);
}
.DE
Next the destination host is looked up with a
\fIgethostbyname\fP call:
.DS
hp = gethostbyname(argv[1]);
if (hp == NULL) {
	fprintf(stderr, "rlogin: %s: unknown host\en", argv[1]);
	exit(2);
}
.DE
With this accomplished, all that is required is to establish a
connection to the server at the requested host and start up the
remote login protocol.  The address buffer is cleared, then filled
in with the Internet address of the foreign host and the port
number at which the login process resides:
.DS
bzero((char *)&sin, sizeof (sin));
bcopy(hp->h_addr, (char *)sin.sin_addr, hp->h_length);
sin.sin_family = hp->h_addrtype;
sin.sin_port = sp->s_port;
.DE
A socket is created, and a connection initiated.
.DS
s = socket(hp->h_addrtype, SOCK_STREAM, 0);
if (s < 0) {
	perror("rlogin: socket");
	exit(3);
}
 ...
if (connect(s, (char *)&sin, sizeof (sin)) < 0) {
	perror("rlogin: connect");
	exit(4);
}
.DE
The details of the remote login protocol will not be considered here.
.NH 2
Connectionless servers
.PP
While connection-based services are the norm, some services
are based on the use of datagram sockets.  One, in particular,
is the \*(lqrwho\*(rq service which provides users with status
information for hosts connected to a local area
network.  This service, while predicated on the ability to
\fIbroadcast\fP information to all hosts connected to a particular
network, is of interest as an example usage of datagram sockets.
.PP
A user on any machine running the rwho server may find out
the current status of a machine with the \fIruptime\fP(1) program.
The output generated is illustrated in Figure 3.
.KF
.DS B
.TS
l r l l l l l.
arpa	up	9:45,	5 users, load	1.15,	1.39,	1.31
cad	up	2+12:04,	8 users, load	4.67,	5.13,	4.59
calder	up	10:10,	0 users, load	0.27,	0.15,	0.14
dali	up	2+06:28,	9 users, load	1.04,	1.20,	1.65
degas	up	25+09:48,	0 users, load	1.49,	1.43,	1.41
ear	up	5+00:05,	0 users, load	1.51,	1.54,	1.56
ernie	down	0:24
esvax	down	17:04
ingres	down	0:26
kim	up	3+09:16,	8 users, load	2.03,	2.46,	3.11
matisse	up	3+06:18,	0 users, load	0.03,	0.03,	0.05
medea	up	3+09:39,	2 users, load	0.35,	0.37,	0.50
merlin	down	19+15:37
miro	up	1+07:20,	7 users, load	4.59,	3.28,	2.12
monet	up	1+00:43,	2 users, load	0.22,	0.09,	0.07
oz	down	16:09
statvax	up	2+15:57,	3 users, load	1.52,	1.81,	1.86
ucbvax	up	9:34,	2 users, load	6.08,	5.16,	3.28
.TE
.DE
.ce
Figure 3. ruptime output.
.sp
.KE
.PP
Status information for each host is periodically broadcast
by rwho server processes on each machine.  The same server
process also receives the status information and uses it
to update a database.  This database is then interpreted
to generate the status information for each host.  Servers
operate autonomously, coupled only by the local network and
its broadcast capabilities.
.PP
The rwho server, in a simplified form, is pictured in Figure
4.  There are two separate tasks performed by the server.  The
first task is to act as a receiver of status information broadcast
by other hosts on the network.  This job is carried out in the
main loop of the program.  Packets received at the rwho port
are interrogated to insure they've been sent by another rwho
server process, then are time stamped with their arrival time
and used to update a file indicating the status of the host.
When a host has not been heard from for an extended period of
time, the database interpretation routines assume the host is
down and indicate such on the status reports.  This algorithm
is prone to error as a server may be down while a host is actually
up, but serves our current needs.
.KF
.DS
.if t .ta .5i 1.0i 1.5i 2.0i
.if n .ta .7i 1.4i 2.1i 2.8i
main()
{
	...
	sp = getservbyname("who", "udp");
	net = getnetbyname("localnet");
	sin.sin_addr = inet_makeaddr(INADDR_ANY, net);
	sin.sin_port = sp->s_port;
	...
	s = socket(AF_INET, SOCK_DGRAM, 0);
	...
	bind(s, &sin, sizeof (sin));
	...
	sigset(SIGALRM, onalrm);
	onalrm();
	for (;;) {
		struct whod wd;
		int cc, whod, len = sizeof (from);

		cc = recvfrom(s, (char *)&wd, sizeof (struct whod), 0, &from, &len);
		if (cc <= 0) {
			if (cc < 0 && errno != EINTR)
				perror("rwhod: recv");
			continue;
		}
		if (from.sin_port != sp->s_port) {
			fprintf(stderr, "rwhod: %d: bad from port\en",
				ntohs(from.sin_port));
			continue;
		}
		...
		if (!verify(wd.wd_hostname)) {
			fprintf(stderr, "rwhod: malformed host name from %x\en",
				ntohl(from.sin_addr.s_addr));
			continue;
		}
		(void) sprintf(path, "%s/whod.%s", RWHODIR, wd.wd_hostname);
		whod = open(path, FWRONLY|FCREATE|FTRUNCATE, 0666);
		...
		(void) time(&wd.wd_recvtime);
		(void) write(whod, (char *)&wd, cc);
		(void) close(whod);
	}
}
.DE
.ce
Figure 4.  rwho server.
.KE
.PP
The second task performed by the server is to supply information
regarding the status of its host.  This involves periodically
acquiring system status information, packaging it up in a message
and broadcasting it on the local network for other rwho servers
to hear.  The supply function is triggered by a timer and 
runs off a signal.  Locating the system status
information is somewhat involved, but uninteresting.  Deciding
where to transmit the resultant packet does, however, indicates
some problems with the current protocol.
.PP
Status information is broadcast on the local network.
For networks which do not support the notion of broadcast another
scheme must be used to simulate or
replace broadcasting.  One possibility is to enumerate the
known neighbors (based on the status received).  This, unfortunately,
requires some bootstrapping information, as a
server started up on a quiet network will have no
known neighbors and thus never receive, or send, any status information.
This is the identical problem faced by the routing table management
process in propagating routing status information.  The standard
solution, unsatisfactory as it may be, is to inform one or more servers
of known neighbors and request that they always communicate with
these neighbors.  If each server has at least one neighbor supplied
it, status information may then propagate through
a neighbor to hosts which
are not (possibly) directly neighbors.  If the server is able to
support networks which provide a broadcast capability, as well as
those which do not, then networks with an
arbitrary topology may share status information*.
.FS
* One must, however, be concerned about \*(lqloops\*(rq.
That is, if a host is connected to multiple networks, it
will receive status information from itself.  This can lead
to an endless, wasteful, exchange of information.
.FE
.PP
The second problem with the current scheme is that the rwho process
services only a single local network, and this network is found by
reading a file.  It is important that software operating in a distributed
environment not have any site-dependent information compiled into it.
This would require a separate copy of the server at each host and
make maintenance a severe headache.  4.2BSD attempts to isolate
host-specific information from applications by providing system
calls which return the necessary information\(dg.
.FS
\(dg An example of such a system call is the \fIgethostname\fP(2)
call which returns the host's \*(lqofficial\*(rq name.
.FE
Unfortunately, no straightforward mechanism currently
exists for finding the collection
of networks to which a host is directly connected.  Thus the
rwho server performs a lookup in a file
to find its local network.  A better, though still
unsatisfactory, scheme used by the routing process is to interrogate
the system data structures to locate those directly connected
networks.  A mechanism to acquire this information from the system
would be a useful addition.
