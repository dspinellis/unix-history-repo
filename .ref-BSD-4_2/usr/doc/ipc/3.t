.ds RH "Network Library Routines
.bp
.nr H1 3
.nr H2 0
.bp
.LG
.B
.ce
3. NETWORK LIBRARY ROUTINES
.sp 2
.R
.NL
.PP
The discussion in section 2 indicated the possible need to
locate and construct network addresses when using the
interprocess communication facilities in a distributed
environment.  To aid in this task a number of routines
have been added to the standard C run-time library.
In this section we will consider the new routines provided
to manipulate network addresses.  While the 4.2BSD networking
facilities support only the DARPA standard Internet protocols,
these routines have been designed with flexibility in mind.
As more communication protocols become available, we hope
the same user interface will be maintained in accessing
network-related address data bases.  The only difference
should be the values returned to the user.  Since these
values are normally supplied the system, users should
not need to be directly aware of the communication protocol
and/or naming conventions in use.
.PP
Locating a service on a remote host requires many levels of
mapping before client and server may
communicate.  A service is assigned a name which is intended
for human consumption; e.g. \*(lqthe \fIlogin server\fP on host
monet\*(rq.
This name, and the name of the peer host, must then be translated
into network \fIaddresses\fP which are not necessarily suitable
for human consumption.  Finally, the address must then used in locating
a physical \fIlocation\fP and \fIroute\fP to the service.  The
specifics of these three mappings is likely to vary between
network architectures.  For instance, it is desirable for a network
to not require hosts
be named in such a way that their physical location is known by
the client host.  Instead, underlying services in the network
may discover the actual location of the host at the time a client
host wishes to communicate.  This ability to have hosts named in
a location independent manner may induce overhead in connection
establishment, as a discovery process must take place,
but allows a host to be physically mobile without requiring it to
notify its clientele of its current location.
.PP
Standard routines are provided for: mapping host names 
to network addresses, network names to network numbers, 
protocol names to protocol numbers, and service names
to port numbers and the appropriate protocol to
use in communicating with the server process.  The
file <\fInetdb.h\fP> must be included when using any of these
routines.
.NH 2
Host names
.PP
A host name to address mapping is represented by
the \fIhostent\fP structure:
.DS
.DT
struct	hostent {
	char	*h_name;	/* official name of host */
	char	**h_aliases;	/* alias list */
	int	h_addrtype;	/* host address type */
	int	h_length;	/* length of address */
	char	*h_addr;	/* address */
};
.DE
The official name of the host and its public aliases are
returned, along with a variable length address and address
type.  The routine \fIgethostbyname\fP(3N) takes a host name
and returns a \fIhostent\fP structure,
while the routine \fIgethostbyaddr\fP(3N)
maps host addresses into a \fIhostent\fP structure.  It is possible
for a host to have many addresses, all having the same name.
\fIGethostybyname\fP returns the first matching entry in the data
base file \fI/etc/hosts\fP; if this is unsuitable, the lower level
routine \fIgethostent\fP(3N) may be used.  For example, to
obtain a \fIhostent\fP structure for a
host on a particular network the following routine might be
used (for simplicity, only Internet addresses are considered):
.DS
.if t .ta .5i 1.0i 1.5i 2.0i
.\" 3.5i went to 3.8i
.if n .ta .7i 1.4i 2.1i 2.8i 3.5i 4.2i
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
 ...
struct hostent *
gethostbynameandnet(name, net)
	char *name;
	int net;
{
	register struct hostent *hp;
	register char **cp;

	sethostent(0);
	while ((hp = gethostent()) != NULL) {
		if (hp->h_addrtype != AF_INET)
			continue;
		if (strcmp(name, hp->h_name)) {
			for (cp = hp->h_aliases; cp && *cp != NULL; cp++)
				if (strcmp(name, *cp) == 0)
					goto found;
			continue;
		}
	found:
		if (in_netof(*(struct in_addr *)hp->h_addr)) == net)
			break;
	}
	endhostent(0);
	return (hp);
}
.DE
(\fIin_netof\fP(3N) is a standard routine which returns
the network portion of an Internet address.)
.NH 2
Network names
.PP
As for host names, routines for mapping network names to numbers,
and back, are provided.  These routines return a \fInetent\fP
structure:
.DS
.DT
/*
 * Assumption here is that a network number
 * fits in 32 bits -- probably a poor one.
 */
struct	netent {
	char	*n_name;	/* official name of net */
	char	**n_aliases;	/* alias list */
	int	n_addrtype;	/* net address type */
	int	n_net;	/* network # */
};
.DE
The routines \fIgetnetbyname\fP(3N), \fIgetnetbynumber\fP(3N),
and \fIgetnetent\fP(3N) are the network counterparts to the
host routines described above.
.NH 2
Protocol names
.PP
For protocols the \fIprotoent\fP structure defines the
protocol-name mapping
used with the routines \fIgetprotobyname\fP(3N),
\fIgetprotobynumber\fP(3N),
and \fIgetprotoent\fP(3N):
.DS
.DT
struct	protoent {
	char	*p_name;	/* official protocol name */
	char	**p_aliases;	/* alias list */
	int	p_proto;	/* protocol # */
};
.DE
.NH 2
Service names
.PP
Information regarding services is a bit more complicated.  A service
is expected to reside at a specific \*(lqport\*(rq and employ
a particular communication protocol.  This view is consistent with
the Internet domain, but inconsistent with other network architectures.
Further, a service may reside on multiple ports or support multiple
protocols.  If either of these occurs, the higher level library routines
will have to be bypassed in favor of homegrown routines similar in
spirit to the \*(lqgethostbynameandnet\*(rq routine described above.
A service mapping is described by the \fIservent\fP structure,
.DS
.DT
struct	servent {
	char	*s_name;	/* official service name */
	char	**s_aliases;	/* alias list */
	int	s_port;	/* port # */
	char	*s_proto;	/* protocol to use */
};
.DE
The routine \fIgetservbyname\fP(3N) maps service
names to a servent structure by specifying a service name and,
optionally, a qualifying protocol.  Thus the call
.DS
sp = getservbyname("telnet", (char *)0);
.DE
returns the service specification for a telnet server using
any protocol, while the call
.DS
sp = getservbyname("telnet", "tcp");
.DE
returns only that telnet server which uses the TCP protocol.
The routines \fIgetservbyport\fP(3N) and \fIgetservent\fP(3N) are
also provided.  The \fIgetservbyport\fP routine has an interface similar
to that provided by \fIgetservbyname\fP; an optional protocol name may
be specified to qualify lookups.
.NH 2
Miscellaneous
.PP
With the support routines described above, an application program
should rarely have to deal directly
with addresses.  This allows
services to be developed as much as possible in a network independent
fashion.  It is clear, however, that purging all network dependencies
is very difficult.  So long as the user is required to supply network
addresses when naming services and sockets there will always some
network dependency in a program.  For example, the normal
code included in client programs, such as the remote login program,
is of the form shown in Figure 1.
.KF
.DS
.if t .ta .5i 1.0i 1.5i 2.0i
.if n .ta .7i 1.4i 2.1i 2.8i
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <netdb.h>
 ...
main(argc, argv)
	char *argv[];
{
	struct sockaddr_in sin;
	struct servent *sp;
	struct hostent *hp;
	int s;
	...
	sp = getservbyname("login", "tcp");
	if (sp == NULL) {
		fprintf(stderr, "rlogin: tcp/login: unknown service\en");
		exit(1);
	}
	hp = gethostbyname(argv[1]);
	if (hp == NULL) {
		fprintf(stderr, "rlogin: %s: unknown host\en", argv[1]);
		exit(2);
	}
	bzero((char *)&sin, sizeof (sin));
	bcopy(hp->h_addr, (char *)&sin.sin_addr, hp->h_length);
	sin.sin_family = hp->h_addrtype;
	sin.sin_port = sp->s_port;
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0) {
		perror("rlogin: socket");
		exit(3);
	}
	...
	if (connect(s, (char *)&sin, sizeof (sin)) < 0) {
		perror("rlogin: connect");
		exit(5);
	}
	...
}
.DE
.ce
Figure 1.  Remote login client code.
.KE
(This example will be considered in more detail in section 4.)
.PP
If we wanted to make the remote login program independent of the 
Internet protocols and addressing scheme we would be forced to add
a layer of routines which masked the network dependent aspects from
the mainstream login code.  For the current facilities available in
the system this does not appear to be worthwhile.  Perhaps when the
system is adapted to different network architectures the utilities
will be reorganized more cleanly.
.PP
Aside from the address-related data base routines, there are several
other routines available in the run-time library which are of interest
to users.  These are intended mostly to simplify manipulation of 
names and addresses.  Table 1 summarizes the routines
for manipulating variable length byte strings and handling byte
swapping of network addresses and values.
.KF
.DS B
.TS
box;
l | l
l | l.
Call	Synopsis
_
bcmp(s1, s2, n)	compare byte-strings; 0 if same, not 0 otherwise
bcopy(s1, s2, n)	copy n bytes from s1 to s2
bzero(base, n)	zero-fill n bytes starting at base
htonl(val)	convert 32-bit quantity from host to network byte order
htons(val)	convert 16-bit quantity from host to network byte order
ntohl(val)	convert 32-bit quantity from network to host byte order
ntohs(val)	convert 16-bit quantity from network to host byte order
.TE
.DE
.ce
Table 1.  C run-time routines.
.KE
.PP
The byte swapping routines are provided because the operating
system expects addresses to be supplied in network order.  On a
VAX, or machine with similar architecture, this
is usually reversed.  Consequently,
programs are sometimes required to byte swap quantities.  The
library routines which return network addresses provide them
in network order so that they may simply be copied into the structures
provided to the system.  This implies users should encounter the
byte swapping problem only when \fIinterpreting\fP network addresses.
For example, if an Internet port is to be printed out the following
code would be required:
.DS
printf("port number %d\en", ntohs(sp->s_port));
.DE
On machines other than the VAX these routines are defined as null
macros.
