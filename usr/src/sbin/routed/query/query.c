#ifndef lint
static char sccsid[] = "@(#)query.c	4.7 %G%";
#endif

#include <sys/param.h>
#include <sys/protosw.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <errno.h>
#include <stdio.h>
#include <netdb.h>
#include "../protocol.h"

#define	WTIME	5		/* Time to wait for responses */

int	s;
int	timedout, timeout();
char	packet[MAXPACKETSIZE];
extern int errno;

main(argc, argv)
	int argc;
	char *argv[];
{
	int cc, count, bits;
	struct sockaddr from;
	int fromlen = sizeof(from);
	struct timeval notime;
	
	if (argc < 2) {
		printf("usage: query hosts...\n");
		exit(1);
	}
	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("socket");
		exit(2);
	}

	argv++, argc--;
	count = argc;
	while (argc > 0) {
		query(*argv);
		argv++, argc--;
	}

	/*
	 * Listen for returning packets;
	 * may be more than one packet per host.
	 */
	bits = 1 << s;
	bzero(&notime, sizeof(notime));
	signal(SIGALRM, timeout);
	alarm(WTIME);
	while ((count > 0 && !timedout) ||
	    select(20, &bits, 0, 0, &notime) > 0) {
		cc = recvfrom(s, packet, sizeof (packet), 0,
		  &from, &fromlen);
		if (cc <= 0) {
			if (cc < 0) {
				if (errno == EINTR)
					continue;
				perror("recvfrom");
				(void) close(s);
				exit(1);
			}
			continue;
		}
		rip_input(&from, cc);
		count--;
	}
}

query(host)
	char *host;
{
	struct sockaddr_in router;
	register struct rip *msg = (struct rip *)packet;
	struct hostent *hp;
	struct servent *sp;

	bzero((char *)&router, sizeof (router));
	hp = gethostbyname(host);
	if (hp == 0) {
		printf("%s: unknown\n", host);
		exit(1);
	}
	bcopy(hp->h_addr, &router.sin_addr, hp->h_length);
	router.sin_family = AF_INET;
	sp = getservbyname("router", "udp");
	if (sp == 0) {
		printf("udp/router: service unknown\n");
		exit(1);
	}
	router.sin_port = sp->s_port;
	msg->rip_cmd = RIPCMD_REQUEST;
	msg->rip_vers = RIPVERSION;
	msg->rip_nets[0].rip_dst.sa_family = htons(AF_UNSPEC);
	msg->rip_nets[0].rip_metric = htonl(HOPCNT_INFINITY);
	if (sendto(s, packet, sizeof (struct rip), 0,
	  &router, sizeof(router)) < 0)
		perror(host);
}

/*
 * Handle an incoming routing packet.
 */
rip_input(from, size)
	struct sockaddr_in *from;
	int size;
{
	register struct rip *msg = (struct rip *)packet;
	struct netinfo *n;
	char *name;
	int lna, net, subnet;
	struct hostent *hp;
	struct netent *np;

	if (msg->rip_cmd != RIPCMD_RESPONSE)
		return;
	hp = gethostbyaddr(&from->sin_addr, sizeof (struct in_addr), AF_INET);
	name = hp == 0 ? "???" : hp->h_name;
	printf("from %s(%s):\n", name, inet_ntoa(from->sin_addr));
	size -= sizeof (int);
	n = msg->rip_nets;
	while (size > 0) {
		register struct sockaddr_in *sin;

		if (size < sizeof (struct netinfo))
			break;
		if (msg->rip_vers > 0) {
			n->rip_dst.sa_family =
				ntohs(n->rip_dst.sa_family);
			n->rip_metric = ntohl(n->rip_metric);
		}
		sin = (struct sockaddr_in *)&n->rip_dst;
		net = inet_netof(sin->sin_addr);
		subnet = inet_subnetof(sin->sin_addr);
		lna = inet_lnaof(sin->sin_addr);
		name = "???";
		if (lna == INADDR_ANY) {
			np = getnetbyaddr(net, AF_INET);
			if (np)
				name = np->n_name;
			else if (net == 0)
				name = "default";
		} else if ((subnet != net) && ((lna & 0xff) == 0) &&
		    (np = getnetbyaddr(subnet, AF_INET))) {
			struct in_addr subnaddr, inet_makeaddr();

			subnaddr = inet_makeaddr(subnet, INADDR_ANY);
			if (bcmp(&sin->sin_addr, &subnaddr, sizeof(subnaddr)) == 0)
				name = np->n_name;
			else
				goto host;
		} else {
host:
			hp = gethostbyaddr(&sin->sin_addr,
			    sizeof (struct in_addr), AF_INET);
			if (hp)
				name = hp->h_name;
		}
		printf("\t%s(%s), metric %d\n", name,
			inet_ntoa(sin->sin_addr), n->rip_metric);
		size -= sizeof (struct netinfo), n++;
	}
}

timeout()
{
	timedout = 1;
}

/*
 * Return the possible subnetwork number from an internet address.
 * If the address is of the form of a subnet address (most significant
 * bit of the host part is set), believe the subnet exists.
 * Otherwise, return the network number.
 * SHOULD FIND OUT WHETHER THIS IS A LOCAL NETWORK BEFORE LOOKING
 * INSIDE OF THE HOST PART.  We can only believe this if we have other
 * information (e.g., we can find a name for this number).
 */
inet_subnetof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);

	if (IN_CLASSA(i)) {
		if (IN_SUBNETA(i))
			return ((i & IN_CLASSA_SUBNET) >> IN_CLASSA_SUBNSHIFT);
		else
			return ((i & IN_CLASSA_NET) >> IN_CLASSA_NSHIFT);
	} else if (IN_CLASSB(i)) {
		if (IN_SUBNETB(i))
			return ((i & IN_CLASSB_SUBNET) >> IN_CLASSB_SUBNSHIFT);
		else
			return ((i & IN_CLASSB_NET) >> IN_CLASSB_NSHIFT);
	} else
		return ((i & IN_CLASSC_NET) >> IN_CLASSC_NSHIFT);
}
