/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)query.c	5.10 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/protosw.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <errno.h>
#include <stdio.h>
#include <netdb.h>
#include <protocols/routed.h>

#define	WTIME	5		/* Time to wait for all responses */
#define	STIME	500000		/* usec to wait for another response */

int	s;
int	timedout, timeout();
char	packet[MAXPACKETSIZE];
extern int errno;
int	nflag;

main(argc, argv)
	int argc;
	char *argv[];
{
	int cc, count, bits;
	struct sockaddr from;
	int fromlen = sizeof(from), size = 32*1024;
	struct timeval shorttime;
	
	if (argc < 2) {
usage:
		printf("usage: query [ -n ] hosts...\n");
		exit(1);
	}
	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("socket");
		exit(2);
	}
	if (setsockopt(s, SOL_SOCKET, SO_RCVBUF, &size, sizeof(size)) < 0)
		perror("setsockopt SO_RCVBUF");

	argv++, argc--;
	if (*argv[0] == '-') {
		switch (argv[0][1]) {
		case 'n':
			nflag++;
			break;
		default:
			goto usage;
		}
		argc--, argv++;
	}
	while (argc > 0) {
		query(*argv);
		count++;
		argv++, argc--;
	}

	/*
	 * Listen for returning packets;
	 * may be more than one packet per host.
	 */
	bits = 1 << s;
	bzero(&shorttime, sizeof(shorttime));
	shorttime.tv_usec = STIME;
	signal(SIGALRM, timeout);
	alarm(WTIME);
	while ((count > 0 && !timedout) ||
	    select(20, &bits, 0, 0, &shorttime) > 0) {
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
	exit (count > 0 ? count : 0);
}

query(host)
	char *host;
{
	struct sockaddr_in router;
	register struct rip *msg = (struct rip *)packet;
	struct hostent *hp;
	struct servent *sp;

	bzero((char *)&router, sizeof (router));
	router.sin_family = AF_INET;
	router.sin_addr.s_addr = inet_addr(host);
	if (router.sin_addr.s_addr == -1) {
		hp = gethostbyname(host);
		if (hp == 0) {
			printf("%s: unknown\n", host);
			exit(1);
		}
		bcopy(hp->h_addr, &router.sin_addr, hp->h_length);
	}
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
	register struct netinfo *n;
	char *name;
	int lna, net, subnet;
	struct hostent *hp;
	struct netent *np;

	if (msg->rip_cmd != RIPCMD_RESPONSE)
		return;
	printf("%d bytes from ", size);
	if (nflag)
		printf("%s:\n", inet_ntoa(from->sin_addr));
	else {
		hp = gethostbyaddr(&from->sin_addr, sizeof (struct in_addr),
			AF_INET);
		name = hp == 0 ? "???" : hp->h_name;
		printf("%s(%s):\n", name, inet_ntoa(from->sin_addr));
	}
	size -= sizeof (int);
	n = msg->rip_nets;
	while (size > 0) {
	    if (size < sizeof (struct netinfo))
		    break;
	    if (msg->rip_vers > 0) {
		    n->rip_dst.sa_family =
			    ntohs(n->rip_dst.sa_family);
		    n->rip_metric = ntohl(n->rip_metric);
	    }
	    switch (n->rip_dst.sa_family) {

	    case AF_INET:
		{ register struct sockaddr_in *sin;

		sin = (struct sockaddr_in *)&n->rip_dst;
		net = inet_netof(sin->sin_addr);
		subnet = inet_subnetof(sin->sin_addr);
		lna = inet_lnaof(sin->sin_addr);
		name = "???";
		if (!nflag) {
			if (sin->sin_addr.s_addr == 0)
				name = "default";
			else if (lna == INADDR_ANY) {
				np = getnetbyaddr(net, AF_INET);
				if (np)
					name = np->n_name;
				else if (net == 0)
					name = "default";
			} else if ((lna & 0xff) == 0 &&
			    (np = getnetbyaddr(subnet, AF_INET))) {
				struct in_addr subnaddr, inet_makeaddr();

				subnaddr = inet_makeaddr(subnet, INADDR_ANY);
				if (bcmp(&sin->sin_addr, &subnaddr,
				    sizeof(subnaddr)) == 0)
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
			printf("\t%-17s metric %2d name %s\n",
				inet_ntoa(sin->sin_addr), n->rip_metric, name);
		} else
			printf("\t%-17s metric %2d\n",
				inet_ntoa(sin->sin_addr), n->rip_metric);
		break;
		}

	    default:
		{ u_short *p = (u_short *)n->rip_dst.sa_data;

		printf("\t(af %d) %x %x %x %x %x %x %x, metric %d\n",
		    p[0], p[1], p[2], p[3], p[4], p[5], p[6],
		    n->rip_dst.sa_family,
		    n->rip_metric);
		break;
		}
			
	    }
	    size -= sizeof (struct netinfo), n++;
	}
}

timeout()
{
	timedout = 1;
}

/*
 * Return the possible subnetwork number from an internet address.
 * SHOULD FIND OUT WHETHER THIS IS A LOCAL NETWORK BEFORE LOOKING
 * INSIDE OF THE HOST PART.  We can only believe this if we have other
 * information (e.g., we can find a name for this number).
 */
inet_subnetof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);

	if (IN_CLASSA(i))
		return ((i & IN_CLASSB_NET) >> IN_CLASSB_NSHIFT);
	else if (IN_CLASSB(i))
		return ((i & IN_CLASSC_NET) >> IN_CLASSC_NSHIFT);
	else
		return ((i & 0xffffffc0) >> 28);
}
