/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)query.c	5.1 (Berkeley) 6/7/85";
#endif not lint

#include <sys/param.h>
#include <sys/protosw.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netns/ns.h>
#include <netns/idp.h>
#include <errno.h>
#include <stdio.h>
#include <netdb.h>
#include "../protocol.h"
#define IDPPORT_RIF 1
#define xnnet(p)	(*(long *)&(p))

#define	WTIME	5		/* Time to wait for responses */

int	s;
int	timedout, timeout();
char	packet[MAXPACKETSIZE];
extern int errno;
struct sockaddr_ns	myaddr = {AF_NS};
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
	s = getsocket(SOCK_DGRAM, 0);
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
		struct nspacket {
			struct idp hdr;
			char	data[512];
		} response;
		cc = recvfrom(s, &response, sizeof (response), 0,
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
		rip_input(&from, response.data, cc);
		count--;
	}
}

query(host)
	char *host;
{
	struct sockaddr_ns router;
	register struct rip *msg = (struct rip *)packet;
	long mynet;
	short work[3];

	bzero((char *)&router, sizeof (router));
	router.sns_family = AF_NS;
	router.sns_addr.x_port = htons(IDPPORT_RIF);
	sscanf(host, "%ld:%hx,%hx,%hx",
			&mynet,work+0, work+1, work+2);
	router.sns_addr.x_host  = *(union ns_host *)work;
	xnnet(router.sns_addr.x_net) =  htonl(mynet);
	msg->rip_cmd = htons(RIPCMD_REQUEST);
	xnnet(msg->rip_nets[0]) = -1;
	msg->rip_nets[0].rip_metric = htons(HOPCNT_INFINITY);
	if (sendto(s, packet, sizeof (struct rip), 0,
	  &router, sizeof(router)) < 0)
		perror(host);
}

/*
 * Handle an incoming routing packet.
 */
rip_input(from, msg,  size)
	struct sockaddr_ns *from;
	register struct rip *msg;
	int size;
{
	struct netinfo *n;
	char *name;
	int lna, net, subnet;
	struct hostent *hp;
	struct netent *np;

	if (htons(msg->rip_cmd) != RIPCMD_RESPONSE)
		return;
	printf("from %d:%x,%x,%x\n", 
		ntohl(xnnet(from->sns_addr.x_net)),
		from->sns_addr.x_host.s_host[0],
		from->sns_addr.x_host.s_host[1],
		from->sns_addr.x_host.s_host[2]);
	size -= sizeof (struct idp);
	size -= sizeof (short);
	n = msg->rip_nets;
	while (size > 0) {
		if (size < sizeof (struct netinfo))
			break;
		printf("\t%d, metric %d\n", ntohl(xnnet(n->rip_dst[0])),
			ntohs(n->rip_metric));
		size -= sizeof (struct netinfo), n++;
	}
}

timeout()
{
	timedout = 1;
}
getsocket(type, proto)
	int type, proto; 
{
	struct sockaddr_ns *sns = &myaddr;
	int domain = sns->sns_family;
	int retry, s, on = 1;

	retry = 1;
	while ((s = socket(domain, type, proto)) < 0 && retry) {
		perror("socket");
		sleep(5 * retry);
		retry <<= 1;
	}
	if (retry == 0)
		return (-1);
	while (bind(s, sns, sizeof (*sns), 0) < 0 && retry) {
		perror("bind");
		sleep(5 * retry);
		retry <<= 1;
	}
	if (retry == 0)
		return (-1);
	if (domain==AF_NS) {
		struct idp idp;
		if (setsockopt(s, 0, SO_HEADERS_ON_INPUT, &on, sizeof(on))) {
			perror("setsockopt SEE HEADERS");
			exit(1);
		}
		idp.idp_pt = NSPROTO_RI;
		if (setsockopt(s, 0, SO_DEFAULT_HEADERS, &idp, sizeof(idp))) {
			perror("setsockopt SET HEADERS");
			exit(1);
		}
	}
	if (setsockopt(s, SOL_SOCKET, SO_BROADCAST, &on, sizeof (on)) < 0) {
		perror("setsockopt SO_BROADCAST");
		exit(1);
	}
	return (s);
}
