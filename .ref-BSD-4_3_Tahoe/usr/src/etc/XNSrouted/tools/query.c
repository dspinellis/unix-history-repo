/*
 * Copyright (c) 1983, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 * This file include significant work done at Cornell University
 * by Bill Nesheim.  That work included by permission.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1986 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)query.c	1.5 (Berkeley) 4/6/87";
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

#define	WTIME	5		/* Time to wait for responses */

int	s;
int	timedout, timeout();
char	packet[MAXPACKETSIZE];
extern int errno;
struct sockaddr_ns	myaddr = {AF_NS};
char *ns_ntoa();
struct ns_addr ns_addr();
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
	query(argv,argc);

	/*
	 * Listen for returning packets;
	 * may be more than one packet per host.
	 */
	bits = 1 << s;
	bzero(&notime, sizeof(notime));
	signal(SIGALRM, timeout);
	alarm(WTIME);
	while (!timedout || 
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
static struct sockaddr_ns router = {AF_NS};
static struct ns_addr zero_addr;
static short allones[] = {-1, -1, -1};

query(argv,argc)
char **argv;
{
	register struct rip *msg = (struct rip *)packet;
	char *host = *argv;
	struct ns_addr specific;

	argv++; argc--;
	router.sns_addr = ns_addr(host);
	router.sns_addr.x_port = htons(IDPPORT_RIF);
	if (ns_hosteq(zero_addr, router.sns_addr)) {
		router.sns_addr.x_host = *(union ns_host *) allones;
	}
	msg->rip_cmd = htons(RIPCMD_REQUEST);
	msg->rip_nets[0].rip_dst = *(union ns_net *) allones;
	msg->rip_nets[0].rip_metric = htons(HOPCNT_INFINITY);
	if (argc > 0) {
		specific = ns_addr(*argv);
		msg->rip_nets[0].rip_dst = specific.x_net;
		specific.x_host = zero_addr.x_host;
		specific.x_port = zero_addr.x_port;
		printf("Net asked for was %s\n", ns_ntoa(specific));
	}
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
	static struct ns_addr work;

	if (htons(msg->rip_cmd) != RIPCMD_RESPONSE)
		return;
	printf("from %s\n", ns_ntoa(from->sns_addr));
	size -= sizeof (struct idp);
	size -= sizeof (short);
	n = msg->rip_nets;
	while (size > 0) {
		union ns_net_u net;
		if (size < sizeof (struct netinfo))
			break;
		net.net_e = n->rip_dst;
		printf("\t%d, metric %d\n", ntohl(net.long_e),
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
