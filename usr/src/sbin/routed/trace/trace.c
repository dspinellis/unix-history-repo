#ifndef lint
static char sccsid[] = "@(#)trace.c	4.2 %G%";
#endif

#include <sys/param.h>
#include <sys/protosw.h>
#include <sys/socket.h>
#include <net/in.h>
#include <errno.h>
#include <stdio.h>
#include <netdb.h>
#include "rip.h"

struct	sockaddr_in myaddr = { AF_INET, IPPORT_RESERVED-1 };

main(argc, argv)
	int argc;
	char *argv[];
{
	int size, s;
	struct sockaddr from;
	struct sockaddr_in router;
	char packet[MAXPACKETSIZE];
	register struct rip *msg = (struct rip *)packet;
	struct hostent *hp;
	struct servent *sp;
	
	if (argc < 3) {
usage:
		printf("usage: trace cmd machines,\n");
		printf("cmd either \"on filename\", or \"off\"\n");
		exit(1);
	}
#ifdef vax || pdp11
	myaddr.sin_port = htons(myaddr.sin_port);
#endif
	s = socket(SOCK_DGRAM, 0, &myaddr, 0);
	if (s < 0) {
		perror("socket");
		exit(2);
	}
	argv++, argc--;
	msg->rip_cmd = strcmp(*argv, "on") == 0 ?
		RIPCMD_TRACEON : RIPCMD_TRACEOFF;
	argv++, argc--;
	size = sizeof (int);
	if (msg->rip_cmd == RIPCMD_TRACEON) {
		strcpy(msg->rip_tracefile, *argv);
		size += strlen(*argv);
		argv++, argc--;
	}
	if (argc == 0)
		goto usage;
	bzero((char *)&router, sizeof (router));
	router.sin_family = AF_INET;
	sp = getservbyname("router", "udp");
	if (sp == 0) {
		printf("udp/router: service unknown\n");
		exit(1);
	}
	router.sin_port = htons(sp->s_port);
	while (argc > 0) {
		hp = gethostbyname(*argv);
		if (hp == 0) {
			printf("%s: unknown\n", *argv);
			continue;
		}
		bcopy(hp->h_addr, &router.sin_addr, hp->h_length);
		if (send(s, &router, packet, size) < 0)
			perror(*argv);
		argv++, argc--;
	}
}
