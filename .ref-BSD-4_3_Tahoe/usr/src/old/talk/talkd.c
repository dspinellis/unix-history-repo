/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)talkd.c	5.1 (Berkeley) 6/6/85";
#endif not lint

/*
 * The top level of the daemon, the format is heavily borrowed
 * from rwhod.c. Basically: find out who and where you are; 
 * disconnect all descriptors and ttys, and then endless
 * loop on waiting for and processing requests
 */
#include <stdio.h>
#include <errno.h>
#include <signal.h>

#include "ctl.h"

struct	sockaddr_in sin = { AF_INET };

CTL_MSG		request;
CTL_RESPONSE	response;

int	sockt;
int	debug = 0;
FILE	*debugout;
int	timeout();
long	lastmsgtime;

char	hostname[32];

#define TIMEOUT 30
#define MAXIDLE 120

main(argc, argv)
	int argc;
	char *argv[];
{
	struct sockaddr_in from;
	int fromlen, cc;
	
	if (debug)
		debugout = (FILE *)fopen ("/usr/tmp/talkd.msgs", "w");

	if (getuid()) {
		fprintf(stderr, "Talkd : not super user\n");
		exit(1);
	}
	gethostname(hostname, sizeof (hostname));
	(void) chdir("/dev");
	signal(SIGALRM, timeout);
	alarm(TIMEOUT);
	for (;;) {
		extern int errno;

		fromlen = sizeof(from);
		cc = recvfrom(0, (char *)&request, sizeof (request), 0,
		    &from, &fromlen);
		if (cc != sizeof(request)) {
			if (cc < 0 && errno != EINTR)
			perror("recvfrom");
			continue;
		}
		lastmsgtime = time(0);
		swapmsg(&request);
		if (debug) print_request(&request);
		process_request(&request, &response);
		/* can block here, is this what I want? */
		cc = sendto(sockt, (char *) &response,
		    sizeof (response), 0, &request.ctl_addr,
		    sizeof (request.ctl_addr));
		if (cc != sizeof(response))
			perror("sendto");
	}
}

timeout()
{

	if (time(0) - lastmsgtime >= MAXIDLE)
		exit(0);
	alarm(TIMEOUT);
}

/*  
 * heuristic to detect if need to swap bytes
 */

swapmsg(req)
	CTL_MSG *req;
{
	if (req->ctl_addr.sin_family == ntohs(AF_INET)) {
		req->id_num = ntohl(req->id_num);
		req->pid = ntohl(req->pid);
		req->addr.sin_family = ntohs(req->addr.sin_family);
		req->ctl_addr.sin_family =
			ntohs(req->ctl_addr.sin_family);
	}
}
