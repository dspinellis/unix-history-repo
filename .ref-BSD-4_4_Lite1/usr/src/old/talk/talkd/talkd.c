/*-
 * Copyright (c) 1983, 1985
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1985\n\
	Regents of the University of California.  All rights reserved.\n";
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
void	timeout();
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
		cc = recvfrom(0, (char *) &request, sizeof (request), 0,
		    (struct sockaddr *)&from, &fromlen);
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
		    sizeof (response), 0, (struct sockaddr *)&request.ctl_addr,
		    sizeof (request.ctl_addr));
		if (cc != sizeof(response))
			perror("sendto");
	}
}

void
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
