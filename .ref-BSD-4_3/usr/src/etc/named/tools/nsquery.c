#ifndef lint
static char sccsid[] = "@(#)nsquery.c	4.1 (Berkeley) 5/12/86";
#endif

/*
 * Copyright (c) 1986 Regents of the University of California
 *	All Rights Reserved
 */

#include <stdio.h>
#include <sys/types.h>
#include <arpa/nameser.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <resolv.h>

struct state orig;
extern struct state _res;

main(c, v)
	char **v;
{
	char h[32];
	register struct hostent *hp;
	register char *s;

	gethostname(h, 32);
	s = h;
	if (c < 2) {
		fprintf(stderr, "Usage: lookup host [server]\n");
		exit(1);
	}
	if (c > 2)
		s = v[2];

	hp = gethostbyname(s);
	if (hp == NULL) {
		hperror(h_errno);
		exit(1);
	}
	printanswer(hp);

	_res.nsaddr.sin_addr = *(struct in_addr *)hp->h_addr;
	_res.options &= ~RES_DEFNAMES;

	hp = gethostbyname(v[1]);
	if (hp == NULL) {
		hperror(h_errno);
		exit(1);
	}
	printanswer(hp);
	exit(0);
}

printanswer(hp)
	register struct hostent *hp;
{
	register char **cp;
	extern char *inet_ntoa();

	printf("Name: %s\n", hp->h_name);
	printf("Address: %s\n", inet_ntoa(*(struct in_addr *)hp->h_addr));
	printf("Aliases:");
	for (cp = hp->h_aliases; cp && *cp && **cp; cp++)
		printf(" %s", *cp);
	printf("\n\n");
}

hperror(errno) 
int errno;
{
switch(errno) {
	case HOST_NOT_FOUND:
		fprintf(stderr,"Host not found.\n");
		break;
	case TRY_AGAIN:
		fprintf(stderr,"Host not found, try again.\n");
		break;
	case NO_RECOVERY:
		fprintf(stderr,"No recovery, Host not found.\n");
		break;
	case NO_ADDRESS:
		fprintf(stderr,"No Address, look for MF record.\n");
		break;
	}
}

