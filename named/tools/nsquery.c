/*
 * Copyright (c) 1986 Regents of the University of California.
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
"@(#) Copyright (c) 1986 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)nsquery.c	4.6 (Berkeley) 10/10/88";
#endif /* not lint */

#include <sys/param.h>
#include <arpa/nameser.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <resolv.h>
#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern struct state _res;
	register struct hostent *hp;
	register char *s;
	char h[MAXHOSTNAMELEN];

	switch(argc) {
	case 2:
		gethostname(h, sizeof(h));
		s = h;
		break;
	case 3:
		s = argv[2];
		break;
	default:
		fprintf(stderr, "usage: nsquery host [server]\n");
		exit(1);
	}

	hp = gethostbyname(s);
	if (hp == NULL) {
		fprintf(stderr, "nsquery: %s: ", s);
		herror((char *)NULL);
		exit(1);
	}
	printanswer(hp);

	_res.nsaddr.sin_addr = *(struct in_addr *)hp->h_addr;
	_res.options &= ~RES_DEFNAMES;

	hp = gethostbyname(argv[1]);
	if (hp == NULL) {
		fprintf(stderr, "nsquery: %s: ", argv[1]);
		herror((char *)NULL);
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
#if BSD >= 43 || defined(h_addr)
	printf("Addresses:");
	for (cp = hp->h_addr_list; cp && *cp; cp++)
		printf(" %s", inet_ntoa(*(struct in_addr *)(*cp)));
	printf("\n");
#else
	printf("Address: %s\n", inet_ntoa(*(struct in_addr *)hp->h_addr));
#endif
	printf("Aliases:");
	for (cp = hp->h_aliases; cp && *cp && **cp; cp++)
		printf(" %s", *cp);
	printf("\n\n");
}
