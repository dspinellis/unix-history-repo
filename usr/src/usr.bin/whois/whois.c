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
static char sccsid[] = "@(#)whois.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <stdio.h>
#include <netdb.h>

#define	NICHOST	"sri-nic.arpa"

main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	register FILE *sfi, *sfo;
	register int c;
	struct sockaddr_in sin;
	struct hostent *hp;
	struct servent *sp;
	int ch, s;
	char *host;

	host = NICHOST;
	while ((ch = getopt(argc, argv, "h:")) != EOF)
		switch((char)ch) {
		case 'h':
			host = optarg;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 1)
		usage();

	hp = gethostbyname(host);
	if (hp == NULL) {
		fprintf(stderr, "whois: %s: ", host);
		herror((char *)NULL);
		exit(1);
	}
	host = hp->h_name;
	s = socket(hp->h_addrtype, SOCK_STREAM, 0, 0);
	if (s < 0) {
		perror("whois: socket");
		exit(2);
	}
	bzero((caddr_t)&sin, sizeof (sin));
	sin.sin_family = hp->h_addrtype;
	if (bind(s, &sin, sizeof (sin), 0) < 0) {
		perror("whois: bind");
		exit(3);
	}
	bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);
	sp = getservbyname("whois", "tcp");
	if (sp == NULL) {
		fprintf(stderr, "whois: whois/tcp: unknown service\n");
		exit(4);
	}
	sin.sin_port = sp->s_port;
	if (connect(s, &sin, sizeof (sin), 0) < 0) {
		perror("whois: connect");
		exit(5);
	}
	sfi = fdopen(s, "r");
	sfo = fdopen(s, "w");
	if (sfi == NULL || sfo == NULL) {
		perror("fdopen");
		close(s);
		exit(1);
	}
	fprintf(sfo, "%s\r\n", *argv);
	(void)fflush(sfo);
	while ((c = getc(sfi)) != EOF)
		putchar(c);
}

static
usage()
{
	fprintf(stderr, "usage: whois [-h host] name\n");
	exit(1);
}
