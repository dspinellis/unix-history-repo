/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
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
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mconnect.c	5.4 (Berkeley) 6/29/88";
#endif /* not lint */

#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#include <sgtty.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

static struct sgttyb TtyBuf;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register FILE *f;
	register int s;
	struct servent *sp;
	struct sockaddr_in SendmailAddress;
	int ch, raw;
	char *host, buf[1000], *index();
	u_long inet_addr();
	int finis();

	raw = 0;
	(void)gtty(0, &TtyBuf);
	(void)signal(SIGINT, finis);

	if ((s = socket(AF_INET, SOCK_STREAM, 0, 0)) < 0) {
		perror("socket");
		exit(-1);
	}

	sp = getservbyname("smtp", "tcp");
	if (sp != NULL)
		SendmailAddress.sin_port = sp->s_port;

	while ((ch = getopt(argc, argv, "hp:r")) != EOF)
		switch((char)ch) {
		case 'h':	/* host */
			break;
		case 'p':	/* port */
			SendmailAddress.sin_port = htons(atoi(optarg));
			break;
		case 'r':	/* raw connection */
			raw = 1;
			TtyBuf.sg_flags &= ~CRMOD;
			stty(0, &TtyBuf);
			TtyBuf.sg_flags |= CRMOD;
			break;
		case '?':
		default:
			fputs("usage: mconnect [-hr] [-p port] [host]\n", stderr);
			exit(-1);
		}
	argc -= optind;
	argv += optind;

	host = argc ? *argv : "localhost";

	if (isdigit(*host))
		SendmailAddress.sin_addr.s_addr = inet_addr(host);
	else {
		register struct hostent *hp = gethostbyname(host);

		if (hp == NULL) {
			fprintf(stderr, "mconnect: unknown host %s\r\n", host);
			finis();
		}
		bcopy(hp->h_addr, &SendmailAddress.sin_addr, hp->h_length);
	}
	SendmailAddress.sin_family = AF_INET;
	printf("connecting to host %s (0x%lx), port 0x%x\r\n", host,
	       SendmailAddress.sin_addr.s_addr, SendmailAddress.sin_port);
	if (connect(s, &SendmailAddress, sizeof(SendmailAddress), 0) < 0) {
		perror("connect");
		exit(-1);
	}

	/* good connection, fork both sides */
	puts("connection open");
	switch(fork()) {
	case -1:
		perror("fork");
		exit(-1);
	case 0: {		/* child -- standard input to sendmail */
		int c;

		f = fdopen(s, "w");
		while ((c = fgetc(stdin)) >= 0) {
			if (!raw && c == '\n')
				fputc('\r', f);
			fputc(c, f);
			if (c == '\n')
				(void)fflush(f);
		}
	}
	default:		/* parent -- sendmail to standard output */
		f = fdopen(s, "r");
		while (fgets(buf, sizeof(buf), f) != NULL) {
			fputs(buf, stdout);
			(void)fflush(stdout);
		}
	}
	finis();
}

finis()
{
	stty(0, &TtyBuf);
	exit(0);
}
