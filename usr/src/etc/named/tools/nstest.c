#ifndef lint
static char sccsid[] = "@(#)nstest.c	4.1 (Berkeley) 5/12/86";
#endif

/*
 * Copyright (c) 1986 Regents of the University of California
 *	All Rights Reserved
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <arpa/nameser.h>
#include <resolv.h>

extern char *inet_ntoa();
char *progname;
FILE *log;

main(argc, argv)
	char **argv;
{
	register char *cp;
	struct hostent *hp;
	short port = htons(NAMESERVER_PORT);
	char buf[BUFSIZ];
	char packet[PACKETSZ];
	char answer[PACKETSZ];
	int n;

	progname = argv[0];
	_res.options |= RES_INIT|RES_DEBUG|RES_RECURSE;
	while (argc > 1 && argv[1][0] == '-') {
		argc--;
		cp = *++argv;
		while (*++cp)
			switch (*cp) {
			case 'p':
				if (--argc <= 0)
					usage();
				port = htons(atoi(*++argv));
				break;

			case 'i':
				_res.options |= RES_IGNTC;
				break;

			case 'v':
				_res.options |= RES_USEVC|RES_STAYOPEN;
				break;

			case 'r':
				_res.options &= ~RES_RECURSE;
				break;

			default:
				usage();
			}
	}
	_res.nsaddr.sin_family = AF_INET;
	_res.nsaddr.sin_addr.s_addr = INADDR_ANY;
	_res.nsaddr.sin_port = port;
 	if (argc > 1)
 		_res.nsaddr.sin_addr.s_addr = inet_addr(argv[1]);
 	if (argc > 2) {
 		log = fopen(argv[2],"w");
 		if (log == NULL) perror(argv[2]);
 	}
	for (;;) {
		printf("> ");
		fflush(stdout);
		if ((cp = (char *)gets(buf)) == NULL)
			break;
		switch (*cp++) {
		case 'a':
			n = res_mkquery(QUERY, cp, C_ANY, T_A, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		case 'A':
			n = ntohl(inet_addr(cp));
			putlong(n, cp);
			n = res_mkquery(IQUERY, "", C_IN, T_A, cp, sizeof(long), NULL,
				packet, sizeof(packet));
			break;

		case 'f':
			n = res_mkquery(QUERY, cp, C_ANY, T_UINFO, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		case 'g':
			n = res_mkquery(QUERY, cp, C_ANY, T_GID, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		case 'G':
			*(int *)cp = htonl(atoi(cp));
			n = res_mkquery(IQUERY, "", C_ANY, T_GID, cp, sizeof(int), NULL,
				packet, sizeof(packet));
			break;

		case 'h':
			n = res_mkquery(QUERY, cp, C_IN, T_HINFO, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		case 'm':
			n = res_mkquery(QUERY, cp, C_IN, T_MX, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		case 'M':
			n = res_mkquery(QUERY, cp, C_IN, T_MAILB, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		case 'n':
			n = res_mkquery(QUERY, cp, C_IN, T_NS, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		case 'p':
			n = res_mkquery(QUERY, cp, C_IN, T_PTR, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		case 's':
			n = res_mkquery(QUERY, cp, C_IN, T_SOA, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		case 'u':
			n = res_mkquery(QUERY, cp, C_ANY, T_UID, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		case 'U':
			*(int *)cp = htonl(atoi(cp));
			n = res_mkquery(IQUERY, "", C_ANY, T_UID, cp, sizeof(int), NULL,
				packet, sizeof(packet));
			break;

		case 'x':
			n = res_mkquery(QUERY, cp, C_IN, T_AXFR, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		case 'w':
			n = res_mkquery(QUERY, cp, C_IN, T_WKS, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		case '*':
			n = res_mkquery(QUERY, cp, C_IN, T_ANY, (char *)0, 0, NULL,
				packet, sizeof(packet));
			break;

		default:
			printf("a{host} - query  T_A\n");
			printf("A{addr} - iquery T_A\n");
			printf("f{host} - query  T_UINFO\n");
			printf("g{host} - query  T_GID\n");
			printf("G{gid}  - iquery T_GID\n");
			printf("h{host} - query  T_HINFO\n");
			printf("p{host} - query  T_PTR\n");
			printf("m{host} - query  T_MX\n");
			printf("M{host} - query  T_MAILB\n");
			printf("n{host} - query  T_NS\n");
			printf("s{host} - query  T_SOA\n");
			printf("u{host} - query  T_UID\n");
			printf("U{uid}  - iquery T_UID\n");
			printf("x{host} - query  T_AXFR\n");
			printf("w{host} - query  T_WKS\n");
			printf("*{host} - query  T_ANY\n");
			continue;
		}
		if (n < 0) {
			printf("res_mkquery: buffer too small\n");
			continue;
		}
		if (log) {
			fprintf(log,"SEND QUERY\n");
			fp_query(packet, log);
		}
		n = res_send(packet, n, answer, sizeof(answer));
		if (n < 0) {
			printf("res_send: send error\n");
			if (log) fprintf(log, "res_send: send error\n");
		}
		else {
			int f;

			f = creat("r", 0644);
			write(f, answer, n);
			(void) close(f);
			if (log) {
				fprintf(log, "GOT ANSWER\n");
				fp_query(answer, log);
			}
		}
	}
}

usage()
{
	fprintf(stderr, "Usage: %s [-v] [-i] [-r] [-p port] hostaddr\n",
		progname);
	exit(1);
}
