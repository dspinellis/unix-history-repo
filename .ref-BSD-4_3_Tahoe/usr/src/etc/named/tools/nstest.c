#ifndef lint
static char sccsid[] = "@(#)nstest.c	4.6 (Berkeley) 12/7/86";
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
#define MAXDATA		256   /* really should get definition from named/db.h */
main(argc, argv)
	char **argv;
{
	register char *cp;
	struct hostent *hp;
	short port = htons(NAMESERVER_PORT);
	char buf[BUFSIZ];
	char packet[PACKETSZ];
	char answer[PACKETSZ];
	struct rrec NewRR;
	char OldRRData[MAXDATA];
	int n, dump_packet;

	NewRR.r_data = (char *) malloc(MAXDATA);
	NewRR.r_data = (char *) malloc(MAXDATA);
	progname = argv[0];
	dump_packet = 0;
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

			case 'd':
				dump_packet++;
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
			n = res_mkquery(QUERY, cp, C_ANY, T_A, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'A':
			n = ntohl(inet_addr(cp));
			putlong(n, cp);
			n = res_mkquery(IQUERY, "", C_IN, T_A, cp, sizeof(long),
				NULL, packet, sizeof(packet));
			break;

		case 'f':
			n = res_mkquery(QUERY, cp, C_ANY, T_UINFO, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'g':
			n = res_mkquery(QUERY, cp, C_ANY, T_GID, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'G':
			*(int *)cp = htonl(atoi(cp));
			n = res_mkquery(IQUERY, "", C_ANY, T_GID, cp,
				sizeof(int), NULL, packet, sizeof(packet));
			break;

		case 'c':
			n = res_mkquery(QUERY, cp, C_IN, T_CNAME, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'h':
			n = res_mkquery(QUERY, cp, C_IN, T_HINFO, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'm':
			n = res_mkquery(QUERY, cp, C_IN, T_MX, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'M':
			n = res_mkquery(QUERY, cp, C_IN, T_MAILB, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'n':
			n = res_mkquery(QUERY, cp, C_IN, T_NS, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'p':
			n = res_mkquery(QUERY, cp, C_IN, T_PTR, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 's':
			n = res_mkquery(QUERY, cp, C_IN, T_SOA, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'u':
			n = res_mkquery(QUERY, cp, C_ANY, T_UID, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'U':
			*(int *)cp = htonl(atoi(cp));
			n = res_mkquery(IQUERY, "", C_ANY, T_UID, cp,
				sizeof(int), NULL, packet, sizeof(packet));
			break;

		case 'x':
			n = res_mkquery(QUERY, cp, C_IN, T_AXFR, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'w':
			n = res_mkquery(QUERY, cp, C_IN, T_WKS, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'b':
			n = res_mkquery(QUERY, cp, C_IN, T_MB, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'B':
			n = res_mkquery(QUERY, cp, C_IN, T_MG, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'i':
			n = res_mkquery(QUERY, cp, C_IN, T_MINFO, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case 'r':
			n = res_mkquery(QUERY, cp, C_IN, T_MR, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

		case '*':
			n = res_mkquery(QUERY, cp, C_IN, T_ANY, (char *)0, 0,
				NULL, packet, sizeof(packet));
			break;

#ifdef ALLOW_UPDATES
		case '^':
			{
			    char IType[10], TempStr[50];
			    int Type, oldnbytes, nbytes, i;
#ifdef ALLOW_T_UNSPEC
			    printf("Data type (a = T_A, u = T_UNSPEC): ");
			    gets(IType);
			    if (IType[0] == 'u') {
			    	Type = T_UNSPEC;
			    	printf("How many data bytes? ");
			    	gets(TempStr); /* Throw away CR */
			    	sscanf(TempStr, "%d", &nbytes);
			    	for (i = 0; i < nbytes; i++) {
			    		(NewRR.r_data)[i] = (char) i;
			    	}
			    } else {
#endif ALLOW_T_UNSPEC
			    	Type = T_A;
			    	nbytes = sizeof(u_long);
			    	printf("Inet addr for new dname (e.g., 192.4.3.2): ");
			    	gets(TempStr);
			    	putlong(ntohl(inet_addr(TempStr)), NewRR.r_data);
#ifdef ALLOW_T_UNSPEC
			    }
#endif ALLOW_T_UNSPEC
			    NewRR.r_class = C_IN;
			    NewRR.r_type = Type;
			    NewRR.r_size = nbytes;
			    NewRR.r_ttl = 99999999;
			    printf("Add, modify, or modify all (a/m/M)? ");
			    gets(TempStr);
			    if (TempStr[0] == 'a') {
			    	n = res_mkquery(UPDATEA, cp, C_IN, Type,
			    			OldRRData, nbytes,
			    			&NewRR, packet,
			    			sizeof(packet));
			    } else {
			    	if (TempStr[0] == 'm') {
			    	    printf("How many data bytes in old RR? ");
			    	    gets(TempStr); /* Throw away CR */
			    	    sscanf(TempStr, "%d", &oldnbytes);
				    for (i = 0; i < oldnbytes; i++) {
					    OldRRData[i] = (char) i;
				    }
					n = res_mkquery(UPDATEM, cp, C_IN, Type,
							OldRRData, oldnbytes,
							&NewRR, packet,
							sizeof(packet));
				} else { /* Modify all */
					n = res_mkquery(UPDATEMA, cp,
							C_IN, Type, NULL, 0,
							&NewRR, packet,
							sizeof(packet));

				}
			    }
			}
			break;

#ifdef ALLOW_T_UNSPEC
		case 'D':
			n = res_mkquery(UPDATEDA, cp, C_IN, T_UNSPEC, (char *)0,
					0, NULL, packet, sizeof(packet));
			break;

		case 'd':
			{
				char TempStr[100];
				int nbytes, i;
				printf("How many data bytes in oldrr data? ");
				gets(TempStr); /* Throw away CR */
				sscanf(TempStr, "%d", &nbytes);
				for (i = 0; i < nbytes; i++) {
					OldRRData[i] = (char) i;
				}
				n = res_mkquery(UPDATED, cp, C_IN, T_UNSPEC,
						OldRRData, nbytes, NULL, packet,
						sizeof(packet));
			}
			break;
#endif ALLOW_T_UNSPEC
#endif ALLOW_UPDATES

		default:
			printf("a{host} - query  T_A\n");
			printf("A{addr} - iquery T_A\n");
			printf("b{user} - query  T_MB\n");
			printf("B{user} - query  T_MG\n");
			printf("f{host} - query  T_UINFO\n");
			printf("g{host} - query  T_GID\n");
			printf("G{gid}  - iquery T_GID\n");
			printf("h{host} - query  T_HINFO\n");
			printf("i{host} - query  T_MINFO\n");
			printf("p{host} - query  T_PTR\n");
			printf("m{host} - query  T_MX\n");
			printf("M{host} - query  T_MAILB\n");
			printf("n{host} - query  T_NS\n");
			printf("r{host} - query  T_MR\n");
			printf("s{host} - query  T_SOA\n");
			printf("u{host} - query  T_UID\n");
			printf("U{uid}  - iquery T_UID\n");
			printf("x{host} - query  T_AXFR\n");
			printf("w{host} - query  T_WKS\n");
			printf("c{host} - query  T_CNAME\n");
			printf("*{host} - query  T_ANY\n");
#ifdef ALLOW_UPDATES
			printf("^{host} - add/mod/moda    (T_A/T_UNSPEC)\n");
#ifdef ALLOW_T_UNSPEC
			printf("D{host} - deletea T_UNSPEC\n");
			printf("d{host} - delete T_UNSPEC\n");
#endif ALLOW_T_UNSPEC
#endif ALLOW_UPDATES
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
			if (dump_packet) {
				int f;
				f = creat("ns_packet.dump", 0644);
				write(f, answer, n);
				(void) close(f);
			}
			if (log) {
				fprintf(log, "GOT ANSWER\n");
				fp_query(answer, log);
			}
		}
	}
}

usage()
{
	fprintf(stderr, "Usage: %s [-v] [-i] [-r] [-d] [-p port] hostaddr\n",
		progname);
	exit(1);
}
