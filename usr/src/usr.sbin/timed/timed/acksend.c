/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)acksend.c	1.3 (Berkeley) %G%";
#endif not lint

#include "globals.h"
#include <protocols/timed.h>

#define RECEIVED	0
#define LOST	 	1
#define SECFORACK	1	/* seconds */
#define USECFORACK	0	/* microseconds */
#define MAXCOUNT	5

struct tsp *answer;
extern int sock;
extern struct sockaddr_in server;
extern int trace;
extern FILE *fd;

/*
 * Acksend implements reliable datagram transmission by using sequence 
 * numbers and retransmission when necessary.
 * `name' is set to name of destination whose address is in global 
 * variable `server'.
 * If `name' is ANYADDR, this routine implements reliable broadcast.
 */

struct tsp *acksend(message, name, ack)
struct tsp *message;
char *name;
int ack;
{
	int count;
	int flag;
	extern u_short sequence;
	struct timeval tout;
	struct tsp *readmsg();

	count = 0;

	message->tsp_vers = TSPVERSION;
	message->tsp_seq = sequence;
	if (trace) {
		fprintf(fd, "acksend: ");
		if (name == ANYADDR)
			fprintf(fd, "broadcast: ");
		else
			fprintf(fd, "%s: ", name);
		print(message);
	}
	bytenetorder(message);
	do {
		if (name == ANYADDR) {
			broadcast(message);
		} else {
			if (sendto(sock, (char *)message, sizeof(struct tsp),
					0, &server, 
					sizeof(struct sockaddr_in)) < 0) {
				syslog(LOG_ERR, "acksend: sendto: %m");
				exit(1);
			}
		}
		tout.tv_sec = SECFORACK;
		tout.tv_usec = USECFORACK;
		answer  = readmsg(ack, name, &tout);
		if (answer != NULL) {
			flag = RECEIVED;
		} else {
			flag = LOST;
			if (++count == MAXCOUNT) {
				break;
			}
		}
	} while (flag != RECEIVED);
	if (++sequence > MAXSEQ)
		sequence = 1;
	return(answer);
}
