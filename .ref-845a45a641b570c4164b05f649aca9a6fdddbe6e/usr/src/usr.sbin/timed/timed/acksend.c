/*-
 * Copyright (c) 1985, 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)acksend.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#ifdef sgi
#ident "$Revision: 1.6 $"
#endif

#include "globals.h"

struct tsp *answer;

extern u_short sequence;

void
xmit(type, seq, addr)
	int type;
	u_int seq;
	struct sockaddr_in *addr;
{
	static struct tsp msg;

	msg.tsp_type = type;
	msg.tsp_seq = seq;
	msg.tsp_vers = TSPVERSION;
	(void)strcpy(msg.tsp_name, hostname);
	bytenetorder(&msg);
	if (sendto(sock, (char *)&msg, sizeof(struct tsp), 0,
		   (struct sockaddr*)addr, sizeof(struct sockaddr)) < 0) {
		trace_sendto_err(addr->sin_addr);
	}
}


/*
 * Acksend implements reliable datagram transmission by using sequence
 * numbers and retransmission when necessary.
 * If `name' is ANYADDR, this routine implements reliable broadcast.
 *
 * Because this function calls readmsg(), none of its args may be in
 *	a message provided by readmsg().
 */
struct tsp *
acksend(message, addr, name, ack, net, bad)
	struct tsp *message;			/* this message */
	struct sockaddr_in *addr;		/* to here */
	char *name;
	int ack;				/* look for this ack */
	struct netinfo *net;			/* receive from this network */
	int bad;				/* 1=losing patience */
{
	struct timeval twait;
	int count;
	long msec;

	message->tsp_vers = TSPVERSION;
	message->tsp_seq = sequence;
	if (trace) {
		fprintf(fd, "acksend: to %s: ",
			(name == ANYADDR ? "broadcast" : name));
		print(message, addr);
	}
	bytenetorder(message);

	msec = 200;
	count = bad ? 1 : 5;	/* 5 packets in 6.4 seconds */
	answer = 0;
	do {
		if (!answer) {
			/* do not go crazy transmitting just because the
			 * other guy cannot keep our sequence numbers
			 * straight.
			 */
			if (sendto(sock, (char *)message, sizeof(struct tsp),
				   0, (struct sockaddr*)addr,
				   sizeof(struct sockaddr)) < 0) {
				trace_sendto_err(addr->sin_addr);
				break;
			}
		}

		mstotvround(&twait, msec);
		answer  = readmsg(ack, name, &twait, net);
		if (answer != 0) {
			if (answer->tsp_seq != sequence) {
				if (trace)
					fprintf(fd,"acksend: seq # %u!=%u\n",
						answer->tsp_seq, sequence);
				continue;
			}
			break;
		}

		msec *= 2;
	} while (--count > 0);
	sequence++;

	return(answer);
}
