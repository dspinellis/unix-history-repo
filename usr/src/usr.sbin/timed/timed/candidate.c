/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)candidate.c	2.6 (Berkeley) %G%";
#endif /* not lint */

#include "globals.h"
#include <protocols/timed.h>

#define ELECTIONWAIT	3	/* seconds */

/*
 * `election' candidates a host as master: it is called by a slave 
 * which runs with the -M option set when its election timeout expires. 
 * Note the conservative approach: if a new timed comes up, or another
 * candidate sends an election request, the candidature is withdrawn.
 */

election(net)
struct netinfo *net;
{
	int ret;
	struct tsp *resp, msg, *readmsg();
	struct timeval wait;
	struct tsp *answer, *acksend();
	long casual();
	struct sockaddr_in server;

	syslog(LOG_INFO, "THIS MACHINE IS A CANDIDATE");
	if (trace) {
		fprintf(fd, "THIS MACHINE IS A CANDIDATE\n");
	}

	ret = MASTER;
	slvcount = 1;

	msg.tsp_type = TSP_ELECTION;
	msg.tsp_vers = TSPVERSION;
	(void)strcpy(msg.tsp_name, hostname);
	bytenetorder(&msg);
	if (sendto(sock, (char *)&msg, sizeof(struct tsp), 0, &net->dest_addr,
	    sizeof(struct sockaddr_in)) < 0) {
		syslog(LOG_ERR, "sendto: %m");
		exit(1);
	}

	do {
		wait.tv_sec = ELECTIONWAIT;
		wait.tv_usec = 0;
		resp = readmsg(TSP_ANY, (char *)ANYADDR, &wait, net);
		if (resp != NULL) {
			switch (resp->tsp_type) {

			case TSP_ACCEPT:
				(void) addmach(resp->tsp_name, &from);
				break;

			case TSP_MASTERUP:
			case TSP_MASTERREQ:
				/*
				 * If a timedaemon is coming up at the same time,
				 * give up the candidature: it will be the master.
				 */
				ret = SLAVE;
				break;

			case TSP_QUIT:
			case TSP_REFUSE:
				/*
				 * Collision: change value of election timer 
				 * using exponential backoff.
				 * The value of timer will be recomputed (in slave.c)
				 * using the original interval when election will 
				 * be successfully completed.
				 */
				backoff *= 2;
				delay2 = casual((long)MINTOUT, 
							(long)(MAXTOUT * backoff));
				ret = SLAVE;
				break;

			case TSP_ELECTION:
				/* no master for another round */
				msg.tsp_type = TSP_REFUSE;
				(void)strcpy(msg.tsp_name, hostname);
				server = from;
				answer = acksend(&msg, &server, resp->tsp_name,
				    TSP_ACK, (struct netinfo *)NULL);
				if (answer == NULL) {
					syslog(LOG_ERR, "error in election");
				} else {
					(void) addmach(resp->tsp_name, &from);
				}
				break;

			case TSP_SLAVEUP:
				(void) addmach(resp->tsp_name, &from);
				break;

			case TSP_SETDATE:
			case TSP_SETDATEREQ:
				break;

			default:
				if (trace) {
					fprintf(fd, "candidate: ");
					print(resp, &from);
				}
				break;
			}
		} else {
			break;
		}
	} while (ret == MASTER);
	return(ret);
}
