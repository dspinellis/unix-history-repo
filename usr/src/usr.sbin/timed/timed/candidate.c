/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)candidate.c	1.1 (Berkeley) %G%";
#endif not lint

#include "globals.h"
#include <protocols/timed.h>

#define MASTER 		1
#define SLAVE 		0
#define ELECTIONWAIT	3	/* seconds */

extern int trace;
extern int slvcount;
extern int backoff;
extern long delay2;
extern char hostname[];
extern struct sockaddr_in from;
extern struct sockaddr_in server;
extern FILE *fd;

/*
 * `election' candidates a host as master: it is called by a slave 
 * which runs with the -M option set when its election timeout expires. 
 * Note the conservative approach: if a new timed comes up, or another
 * candidate sends an election request, the candidature is withdrawn.
 */

election()
{
	int ret;
	char *strcpy();
	struct tsp *resp, msg, *readmsg();
	struct timeval wait;
	struct tsp *answer, *acksend();
	long casual();

	syslog(LOG_ERR, "timed: THIS MACHINE IS A CANDIDATE\n");
	if (trace) {
		fprintf(fd, "THIS MACHINE IS A CANDIDATE\n");
	}

	ret = MASTER;
	slvcount = 1;

	msg.tsp_type = TSP_ELECTION;
	(void)strcpy(msg.tsp_name, hostname);
	broadcast(&msg);

loop:
	wait.tv_sec = ELECTIONWAIT;
	wait.tv_usec = 0;
	resp = readmsg(TSP_ANY, (char *)ANYADDR, &wait);
	if (resp != NULL) {
		switch (resp->tsp_type) {

		case TSP_ACCEPT:
			(void) addmach(resp->tsp_name);
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
			answer = acksend(&msg, resp->tsp_name, TSP_ACK);
			if (answer == NULL) {
				syslog(LOG_ERR, "timed: error in election\n");
			} else {
				(void) addmach(resp->tsp_name);
			}
			break;
		case TSP_SLAVEUP:
			(void) addmach(resp->tsp_name);
			break;
		case TSP_DATE:
		case TSP_DATEREQ:
			break;
		default:
			if (trace) {
				fprintf(fd, "candidate: ");
				print(resp);
			}
			break;
		}

		if (ret == MASTER)
			goto loop;
	}
	return(ret);
}
