/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)readmsg.c	1.1 (Berkeley) %G%";
#endif not lint

#include "globals.h"
#include <protocols/timed.h>

#define SLAVE 	0
#define MASTER	1

/*
 * LOOKAT checks if the message is of the requested type and comes from
 * the right machine, returning 1 in case of affirmative answer 
 */

#define LOOKAT(msg) \
	((((type == TSP_ANY) || (type == (msg).tsp_type)) && \
	((machfrom == NULL) || (strcmp(machfrom, (msg).tsp_name) == 0))) \
	? 1 : 0)

#define ISTOUTOFF \
	((rtime.tv_sec > rtout.tv_sec || (rtime.tv_sec == rtout.tv_sec && \
				rtime.tv_usec >= rtout.tv_usec)) \
	? 1 : 0)

struct timeval rtime, rwait, rtout;
struct tsp msgin;
static struct tsplist {
	struct tsp info;
	struct sockaddr_in addr;
	struct tsplist *p;
} msgslist;
struct tsplist *ptr, *prev;
struct sockaddr_in from;
extern int trace;
extern int sock;
extern char hostname[];
extern FILE *fd;

/*
 * `readmsg' returns message `type' sent by `machfrom' if it finds it 
 * either in the receive queue, or in a linked list of previously received 
 * messages that it maintains.
 * Otherwise it waits to see if the appropriate message arrives within
 * `intvl' seconds. If not, it returns NULL.
 */

struct tsp *readmsg(type, machfrom, intvl)
int type;
char *machfrom;
struct timeval *intvl;
{
	int length;
	int ready, found;
	static struct tsp *ret;
	extern int status;
	static struct tsplist *head = &msgslist;
	static struct tsplist *tail = &msgslist;
	int inet_netof();
	char *malloc(), *strcpy();
	int bytenetorder(), bytehostorder();

	ret = NULL;

	if (trace) {
		ptr = head->p;
		fprintf(fd, "msgqueue:\n");
		while (ptr != NULL) {
			fprintf(fd, "\t");
			print(&ptr->info);
			ptr = ptr->p;
		}
	}

	ptr = head->p;
	prev = head;

	/*
	 * Look for the requested message scanning through the 
	 * linked list. If found, return it and free the space 
	 */

	while (ptr != NULL) {
		if (LOOKAT(ptr->info)) {
			ret = (struct tsp *)malloc(sizeof(struct tsp)); 
			*ret = ptr->info;
			from = ptr->addr;
			prev->p = ptr->p;
			if (ptr == tail) 
				tail = prev;
			free((char *)ptr);
			break;
		} else {
			prev = ptr;
			ptr = ptr->p;
		}
	}

	if (ret != NULL)
		goto out;

	/*
	 * If the message was not in the linked list, it may still be
	 * coming from the network. Set the timer and wait 
	 * on a select to read the next incoming message: if it is the
	 * right one, return it, otherwise insert it in the linked list.
	 */

	(void)gettimeofday(&rtime, (struct timezone *)0);
	rtout.tv_sec = rtime.tv_sec + intvl->tv_sec;
	rtout.tv_usec = rtime.tv_usec + intvl->tv_usec;
	if (rtout.tv_usec > 1000000) {
		rtout.tv_usec -= 1000000;
		rtout.tv_sec++;
	}

	for (;;) {
		rwait.tv_sec = rtout.tv_sec - rtime.tv_sec;
		rwait.tv_usec = rtout.tv_usec - rtime.tv_usec;
		if (rwait.tv_usec < 0) {
			rwait.tv_usec += 1000000;
			rwait.tv_sec--;
		}
		if (rwait.tv_sec < 0) 
			rwait.tv_sec = rwait.tv_usec = 0;

		if (trace) {
			fprintf(fd, "readmsg: wait: (%d %d)\n", 
						rwait.tv_sec, rwait.tv_usec);
			(void)fflush(fd);
		}
		ready = 1<<sock;
		found = select(20, &ready, (int *)0, (int *)0, &rwait);
		if (found) {
			length = sizeof(struct sockaddr_in);
			if (recvfrom(sock, (char *)&msgin, sizeof(struct tsp), 
						0, &from, &length) < 0) {
				syslog(LOG_ERR, "timed: receiving datagram packet: %m");
				exit(1);
			}

			bytehostorder(&msgin);

/*
 * Should check here to see if message comes from local net.
 * Until done, master cannot run on gateway conneting
 * two subnets each with running timedaemons.
 */
			/*
			 * Throw away messages coming from this machine, unless
			 * they are of some particular type.
			 * This gets rid of broadcast messages and reduces
			 * master processing time.
			 */
			if ( !(strcmp(msgin.tsp_name, hostname) != 0 ||
					msgin.tsp_type == TSP_DATE ||
#ifdef TESTING
					msgin.tsp_type == TSP_TEST ||
#endif
					msgin.tsp_type == TSP_MSITE ||
					msgin.tsp_type == TSP_TRACEON ||
					msgin.tsp_type == TSP_TRACEOFF)) {
				if (trace) {
					fprintf(fd, "readmsg: discarded: ");
					print(&msgin);
				}
				(void)gettimeofday(&rtime,(struct timezone *)0);
				if (ISTOUTOFF)
					break;
				else
					continue;
			}

			/*
			 * Send acknowledgements here; this is faster and avoids
			 * deadlocks that would occur if acks were sent from a 
			 * higher level routine.  Different acknowledgements are
			 * necessary, depending on status.
			 */
			if (status == MASTER)
				masterack();
			else 
				slaveack();

			if (LOOKAT(msgin)) {
				ret = &msgin;
				break;
			} else {
				tail->p = (struct tsplist *)
						malloc(sizeof(struct tsplist)); 
				tail = tail->p;
				tail->p = NULL;
				tail->info = msgin;
				tail->addr = from;
			}

			(void)gettimeofday(&rtime, (struct timezone *)0);
			if (ISTOUTOFF)
				break;
		} else {
			break;
		}
	}
out:
	if (ret != NULL) {
		if (trace) {
			fprintf(fd, "readmsg: ");
			print(ret);
		}
	}
	return(ret);
}

/*
 * `slaveack' sends the necessary acknowledgements: 
 * only the type ACK is to be sent by a slave 
 */

slaveack()
{
	int length;
	struct tsp resp;

	length = sizeof(struct sockaddr_in);
	switch(msgin.tsp_type) {

	case TSP_ADJTIME:
	case TSP_SETTIME:
	case TSP_ACCEPT:
	case TSP_REFUSE:
	case TSP_TRACEON:
	case TSP_TRACEOFF:
		resp = msgin;
		resp.tsp_type = TSP_ACK;
		resp.tsp_vers = TSPVERSION;
		(void)strcpy(resp.tsp_name, hostname);
		bytenetorder(&resp);     /* this is not really necessary here */
		if (sendto(sock, (char *)&resp, sizeof(struct tsp), 0, 
						&from, length) < 0) {
			syslog(LOG_ERR, "timed: sendto: %m");
			exit(1);
		}
		break;
	default:
		break;
	}
}

/*
 * `masterack' sends the necessary acknowledgments 
 * to the messages received by a master 
 */

masterack()
{
	int length;
	struct tsp resp;

	length = sizeof(struct sockaddr_in);

	resp = msgin;
	resp.tsp_vers = TSPVERSION;
	(void)strcpy(resp.tsp_name, hostname);
	bytenetorder(&resp); 	     /* this is not really necessary here */

	switch(msgin.tsp_type) {

	case TSP_QUIT:
	case TSP_TRACEON:
	case TSP_TRACEOFF:
	case TSP_MSITE:
	case TSP_MSITEREQ:
		resp.tsp_type = TSP_ACK;
		if (sendto(sock, (char *)&resp, sizeof(struct tsp), 0, 
						&from, length) < 0) {
			syslog(LOG_ERR, "timed: sendto: %m");
			exit(1);
		}
		break;
	case TSP_RESOLVE:
	case TSP_MASTERREQ:
		resp.tsp_type = TSP_MASTERACK;
		if (sendto(sock, (char *)&resp, sizeof(struct tsp), 0, 
						&from, length) < 0) {
			syslog(LOG_ERR, "timed: sendto: %m");
			exit(1);
		}
		break;
	case TSP_DATEREQ:
		resp.tsp_type = TSP_DATEACK;
		if (sendto(sock, (char *)&resp, sizeof(struct tsp), 0, 
						&from, length) < 0) {
			syslog(LOG_ERR, "timed: sendto: %m");
			exit(1);
		}
		break;
	default:
		break;
	}
}

/*
 * Print a TSP message 
 */
print(msg)
struct tsp *msg;
{
	extern char *tsptype[];

 	fprintf(fd, "%s %d %d (%d, %d) %s\n",
		tsptype[msg->tsp_type],
		msg->tsp_vers,
		msg->tsp_seq,
		msg->tsp_time.tv_sec, 
		msg->tsp_time.tv_usec, 
		msg->tsp_name);
	(void)fflush(fd);
}
