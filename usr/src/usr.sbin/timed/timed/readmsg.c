/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)readmsg.c	2.3 (Berkeley) %G%";
#endif not lint

#include "globals.h"
#include <protocols/timed.h>

extern char *tsptype[];

/*
 * LOOKAT checks if the message is of the requested type and comes from
 * the right machine, returning 1 in case of affirmative answer 
 */

#define LOOKAT(msg, mtype, mfrom, netp, froms) \
	(((((mtype) == TSP_ANY) || ((mtype) == (msg).tsp_type)) && \
	(((mfrom) == NULL) || (strcmp((mfrom), (msg).tsp_name) == 0)) && \
	(((netp) == NULL) || \
	(((netp)->mask & (froms).sin_addr.s_addr) == (netp)->net))) \
	? 1 : 0)

#define ISTOUTOFF(rtime, rtout) \
	(((rtime).tv_sec > (rtout).tv_sec || \
	    ((rtime).tv_sec == (rtout).tv_sec && \
		(rtime).tv_usec >= (rtout).tv_usec)) \
	? 1 : 0)

struct timeval rtime, rwait, rtout;
struct tsp msgin;
static struct tsplist {
	struct tsp info;
	struct sockaddr_in addr;
	struct tsplist *p;
} msgslist;
struct sockaddr_in from;
struct netinfo *fromnet;

/*
 * `readmsg' returns message `type' sent by `machfrom' if it finds it 
 * either in the receive queue, or in a linked list of previously received 
 * messages that it maintains.
 * Otherwise it waits to see if the appropriate message arrives within
 * `intvl' seconds. If not, it returns NULL.
 */

struct tsp *
readmsg(type, machfrom, intvl, netfrom)

int type;
char *machfrom;
struct timeval *intvl;
struct netinfo *netfrom;
{
	int length;
	fd_set ready;
	struct tsp *ret = NULL;
	static struct tsplist *head = &msgslist;
	static struct tsplist *tail = &msgslist;
	int inet_netof();
	int bytenetorder(), bytehostorder();
	struct tsplist *prev;
	register struct netinfo *ntp;
	register struct tsplist *ptr;

	if (trace) {
		fprintf(fd, "looking for %s from %s\n",
			tsptype[type], machfrom);
		ptr = head->p;
		fprintf(fd, "msgqueue:\n");
		while (ptr != NULL) {
			fprintf(fd, "\t");
			print(&ptr->info, &ptr->addr);
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
		if (LOOKAT(ptr->info, type, machfrom, netfrom, ptr->addr)) {
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

	FD_ZERO(&ready);
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
		}
		FD_SET(sock, &ready);
		if (select(FD_SETSIZE, &ready, (fd_set *)0, (fd_set *)0,
		    &rwait)) {
			length = sizeof(struct sockaddr_in);
			if (recvfrom(sock, (char *)&msgin, sizeof(struct tsp), 
						0, &from, &length) < 0) {
				syslog(LOG_ERR, "receiving datagram packet: %m");
				exit(1);
			}

			bytehostorder(&msgin);

			/*
			 * Throw away messages coming from this machine, unless
			 * they are of some particular type.
			 * This gets rid of broadcast messages and reduces
			 * master processing time.
			 */
			if ( !(strcmp(msgin.tsp_name, hostname) != 0 ||
					msgin.tsp_type == TSP_SETDATE ||
#ifdef TESTING
					msgin.tsp_type == TSP_TEST ||
#endif
					msgin.tsp_type == TSP_MSITE ||
					msgin.tsp_type == TSP_TRACEON ||
					msgin.tsp_type == TSP_TRACEOFF)) {
				if (trace) {
					fprintf(fd, "readmsg: discarded: ");
					print(&msgin, &from);
				}
				(void)gettimeofday(&rtime,(struct timezone *)0);
				if (ISTOUTOFF(rtime, rtout))
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
			for (ntp = nettab; ntp != NULL; ntp = ntp->next) {
				if ((ntp->mask & from.sin_addr.s_addr) ==
				    ntp->net) {
					if (ntp->status == MASTER)
						masterack();
					else
						slaveack();
					break;
				}
			}
			if (LOOKAT(msgin, type, machfrom, netfrom, from)) {
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
			if (ISTOUTOFF(rtime, rtout))
				break;
		} else {
			break;
		}
	}
out:
	if (ret != NULL) {
		if (trace) {
			fprintf(fd, "readmsg: ");
			print(ret, &from);
		}
		fromnet = NULL;
		for (ntp = nettab; ntp != NULL; ntp = ntp->next) {
			if ((ntp->mask & from.sin_addr.s_addr) ==
			    ntp->net) {
				fromnet = ntp;
				break;
			}
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
	case TSP_QUIT:
		resp = msgin;
		resp.tsp_type = TSP_ACK;
		resp.tsp_vers = TSPVERSION;
		(void)strcpy(resp.tsp_name, hostname);
		if (trace) {
			fprintf(fd, "Slaveack: ");
			print(&resp, &from);
		}
		bytenetorder(&resp);     /* this is not really necessary here */
		if (sendto(sock, (char *)&resp, sizeof(struct tsp), 0, 
						&from, length) < 0) {
			syslog(LOG_ERR, "sendto: %m");
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

	switch(msgin.tsp_type) {

	case TSP_QUIT:
	case TSP_TRACEON:
	case TSP_TRACEOFF:
	case TSP_MSITE:
	case TSP_MSITEREQ:
		resp.tsp_type = TSP_ACK;
		bytenetorder(&resp);
		if (trace) {
			fprintf(fd, "Masterack: ");
			print(&resp, &from);
		}
		if (sendto(sock, (char *)&resp, sizeof(struct tsp), 0, 
						&from, length) < 0) {
			syslog(LOG_ERR, "sendto: %m");
			exit(1);
		}
		break;
	case TSP_RESOLVE:
	case TSP_MASTERREQ:
		resp.tsp_type = TSP_MASTERACK;
		bytenetorder(&resp);
		if (trace) {
			fprintf(fd, "Masterack: ");
			print(&resp, &from);
		}
		if (sendto(sock, (char *)&resp, sizeof(struct tsp), 0, 
						&from, length) < 0) {
			syslog(LOG_ERR, "sendto: %m");
			exit(1);
		}
		break;
	case TSP_SETDATEREQ:
		resp.tsp_type = TSP_DATEACK;
		bytenetorder(&resp);
		if (trace) {
			fprintf(fd, "Masterack: ");
			print(&resp, &from);
		}
		if (sendto(sock, (char *)&resp, sizeof(struct tsp), 0, 
						&from, length) < 0) {
			syslog(LOG_ERR, "sendto: %m");
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
print(msg, addr)
struct tsp *msg;
struct sockaddr_in *addr;
{
 	fprintf(fd, "%s %d %d (%d, %d) %s %s\n",
		tsptype[msg->tsp_type],
		msg->tsp_vers,
		msg->tsp_seq,
		msg->tsp_time.tv_sec, 
		msg->tsp_time.tv_usec, 
		msg->tsp_name,
		inet_ntoa(addr->sin_addr));
}
