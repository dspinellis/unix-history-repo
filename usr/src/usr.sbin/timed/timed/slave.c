/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)slave.c	2.1 (Berkeley) %G%";
#endif not lint

#include "globals.h"
#include <protocols/timed.h>
#include <setjmp.h>

extern jmp_buf jmpenv;

slave()
{
	int length;
	int senddateack;
	long electiontime, refusetime;
	u_short seq;
	char candidate[MAXHOSTNAMELEN];
	struct tsp *msg, *readmsg();
	struct sockaddr_in saveaddr, msaveaddr;
	struct timeval wait;
	struct timeval time;
	struct tsp *answer, *acksend();
	int timeout();
	char *date();
	long casual();
	int bytenetorder();
	char olddate[32];
	struct sockaddr_in server;
	register struct netinfo *ntp;
#ifdef MEASURE
	extern FILE *fp;
#endif

	if (status & MASTER) {
#ifdef MEASURE
		fp = fopen("/usr/adm/timed.masterlog", "w");
		setlinebuf(fp);
#endif
		syslog(LOG_NOTICE, "THIS MACHINE IS A SUBMASTER");
		if (trace) {
			fprintf(fd, "THIS MACHINE IS A SUBMASTER\n");
		}
		for (ntp = nettab; ntp != NULL; ntp = ntp->next)
			if (ntp->status == MASTER)
				masterup(ntp);
	} else {
		syslog(LOG_NOTICE, "THIS MACHINE IS A SLAVE");
		if (trace) {
			fprintf(fd, "THIS MACHINE IS A SLAVE\n");
		}
	}

	seq = 0;
	senddateack = OFF;
	refusetime = 0;

	(void)gettimeofday(&time, (struct timezone *)0);
	electiontime = time.tv_sec + delay2;

loop:
	length = sizeof(struct sockaddr_in);
	(void)gettimeofday(&time, (struct timezone *)0);
	if (time.tv_sec > electiontime) {
		if (trace) 
			fprintf(fd, "election timer expired\n");
		longjmp(jmpenv, 1);
	}
	wait.tv_sec = electiontime - time.tv_sec + 10;
	wait.tv_usec = 0;
	msg = readmsg(TSP_ANY, (char *)ANYADDR, &wait, (struct netinfo *)NULL);
	if (msg != NULL) {
		switch (msg->tsp_type) {

		case TSP_ADJTIME:
			(void)gettimeofday(&time, (struct timezone *)0);
			electiontime = time.tv_sec + delay2;
			if (seq != msg->tsp_seq) {
				seq = msg->tsp_seq;
				if ((status & SUBMASTER) == SUBMASTER) {
					synch((msg->tsp_time.tv_sec * 1000) + 
					    (msg->tsp_time.tv_usec / 1000));
				} else {
					adjclock(&(msg->tsp_time));
				}
			}
			break;
		case TSP_SETTIME:
			if (seq == msg->tsp_seq)
				break;

			seq = msg->tsp_seq;

			(void)strcpy(olddate, date());
			(void)settimeofday(&msg->tsp_time,
				(struct timezone *)0);
			syslog(LOG_NOTICE, "date changed by %s from: %s",
				msg->tsp_name, olddate);
			if ((status & SUBMASTER) == SUBMASTER)
				spreadtime();
			electiontime = time.tv_sec + delay2;

			if (senddateack == ON) {
				senddateack = OFF;
				msg->tsp_type = TSP_DATEACK;
				(void)strcpy(msg->tsp_name, hostname);
				bytenetorder(msg);
				length = sizeof(struct sockaddr_in);
				if (sendto(sock, (char *)msg, 
						sizeof(struct tsp), 0,
						&saveaddr, length) < 0) {
					syslog(LOG_ERR, "sendto: %m");
					exit(1);
				}
			}
			break;
		case TSP_MASTERUP:
			msg->tsp_type = TSP_SLAVEUP;
			msg->tsp_vers = TSPVERSION;
			(void)strcpy(msg->tsp_name, hostname);
			bytenetorder(msg);
			answerdelay();
			length = sizeof(struct sockaddr_in);
			if (sendto(sock, (char *)msg, sizeof(struct tsp), 0, 
						&from, length) < 0) {
				syslog(LOG_ERR, "sendto: %m");
				exit(1);
			}
			backoff = 1;
			delay2 = casual((long)MINTOUT, (long)MAXTOUT);
			(void)gettimeofday(&time, (struct timezone *)0);
			electiontime = time.tv_sec + delay2;
			refusetime = 0;
			break;
		case TSP_MASTERREQ:
			(void)gettimeofday(&time, (struct timezone *)0);
			electiontime = time.tv_sec + delay2;
			break;
		case TSP_DATE:
			saveaddr = from;
			msg->tsp_time.tv_usec = 0;
			msg->tsp_type = TSP_DATEREQ;
			msg->tsp_vers = TSPVERSION;
			(void)strcpy(msg->tsp_name, hostname);
			for (ntp = nettab; ntp != NULL; ntp = ntp->next) {
				if (ntp->status == SLAVE)
					break;
			}
			answer = acksend(msg, &ntp->dest_addr, (char *)ANYADDR,
			    TSP_DATEACK, ntp);
			if (answer != NULL) {
				msg->tsp_type = TSP_ACK;
				bytenetorder(msg);
				length = sizeof(struct sockaddr_in);
				if (sendto(sock, (char *)msg,
				    sizeof(struct tsp), 0, &saveaddr,
				    length) < 0) {
					syslog(LOG_ERR, "sendto: %m");
					exit(1);
				}
				senddateack = ON;
			}
			break;
		case TSP_DATEREQ:
			if (status != SUBMASTER)
				break;
			for (ntp = nettab; ntp != NULL; ntp = ntp->next) {
				if (ntp->status == SLAVE)
					break;
			}
			answer = acksend(msg, &ntp->dest_addr, (char *)ANYADDR,
			    TSP_DATEACK, ntp);
			if (answer != NULL) {
				msg->tsp_type = TSP_DATEACK;
				bytenetorder(msg);
				length = sizeof(struct sockaddr_in);
				if (sendto(sock, (char *)msg,
				    sizeof(struct tsp), 0, &saveaddr,
				    length) < 0) {
					syslog(LOG_ERR, "sendto: %m");
					exit(1);
				}
			}
			break;
		case TSP_TRACEON:
			if (!(trace)) {
				fd = fopen(tracefile, "w");
				setlinebuf(fd);
				fprintf(fd, "Tracing started on: %s\n\n", 
								date());
			}
			trace = ON;
			break;
		case TSP_TRACEOFF:
			if (trace) {
				fprintf(fd, "Tracing ended on: %s\n", date());
				(void)fclose(fd);
			}
#ifdef GPROF
			moncontrol(0);
			_mcleanup();
			moncontrol(1);
#endif
			trace = OFF;
			break;
		case TSP_SLAVEUP:
			if (!(status & MASTER))
				break;
			for (ntp = nettab; ntp != NULL; ntp = ntp->next) {
				if ((ntp->mask & from.sin_addr.s_addr) ==
				    ntp->net) {
					if (ntp->status == MASTER)
						addmach(msg->tsp_name, &from);
					break;
				}
			}
			break;
		case TSP_ELECTION:
			(void)gettimeofday(&time, (struct timezone *)0);
			electiontime = time.tv_sec + delay2;
			seq = 0;            /* reset sequence number */
			if (time.tv_sec < refusetime)
				msg->tsp_type = TSP_REFUSE;
			else {
				msg->tsp_type = TSP_ACCEPT;
				refusetime = time.tv_sec + 30;
			}
			(void)strcpy(candidate, msg->tsp_name);
			(void)strcpy(msg->tsp_name, hostname);
			answerdelay();
			server = from;
			answer = acksend(msg, &server, candidate, TSP_ACK,
			    (struct netinfo *)NULL);
			if (answer == NULL) {
				syslog(LOG_ERR, "problem in election\n");
			}
			break;
		case TSP_MSITE:
			msaveaddr = from;
			msg->tsp_type = TSP_MSITEREQ;
			msg->tsp_vers = TSPVERSION;
			(void)strcpy(msg->tsp_name, hostname);
			for (ntp = nettab; ntp != NULL; ntp = ntp->next) {
				if (ntp->status == SLAVE)
					break;
			}
			answer = acksend(msg, &ntp->dest_addr, (char *)ANYADDR,
			    TSP_ACK, ntp);
			if (answer != NULL) {
				msg->tsp_type = TSP_ACK;
				length = sizeof(struct sockaddr_in);
				bytenetorder(msg);
				if (sendto(sock, (char *)msg, 
						sizeof(struct tsp), 0,
						&msaveaddr, length) < 0) {
					syslog(LOG_ERR, "sendto: %m");
					exit(1);
				}
			}
			break;
		case TSP_ACCEPT:
		case TSP_REFUSE:
			break;
#ifdef TESTING
		case TSP_TEST:
			electiontime = 0;
			break;
#endif
		case TSP_MSITEREQ:
			if (status & MASTER)
				break;
			if (trace) {
				fprintf(fd, "garbage: ");
				print(msg);
			}
			break;
		default:
			if (trace) {
				fprintf(fd, "garbage: ");
				print(msg);
			}
			break;
		}
	}
	goto loop;
}

/*
 * Used before answering a broadcast message to avoid network
 * contention and likely collisions.
 */
answerdelay()
{
	struct timeval timeout;

	timeout.tv_sec = 0;
	timeout.tv_usec = delay1;

	(void)select(0, (fd_set *)NULL, (fd_set *)NULL, (fd_set *)NULL,
	    &timeout);
	return;
}
