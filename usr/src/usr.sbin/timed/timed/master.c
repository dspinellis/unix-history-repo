/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)master.c	2.1 (Berkeley) %G%";
#endif not lint

#include "globals.h"
#include <protocols/timed.h>
#include <setjmp.h>

extern int machup;
extern int measure_delta;
extern jmp_buf jmpenv;

#ifdef MEASURE
int header;
char *fi;
FILE *fp;
#endif

/*
 * The main function of `master' is to periodically compute the differences 
 * (deltas) between its clock and the clocks of the slaves, to compute the 
 * network average delta, and to send to the slaves the differences between 
 * their individual deltas and the network delta.
 * While waiting, it receives messages from the slaves (i.e. requests for
 * master's name, remote requests to set the network time, ...), and
 * takes the appropriate action.
 */

master()
{
	int ind;
	long pollingtime;
	struct timeval wait;
	struct timeval time;
	struct timezone tzone;
	struct timeval mytime;
	struct tsp *msg, to;
	struct sockaddr_in saveaddr;
	int findhost();
	char *date();
	struct tsp *readmsg();
	struct tsp *answer, *acksend();
	char olddate[32];
	struct sockaddr_in server;
	register struct netinfo *ntp;

#ifdef MEASURE
	fi = "/usr/adm/timed.masterlog";
	fp = fopen(fi, "w");
	setlinebuf(fp);
#endif

	syslog(LOG_INFO, "THIS MACHINE IS MASTER");
	if (trace)
		fprintf(fd, "THIS MACHINE IS MASTER\n");

	for (ntp = nettab; ntp != NULL; ntp = ntp->next)
		if (ntp->status == MASTER)
			masterup(ntp);
	pollingtime = 0;

loop:
	(void)gettimeofday(&time, (struct timezone *)0);
	if (time.tv_sec >= pollingtime) {
		pollingtime = time.tv_sec + SAMPLEINTVL;
		synch(0L);
	}

	wait.tv_sec = pollingtime - time.tv_sec;
	wait.tv_usec = 0;
	msg = readmsg(TSP_ANY, (char *)ANYADDR, &wait, (struct netinfo *)NULL);
	if (msg != NULL) {
		switch (msg->tsp_type) {

		case TSP_MASTERREQ:
			ind = addmach(msg->tsp_name, &from);
			if (trace)
				prthp();
			if (hp[ind].seq !=  msg->tsp_seq) {
				hp[ind].seq = msg->tsp_seq;
				to.tsp_type = TSP_SETTIME;
				(void)strcpy(to.tsp_name, hostname);
				/*
				 * give the upcoming slave the time
				 * to check its input queue before
				 * setting the time
				 */
				sleep(1);
				to.tsp_time.tv_usec = 0;
				(void) gettimeofday(&mytime,
				    (struct timezone *)0);
				to.tsp_time.tv_sec = mytime.tv_sec;
				answer = acksend(&to, &hp[ind].addr,
				    hp[ind].name, TSP_ACK,
				    (struct netinfo *)NULL);
				if (answer == NULL) {
					syslog(LOG_ERR,
					    "ERROR ON SETTIME machine: %s",
					    hp[ind].name);
					slvcount--;
				}
			}
			break;
		case TSP_SLAVEUP:
			(void) addmach(msg->tsp_name, &from);
			break;
		case TSP_DATE:
			saveaddr = from;
			msg->tsp_time.tv_usec = 0;
			/*
			 * the following line is necessary due to syslog
			 * calling ctime() which clobbers the static buffer
			 */
			(void)strcpy(olddate, date());
			(void)gettimeofday(&time, &tzone);
			time.tv_sec += msg->tsp_time.tv_sec;
			time.tv_sec++;
			(void)settimeofday(&time, &tzone);
			syslog(LOG_NOTICE, "date changed from: %s", olddate);
			msg->tsp_type = TSP_DATEACK;
			msg->tsp_vers = TSPVERSION;
			(void)strcpy(msg->tsp_name, hostname);
			bytenetorder(msg);
			if (sendto(sock, (char *)msg, sizeof(struct tsp), 0,
			    &saveaddr, sizeof(struct sockaddr_in)) < 0) {
				syslog(LOG_ERR, "sendto: %m");
				exit(1);
			}
			spreadtime();
			pollingtime = 0;
			break;
		case TSP_DATEREQ:
			ind = findhost(msg->tsp_name);
			if (ind < 0) { 
			    syslog(LOG_ERR,
				"DATEREQ from uncontrolled machine");
			    break;
			}
			if (hp[ind].seq !=  msg->tsp_seq) {
				hp[ind].seq = msg->tsp_seq;
				msg->tsp_time.tv_usec = 0;
				/*
				 * the following line is necessary due to syslog
				 * calling ctime() which clobbers the static buffer
				 */
				(void)strcpy(olddate, date());
				(void)gettimeofday(&time, &tzone);
				time.tv_sec += msg->tsp_time.tv_sec;
				time.tv_sec++;
				(void)settimeofday(&time, &tzone);
				syslog(LOG_NOTICE,
				    "date changed by %s from: %s",
				    msg->tsp_name, olddate);
				spreadtime();
				pollingtime = 0;
			}
			break;
		case TSP_MSITE:
		case TSP_MSITEREQ:
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
		case TSP_ELECTION:
			to.tsp_type = TSP_QUIT;
			(void)strcpy(to.tsp_name, hostname);
			server = from;
			answer = acksend(&to, &server, msg->tsp_name, TSP_ACK,
			    (struct netinfo *)NULL);
			if (answer == NULL) {
				syslog(LOG_ERR, "election error");
			} else {
				(void) addmach(msg->tsp_name, &from);
			}
			pollingtime = 0;
			break;
		case TSP_CONFLICT:
			/*
			 * After a network partition, there can be 
			 * more than one master: the first slave to 
			 * come up will notify here the situation.
			 */

			(void)strcpy(to.tsp_name, hostname);

			for (ntp = nettab; ntp != NULL; ntp = ntp->next) {
				if ((ntp->mask & from.sin_addr.s_addr) ==
				    ntp->net)
					break;
			}
			if (ntp == NULL)
				break;
			for(;;) {
				to.tsp_type = TSP_RESOLVE;
				answer = acksend(&to, &ntp->dest_addr,
				    (char *)ANYADDR, TSP_MASTERACK, ntp);
				if (answer == NULL)
					break;
				to.tsp_type = TSP_QUIT;
				server = from;
				msg = acksend(&to, &server, answer->tsp_name, 
				    TSP_MASTERACK, (struct netinfo *)NULL);
				if (msg == NULL) {
					syslog(LOG_ERR, "error on sending QUIT");
				} else {
					(void) addmach(answer->tsp_name, &from);
				}
			}
			masterup(ntp);
			pollingtime = 0;
			break;
		case TSP_RESOLVE:
			/*
			 * do not want to call synch() while waiting
			 * to be killed!
			 */
			(void)gettimeofday(&time, (struct timezone *)0);
			pollingtime = time.tv_sec + SAMPLEINTVL;
			break;
		case TSP_QUIT:
			/* become slave */
#ifdef MEASURE
			(void)fclose(fp);
#endif
			longjmp(jmpenv, 2);
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
 * `synch' synchronizes all the slaves by calling measure, 
 * networkdelta and correct 
 */

synch(mydelta)
long mydelta;
{
	int i;
	int measure_status;
	long netdelta;
	struct timeval tack;
#ifdef MEASURE
#define MAXLINES	8
	static int lines = 1;
	struct timeval start, end;
#endif
	int measure();
	int correct();
	long networkdelta();
	char *date();

	if (slvcount > 1) {
#ifdef MEASURE
		(void)gettimeofday(&start, (struct timezone *)0);
		if (header == ON || --lines == 0) {
			fprintf(fp, "%s\n", date());
			for (i=0; i<slvcount; i++)
				fprintf(fp, "%.7s\t", hp[i].name);
			fprintf(fp, "\n");
			lines = MAXLINES;
			header = OFF;
		}
#endif
		machup = 1;
		hp[0].delta = 0;
		for(i=1; i<slvcount; i++) {
			tack.tv_sec = 0;
			tack.tv_usec = 100000;
			if ((measure_status = measure(&tack, &hp[i].addr,
			    ON)) < 0) {
				syslog(LOG_ERR, "measure: %m");
				exit(1);
			}
			hp[i].delta = measure_delta;
			if (measure_status == GOOD)
				machup++;
		}
		if (status & SLAVE) {
			/* called by a submaster */
			if (trace)
				fprintf(fd, "submaster correct: %d ms.\n",
				    mydelta);
			correct(mydelta);	
		} else {
			if (machup > 1) {
				netdelta = networkdelta();
				if (trace)
					fprintf(fd,
					    "master correct: %d ms.\n",
					    mydelta);
				correct(netdelta);
			}
		}
#ifdef MEASURE
		gettimeofday(&end, 0);
		end.tv_sec -= start.tv_sec;
		end.tv_usec -= start.tv_usec;
		if (end.tv_usec < 0) {
			end.tv_sec -= 1;
			end.tv_usec += 1000000;
		}
		fprintf(fp, "%d ms.\n", (end.tv_sec*1000+end.tv_usec/1000));
#endif
		for(i=1; i<slvcount; i++) {
			if (hp[i].delta == HOSTDOWN) {
				free((char *)hp[i].name);
				hp[i] = hp[--slvcount];
#ifdef MEASURE
				header = ON;
#endif
			}
		}
	} else {
		if (status & SLAVE) {
			correct(mydelta);
		}
	}
}

/*
 * 'spreadtime' sends the time to each slave after the master
 * has received the command to set the network time 
 */

spreadtime()
{
	int i;
	struct tsp to;
	struct tsp *answer, *acksend();

	for(i=1; i<slvcount; i++) {
		to.tsp_type = TSP_SETTIME;
		(void)strcpy(to.tsp_name, hostname);
		(void)gettimeofday(&to.tsp_time, (struct timezone *)0);
		answer = acksend(&to, &hp[i].addr, hp[i].name, TSP_ACK,
		    (struct netinfo *)NULL);
		if (answer == NULL) {
			syslog(LOG_ERR, "ERROR ON SETTIME machine: %s", hp[i].name);
		}
	}
}

findhost(name)
char *name;
{
	int i;
	int ind;

	ind = -1;
	for (i=1; i<slvcount; i++) {
		if (strcmp(name, hp[i].name) == 0) {
			ind = i;
			break;
		}
	}
	return(ind);
}

/*
 * 'addmach' adds a host to the list of controlled machines
 * if not already there 
 */

addmach(name, addr)
char *name;
struct sockaddr_in *addr;
{
	int ret;
	int findhost();

	ret = findhost(name);
	if (ret < 0) {
		hp[slvcount].addr = *addr;
		hp[slvcount].name = (char *)malloc(MAXHOSTNAMELEN);
		(void)strcpy(hp[slvcount].name, name);
		hp[slvcount].seq = 0;
		ret = slvcount;
		if (slvcount < NHOSTS)
			slvcount++;
		else {
			syslog(LOG_ALERT, "no more slots in host table");
		}
	} else {
		/* need to clear sequence number anyhow */
		hp[ret].seq = 0;
	}
#ifdef MEASURE
	header = ON;
#endif
	return(ret);
}

prthp()
{
	int i;

	fprintf(fd, "host table:");
	for (i=1; i<slvcount; i++)
		fprintf(fd, " %s", hp[i].name);
	fprintf(fd, "\n");
}

masterup(net)
struct netinfo *net;
{
	struct timeval wait;
	struct tsp to, *msg, *readmsg();

	to.tsp_type = TSP_MASTERUP;
	to.tsp_vers = TSPVERSION;
	(void)strcpy(to.tsp_name, hostname);
	bytenetorder(&to);
	if (sendto(sock, (char *)&to, sizeof(struct tsp), 0, &net->dest_addr,
	    sizeof(struct sockaddr_in)) < 0) {
		syslog(LOG_ERR, "sendto: %m");
		exit(1);
	}

	for (;;) {
		wait.tv_sec = 1;
		wait.tv_usec = 0;
		msg = readmsg(TSP_SLAVEUP, (char *)ANYADDR, &wait, net);
		if (msg != NULL) {
			(void) addmach(msg->tsp_name, &from);
		} else
			break;
	}
}
