/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)master.c	1.1 (Berkeley) %G%";
#endif not lint

#include "globals.h"
#include <protocols/timed.h>
#include <setjmp.h>

#define OFF	0
#define ON	1

extern struct sockaddr_in from;
extern struct sockaddr_in server;

extern int trace;
extern int machup;
extern int slvcount;
extern int measure_delta;
extern int sock;
extern char hostname[];
extern struct host hp[];
extern char *fj;
extern FILE *fd;

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
	int length;
	long pollingtime;
	struct timeval wait;
	struct timeval time;
	struct timezone tzone;
	struct timeval mytime;
	struct tsp *msg, to;
	struct sockaddr_in saveaddr;
	extern jmp_buf jmpenv;
	int findhost();
	char *date();
	char *strcpy();
	struct tsp *readmsg();
	struct tsp *answer, *acksend();

#ifdef MEASURE
	fi = "/usr/adm/timed.masterlog";
	fp = fopen(fi, "w");
#endif

	syslog(LOG_ERR, "timed: THIS MACHINE IS MASTER\n");
	if (trace)
		fprintf(fd, "THIS MACHINE IS MASTER\n");

	masterup();
	pollingtime = 0;

loop:
	(void)gettimeofday(&time, (struct timezone *)0);
	if (time.tv_sec >= pollingtime) {
		pollingtime = time.tv_sec + SAMPLEINTVL;
		synch();
	}

	wait.tv_sec = pollingtime - time.tv_sec;
	wait.tv_usec = 0;
	msg = readmsg(TSP_ANY, (char *)ANYADDR, &wait);
	if (msg != NULL) {
		switch (msg->tsp_type) {

		case TSP_MASTERREQ:
			ind = addmach(msg->tsp_name);
			if (trace)
				prthp();
			if (hp[ind].seq !=  msg->tsp_seq) {
				hp[ind].seq = msg->tsp_seq;
				bcopy((char *)&hp[ind].addr, 
					    (char *)&(server.sin_addr.s_addr),
					    hp[ind].length);
				to.tsp_type = TSP_SETTIME;
				(void)strcpy(to.tsp_name, hostname);
				/*
				 * give the upcoming slave the time
				 * to check its input queue before
				 * setting the time
				 */
				sleep(1);
				to.tsp_time.tv_usec = 0;
				(void)gettimeofday(&mytime, (struct timezone *)0);
				to.tsp_time.tv_sec = mytime.tv_sec;
				answer = acksend(&to, hp[ind].name, TSP_ACK);
				if (answer == NULL) {
					syslog(LOG_ERR, "timed: ERROR ON SETTIME machine: %s\n", hp[ind].name);
					slvcount--;
				}
				pollingtime = 0;
			}
			break;
		case TSP_SLAVEUP:
			(void) addmach(msg->tsp_name);
			pollingtime = 0;
			break;
		case TSP_DATE:
			saveaddr = from;
			msg->tsp_time.tv_usec = 0;
			(void)gettimeofday(&time, &tzone);
			time.tv_sec += msg->tsp_time.tv_sec;
			time.tv_sec++;
			(void)settimeofday(&time, &tzone);
			syslog(LOG_ERR, "timed: date changed to: %s\n", date());
			msg->tsp_type = TSP_DATEACK;
			msg->tsp_vers = TSPVERSION;
			(void)strcpy(msg->tsp_name, hostname);
			bytenetorder(msg);
			length = sizeof(struct sockaddr_in);
			if (sendto(sock, (char *)msg, sizeof(struct tsp), 0,
						&saveaddr, length) < 0) {
				syslog(LOG_ERR, "timed: sendto: %m");
				exit(1);
			}
			spreadtime();
			pollingtime = 0;
			break;
		case TSP_DATEREQ:
			ind = findhost(msg->tsp_name);
			if (ind < 0) { 
				syslog(LOG_ERR, "timed: error on DATEREQ\n");
				break;
			}
			if (hp[ind].seq !=  msg->tsp_seq) {
				hp[ind].seq = msg->tsp_seq;
				msg->tsp_time.tv_usec = 0;
				(void)gettimeofday(&time, &tzone);
				time.tv_sec += msg->tsp_time.tv_sec;
				time.tv_sec++;
				(void)settimeofday(&time, &tzone);
				syslog(LOG_ERR, "timed: date changed to: %s\n", date());
				spreadtime();
				pollingtime = 0;
			}
			break;
		case TSP_MSITE:
			msg->tsp_type = TSP_ACK;
			msg->tsp_vers = TSPVERSION;
			(void)strcpy(msg->tsp_name, hostname);
			bytenetorder(msg);
			length = sizeof(struct sockaddr_in);
			if (sendto(sock, (char *)msg, sizeof(struct tsp), 0,
						&from, length) < 0) {
				syslog(LOG_ERR, "timed: sendto: %m");
				exit(1);
			}
			break;
		case TSP_MSITEREQ:
			break;
		case TSP_TRACEON:
			if (!(trace)) {
				fd = fopen(fj, "w");
				fprintf(fd, "Tracing started on: %s\n\n", 
							date());
				(void)fflush(fd);
			}
			trace = ON;
			break;
		case TSP_TRACEOFF:
			if (trace) {
				fprintf(fd, "Tracing ended on: %s\n", date());
				(void)fflush(fd);
				(void)close((int)fd);
			}
			trace = OFF;
			break;
		case TSP_ELECTION:
			to.tsp_type = TSP_QUIT;
			(void)strcpy(to.tsp_name, hostname);
			server = from;
			answer = acksend(&to, msg->tsp_name, TSP_ACK);
			if (answer == NULL) {
				syslog(LOG_ERR, "timed: election error\n");
			} else {
				(void) addmach(msg->tsp_name);
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

			for(;;) {
				to.tsp_type = TSP_RESOLVE;
				answer = acksend(&to, (char *)ANYADDR, 
								TSP_MASTERACK);
				if (answer == NULL)
					break;
				(void) addmach(answer->tsp_name);
				to.tsp_type = TSP_QUIT;
				server = from;
				msg = acksend(&to, answer->tsp_name, 
								TSP_MASTERACK);
				if (msg == NULL) {
					syslog(LOG_ERR, "timed: error on sending QUIT\n");
				}
			}
			masterup();
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
			(void)close((int)fp);
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

synch()
{
	int i;
	int measure_status;
	long netdelta;
	struct timeval tack;
#ifdef MEASURE
#define MAXLINES	8
	static int lines;
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
			bcopy((char *)&hp[i].addr, 
					(char *)&(server.sin_addr.s_addr), 
					hp[i].length); 
			tack.tv_sec = 0;
			tack.tv_usec = 100000;
			if ((measure_status = measure(&tack, ON)) < 0) {
				syslog(LOG_ERR, "timed: measure: %m\n");
				exit(1);
			}
			hp[i].delta = measure_delta;
			if (measure_status == GOOD)
				machup++;
		}
		if (machup > 1) {
			netdelta = networkdelta();
			correct(netdelta);
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
		(void)fflush(fp);
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
	}
}

/*
 * 'spreadtime' sends the time to each slave after the master
 * has received the command to set the network time 
 */

spreadtime()
{
	int i;
	struct timeval mytime;
	struct tsp to;
	struct tsp *answer, *acksend();

	for(i=1; i<slvcount; i++) {
		bcopy((char *)&hp[i].addr, (char *)&(server.sin_addr.s_addr), 
						hp[i].length); 
		to.tsp_type = TSP_SETTIME;
		to.tsp_time.tv_usec = 0;
		(void)strcpy(to.tsp_name, hostname);
		(void)gettimeofday(&mytime, (struct timezone *)0);
		to.tsp_time.tv_sec = mytime.tv_sec;
		answer = acksend(&to, hp[i].name, TSP_ACK);
		if (answer == NULL) {
			syslog(LOG_ERR, "timed: ERROR ON SETTIME machine: %s\n", hp[i].name);
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

addmach(name)
char *name;
{
	int ret;
	int findhost();
	char *malloc();
	struct hostent *hptmp, *gethostbyname();

	ret = findhost(name);
	if (ret < 0) {
		hptmp = gethostbyname(name);
		if (hptmp == NULL) {
			syslog(LOG_ERR, "timed: gethostbyname: %m\n");
			exit(1);
		}
		hp[slvcount].length = hptmp->h_length;
		bcopy((char *)hptmp->h_addr, (char *)&hp[slvcount].addr, 
						hptmp->h_length); 
		hp[slvcount].name = (char *)malloc(32);
		(void)strcpy(hp[slvcount].name, hptmp->h_name);
		hp[slvcount].seq = 0;
		ret = slvcount;
		if (slvcount < NHOSTS)
			slvcount++;
		else {
			syslog(LOG_EMERG, "timed: no more slots in host table\n");
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

masterup()
{
	struct timeval wait;
	char *strcpy();
	struct tsp to, *msg, *readmsg();

	to.tsp_type = TSP_MASTERUP;
	(void)strcpy(to.tsp_name, hostname);
	broadcast(&to);

	for (;;) {
		wait.tv_sec = 1;
		wait.tv_usec = 0;
		msg = readmsg(TSP_SLAVEUP, (char *)ANYADDR, &wait);
		if (msg != NULL) {
			(void) addmach(msg->tsp_name);
		} else
			break;
	}
}
