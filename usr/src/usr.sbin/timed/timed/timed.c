/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)timed.c	1.1 (Berkeley) %G%";
#endif not lint

#include "globals.h"
#define TSPTYPES
#include <protocols/timed.h>
#include <net/if.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <setjmp.h>

#define OFF		0
#define ON		1
#define SLAVE		0
#define MASTER		1
#define MAXRANDOM	2147483648	/* 2**31, max random number */

int id;
int trace;
int sock, sock_raw;
int status;     			/* either MASTER or SLAVE */
int backoff;
int slvcount;				/* no. of slaves controlled by master */
int machup;
u_short sequence;			/* sequence number */
long delay1;
long delay2;
char hostname[32];
struct in_addr broadcastaddr;		/* local net broadcast address */
struct sockaddr_in server;
struct host hp[NHOSTS];
char *fj;
FILE *fd;
jmp_buf jmpenv;

extern struct sockaddr_in from;

/*
 * The timedaemons synchronize the clocks of hosts in a local area network.
 * One daemon runs as master, all the others as slaves. The master
 * performs the task of computing clock differences and sends correction
 * values to the slaves. 
 * Slaves start an election to choose a new master when the latter disappears 
 * because of a machine crash, network partition, or when killed.
 * A resolution protocol is used to kill all but one of the masters
 * that happen to exist in segments of a partitioned network when the 
 * network partition is fixed.
 *
 * Authors: Riccardo Gusella & Stefano Zatti
 *
 * For problems and suggestions, please send mail to gusella@BERKELEY
 */

main(argc, argv)
int argc;
char **argv;
{
	int on;
	int ret;
	long seed;
	int Mflag;
	int nflag;
	char mastername[32];
	char *netname;
	struct timeval time;
	struct servent *srvp;
	struct netent *getnetent();
	struct netent *localnet;
	struct sockaddr_in masteraddr;
	struct tsp resp, conflict, *answer, *readmsg(), *acksend();
	long casual();
	char *malloc(), *strcpy();
	char *date();
	struct in_addr inet_makeaddr();

	Mflag = SLAVE;
	on = 1;
	backoff = 1;
	fj = "/usr/adm/timed.log";
	trace = OFF;
	nflag = OFF;

	if (getuid() != 0) {
		printf("Sorry: need to be root\n");
		exit(1);
	}

	while (--argc > 0 && **++argv == '-') {
		(*argv)++;
		do {
			switch (**argv) {

			case 'M':
				Mflag = MASTER; 
				break;
			case 't':
				trace = ON; 
				fd = fopen(fj, "w");
				fprintf(fd, "Tracing started on: %s\n\n", 
							date());
				(void)fflush(fd);
				break;
			case 'n':
				argc--, argv++;
				nflag = ON;
				netname = *argv;
				while (*(++(*argv)+1)) ;
				break;
			default:
				fprintf(stderr, "timed: -%c: unknown option\n", 
							**argv);
				break;
			}
		} while (*++(*argv));
	}

	srvp = getservbyname("timed", "udp");
	if (srvp == 0) {
		syslog(LOG_ERR, "udp/timed: unknown service\n");
		exit(1);
	}
	server.sin_port = srvp->s_port;
	server.sin_family = AF_INET;
	sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (sock < 0) {
		syslog(LOG_ERR, "timed: opening socket\n");
		exit(1);
	}
	if (setsockopt(sock, SOL_SOCKET, SO_BROADCAST, (char *)&on, 
							sizeof(on)) < 0) {
		syslog(LOG_ERR, "timed: setsockopt");
		exit(1);
	}
	if (bind(sock, &server, sizeof(server))) {
		if (errno == EADDRINUSE)
			fprintf(stderr, "timed already running\n");
		else
			syslog(LOG_ERR, "timed: bind");
		exit(1);
	}

	/* choose a unique seed for random number generation */
	(void)gettimeofday(&time, (struct timezone *)0);
	seed = time.tv_sec + time.tv_usec;
	srandom(seed);

	sequence = casual((long)1, (long)MAXSEQ);     /* initial seq number */

	/* rounds kernel variable time to multiple of 5 ms. */
	time.tv_sec = 0;
	time.tv_usec = -((time.tv_usec/1000) % 5) * 1000;
	(void)adjtime(&time, (struct timeval *)0);

	id = getpid();

	if (gethostname(hostname, sizeof(hostname) - 1) < 0) {
		syslog(LOG_ERR, "timed: gethostname: %m");
		exit(1);
	}
	hp[0].name = hostname;

	if (nflag) {
		localnet = getnetbyname(netname);
		if (localnet == NULL) {
			syslog(LOG_ERR, "timed: getnetbyname: %m");
			exit(1);
		}
		if (localnet == NULL) {
			syslog(LOG_ERR, "timed: no network: %m");
			exit(1);
		}
		broadcastaddr = inet_makeaddr(localnet->n_net,INADDR_BROADCAST);
	} else {
		int n;
		int n_addrlen;
		char *n_addr;
		char buf[BUFSIZ];
		struct ifconf ifc;
		struct ifreq ifreq, *ifr;
		struct sockaddr_in *sin;
	
		ifc.ifc_len = sizeof(buf);
		ifc.ifc_buf = buf;
		if (ioctl(sock, (int)SIOCGIFCONF, (char *)&ifc) < 0) {
			syslog(LOG_ERR, "timed: (get interface configuration)");
			exit(1);
		}
		ifr = ifc.ifc_req;
		for (n = ifc.ifc_len/sizeof(struct ifreq); n > 0; n--, ifr++) {
			ifreq = *ifr;
			if (ioctl(sock, (int)SIOCGIFFLAGS, 
						(char *)&ifreq) < 0) {
				syslog(LOG_ERR, "timed: (get interface flags)");
				continue;
			}
			if ((ifreq.ifr_flags & IFF_UP) == 0 ||
		    		(ifreq.ifr_flags & IFF_BROADCAST) == 0) {
				continue;
			}
			if (ioctl(sock, (int)SIOCGIFBRDADDR, 
						(char *)&ifreq) < 0) {
				syslog(LOG_ERR, "timed: (get broadaddr)");
				continue;
			}
			n_addrlen = sizeof(ifr->ifr_addr);
			n_addr = (char *)malloc((unsigned)n_addrlen);
			bcopy((char *)&ifreq.ifr_broadaddr, n_addr, n_addrlen);
			sin = (struct sockaddr_in *)n_addr;
			broadcastaddr = sin->sin_addr;
			break;
		}
	}

	/* us. delay to be used in response to broadcast */
	delay1 = casual((long)10000, 200000);	

	/* election timer delay in secs. */
	delay2 = casual((long)MINTOUT, (long)MAXTOUT);

	/* look for master */
	resp.tsp_type = TSP_MASTERREQ;
	(void)strcpy(resp.tsp_name, hostname);
	answer = acksend(&resp, (char *)ANYADDR, TSP_MASTERACK);
	if (answer == NULL) {
		status = MASTER;
	} else {
		status = SLAVE;
		(void)strcpy(mastername, answer->tsp_name);
		masteraddr = from;

		/*
		 * If network has been partitioned, there might be other
		 * masters; tell the one we have just acknowledged that 
		 * it has to gain control over the others. 
		 */
		time.tv_sec = 0;
		time.tv_usec = 300000;
		answer = readmsg(TSP_MASTERACK, (char *)ANYADDR, &time);
		/*
		 * checking also not to send CONFLICT to ack'ed master
		 * due to duplicated MASTERACKs
		 */
		if (answer != NULL && 
				strcmp(answer->tsp_name, mastername) != 0) {
			conflict.tsp_type = TSP_CONFLICT;
			(void)strcpy(conflict.tsp_name, hostname);
			server = masteraddr;
			if (acksend(&conflict, (char *)mastername, 
							TSP_ACK) == NULL) {
				syslog(LOG_ERR, "timed: error on sending TSP_CONFLICT\n");
				exit(1);
			}
		}
	}
	if (Mflag) {
		/* open raw socket used to measure time differences */
		sock_raw = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP); 
		if (sock_raw < 0)  {
			fprintf(stderr, "timed: opening raw socket");
			exit (1);
		}

		/*
		 * number (increased by 1) of slaves controlled by master: 
		 * used in master.c, candidate.c, networkdelta.c, and 
		 * correct.c 
		 */
		slvcount = 1;

		/*
		 * Various conditions can cause conflict: race between
		 * two just started timedaemons when no master is present,
		 * or timedaemon started during an election.
		 * Conservative approach is taken: give up and became a
		 * slave postponing election of a master until first
		 * timer expires.
		 */
		if (status == MASTER) {
			time.tv_sec = time.tv_usec = 0;
			answer = readmsg(TSP_MASTERREQ, (char *)ANYADDR, &time);
			if (answer != NULL) {
				status = SLAVE;
				goto startd;
			}
	
			time.tv_sec = time.tv_usec = 0;
			answer = readmsg(TSP_MASTERUP, (char *)ANYADDR, &time);
			if (answer != NULL) {
				status = SLAVE;
				goto startd;
			}
	
			time.tv_sec = time.tv_usec = 0;
			answer = readmsg(TSP_ELECTION, (char *)ANYADDR, &time);
			if (answer != NULL) 
				status = SLAVE;
		}
startd:
		ret = setjmp(jmpenv);
		switch (ret) {

		case 0: 
			break;
		case 1: 
			/* from slave */
			status = election();
			break;
		case 2:
			/* from master */
			status = SLAVE;
			break;
		default:
			/* this should not happen */
			syslog(LOG_ERR, 
				"timed: error on return from longjmp\n");
			break;
		}
			
		if (status) 
			master();
		else 
			slave();
	} else {
		/* if Mflag is not set timedaemon is forced to act as a slave */
		if (setjmp(jmpenv)) {
			resp.tsp_type = TSP_SLAVEUP;
			(void)strcpy(resp.tsp_name, hostname);
			broadcast(&resp);
		}
		slave();
	}
}

/* 
 * `casual' returns a random number in the range [inf, sup]
 */

long casual(inf, sup)
long inf;
long sup;
{
	float value;
	long random();

	value = (float)random();
	value /= MAXRANDOM;
	if (value < 0) 
		value = -value;
	return(inf + (sup - inf) * value);
}

char *date()
{
	char	*ret;
	char    *asctime();
	struct	timeval tv;
	struct	tm *localtime();
	struct  tm *tp;

	(void)gettimeofday(&tv, (struct timezone *)0);
	tp = localtime((time_t *)&tv.tv_sec);
	ret = asctime(tp);
	ret[19] = '\0';
	return(ret);
}
