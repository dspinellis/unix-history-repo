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
static char sccsid[] = "@(#)timed.c	1.4 (Berkeley) %G%";
#endif not lint

#include "globals.h"
#define TSPTYPES
#include <protocols/timed.h>
#include <net/if.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <setjmp.h>

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
char hostname[MAXHOSTNAMELEN];
struct in_addr broadcastaddr;		/* local net broadcast address */
u_long netmask, mynet = 0;		/* my network number & netmask */
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
	char mastername[MAXHOSTNAMELEN];
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
	int n;
	int n_addrlen;
	char *n_addr;
	char buf[BUFSIZ];
	struct ifconf ifc;
	struct ifreq ifreq, *ifr;
	struct sockaddr_in *sin;
	

	Mflag = 0;
	on = 1;
	backoff = 1;
	fj = "/usr/adm/timed.log";
	trace = OFF;
	nflag = OFF;
	openlog("timed", LOG_ODELAY, LOG_DAEMON);

	if (getuid() != 0) {
		fprintf(stderr, "Timed: not superuser\n");
		exit(1);
	}

	while (--argc > 0 && **++argv == '-') {
		(*argv)++;
		do {
			switch (**argv) {

			case 'M':
				Mflag = 1; 
				break;
			case 't':
				trace = ON; 
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

#ifndef DEBUG
	if (fork())
		exit(0);
	{ int s;
	  for (s = 0; s < 10; s++)
		(void) close(s);
	  (void) open("/", 0);
	  (void) dup2(0, 1);
	  (void) dup2(0, 2);
	  s = open("/dev/tty", 2);
	  if (s >= 0) {
		(void) ioctl(s, (int)TIOCNOTTY, (char *)0);
		(void) close(s);
	  }
	}
#endif

	if (trace == ON) {
		fd = fopen(fj, "w");
		fprintf(fd, "Tracing started on: %s\n\n", 
					date());
		(void)fflush(fd);
	}
	openlog("timed", LOG_ODELAY|LOG_CONS, LOG_DAEMON);

	srvp = getservbyname("timed", "udp");
	if (srvp == 0) {
		syslog(LOG_CRIT, "unknown service 'timed/udp'");
		exit(1);
	}
	server.sin_port = srvp->s_port;
	server.sin_family = AF_INET;
	sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (sock < 0) {
		syslog(LOG_ERR, "socket: %m");
		exit(1);
	}
	if (setsockopt(sock, SOL_SOCKET, SO_BROADCAST, (char *)&on, 
							sizeof(on)) < 0) {
		syslog(LOG_ERR, "setsockopt: %m");
		exit(1);
	}
	if (bind(sock, &server, sizeof(server))) {
		if (errno == EADDRINUSE)
		        syslog(LOG_ERR, "server already running");
		else
		        syslog(LOG_ERR, "bind: %m");
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
		syslog(LOG_ERR, "gethostname: %m");
		exit(1);
	}
	hp[0].name = hostname;

	if (nflag) {
		localnet = getnetbyname(netname);
		if (localnet == NULL) {
			syslog(LOG_ERR, "getnetbyname: unknown net %s",
				netname);
			exit(1);
		}
	}
	ifc.ifc_len = sizeof(buf);
	ifc.ifc_buf = buf;
	if (ioctl(sock, (int)SIOCGIFCONF, (char *)&ifc) < 0) {
		syslog(LOG_ERR, "get interface configuration: %m");
		exit(1);
	}
	ifr = ifc.ifc_req;
	for (n = ifc.ifc_len/sizeof(struct ifreq); n > 0; n--, ifr++) {
		ifreq = *ifr;
		if (ioctl(sock, (int)SIOCGIFFLAGS, 
					(char *)&ifreq) < 0) {
			syslog(LOG_ERR, "get interface flags: %m");
			continue;
		}
		if ((ifreq.ifr_flags & IFF_UP) == 0 ||
			(ifreq.ifr_flags & IFF_BROADCAST) == 0) {
			continue;
		}
		if (ioctl(sock, (int)SIOCGIFNETMASK, 
					(char *)&ifreq) < 0) {
			syslog(LOG_ERR, "get broadaddr: %m");
			continue;
		}
		netmask = ((struct sockaddr_in *)
			&ifreq.ifr_addr)->sin_addr.s_addr;
		if (ioctl(sock, (int)SIOCGIFBRDADDR, 
					(char *)&ifreq) < 0) {
			syslog(LOG_ERR, "get broadaddr: %m");
			continue;
		}
		n_addrlen = sizeof(ifr->ifr_addr);
		n_addr = (char *)malloc((unsigned)n_addrlen);
		bcopy((char *)&ifreq.ifr_broadaddr, n_addr, n_addrlen);
		sin = (struct sockaddr_in *)n_addr;
		broadcastaddr = sin->sin_addr;
		if (nflag) {
			u_long addr, mask;

			addr = ntohl(broadcastaddr.s_addr);
			mask = ntohl(netmask);
			while ((mask & 1) == 0) {
				addr >>= 1;
				mask >>= 1;
			}
			if (addr != localnet->n_net)
				continue;
		}
		mynet = netmask & broadcastaddr.s_addr;
		break;
	}
	if (!mynet) {
		syslog(LOG_ERR, "No network usable");
		exit(1);
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
				syslog(LOG_ERR, "error on sending TSP_CONFLICT");
				exit(1);
			}
		}
	}
	if (Mflag) {
		/* open raw socket used to measure time differences */
		sock_raw = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP); 
		if (sock_raw < 0)  {
			syslog(LOG_ERR, "opening raw socket: %m");
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
			syslog(LOG_ERR, "Attempt to enter invalid state");
			break;
		}
			
		if (status == MASTER) 
			master();
		else 
			slave();
	} else {
		status = SLAVE;
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

	value = (float)(random() & 0x7fffffff) / 0x7fffffff;
	return(inf + (sup - inf) * value);
}

char *
date()
{
	char	*ret;
	char    *ctime();
	struct	timeval tv;

	(void)gettimeofday(&tv, (struct timezone *)0);
	ret = ctime(&tv.tv_sec);
	return(ret);
}
