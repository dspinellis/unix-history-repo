/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.17 (Berkeley) 2/20/89";
#endif /* not lint */

/*
 * Routing Table Management Daemon
 */
#include "defs.h"
#include <sys/ioctl.h>
#include <sys/file.h>

#include <net/if.h>

#include <sys/errno.h>
#include <sys/signal.h>
#include <sys/syslog.h>

int	supplier = -1;		/* process should supply updates */
int	gateway = 0;		/* 1 if we are a gateway to parts beyond */
int	debug = 0;
int	bufspace = 127*1024;	/* max. input buffer size to request */

struct	rip *msg = (struct rip *)packet;
int	hup(), rtdeleteall(), sigtrace();

main(argc, argv)
	int argc;
	char *argv[];
{
	int n, cc, nfd, omask, tflags = 0;
	struct sockaddr from;
	struct timeval *tvp, waittime;
	struct itimerval itval;
	fd_set ibits;
	u_char retry;
	
	argv0 = argv;
#if BSD >= 43
	openlog("routed", LOG_PID | LOG_ODELAY, LOG_DAEMON);
	setlogmask(LOG_UPTO(LOG_WARNING));
#else
	openlog("routed", LOG_PID);
#define LOG_UPTO(x) (x)
#define setlogmask(x) (x)
#endif
	sp = getservbyname("router", "udp");
	if (sp == NULL) {
		fprintf(stderr, "routed: router/udp: unknown service\n");
		exit(1);
	}
	addr.sin_family = AF_INET;
	addr.sin_port = sp->s_port;
	s = getsocket(AF_INET, SOCK_DGRAM, &addr);
	if (s < 0)
		exit(1);
	argv++, argc--;
	while (argc > 0 && **argv == '-') {
		if (strcmp(*argv, "-s") == 0) {
			supplier = 1;
			argv++, argc--;
			continue;
		}
		if (strcmp(*argv, "-q") == 0) {
			supplier = 0;
			argv++, argc--;
			continue;
		}
		if (strcmp(*argv, "-t") == 0) {
			tflags++;
			setlogmask(LOG_UPTO(LOG_DEBUG));
			argv++, argc--;
			continue;
		}
		if (strcmp(*argv, "-d") == 0) {
			debug++;
			setlogmask(LOG_UPTO(LOG_DEBUG));
			argv++, argc--;
			continue;
		}
		if (strcmp(*argv, "-g") == 0) {
			gateway = 1;
			argv++, argc--;
			continue;
		}
		fprintf(stderr,
			"usage: routed [ -s ] [ -q ] [ -t ] [ -g ]\n");
		exit(1);
	}

	if (debug == 0) {
		int t;

		if (fork())
			exit(0);
		for (t = 0; t < 20; t++)
			if (t != s)
				(void) close(t);
		(void) open("/", 0);
		(void) dup2(0, 1);
		(void) dup2(0, 2);
		t = open("/dev/tty", 2);
		if (t >= 0) {
			ioctl(t, TIOCNOTTY, (char *)0);
			(void) close(t);
		}
	}
	/*
	 * Any extra argument is considered
	 * a tracing log file.
	 */
	if (argc > 0)
		traceon(*argv);
	while (tflags-- > 0)
		bumploglevel();

	(void) gettimeofday(&now, (struct timezone *)NULL);
	/*
	 * Collect an initial view of the world by
	 * checking the interface configuration and the gateway kludge
	 * file.  Then, send a request packet on all
	 * directly connected networks to find out what
	 * everyone else thinks.
	 */
	rtinit();
	ifinit();
	gwkludge();
	if (gateway > 0)
		rtdefault();
	if (supplier < 0)
		supplier = 0;
	msg->rip_cmd = RIPCMD_REQUEST;
	msg->rip_vers = RIPVERSION;
	if (sizeof(msg->rip_nets[0].rip_dst.sa_family) > 1)	/* XXX */
		msg->rip_nets[0].rip_dst.sa_family = htons((u_short)AF_UNSPEC);
	else
		msg->rip_nets[0].rip_dst.sa_family = AF_UNSPEC;
	msg->rip_nets[0].rip_metric = htonl((u_long)HOPCNT_INFINITY);
	toall(sendmsg);
	signal(SIGALRM, timer);
	signal(SIGHUP, hup);
	signal(SIGTERM, hup);
	signal(SIGINT, rtdeleteall);
	signal(SIGUSR1, sigtrace);
	signal(SIGUSR2, sigtrace);
	itval.it_interval.tv_sec = TIMER_RATE;
	itval.it_value.tv_sec = TIMER_RATE;
	itval.it_interval.tv_usec = 0;
	itval.it_value.tv_usec = 0;
	srandom(getpid());
	if (setitimer(ITIMER_REAL, &itval, (struct itimerval *)NULL) < 0)
		syslog(LOG_ERR, "setitimer: %m\n");

	FD_ZERO(&ibits);
	nfd = s + 1;			/* 1 + max(fd's) */
	for (;;) {
		FD_SET(s, &ibits);
		/*
		 * If we need a dynamic update that was held off,
		 * needupdate will be set, and nextbcast is the time
		 * by which we want select to return.  Compute time
		 * until dynamic update should be sent, and select only
		 * until then.  If we have already passed nextbcast,
		 * just poll.
		 */
		if (needupdate) {
			waittime = nextbcast;
			timevalsub(&waittime, &now);
			if (waittime.tv_sec < 0) {
				waittime.tv_sec = 0;
				waittime.tv_usec = 0;
			}
			if (traceactions)
				fprintf(ftrace,
				 "select until dynamic update %d/%d sec/usec\n",
				    waittime.tv_sec, waittime.tv_usec);
			tvp = &waittime;
		} else
			tvp = (struct timeval *)NULL;
		n = select(nfd, &ibits, 0, 0, tvp);
		if (n <= 0) {
			/*
			 * Need delayed dynamic update if select returned
			 * nothing and we timed out.  Otherwise, ignore
			 * errors (e.g. EINTR).
			 */
			if (n < 0) {
				if (errno == EINTR)
					continue;
				syslog(LOG_ERR, "select: %m");
			}
			omask = sigblock(sigmask(SIGALRM));
			if (n == 0 && needupdate) {
				if (traceactions)
					fprintf(ftrace,
					    "send delayed dynamic update\n");
				(void) gettimeofday(&now,
					    (struct timezone *)NULL);
				toall(supply, RTS_CHANGED,
				    (struct interface *)NULL);
				lastbcast = now;
				needupdate = 0;
				nextbcast.tv_sec = 0;
			}
			sigsetmask(omask);
			continue;
		}
		(void) gettimeofday(&now, (struct timezone *)NULL);
		omask = sigblock(sigmask(SIGALRM));
#ifdef doesntwork
/*
printf("s %d, ibits %x index %d, mod %d, sh %x, or %x &ibits %x\n",
	s,
	ibits.fds_bits[0],
	(s)/(sizeof(fd_mask) * 8),
	((s) % (sizeof(fd_mask) * 8)),
	(1 << ((s) % (sizeof(fd_mask) * 8))),
	ibits.fds_bits[(s)/(sizeof(fd_mask) * 8)] & (1 << ((s) % (sizeof(fd_mask) * 8))),
	&ibits
	);
*/
		if (FD_ISSET(s, &ibits))
#else
		if (ibits.fds_bits[s/32] & (1 << s))
#endif
			process(s);
		/* handle ICMP redirects */
		sigsetmask(omask);
	}
}

timevaladd(t1, t2)
	struct timeval *t1, *t2;
{

	t1->tv_sec += t2->tv_sec;
	if ((t1->tv_usec += t2->tv_usec) > 1000000) {
		t1->tv_sec++;
		t1->tv_usec -= 1000000;
	}
}

timevalsub(t1, t2)
	struct timeval *t1, *t2;
{

	t1->tv_sec -= t2->tv_sec;
	if ((t1->tv_usec -= t2->tv_usec) < 0) {
		t1->tv_sec--;
		t1->tv_usec += 1000000;
	}
}

process(fd)
	int fd;
{
	struct sockaddr from;
	int fromlen, cc;
	union {
		char	buf[MAXPACKETSIZE+1];
		struct	rip rip;
	} inbuf;

	for (;;) {
		fromlen = sizeof (from);
		cc = recvfrom(fd, &inbuf, sizeof (inbuf), 0, &from, &fromlen);
		if (cc <= 0) {
			if (cc < 0 && errno != EWOULDBLOCK)
				perror("recvfrom");
			break;
		}
		if (fromlen != sizeof (struct sockaddr_in))
			break;
		rip_input(&from, &inbuf.rip, cc);
	}
}

getsocket(domain, type, sin)
	int domain, type;
	struct sockaddr_in *sin;
{
	int sock, on = 1;

	if ((sock = socket(domain, type, 0)) < 0) {
		perror("socket");
		syslog(LOG_ERR, "socket: %m");
		return (-1);
	}
#ifdef SO_BROADCAST
	if (setsockopt(sock, SOL_SOCKET, SO_BROADCAST, &on, sizeof (on)) < 0) {
		syslog(LOG_ERR, "setsockopt SO_BROADCAST: %m");
		close(sock);
		return (-1);
	}
#endif
#ifdef SO_RCVBUF
	for (on = bufspace; ; on -= 1024) {
		if (setsockopt(sock, SOL_SOCKET, SO_RCVBUF,
		    &on, sizeof (on)) == 0)
			break;
		if (on <= 8*1024) {
			syslog(LOG_ERR, "setsockopt SO_RCVBUF: %m");
			break;
		}
	}
	if (traceactions)
		fprintf(ftrace, "recv buf %d\n", on);
#endif
	if (bind(sock, sin, sizeof (*sin), 0) < 0) {
		perror("bind");
		syslog(LOG_ERR, "bind: %m");
		close(sock);
		return (-1);
	}
	if (fcntl(sock, F_SETFL, FNDELAY) == -1)
		syslog(LOG_ERR, "fcntl FNDELAY: %m\n");
	return (sock);
}
