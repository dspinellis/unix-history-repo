#ifndef lint
static char sccsid[] = "@(#)date.c	4.10 (Berkeley) %G%";
#endif not lint

/*
 * Date - print and set date
 * Modified by Riccardo Gusella to work with timed 
 */

#include <sys/param.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#define TSPTYPES
#include <protocols/timed.h>
#include <sys/file.h>
#include <errno.h>
#include <syslog.h>
#include <utmp.h>

#define WTMP "/usr/adm/wtmp"
#define WAITACK		2	/* seconds */
#define WAITDATEACK	5	/* seconds */

struct	timeval tv, now;
struct	timezone tz;
char	*ap, *ep, *sp;
int	uflag;

char	*timezone();
static	int	dmsize[12] =
    { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
static char *usage = "usage: date [-u] [yymmddhhmm[.ss]]\n";

struct utmp wtmp[2] = {
	{ "|", "", "", 0 },
	{ "{", "", "", 0 }
};

char	*ctime();
char	*asctime();
struct	tm *localtime();
struct	tm *gmtime();

char *strcpy();
char *username, *getlogin();

int sock, length, port;
char hostname[MAXHOSTNAMELEN];
struct timeval tout;
struct servent *srvp;
struct tsp msg;
struct sockaddr_in sin, dest, from;

main(argc, argv)
	int argc;
	char *argv[];
{
	int wf;
	int timed_ack, found;
	long waittime;
	register char *tzn;
	fd_set ready;
	extern int errno;
	int bytenetorder(), bytehostorder();

	(void) gettimeofday(&tv, &tz);
	now = tv;

	if (argc > 1 && strcmp(argv[1], "-u") == 0) {
		argc--;
		argv++;
		uflag++;
	}
	if (argc > 2) {
		printf(usage);
		exit(1);
	}
	if (argc == 1) 
		goto display;

	if (getuid() != 0) {
		printf("You are not superuser: date not set\n");
		goto display;
	}
	username = getlogin();
	if (username == NULL)		/* single-user or no tty */
		username = "root";
	syslog(LOG_SECURITY, "date: set by %s", username);

	ap = argv[1];
	wtmp[0].ut_time = tv.tv_sec;
	if (gtime()) {
		printf(usage);
		goto display;
	}
	/* convert to GMT assuming local time */
	if (uflag == 0) {
		tv.tv_sec += (long)tz.tz_minuteswest*60;
		/* now fix up local daylight time */
		if (localtime((time_t *)&tv.tv_sec)->tm_isdst)
			tv.tv_sec -= 60*60;
	}
	
/*
 * Here we set the date in the machines controlled by timedaemons
 * by communicating the new date to the local timedaemon. 
 * If the timedaemon is in the master state, it performs the correction on 
 * all slaves. If it is in the slave state, it notifies the master 
 * that a correction is needed.
 */

	srvp = getservbyname("timed", "udp");
	if (srvp == 0) {
		fprintf(stderr, "udp/timed: unknown service\n");
		goto oldway;
	}	
	dest.sin_port = srvp->s_port;
	dest.sin_family = AF_INET;
	dest.sin_addr.s_addr = htonl((u_long)INADDR_ANY);
	sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (sock < 0) {
		if (errno != EPROTONOSUPPORT)
			perror("socket");
		goto oldway;
	}

	sin.sin_family = AF_INET;
	for (port = IPPORT_RESERVED - 1; port > IPPORT_RESERVED / 2; port--) {
		sin.sin_port = htons((u_short)port);
		if (bind(sock, (struct sockaddr *)&sin, sizeof (sin)) >= 0)
			break;
		if (errno != EADDRINUSE) {
			perror("bind");
			(void) close(sock);
			goto oldway;
		}
	}
	if (port == IPPORT_RESERVED / 2) {
		fprintf(stderr, "socket: All ports in use\n");
		(void) close(sock);
		goto oldway;
	}

	msg.tsp_type = TSP_DATE;
	msg.tsp_vers = TSPVERSION;
	(void) gethostname(hostname, sizeof(hostname));
	(void) strcpy(msg.tsp_name, hostname);
	msg.tsp_time = tv;
	timevalsub(&msg.tsp_time, &now);
	bytenetorder(&msg);
	length = sizeof(struct sockaddr_in);
	if (sendto(sock, (char *)&msg, sizeof(struct tsp), 0, 
						&dest, length) < 0) {
		if (errno != ECONNREFUSED)
			perror("sendto");
		goto oldway;
	}

	timed_ack = -1;
	waittime = WAITACK;
loop:
	tout.tv_sec = waittime;
	tout.tv_usec = 0;
	FD_ZERO(&ready);
	FD_SET(sock, &ready);
	found = select(FD_SETSIZE, &ready, (fd_set *)0, (fd_set *)0, &tout);
	if (found > 0 && FD_ISSET(sock, &ready)) {
		length = sizeof(struct sockaddr_in);
		if (recvfrom(sock, (char *)&msg, sizeof(struct tsp), 0, 
							&from, &length) < 0) {
			perror("recvfrom");
			goto oldway;
		}
		bytehostorder(&msg);

		switch (msg.tsp_type) {

		case TSP_ACK:
			timed_ack = TSP_ACK;
			waittime = WAITDATEACK;
			goto loop;
		case TSP_DATEACK:
			goto display;
		default:
			printf("Wrong ack received from timed: %s\n", 
						tsptype[msg.tsp_type]);
			timed_ack = -1;
			break;
		}
	}

	if (timed_ack == TSP_ACK)
		printf("Network date being set\n");
	else {
		printf("Communication error with timed\n");
oldway:
		if (settimeofday(&tv, (struct timezone *)0) < 0) {
			perror("settimeofday");
			goto display;
		}
		if ((wf = open(WTMP, 1)) >= 0) {
			(void) time((time_t *)&wtmp[1].ut_time);
			(void) lseek(wf, (off_t)0L, 2);
			(void) write(wf, (char *)wtmp, sizeof(wtmp));
			(void) close(wf);
		}
	}

display:
	(void) gettimeofday(&tv, (struct timezone *)0);
	if (uflag) {
		ap = asctime(gmtime((time_t *)&tv.tv_sec));
		tzn = "GMT";
	} else {
		struct tm *tp;
		tp = localtime((time_t *)&tv.tv_sec);
		ap = asctime(tp);
		tzn = timezone(tz.tz_minuteswest, tp->tm_isdst);
	}
	printf("%.20s", ap);
	if (tzn)
		printf("%s", tzn);
	printf("%s", ap+19);
}

gtime()
{
	register int i, year, month;
	int day, hour, mins, secs;
	struct tm *L;
	char x;

	ep = ap;
	while(*ep) ep++;
	sp = ap;
	while(sp < ep) {
		x = *sp;
		*sp++ = *--ep;
		*ep = x;
	}
	sp = ap;
	(void) gettimeofday(&tv, (struct timezone *)0);
	L = localtime((time_t *)&tv.tv_sec);
	secs = gp(-1);
	if (*sp != '.') {
		mins = secs;
		secs = 0;
	} else {
		sp++;
		mins = gp(-1);
	}
	hour = gp(-1);
	day = gp(L->tm_mday);
	month = gp(L->tm_mon+1);
	year = gp(L->tm_year);
	if (*sp)
		return (1);
	if (month < 1 || month > 12 ||
	    day < 1 || day > 31 ||
	    mins < 0 || mins > 59 ||
	    secs < 0 || secs > 59)
		return (1);
	if (hour == 24) {
		hour = 0;
		day++;
	}
	if (hour < 0 || hour > 23)
		return (1);
	tv.tv_sec = 0;
	year += 1900;
	for (i = 1970; i < year; i++)
		tv.tv_sec += dysize(i);
	/* Leap year */
	if (dysize(year) == 366 && month >= 3)
		tv.tv_sec++;
	while (--month)
		tv.tv_sec += dmsize[month-1];
	tv.tv_sec += day-1;
	tv.tv_sec = 24*tv.tv_sec + hour;
	tv.tv_sec = 60*tv.tv_sec + mins;
	tv.tv_sec = 60*tv.tv_sec + secs;
	return (0);
}

gp(dfault)
{
	register int c, d;

	if (*sp == 0)
		return (dfault);
	c = (*sp++) - '0';
	d = (*sp ? (*sp++) - '0' : 0);
	if (c < 0 || c > 9 || d < 0 || d > 9)
		return (-1);
	return (c+10*d);
}

timevalsub(t1, t2)
struct timeval *t1, *t2;
{
	t1->tv_sec -= t2->tv_sec;
	t1->tv_usec -= t2->tv_usec;
	if (t1->tv_usec < 0) {
		t1->tv_sec--;
		t1->tv_usec += 1000000;
	}
	if (t1->tv_usec >= 1000000) {
		t1->tv_sec++;
		t1->tv_usec -= 1000000;
	}
}

bytenetorder(ptr)
struct tsp *ptr;
{
	ptr->tsp_seq = htons((u_short)ptr->tsp_seq);
	ptr->tsp_time.tv_sec = htonl((u_long)ptr->tsp_time.tv_sec);
	ptr->tsp_time.tv_usec = htonl((u_long)ptr->tsp_time.tv_usec);
}

bytehostorder(ptr)
struct tsp *ptr;
{
	ptr->tsp_seq = ntohs((u_short)ptr->tsp_seq);
	ptr->tsp_time.tv_sec = ntohl((u_long)ptr->tsp_time.tv_sec);
	ptr->tsp_time.tv_usec = ntohl((u_long)ptr->tsp_time.tv_usec);
}
