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
static char sccsid[] = "@(#)rwhod.c	5.2 (Berkeley) %G%";
#endif not lint

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/file.h>

#include <net/if.h>
#include <netinet/in.h>

#include <nlist.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <utmp.h>
#include <ctype.h>
#include <netdb.h>
#include <syslog.h>
#include <protocols/rwhod.h>

/*
 * Alarm interval. Don't forget to change the down time check in ruptime
 * if this is changed.
 */
#define AL_INTERVAL (3 * 60)

struct	sockaddr_in sin = { AF_INET };

extern	errno;

char	myname[32];

struct	nlist nl[] = {
#define	NL_AVENRUN	0
	{ "_avenrun" },
#define	NL_BOOTTIME	1
	{ "_boottime" },
	0
};

/*
 * We communicate with each neighbor in
 * a list constructed at the time we're
 * started up.  Neighbors are currently
 * directly connected via a hardware interface.
 */
struct	neighbor {
	struct	neighbor *n_next;
	char	*n_name;		/* interface name */
	char	*n_addr;		/* who to send to */
	int	n_addrlen;		/* size of address */
	int	n_flags;		/* should forward?, interface flags */
};

struct	neighbor *neighbors;
struct	whod mywd;
struct	servent *sp;
int	s, utmpf, kmemf = -1;

#define	WHDRSIZE	(sizeof (mywd) - sizeof (mywd.wd_we))
#define	RWHODIR		"/usr/spool/rwho"

int	onalrm();
char	*strcpy(), *sprintf(), *malloc();
long	lseek();
int	getkmem();
struct	in_addr inet_makeaddr();

main()
{
	struct sockaddr_in from;
	char path[64];
	int addr, on = 1;
	struct hostent *hp;

	if (getuid()) {
		fprintf(stderr, "rwhod: not super user\n");
		exit(1);
	}
	sp = getservbyname("who", "udp");
	if (sp == 0) {
		fprintf(stderr, "rwhod: udp/who: unknown service\n");
		exit(1);
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
		ioctl(s, TIOCNOTTY, 0);
		(void) close(s);
	  }
	}
#endif
	(void) chdir("/dev");
	(void) signal(SIGHUP, getkmem);
	openlog("rwhod", LOG_PID, 0);
	/*
	 * Establish host name as returned by system.
	 */
	if (gethostname(myname, sizeof (myname) - 1) < 0) {
		syslog(LOG_ERR, "gethostname: %m");
		exit(1);
	}
	strncpy(mywd.wd_hostname, myname, sizeof (myname) - 1);
	utmpf = open("/etc/utmp", O_RDONLY);
	if (utmpf < 0) {
		(void) close(creat("/etc/utmp", 0644));
		utmpf = open("/etc/utmp", O_RDONLY);
	}
	if (utmpf < 0) {
		syslog(LOG_ERR, "/etc/utmp: %m");
		exit(1);
	}
	getkmem();
	if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		syslog(LOG_ERR, "socket: %m");
		exit(1);
	}
	if (setsockopt(s, SOL_SOCKET, SO_BROADCAST, &on, sizeof (on)) < 0) {
		syslog(LOG_ERR, "setsockopt SO_BROADCAST: %m");
		exit(1);
	}
	hp = gethostbyname(myname);
	if (hp == NULL) {
		syslog(LOG_ERR, "%s: don't know my own name", myname);
		exit(1);
	}
	sin.sin_family = hp->h_addrtype;
	sin.sin_port = sp->s_port;
	if (bind(s, &sin, sizeof (sin)) < 0) {
		syslog(LOG_ERR, "bind: %m");
		exit(1);
	}
	if (!configure(s))
		exit(1);
	signal(SIGALRM, onalrm);
	onalrm();
	for (;;) {
		struct whod wd;
		int cc, whod, len = sizeof (from);

		cc = recvfrom(s, (char *)&wd, sizeof (struct whod), 0,
			&from, &len);
		if (cc <= 0) {
			if (cc < 0 && errno != EINTR)
				syslog(LOG_WARNING, "recv: %m");
			continue;
		}
		if (from.sin_port != sp->s_port) {
			syslog(LOG_WARNING, "%d: bad from port",
				ntohs(from.sin_port));
			continue;
		}
#ifdef notdef
		if (gethostbyname(wd.wd_hostname) == 0) {
			syslog(LOG_WARNING, "%s: unknown host",
				wd.wd_hostname);
			continue;
		}
#endif
		if (wd.wd_vers != WHODVERSION)
			continue;
		if (wd.wd_type != WHODTYPE_STATUS)
			continue;
		if (!verify(wd.wd_hostname)) {
			syslog(LOG_WARNING, "malformed host name from %x",
				from.sin_addr);
			continue;
		}
		(void) sprintf(path, "%s/whod.%s", RWHODIR, wd.wd_hostname);
		whod = creat(path, 0666);
		if (whod < 0) {
			syslog(LOG_WARNING, "%s: %m", path);
			continue;
		}
#if vax || pdp11
		{
			int i, n = (cc - WHDRSIZE)/sizeof(struct whoent);
			struct whoent *we;

			/* undo header byte swapping before writing to file */
			wd.wd_sendtime = ntohl(wd.wd_sendtime);
			for (i = 0; i < 3; i++)
				wd.wd_loadav[i] = ntohl(wd.wd_loadav[i]);
			wd.wd_boottime = ntohl(wd.wd_boottime);
			we = wd.wd_we;
			for (i = 0; i < n; i++) {
				we->we_idle = ntohl(we->we_idle);
				we->we_utmp.out_time =
				    ntohl(we->we_utmp.out_time);
				we++;
			}
		}
#endif
		(void) time(&wd.wd_recvtime);
		(void) write(whod, (char *)&wd, cc);
		(void) close(whod);
	}
}

/*
 * Check out host name for unprintables
 * and other funnies before allowing a file
 * to be created.  Sorry, but blanks aren't allowed.
 */
verify(name)
	register char *name;
{
	register int size = 0;

	while (*name) {
		if (!isascii(*name) || !(isalnum(*name) || ispunct(*name)))
			return (0);
		name++, size++;
	}
	return (size > 0);
}

int	utmptime;
int	utmpent;
int	utmpsize = 0;
struct	utmp *utmp;
int	alarmcount;

onalrm()
{
	register int i;
	struct stat stb;
	register struct whoent *we = mywd.wd_we, *wlast;
	int cc;
	double avenrun[3];
	time_t now = time(0);
	register struct neighbor *np;

	if (alarmcount % 10 == 0)
		getkmem();
	alarmcount++;
	(void) fstat(utmpf, &stb);
	if ((stb.st_mtime != utmptime) || (stb.st_size > utmpsize)) {
		utmptime = stb.st_mtime;
		if (stb.st_size > utmpsize) {
			utmpsize = stb.st_size + 10 * sizeof(struct utmp);
			if (utmp)
				utmp = (struct utmp *)realloc(utmp, utmpsize);
			else
				utmp = (struct utmp *)malloc(utmpsize);
			if (! utmp) {
				fprintf(stderr, "rwhod: malloc failed\n");
				utmpsize = 0;
				goto done;
			}
		}
		(void) lseek(utmpf, (long)0, L_SET);
		cc = read(utmpf, (char *)utmp, stb.st_size);
		if (cc < 0) {
			perror("/etc/utmp");
			goto done;
		}
		wlast = &mywd.wd_we[1024 / sizeof (struct whoent) - 1];
		utmpent = cc / sizeof (struct utmp);
		for (i = 0; i < utmpent; i++)
			if (utmp[i].ut_name[0]) {
				bcopy(utmp[i].ut_line, we->we_utmp.out_line,
				   sizeof (utmp[i].ut_line));
				bcopy(utmp[i].ut_name, we->we_utmp.out_name,
				   sizeof (utmp[i].ut_name));
				we->we_utmp.out_time = htonl(utmp[i].ut_time);
				if (we >= wlast)
					break;
				we++;
			}
		utmpent = we - mywd.wd_we;
	}
	we = mywd.wd_we;
	for (i = 0; i < utmpent; i++) {
		if (stat(we->we_utmp.out_line, &stb) >= 0)
			we->we_idle = htonl(now - stb.st_atime);
		we++;
	}
	(void) lseek(kmemf, (long)nl[NL_AVENRUN].n_value, L_SET);
	(void) read(kmemf, (char *)avenrun, sizeof (avenrun));
	for (i = 0; i < 3; i++)
		mywd.wd_loadav[i] = htonl((u_long)(avenrun[i] * 100));
	cc = (char *)we - (char *)&mywd;
	mywd.wd_sendtime = htonl(time(0));
	mywd.wd_vers = WHODVERSION;
	mywd.wd_type = WHODTYPE_STATUS;
	for (np = neighbors; np != NULL; np = np->n_next)
		(void) sendto(s, (char *)&mywd, cc, 0,
			np->n_addr, np->n_addrlen);
done:
	(void) alarm(AL_INTERVAL);
}

getkmem()
{
	struct nlist *nlp;
	static ino_t vmunixino;
	static time_t vmunixctime;
	struct stat sb;

	if (stat("/vmunix", &sb) < 0) {
		if (vmunixctime)
			return;
	} else {
		if (sb.st_ctime == vmunixctime && sb.st_ino == vmunixino)
			return;
		vmunixctime = sb.st_ctime;
		vmunixino= sb.st_ino;
	}
	if (kmemf >= 0)
		(void) close(kmemf);
loop:
	if (nlist("/vmunix", nl)) {
		syslog(LOG_WARNING, "/vmunix namelist botch");
		sleep(300);
		goto loop;
	}
	kmemf = open("/dev/kmem", O_RDONLY);
	if (kmemf < 0) {
		syslog(LOG_ERR, "/dev/kmem: %m");
		exit(1);
	}
	(void) lseek(kmemf, (long)nl[NL_BOOTTIME].n_value, L_SET);
	(void) read(kmemf, (char *)&mywd.wd_boottime,
	    sizeof (mywd.wd_boottime));
	mywd.wd_boottime = htonl(mywd.wd_boottime);
}

/*
 * Figure out device configuration and select
 * networks which deserve status information.
 */
configure(s)
	int s;
{
	char buf[BUFSIZ];
	struct ifconf ifc;
	struct ifreq ifreq, *ifr;
	struct sockaddr_in *sin;
	register struct neighbor *np;
	int n;

	ifc.ifc_len = sizeof (buf);
	ifc.ifc_buf = buf;
	if (ioctl(s, SIOCGIFCONF, (char *)&ifc) < 0) {
		syslog(LOG_ERR, "ioctl (get interface configuration)");
		return (0);
	}
	ifr = ifc.ifc_req;
	for (n = ifc.ifc_len / sizeof (struct ifreq); n > 0; n--, ifr++) {
		for (np = neighbors; np != NULL; np = np->n_next)
			if (np->n_name &&
			    strcmp(ifr->ifr_name, np->n_name) == 0)
				break;
		if (np != NULL)
			continue;
		ifreq = *ifr;
		np = (struct neighbor *)malloc(sizeof (*np));
		if (np == NULL)
			continue;
		np->n_name = malloc(strlen(ifr->ifr_name) + 1);
		if (np->n_name == NULL) {
			free((char *)np);
			continue;
		}
		strcpy(np->n_name, ifr->ifr_name);
		np->n_addrlen = sizeof (ifr->ifr_addr);
		np->n_addr = malloc(np->n_addrlen);
		if (np->n_addr == NULL) {
			free(np->n_name);
			free((char *)np);
			continue;
		}
		bcopy((char *)&ifr->ifr_addr, np->n_addr, np->n_addrlen);
		if (ioctl(s, SIOCGIFFLAGS, (char *)&ifreq) < 0) {
			syslog(LOG_ERR, "ioctl (get interface flags)");
			free((char *)np);
			continue;
		}
		if ((ifreq.ifr_flags & IFF_UP) == 0 ||
		    (ifreq.ifr_flags & (IFF_BROADCAST|IFF_POINTOPOINT)) == 0) {
			free((char *)np);
			continue;
		}
		np->n_flags = ifreq.ifr_flags;
		if (np->n_flags & IFF_POINTOPOINT) {
			if (ioctl(s, SIOCGIFDSTADDR, (char *)&ifreq) < 0) {
				syslog(LOG_ERR, "ioctl (get dstaddr)");
				free((char *)np);
				continue;
			}
			/* we assume addresses are all the same size */
			bcopy((char *)&ifreq.ifr_dstaddr,
			  np->n_addr, np->n_addrlen);
		}
		if (np->n_flags & IFF_BROADCAST) {
			if (ioctl(s, SIOCGIFBRDADDR, (char *)&ifreq) < 0) {
				syslog(LOG_ERR, "ioctl (get broadaddr)");
				free((char *)np);
				continue;
			}
			/* we assume addresses are all the same size */
			bcopy((char *)&ifreq.ifr_broadaddr,
			  np->n_addr, np->n_addrlen);
		}
		/* gag, wish we could get rid of Internet dependencies */
		sin = (struct sockaddr_in *)np->n_addr;
		sin->sin_port = sp->s_port;
		np->n_next = neighbors;
		neighbors = np;
	}
	return (1);
}

/*
 * Return the possible subnetwork number from an internet address.
 * If the address is of the form of a subnet address (most significant
 * bit of the host part is set), believe the subnet exists.
 * Otherwise, return the network number.  Any subnet number is only valid
 * if this is a ``local'' net.
 */
inet_subnetof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);

	if (IN_CLASSA(i)) {
		if (IN_SUBNETA(i))
			return ((i & IN_CLASSA_SUBNET) >> IN_CLASSA_SUBNSHIFT);
		else
			return ((i & IN_CLASSA_NET) >> IN_CLASSA_NSHIFT);
	} else if (IN_CLASSB(i)) {
		if (IN_SUBNETB(i))
			return ((i & IN_CLASSB_SUBNET) >> IN_CLASSB_SUBNSHIFT);
		else
			return ((i & IN_CLASSB_NET) >> IN_CLASSB_NSHIFT);
	} else
		return ((i & IN_CLASSC_NET) >> IN_CLASSC_NSHIFT);
}

#ifdef DEBUG
sendto(s, buf, cc, flags, to, tolen)
	int s;
	char *buf;
	int cc, flags;
	char *to;
	int tolen;
{
	register struct whod *w = (struct whod *)buf;
	register struct whoent *we;
	struct sockaddr_in *sin = (struct sockaddr_in *)to;
	char *interval();

	printf("sendto %x.%d\n", ntohl(sin->sin_addr), ntohs(sin->sin_port));
	printf("hostname %s %s\n", w->wd_hostname,
	   interval(ntohl(w->wd_sendtime) - ntohl(w->wd_boottime), "  up"));
	printf("load %4.2f, %4.2f, %4.2f\n",
	    ntohl(w->wd_loadav[0]) / 100.0, ntohl(w->wd_loadav[1]) / 100.0,
	    ntohl(w->wd_loadav[2]) / 100.0);
	cc -= WHDRSIZE;
	for (we = w->wd_we, cc /= sizeof (struct whoent); cc > 0; cc--, we++) {
		time_t t = ntohl(we->we_utmp.out_time);
		printf("%-8.8s %s:%s %.12s",
			we->we_utmp.out_name,
			w->wd_hostname, we->we_utmp.out_line,
			ctime(&t)+4);
		we->we_idle = ntohl(we->we_idle) / 60;
		if (we->we_idle) {
			if (we->we_idle >= 100*60)
				we->we_idle = 100*60 - 1;
			if (we->we_idle >= 60)
				printf(" %2d", we->we_idle / 60);
			else
				printf("   ");
			printf(":%02d", we->we_idle % 60);
		}
		printf("\n");
	}
}

char *
interval(time, updown)
	int time;
	char *updown;
{
	static char resbuf[32];
	int days, hours, minutes;

	if (time < 0 || time > 3*30*24*60*60) {
		(void) sprintf(resbuf, "   %s ??:??", updown);
		return (resbuf);
	}
	minutes = (time + 59) / 60;		/* round to minutes */
	hours = minutes / 60; minutes %= 60;
	days = hours / 24; hours %= 24;
	if (days)
		(void) sprintf(resbuf, "%s %2d+%02d:%02d",
		    updown, days, hours, minutes);
	else
		(void) sprintf(resbuf, "%s    %2d:%02d",
		    updown, hours, minutes);
	return (resbuf);
}
#endif
