#include <stdio.h>
#include <sys/types.h>
#include <sgtty.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <net/if.h>
#include <netdb.h>
#include <fcntl.h>

#ifndef lint
static char rcsid[] = "$Header: slattach.c,v 1.1 84/10/04 12:57:12 rick Exp $";
#endif

struct	ifreq ifr;
struct	sockaddr_in sin = {AF_INET};

#define DEFAULT_BAUD	9600
int	speed;
int	slipdisc = SLIPDISC;

char	devname[32];
char	hostname[32];

extern int errno;

main(argc, argv)
	int argc;
	char *argv[];
{
	register FILE *fp;
	register int fd;
	register char *dev = argv[1];
	struct sgttyb sgtty;
	int n;

	if (argc < 4 || argc > 5)
		error(1, 0, "usage: %s ttyname srcaddr dstaddr [baud]\n",
		      argv[0]);
	speed = argc == 5 ? findspeed(atoi(argv[4])) : findspeed(DEFAULT_BAUD);
	if (speed == 0)
		error(1, errno, "unknown speed %s", argv[4]);
	if (strncmp("/dev/", dev, 5)) {
		sprintf(devname, "/dev/%s", dev);
		dev = devname;
	}
	if ((fd = open(dev, O_RDWR | O_NDELAY)) < 0)
		error(1, errno, "%s: cannot open", dev);
	sgtty.sg_flags = RAW | ANYP;
	sgtty.sg_ispeed = sgtty.sg_ospeed = speed;
	if (ioctl(fd, TIOCSETP, &sgtty) < 0)
		error(1, errno, "ioctl(TIOCSETP) on %s", dev);
	if (ioctl(fd, TIOCSETD, &slipdisc) < 0)
		error(1, errno, "ioctl(TIOCSETD)");
	if (ioctl(fd, TIOCGETD, &n) < 0)
		error(1, errno, "ioctl(TIOCGETD)");

	fd = socket(AF_INET, SOCK_DGRAM, 0);
	if (fd < 0)
		error(1, errno, "socket(AF_INET, SOCK_DGRAM)");
	sprintf(ifr.ifr_name, "sl%d", n);

#ifdef sun
	/*
	 * This crap is necessary because sun changed the ioctl handling so
	 * that SIOCSIFDSTADDR ONLY gets handed the value of ifr_data, not
	 * the whole ifr structure like any sane implementation would. This
	 * is clearly a mistake on their part. Fortunately, sizeof (caddr_t)
	 * == sizeof(sin.sin_addr.s_addr) if it's not on your machine, you
	 * lose. 
	 */
	getaddr(argv[3], &sin);
	ifr.ifr_data = (caddr_t) sin.sin_addr.s_addr;
#else vax
	getaddr(argv[3], (struct sockaddr_in *)&ifr.ifr_dstaddr);
#endif vax
	if (ioctl(fd, SIOCSIFDSTADDR, (caddr_t)&ifr) < 0)
		error(1, errno, "ioctl(SIOCSIFDSTADDR)");
	getaddr(argv[2], (struct sockaddr_in *)&ifr.ifr_addr);
	if (ioctl(fd, SIOCSIFADDR, (caddr_t)&ifr) < 0)
		error(1, errno, "ioctl(SIOCSIFADDR)");
	for (;;)
		sigpause(0);
}

struct sg_spds {
	int sp_val, sp_name;
}       spds[] = {
#ifdef B50
	{ 50, B50 },
#endif
#ifdef B75
	{ 75, B75 },
#endif
#ifdef B110
	{ 110, B110 },
#endif
#ifdef B150
	{ 150, B150 },
#endif
#ifdef B200
	{ 200, B200 },
#endif
#ifdef B300
	{ 300, B300 },
#endif
#ifdef B600
	{ 600, B600 },
#endif
#ifdef B1200
	{ 1200, B1200 },
#endif
#ifdef B1800
	{ 1800, B1800 },
#endif
#ifdef B2000
	{ 2000, B2000 },
#endif
#ifdef B2400
	{ 2400, B2400 },
#endif
#ifdef B3600
	{ 3600, B3600 },
#endif
#ifdef B4800
	{ 4800, B4800 },
#endif
#ifdef B7200
	{ 7200, B7200 },
#endif
#ifdef B9600
	{ 9600, B9600 },
#endif
#ifdef EXTA
	{ 19200, EXTA },
#endif
#ifdef EXTB
	{ 38400, EXTB },
#endif
	{ 0, 0 }
};

findspeed(speed)
	register int speed;
{
	register struct sg_spds *sp;

	sp = spds;
	while (sp->sp_val && sp->sp_val != speed)
		sp++;
	return (sp->sp_name);
}

struct in_addr inet_makeaddr();

getaddr(s, sin)
	char *s;
	register struct sockaddr_in *sin;
{
	register struct hostent *hp;
	register struct netent *np;
	int val;

	bzero(sin, sizeof(struct sockaddr_in));
	hp = gethostbyname(s);
	if (hp) {
		sin->sin_family = hp->h_addrtype;
		bcopy(hp->h_addr, (char *) &sin->sin_addr, hp->h_length);
		return;
	}
	np = getnetbyname(s);
	if (np) {
		sin->sin_family = np->n_addrtype;
		sin->sin_addr = inet_makeaddr(np->n_net, INADDR_ANY);
		return;
	}
	sin->sin_family = AF_INET;
	val = inet_addr(s);
	if (val != -1) {
		sin->sin_addr.s_addr = val;
		return;
	}
	val = inet_network(s);
	if (val != -1) {
		sin->sin_addr = inet_makeaddr(val, INADDR_ANY);
		return;
	}
	error(1, 0, "%s: bad value\n", s);
}
