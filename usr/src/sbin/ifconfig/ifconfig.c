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
static char sccsid[] = "@(#)ifconfig.c	4.11 (Berkeley) %G%";
#endif not lint


#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#include <netinet/in.h>
#include <net/if.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>

struct	ifreq ifr;
struct	sockaddr_in sin = { AF_INET };
struct	sockaddr_in broadaddr;
struct	sockaddr_in netmask = { AF_INET };
char	name[30];
int	flags;
int	setaddr;
int	setmask;
int	setbroadaddr;
int	s;
extern	int errno;

int	setifflags(), setifaddr(), setifdstaddr(), setifnetmask();
int	setifbroadaddr();

#define	NEXTARG		0xffffff

struct	cmd {
	char	*c_name;
	int	c_parameter;		/* NEXTARG means next argv */
	int	(*c_func)();
} cmds[] = {
	{ "up",		IFF_UP,		setifflags } ,
	{ "down",	-IFF_UP,	setifflags },
	{ "trailers",	-IFF_NOTRAILERS,setifflags },
	{ "-trailers",	IFF_NOTRAILERS,	setifflags },
	{ "arp",	-IFF_NOARP,	setifflags },
	{ "-arp",	IFF_NOARP,	setifflags },
#ifdef IFF_LOCAL
	{ "local",	IFF_LOCAL,	setifflags },
	{ "-local",	-IFF_LOCAL,	setifflags },
#endif
	{ "debug",	IFF_DEBUG,	setifflags },
	{ "-debug",	-IFF_DEBUG,	setifflags },
#ifdef notdef
#define	EN_SWABIPS	0x1000
	{ "swabips",	EN_SWABIPS,	setifflags },
	{ "-swabips",	-EN_SWABIPS,	setifflags },
#endif
	{ "netmask",	NEXTARG,	setifnetmask },
	{ "broadcast",	NEXTARG,	setifbroadaddr },
	{ 0,		0,		setifaddr },
	{ 0,		0,		setifdstaddr },
};

main(argc, argv)
	int argc;
	char *argv[];
{

	if (argc < 2) {
		fprintf(stderr, "usage: ifconfig interface %s %s %s\n",
		    "[ address [ dest_addr ] ] [ up ] [ down ]",
		    "[ netmask mask ]",
		    "[ trailers | -trailers ]",
		    "[ arp | -arp ]");
		exit(1);
	}
	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("ifconfig: socket");
		exit(1);
	}
	argc--, argv++;
	strcpy(name, *argv);
	strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
	if (ioctl(s, SIOCGIFFLAGS, (caddr_t)&ifr) < 0) {
		Perror("ioctl (SIOCGIFFLAGS)");
		exit(1);
	}
	flags = ifr.ifr_flags;
	strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
	if (ioctl(s, SIOCGIFNETMASK, (caddr_t)&ifr) < 0) {
		if (errno != EADDRNOTAVAIL)
			Perror("ioctl (SIOCGIFNETMASK)");
	} else
		netmask.sin_addr = ((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr;
	argc--, argv++;
	if (argc == 0) {
		status();
		exit(0);
	}
	while (argc > 0) {
		register struct cmd *p;

		for (p = cmds; p->c_name; p++)
			if (strcmp(*argv, p->c_name) == 0)
				break;
		if (p->c_name == 0 && setaddr)
			p++;	/* got src, do dst */
		if (p->c_func) {
			if (p->c_parameter == NEXTARG) {
				(*p->c_func)(argv[1]);
				argc--, argv++;
			} else
				(*p->c_func)(*argv, p->c_parameter);
		}
		argc--, argv++;
	}
	if (setmask || setaddr) {
		/*
		 * If setting the address and not the mask,
		 * clear any existing mask and the kernel will then
		 * assign the default.  If setting both,
		 * set the mask first, so the address will be
		 * interpreted correctly.
		 */
		strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		ifr.ifr_addr = *(struct sockaddr *)&netmask;
		if (ioctl(s, SIOCSIFNETMASK, (caddr_t)&ifr) < 0)
			Perror("ioctl (SIOCSIFNETMASK)");
	}
	if (setaddr) {
		strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		ifr.ifr_addr = *(struct sockaddr *) &sin;
		if (ioctl(s, SIOCSIFADDR, (caddr_t)&ifr) < 0)
			Perror("ioctl (SIOCSIFADDR)");
	}
	if (setbroadaddr) {
		strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		ifr.ifr_addr = *(struct sockaddr *)&broadaddr;
		if (ioctl(s, SIOCSIFBRDADDR, (caddr_t)&ifr) < 0)
			Perror("ioctl (SIOCSIFBRDADDR)");
	}
	exit(0);
}

/*ARGSUSED*/
setifaddr(addr, param)
	char *addr;
	int param;
{

	getaddr(addr, &sin);
	/*
	 * Delay the ioctl to set the interface addr until flags are all set.
	 * The address interpretation may depend on the flags,
	 * and the flags may change when the address is set.
	 */
	setaddr++;
}

setifnetmask(addr)
	char *addr;
{

	getaddr(addr, &netmask);
	setmask++;
}

setifbroadaddr(addr)
	char *addr;
{
	getaddr(addr, &broadaddr);
	setbroadaddr++;
}

/*ARGSUSED*/
setifdstaddr(addr, param)
	char *addr;
	int param;
{

	getaddr(addr, (struct sockaddr_in *)&ifr.ifr_dstaddr);
	strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
	if (ioctl(s, SIOCSIFDSTADDR, (caddr_t)&ifr) < 0)
		Perror("ioctl (SIOCSIFDSTADDR)");
}

setifflags(vname, value)
	char *vname;
	int value;
{

	if (value < 0) {
		value = -value;
		flags &= ~value;
	} else
		flags |= value;
	strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
	ifr.ifr_flags = flags;
	if (ioctl(s, SIOCSIFFLAGS, (caddr_t)&ifr) < 0)
		Perror(vname);
}

status()
{
	struct sockaddr_in *sin;

	strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
	if (ioctl(s, SIOCGIFADDR, (caddr_t)&ifr) < 0) {
		if (errno == EADDRNOTAVAIL)
			bzero((char *)&ifr.ifr_addr, sizeof(ifr.ifr_addr));
		else
			Perror("ioctl (SIOCGIFADDR)");
	}
	sin = (struct sockaddr_in *)&ifr.ifr_addr;
	printf("%s: %s ", name, inet_ntoa(sin->sin_addr));
	if (flags & IFF_POINTOPOINT) {
		strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		if (ioctl(s, SIOCGIFDSTADDR, (caddr_t)&ifr) < 0) {
			if (errno == EADDRNOTAVAIL)
			    bzero((char *)&ifr.ifr_addr, sizeof(ifr.ifr_addr));
			else
			    Perror("ioctl (SIOCGIFDSTADDR)");
		}
		sin = (struct sockaddr_in *)&ifr.ifr_dstaddr;
		printf("--> %s ", inet_ntoa(sin->sin_addr));
	}
	printf("netmask %x ", ntohl(netmask.sin_addr.s_addr));
#define	IFFBITS \
"\020\1UP\2BROADCAST\3DEBUG\4ROUTE\5POINTOPOINT\6NOTRAILERS\7RUNNING\10NOARP\
\11LOCAL"
	printb("flags", flags, IFFBITS); putchar('\n');
	if (flags & IFF_BROADCAST) {
		strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		if (ioctl(s, SIOCGIFBRDADDR, (caddr_t)&ifr) < 0) {
			if (errno == EADDRNOTAVAIL)
				return;
			Perror("ioctl (SIOCGIFADDR)");
		}
		sin = (struct sockaddr_in *)&ifr.ifr_addr;
		printf("broadcast: %s\n", inet_ntoa(sin->sin_addr));
	}
}

Perror(cmd)
	char *cmd;
{
	extern int errno;

	fprintf(stderr, "ifconfig: ");
	switch (errno) {

	case ENXIO:
		fprintf(stderr, "%s: ", cmd);
		fprintf(stderr, "no such interface\n");
		break;

	case EPERM:
		fprintf(stderr, "%s: permission denied\n", cmd);
		break;

	default:
		perror(cmd);
	}
	exit(1);
}

struct	in_addr inet_makeaddr();

getaddr(s, sin)
	char *s;
	struct sockaddr_in *sin;
{
	struct hostent *hp;
	struct netent *np;
	int val;

	sin->sin_family = AF_INET;
	val = inet_addr(s);
	if (val != -1) {
		sin->sin_addr.s_addr = val;
		return;
	}
	hp = gethostbyname(s);
	if (hp) {
		sin->sin_family = hp->h_addrtype;
		bcopy(hp->h_addr, (char *)&sin->sin_addr, hp->h_length);
		return;
	}
	np = getnetbyname(s);
	if (np) {
		sin->sin_family = np->n_addrtype;
		sin->sin_addr = inet_makeaddr(np->n_net, INADDR_ANY);
		return;
	}
	fprintf(stderr, "%s: bad value\n", s);
	exit(1);
}

/*
 * Print a value a la the %b format of the kernel's printf
 */
printb(s, v, bits)
	char *s;
	register char *bits;
	register unsigned short v;
{
	register int i, any = 0;
	register char c;

	if (bits && *bits == 8)
		printf("%s=%o", s, v);
	else
		printf("%s=%x", s, v);
	bits++;
	if (bits) {
		putchar('<');
		while (i = *bits++) {
			if (v & (1 << (i-1))) {
				if (any)
					putchar(',');
				any = 1;
				for (; (c = *bits) > 32; bits++)
					putchar(c);
			} else
				for (; *bits > 32; bits++)
					;
		}
		putchar('>');
	}
}
