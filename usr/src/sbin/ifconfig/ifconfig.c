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
static char sccsid[] = "@(#)ifconfig.c	4.12 (Berkeley) %G%";
#endif not lint


#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#include <net/if.h>
#include <netinet/in.h>

#define	NSIP
#include <netns/ns.h>
#include <netns/ns_if.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>

extern int errno;
struct	ifreq ifr;
struct	sockaddr_in sin = { AF_INET };
struct	sockaddr_in broadaddr;
struct	sockaddr_in netmask = { AF_INET };
struct	sockaddr_in ipdst = { AF_INET };
char	name[30];
int	flags;
int	setaddr;
int	setmask;
int	setbroadaddr;
int	setipdst;
int	s;
extern	int errno;

int	setifflags(), setifaddr(), setifdstaddr(), setifnetmask();
int	setifbroadaddr(), setifipdst();

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
	{ "ipdst",	NEXTARG,	setifipdst },
	{ 0,		0,		setifaddr },
	{ 0,		0,		setifdstaddr },
};

/*
 * XNS support liberally adapted from
 * code written at the University of Maryland
 * principally by James O'Toole and Chris Torek.
 */

int	in_status(), in_getaddr();
int	xns_status(), xns_getaddr();

/* Known address families */
struct afswtch {
	char *af_name;
	short af_af;
	int (*af_status)();
	int (*af_getaddr)();
} afs[] = {
	{ "inet",	AF_INET,	in_status,	in_getaddr },
	{ "ns",		AF_NS,		xns_status,	xns_getaddr },
	{ 0,		0,		0,		0 }
};

struct afswtch *afp;	/*the address family being set or asked about*/

main(argc, argv)
	int argc;
	char *argv[];
{
	int af = AF_INET;
	if (argc < 2) {
		fprintf(stderr, "usage: ifconfig interface [ af %s %s %s %s\n",
		    "[ address [ dest_addr ] ] [ up ] [ down ]",
		    "[ netmask mask ] ]",
		    "[ trailers | -trailers ]",
		    "[ arp | -arp ] ]");
		exit(1);
	}
	argc--, argv++;
	strncpy(name, *argv, sizeof(name - 1));
	name[sizeof name - 1] = 0;
	strncpy(ifr.ifr_name, name, sizeof(ifr.ifr_name));
	argc--, argv++;
	if (argc > 0) {
		struct afswtch *myafp;
		
		for (myafp = afp = afs; myafp->af_name; myafp++)
			if (strcmp(myafp->af_name, *argv) == 0) {
				afp = myafp; argc--; argv++;
				break;
			}
		af = ifr.ifr_addr.sa_family = afp->af_af;
	}
	s = socket(af, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("ifconfig: socket");
		exit(1);
	}
	if (ioctl(s, SIOCGIFFLAGS, (caddr_t)&ifr) < 0) {
		Perror("ioctl (SIOCGIFFLAGS)");
		exit(1);
	}
	strncpy(ifr.ifr_name, name, sizeof ifr.ifr_name);
	flags = ifr.ifr_flags;
	if (af == AF_INET) {
		if (ioctl(s, SIOCGIFNETMASK, (caddr_t)&ifr) < 0) {
			if (errno != EADDRNOTAVAIL)
				Perror("ioctl (SIOCGIFNETMASK)");
		} else
			netmask.sin_addr =
			      ((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr;
		strncpy(ifr.ifr_name, name, sizeof ifr.ifr_name);
	}
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
	if ((setmask || setaddr) && (af == AF_INET)){
		/*
		 * If setting the address and not the mask,
		 * clear any existing mask and the kernel will then
		 * assign the default.  If setting both,
		 * set the mask first, so the address will be
		 * interpreted correctly.
		 */
		ifr.ifr_addr = *(struct sockaddr *)&netmask;
		if (ioctl(s, SIOCSIFNETMASK, (caddr_t)&ifr) < 0)
			Perror("ioctl (SIOCSIFNETMASK)");
	}
	if (setipdst && af==AF_NS) {
		struct nsip_req rq;
		int size = sizeof(rq);

		rq.rq_ns = *(struct sockaddr *) &sin;
		rq.rq_ip = *(struct sockaddr *) &ipdst;

		if (setsockopt(s, 0, SO_NSIP_ROUTE, &rq, size) < 0)
			Perror("Encapsulation Routing");
		setaddr = 0;
	}
	if (setaddr) {
		ifr.ifr_addr = *(struct sockaddr *) &sin;
		if (ioctl(s, SIOCSIFADDR, (caddr_t)&ifr) < 0)
			Perror("ioctl (SIOCSIFADDR)");
	}
	if (setbroadaddr) {
		ifr.ifr_addr = *(struct sockaddr *)&broadaddr;
		if (ioctl(s, SIOCSIFBRDADDR, (caddr_t)&ifr) < 0)
			Perror("ioctl (SIOCSIFBRDADDR)");
	}
	exit(0);
}

/*ARGSUSED*/
setifaddr(addr, param)
	char *addr;
	short param;
{
	/*
	 * Delay the ioctl to set the interface addr until flags are all set.
	 * The address interpretation may depend on the flags,
	 * and the flags may change when the address is set.
	 */
	setaddr++;
	(*afp->af_getaddr)(addr, &sin);
}

setifnetmask(addr)
	char *addr;
{
	in_getaddr(addr, &netmask);
	setmask++;
}

setifbroadaddr(addr)
	char *addr;
{
	(*afp->af_getaddr)(addr, &broadaddr);
	setbroadaddr++;
}

setifipdst(addr)
	char *addr;
{
	in_getaddr(addr, &ipdst);
	setipdst++;
}

/*ARGSUSED*/
setifdstaddr(addr, param)
	char *addr;
	int param;
{

	(*afp->af_getaddr)(addr, &ifr.ifr_addr);
	if (ioctl(s, SIOCSIFDSTADDR, (caddr_t)&ifr) < 0)
		Perror("ioctl (SIOCSIFDSTADDR)");
}

setifflags(vname, value)
	char *vname;
	short value;
{
 	if (ioctl(s, SIOCGIFFLAGS, (caddr_t)&ifr) < 0) {
 		Perror("ioctl (SIOCGIFFLAGS)");
 		exit(1);
 	}
	strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
 	flags = ifr.ifr_flags;

	if (value < 0) {
		value = -value;
		flags &= ~value;
	} else
		flags |= value;
	ifr.ifr_flags = flags;
	if (ioctl(s, SIOCSIFFLAGS, (caddr_t)&ifr) < 0)
		Perror(vname);
}

/*
 * Print the status of the interface.  If an address family was
 * specified, show it and it only; otherwise, show them all.
 */
status()
{
	register struct afswtch *p = afp;
	short af = ifr.ifr_addr.sa_family;

	if ((p = afp) != NULL) {
		(*p->af_status)();
		return;
	}
	for (p = afs; p->af_name; p++) {
		ifr.ifr_addr.sa_family = p->af_af;
		(*p->af_status)();
	}
}

#define	IFFBITS \
"\020\1UP\2BROADCAST\3DEBUG\4ROUTE\5POINTOPOINT\6NOTRAILERS\7RUNNING\10NOARP\
\11LOCAL"

in_status()
{
	struct sockaddr_in *sin;
	char *inet_ntoa();

	if (ioctl(s, SIOCGIFADDR, (caddr_t)&ifr) < 0) {
		if (errno == EADDRNOTAVAIL)
			bzero((char *)&ifr.ifr_addr, sizeof(ifr.ifr_addr));
		else
			Perror("ioctl (SIOCGIFADDR)");
	}
	strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
	sin = (struct sockaddr_in *)&ifr.ifr_addr;
	printf("%s: %s ", name, inet_ntoa(sin->sin_addr));
	if (flags & IFF_POINTOPOINT) {
		if (ioctl(s, SIOCGIFDSTADDR, (caddr_t)&ifr) < 0) {
			if (errno == EADDRNOTAVAIL)
			    bzero((char *)&ifr.ifr_addr, sizeof(ifr.ifr_addr));
			else
			    Perror("ioctl (SIOCGIFDSTADDR)");
		}
		strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		sin = (struct sockaddr_in *)&ifr.ifr_dstaddr;
		printf("--> %s ", inet_ntoa(sin->sin_addr));
	}
	printf("netmask %x ", ntohl(netmask.sin_addr.s_addr));
	printb("flags", flags, IFFBITS); putchar('\n');
	if (flags & IFF_BROADCAST) {
		if (ioctl(s, SIOCGIFBRDADDR, (caddr_t)&ifr) < 0) {
			if (errno == EADDRNOTAVAIL)
				return;
			Perror("ioctl (SIOCGIFADDR)");
		}
		strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		sin = (struct sockaddr_in *)&ifr.ifr_addr;
		printf("broadcast: %s\n", inet_ntoa(sin->sin_addr));
	}
}


xns_status()
{
	struct sockaddr_ns *sns;
	char *xns_ntoa();

	close(s);
	s = socket(AF_NS, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("ifconfig: socket");
		exit(1);
	}
	if (ioctl(s, SIOCGIFADDR, (caddr_t)&ifr) < 0) {
		if (errno == EAFNOSUPPORT)
			return;
		Perror("ioctl (SIOCGIFADDR)");
	}
	strncpy(ifr.ifr_name, name, sizeof ifr.ifr_name);
	sns = (struct sockaddr_ns *)&ifr.ifr_addr;
	printf("%s: xns %s ", name, xns_ntoa(sns));
	printb("flags", flags, IFFBITS);
	putchar('\n');
}

Perror(cmd)
	char *cmd;
{
	extern int errno;

	fprintf(stderr, "ifconfig: ");
	switch (errno) {

	case ENXIO:
		fprintf(stderr, "%s: no such interface\n", cmd);
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

in_getaddr(s, saddr)
	char *s;
	struct sockaddr *saddr;
{
	register struct sockaddr_in *sin = (struct sockaddr_in *)saddr;
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

#define setxnnet(a,b) {a = * (union ns_net *) &(b);}

xns_getaddr(addr, saddr)
char *addr;
struct sockaddr *saddr;
{
	register struct sockaddr_ns *sns = (struct sockaddr_ns *)saddr;
	u_long netnum;
	char *index();
	register char *s = index(addr, ':');
	register int i;

	if (s!=NULL) *s = 0;
	netnum = atoi(addr);
	netnum = htonl(netnum);
	setxnnet(sns->sns_addr.x_net, netnum);
	sns->sns_family = AF_NS;

	for (i = 0; i < 6; i++) {
		if (s == NULL || *++s == 0) 
			break;
		sns->sns_addr.x_host.c_host[i] = xtoi(s);
		s = index(s, '.');
	}

}

char *
xns_ntoa(sns)
register struct sockaddr_ns *sns;
{
	static char buf[30];

	sprintf (buf, "%d:%x.%x.%x.%x.%x.%x",
	    ntohl(ns_netof(sns->sns_addr)),
	    sns->sns_addr.x_host.c_host[0], sns->sns_addr.x_host.c_host[1],
	    sns->sns_addr.x_host.c_host[2], sns->sns_addr.x_host.c_host[3],
	    sns->sns_addr.x_host.c_host[4], sns->sns_addr.x_host.c_host[5]);
	return (buf);
}

int xtoi(s)
register char *s;
{
	register int res = 0, delta;
	register char *cp;
	static char base[] = "0123456789ABCDEFabcdef";

	for(; *s; s++) {
		cp = index(base, *s);	
		if (cp==NULL)
			break;
		if ((delta = cp - base) > 15)
			delta -= 6;
		res = (res << 4) + delta;
	}
	return(res);
}
