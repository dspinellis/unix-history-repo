#ifndef lint
static char sccsid[] = "@(#)route.c	4.14 (Berkeley) 85/06/03";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/mbuf.h>

#include <net/route.h>
#include <netinet/in.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>

struct	rtentry route;
int	s;
int	forcehost, forcenet;
struct	sockaddr_in sin = { AF_INET };
struct	in_addr inet_makeaddr();

main(argc, argv)
	int argc;
	char *argv[];
{

	if (argc < 2)
		printf("usage: route [ -f ] [ [ -h ] [ -n ] cmd args ]\n"),
		exit(1);
	s = socket(AF_INET, SOCK_RAW, 0);
	if (s < 0) {
		perror("route: socket");
		exit(1);
	}
	argc--, argv++;
	for (; argc >  0 && argv[0][0] == '-'; argc--, argv++) {
		for (argv[0]++; *argv[0]; argv[0]++)
			switch (*argv[0]) {
			case 'f':
				flushroutes();
				break;
			case 'h':
				forcehost++;
				break;
			case 'n':
				forcenet++;
				break;
			}
	}
	if (forcehost && forcenet) {
		fprintf(stderr, "-n and -h are incompatible\n");
		exit(1);
	}
	if (argc > 0) {
		if (strcmp(*argv, "add") == 0)
			newroute(argc, argv);
		else if (strcmp(*argv, "delete") == 0)
			newroute(argc, argv);
		else if (strcmp(*argv, "change") == 0)
			changeroute(argc-1, argv+1);
		else
			printf("%s: huh?\n", *argv);
	}
}

/*
 * Purge all entries in the routing tables not
 * associated with network interfaces.
 */
#include <nlist.h>

struct nlist nl[] = {
#define	N_RTHOST	0
	{ "_rthost" },
#define	N_RTNET		1
	{ "_rtnet" },
#define N_RTHASHSIZE	2
	{ "_rthashsize" },
	"",
};

flushroutes()
{
	struct mbuf mb;
	register struct rtentry *rt;
	register struct mbuf *m;
	struct mbuf **routehash;
	int rthashsize, i, doinghost = 1, kmem;
	char *routename();

	nlist("/vmunix", nl);
	if (nl[N_RTHOST].n_value == 0) {
		printf("route: \"rthost\", symbol not in namelist\n");
		exit(1);
	}
	if (nl[N_RTNET].n_value == 0) {
		printf("route: \"rtnet\", symbol not in namelist\n");
		exit(1);
	}
	if (nl[N_RTHASHSIZE].n_value == 0) {
		printf("route: \"rthashsize\", symbol not in namelist\n");
		exit(1);
	}
	kmem = open("/dev/kmem", 0);
	if (kmem < 0) {
		perror("route: /dev/kmem");
		exit(1);
	}
	lseek(kmem, nl[N_RTHASHSIZE].n_value, 0);
	read(kmem, &rthashsize, sizeof (rthashsize));
	routehash = (struct mbuf **)malloc(rthashsize*sizeof (struct mbuf *));

	lseek(kmem, nl[N_RTHOST].n_value, 0);
	read(kmem, routehash, rthashsize*sizeof (struct mbuf *));
	printf("Flushing routing tables:\n");
again:
	for (i = 0; i < rthashsize; i++) {
		if (routehash[i] == 0)
			continue;
		m = routehash[i];
		while (m) {
			lseek(kmem, m, 0);
			read(kmem, &mb, sizeof (mb));
			rt = mtod(&mb, struct rtentry *);
			if (rt->rt_flags & RTF_GATEWAY) {
				struct sockaddr_in *sin;

				sin = (struct sockaddr_in *)&rt->rt_dst;
				printf("%-15.15s ", routename(sin->sin_addr));
				sin = (struct sockaddr_in *)&rt->rt_gateway;
				printf("%-15.15s ", routename(sin->sin_addr));
				if (ioctl(s, SIOCDELRT, (caddr_t)rt) < 0)
					error("delete");
				else
					printf("done\n");
			}
			m = mb.m_next;
		}
	}
	if (doinghost) {
		lseek(kmem, nl[N_RTNET].n_value, 0);
		read(kmem, routehash, rthashsize*sizeof (struct mbuf *));
		doinghost = 0;
		goto again;
	}
	close(kmem);
	free(routehash);
}

char *
routename(in)
	struct in_addr in;
{
	char *cp = 0;
	static char line[50];
	struct hostent *hp;
	struct netent *np;
	int lna, net;

	net = inet_netof(in);
	lna = inet_lnaof(in);
	if (in.s_addr == INADDR_ANY)
		cp = "default";
	if (cp == 0 && (lna == INADDR_ANY || forcenet)) {
		np = getnetbyaddr(net, AF_INET);
		if (np)
			cp = np->n_name;
	}
	if (cp == 0) {
		hp = gethostbyaddr(&in, sizeof (struct in_addr), AF_INET);
		if (hp)
			cp = hp->h_name;
	}
	if (cp)
		strcpy(line, cp);
	else {
		u_char *ucp = (u_char *)&in;
		if (lna == INADDR_ANY)
			sprintf(line, "%u.%u.%u", ucp[0], ucp[1], ucp[2]);
		else
			sprintf(line, "%u.%u.%u.%u", ucp[0], ucp[1],
				ucp[2], ucp[3]);
	}
	return (line);
}

newroute(argc, argv)
	int argc;
	char *argv[];
{
	struct sockaddr_in *sin;
	char *cmd;
	int ishost, metric = 0;

	cmd = argv[0];
	if (*cmd == 'a') {
		if (argc != 4) {
			printf("usage: %s destination gateway metric\n", cmd);
			printf("(metric of 0 if gateway is this host)\n");
			return;
		}
		metric = atoi(argv[3]);
	} else {
		if (argc != 3) {
			printf("usage: %s destination gateway\n", cmd);
			return;
		}
	}
	ishost = getaddr(argv[1], &route.rt_dst);
	if (forcehost)
		ishost = 1;
	if (forcenet)
		ishost = 0;
	(void) getaddr(argv[2], &route.rt_gateway);
	sin = (struct sockaddr_in *)&route.rt_dst;
	route.rt_flags = RTF_UP;
	if (ishost)
		route.rt_flags |= RTF_HOST;
	if (metric > 0)
		route.rt_flags |= RTF_GATEWAY;
	printf("%s %s: gateway ", cmd, routename(sin->sin_addr));
	sin = (struct sockaddr_in *)&route.rt_gateway;
	printf("%s, flags %x\n", routename(sin->sin_addr), route.rt_flags);
	if (ioctl(s, *cmd == 'a' ? SIOCADDRT : SIOCDELRT, (caddr_t)&route))
		error(cmd);
}

changeroute(argc, argv)
	int argc;
	char *argv[];
{
	printf("not supported\n");
}

error(cmd)
	char *cmd;
{
	extern int errno;

	if (errno == ESRCH)
		fprintf(stderr, "not in table\n");
	else if (errno == EBUSY)
		fprintf(stderr, "entry in use\n");
	else if (errno == ENOBUFS)
		fprintf(stderr, "routing table overflow\n");
	else
		perror(cmd);
}

/*
 * Interpret an argument as a network address of some kind,
 * returning 1 if a host address, 0 if a network address.
 */
getaddr(s, sin)
	char *s;
	struct sockaddr_in *sin;
{
	struct hostent *hp;
	struct netent *np;
	u_long val;

	if (strcmp(s, "default") == 0) {
		sin->sin_family = AF_INET;
		sin->sin_addr = inet_makeaddr(0, INADDR_ANY);
		return(0);
	}
	np = getnetbyname(s);
	if (np) {
		sin->sin_family = np->n_addrtype;
		sin->sin_addr = inet_makeaddr(np->n_net, INADDR_ANY);
		return(0);
	}
	hp = gethostbyname(s);
	if (hp) {
		sin->sin_family = hp->h_addrtype;
		bcopy(hp->h_addr, &sin->sin_addr, hp->h_length);
		return(1);
	}
	sin->sin_family = AF_INET;
	val = inet_addr(s);
	if (val != -1) {
		sin->sin_addr.s_addr = val;
		return(inet_lnaof(sin->sin_addr) != INADDR_ANY);
	}
	val = inet_network(s);
	if (val != -1) {
		sin->sin_addr = inet_makeaddr(val, INADDR_ANY);
		return(0);
	}
	fprintf(stderr, "%s: bad value\n", s);
	exit(1);
}
