#ifndef lint
static char sccsid[] = "@(#)route.c	4.6 83/01/06";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#include <net/route.h>
#include <netinet/in.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>

struct	rtentry route;
int	options;
int	s;
struct	sockaddr_in sin = { AF_INET };
struct	in_addr inet_makeaddr();

main(argc, argv)
	int argc;
	char *argv[];
{

	if (argc < 2)
		printf("usage: route [ cmd ] [ args ]\n"), exit(1);
	s = socket(AF_INET, SOCK_RAW, 0, 0);
	if (s < 0) {
		perror("route: socket");
		exit(1);
	}
	argc--, argv++;
	if (strcmp(*argv, "add") == 0)
		newroute(argc, argv);
	else if (strcmp(*argv, "delete") == 0)
		newroute(argc, argv);
	else if (strcmp(*argv, "change") == 0)
		changeroute(argc-1, argv+1);
	else
		printf("%s: huh?\n", *argv);
}

newroute(argc, argv)
	int argc;
	char *argv[];
{
	struct sockaddr_in *sin;
	char *cmd;

	if (argc < 3 || argc > 4) {
		printf("usage: %s destination gateway [ metric ]\n", argv[0]);
		return;
	}
	cmd = argv[0];
	getaddr(argv[1], &route.rt_dst);
	getaddr(argv[2], &route.rt_gateway);
	sin = (struct sockaddr_in *)&route.rt_dst;
	route.rt_flags = RTF_UP;
	if (inet_lnaof(sin->sin_addr) != 0)
		route.rt_flags |= RTF_HOST;
	if (argc > 3 && atoi(argv[3]) > 0)
		route.rt_flags |= RTF_GATEWAY;
	printf("%s %x: gateway %x, flags %x\n", cmd, sin->sin_addr,
		((struct sockaddr_in *)&route.rt_gateway)->sin_addr,
		route.rt_flags);
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

getaddr(s, sin)
	char *s;
	struct sockaddr_in *sin;
{
	struct hostent *hp;
	struct netent *np;
	u_long val;

	hp = gethostbyname(s);
	if (hp) {
		sin->sin_family = hp->h_addrtype;
		bcopy(hp->h_addr, &sin->sin_addr, hp->h_length);
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
	fprintf(stderr, "%s: bad value\n", s);
	exit(1);
}
