#ifndef lint
static char sccsid[] = "@(#)route.c	4.1 82/04/02";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/route.h>
#include <net/in.h>
#include <errno.h>
#include <ctype.h>

struct	rtentry route;
int	options;
int	s;
struct	sockaddr_in sin = { AF_INET };

main(argc, argv)
	int argc;
	char *argv[];
{

	if (argc < 2)
		printf("usage: route [ cmd ] [ args ]\n"), exit(1);
	s = socket(SOCK_RAW, 0, 0, options);
	if (s < 0) {
		perror("socket");
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

	if (argc < 3) {
		printf("usage: %s destination gateway\n", argv[0]);
		return;
	}
	cmd = *argv++;
	getaddr(*argv++, &route.rt_dst);
	getaddr(*argv, &route.rt_gateway);
	sin = (struct sockaddr_in *)&route.rt_dst;
	route.rt_flags = RTF_UP;
	if (sin->sin_addr.s_host || sin->sin_addr.s_imp)
		route.rt_flags |= RTF_HOST;
	printf("%s %x: gateway %x\n", cmd, sin->sin_addr,
		((struct sockaddr_in *)&route.rt_gateway)->sin_addr);
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
	sin->sin_family = AF_INET;
	if ((sin->sin_addr.s_addr = rhost(&s)) != -1)
		return;
	if (!isdigit(*s)) {
		fprintf(stderr, "%s: unknown host\n", s);
		exit(1);
	}
	sin->sin_addr.s_addr = atoi(s);
	if (sin->sin_addr.s_addr == -1) {
		fprintf(stderr, "%s: bad value\n", s);
		exit(1);
	}
}
