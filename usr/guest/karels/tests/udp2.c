#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

int conn;

main(argc, argv)
	char *argv[];
{
	struct sockaddr_in sin, sin2;
	struct hostent *hp;
	struct servent *sp;
	short port;
	int s, buflen = 1024;
	register int count = 1024;
	char buf[BUFSIZ], *ntoa();
	char *myname;

	myname = argv[0];
	argc--, argv++;
	for (; argc >  0 && argv[0][0] == '-'; argc--, argv++) {
		for (argv[0]++; *argv[0]; argv[0]++)
			switch (*argv[0]) {
			case 'c':
				conn++;
				break;
			default:
				goto usage;
			}
	}
	if (argc < 2) {
usage:
		printf("usage: %s [ -c ] port host host2\n",
		    myname);
		exit(1);
	}
	sp = getservbyname(argv[0], "udp");
	if (sp)
		port = sp->s_port;
	else {
		port = atoi(argv[0]);
		if (port == 0) {
			printf("%s: no service\n", argv[0]);
			exit(1);
		}
		port = htons(port);
	}
	bzero(&sin, sizeof (sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = inet_addr(argv[1]);
	if (sin.sin_addr.s_addr == -1) {
		hp = gethostbyname(argv[1]);
		if (hp) {
			sin.sin_family = hp->h_addrtype;
			bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);
		} else {
			printf("unknown host %s\n", argv[1]);
			return;
		}
	}
	argv++;
	bzero(&sin2, sizeof (sin2));
	sin2.sin_family = AF_INET;
	sin2.sin_addr.s_addr = inet_addr(argv[1]);
	if (sin2.sin_addr.s_addr == -1) {
		hp = gethostbyname(argv[1]);
		if (hp) {
			sin2.sin_family = hp->h_addrtype;
			bcopy(hp->h_addr, &sin2.sin_addr, hp->h_length);
		} else {
			printf("unknown host %s\n", argv[1]);
			return;
		}
	}

	sin.sin_port = port;
	sin2.sin_port = port;
	s = socket(SOCK_DGRAM, AF_INET, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	if (conn) {
		int warned = 0;

		if (connect(s, &sin, sizeof (sin)) < 0)
			perror("connect");
		printf("send %s.%d\n", ntoa(sin.sin_addr),
			(unsigned)ntohs(sin.sin_port));
		while (count--)
			if (send(s, buf, buflen, 0) < 0 && !warned++)
				perror("send");
	} else {
		printf("sendto %s.%d\n", ntoa(sin.sin_addr),
			(unsigned)ntohs(sin.sin_port));
		while (count -= 2) {
			if (sendto(s, buf, buflen, 0, &sin, sizeof (sin)) < 0)
				perror("sendto");
			if (sendto(s, buf, buflen, 0, &sin2, sizeof (sin2)) < 0)
				perror("sendto");
		}
	}
}

/*
 * Convert network-format internet address
 * to base 256 d.d.d.d representation.
 */
char *
ntoa(in)
	struct in_addr in;
{
	static char b[18];
	register char *p;

	p = (char *)&in;
#define	UC(b)	(((int)b)&0xff)
	sprintf(b, "%d.%d.%d.%d", UC(p[0]), UC(p[1]), UC(p[2]), UC(p[3]));
	return (b);
}
