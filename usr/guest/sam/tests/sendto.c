#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

main(argc, argv)
	char *argv[];
{
	struct sockaddr_in sin;
	struct hostent *hp;
	struct servent *sp;
	int s, buflen;
	char buf[BUFSIZ], *ntoa();

	if (argc < 2) {
		printf("usage: mesg service\n");
		exit(1);
	}
	sp = getservbyname(argv[1], "udp");
	if (sp == NULL)   {
		printf("biff: no service\n");
		exit(1);
	}
	hp = gethostbyname("localhost");
	if (hp == NULL) {
		printf("localhost: no host entry\n");
		exit(1);
	}
	bzero(&sin, sizeof (sin));
	sin.sin_family = hp->h_addrtype;
	sin.sin_port = sp->s_port;
	bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);
	s = socket(SOCK_DGRAM, AF_INET, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	printf("Test message?\n");
	fflush(stdout);
	if (fgets(buf, sizeof (buf), stdin) == NULL) {
		printf("No text?\n");
		exit(1);
	}
	buflen = strlen(buf);
	printf("sendto %s.%d\n", ntoa(sin.sin_addr),
		(unsigned)ntohs(sin.sin_port));
	if (sendto(s, buf, buflen, 0, &sin, sizeof (sin)) < 0)
		perror("sendto");
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
