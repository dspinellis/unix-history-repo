#include <stdio.h>
#include <netdb.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <sys/errno.h>

#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>

#define MAXHOPS	10
char srcrt[4 + MAXHOPS * sizeof(struct in_addr)];
struct	sockaddr_in dst = { AF_INET };

int	nhops;
int	sock;
int	sendit();
extern	int errno;
char	*service = "echo";

main(argc, argv)
	char *argv[];
{
	int n, i;
	struct hostent *hp;
	struct servent *sp;
	struct protoent *pp;
	char buf[4096];
	char *cp = srcrt;
	struct sockaddr_in from;
	struct in_addr in;
	int noopt = 0;
	
	argc--, argv++;
	while (argc && argv[0][0] == '-') {
		switch (argv[0][1]) {

		case 'n':
			noopt++;
			break;
		default:
			goto usage;
		}
		argc--, argv++;
	}
	if (argc == 0) {
usage:
		fprintf(stderr, "usage: ipsrcrt [ -n ] [ gateway ... ] dest\n");
		exit(1);
	}
	*cp++ = IPOPT_NOP;
	*cp++ = IPOPT_LSRR;
	*cp++ = 3;
	*cp++ = IPOPT_MINOFF;
	while (argc) {
		if (nhops == MAXHOPS) {
			fprintf(stderr, "too many hops, limit %d\n", MAXHOPS);
			exit(1);
		}
		if ((in.s_addr = inet_addr(*argv)) != -1)
			bcopy((char *)&in, cp, sizeof(struct in_addr));
		else {
			hp = gethostbyname(*argv);
			if (hp == 0) {
				fprintf(stderr, "%s: unknown host\n", *argv);
				exit(1);
			}
			bcopy(hp->h_addr, cp, sizeof(struct in_addr));
		}
		cp += sizeof(struct in_addr);
		nhops++;
		argc--; argv++;
	}
	if ((sp = getservbyname(service, "tcp")) == 0) {
		fprintf(stderr, "%s: unknown service\n", service);
		exit(10);
	}
	dst.sin_port = sp->s_port;
	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock < 0) {
		perror("socket");
		exit(10);
	}
	if ((pp = getprotobyname("ip")) == 0) {
		fprintf(stderr, "ip: unknown protocol\n");
		exit(10);
	}
	srcrt[IPOPT_OLEN + 1] = cp - srcrt - 1;		/* -1 is NOP */
/*	*cp++ = IPOPT_EOL; */
	if (!noopt) {
		n = 4 + nhops * sizeof(struct in_addr);
		printf("options %d long\n", n);
		for (i = 0; i < (n+3)/4; i++)
			printf("%x ", ((int *)srcrt)[i]);
		printf("\n");
		if (setsockopt(sock, pp->p_proto, IP_OPTIONS, srcrt,
		    4 + nhops * sizeof(struct in_addr)) < 0) {
			perror("setsockopt");
			exit(10);
		}
		n = sizeof(buf);
		if (getsockopt(sock, pp->p_proto, IP_OPTIONS, buf, &n) < 0)
			perror("getsockopt");
		else {
			printf("options now %d long\n", n);
			for (i = 0; i < (n+3)/4; i++)
				printf("%x ", ((int *)buf)[i]);
			printf("\n");
		}
	}
		
	bcopy(cp - sizeof(struct in_addr), &dst.sin_addr,
	    sizeof(struct in_addr));
	if (connect(sock, &dst, sizeof(dst)) < 0)
		perror("connect");
	signal(SIGALRM, sendit);
	sendit();

	for (;;) {
		if ((n = recv(sock, buf, sizeof(buf), 0)) < 0) {
			if (errno != EINTR)
				perror("recv");
		} else
		    printf("received %d from %s: \"%s\"\n", n,
			inet_ntoa(from.sin_addr), buf);
	}
}

char sbuf[100] = "Hi there";

sendit()
{

	alarm(1);
	if (send(sock, sbuf, sizeof(sbuf), 0) < 0)
		perror("sendto");
}
