#include <stdio.h>
#include <netdb.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/errno.h>

#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>

#define MAXHOPS	10
char srcrt[4 + MAXHOPS * sizeof(struct in_addr)];
struct	sockaddr_in dst = { AF_INET };

int	nhops;
int	sock;
int	send();
int	sent, recvd;
extern	int errno;
char	*service = "echo";

struct {
	struct timeval t;
	int n;
	char sbuf[100];
} rbuf, sbuf = { {0}, "Hi there"};

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
	struct timeval now;
	int noopt = 0, loose = 0;
	
	argc--, argv++;
	while (argc && argv[0][0] == '-') {
		switch (argv[0][1]) {

		case 'l':
			loose++;
			break;
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
	if (loose)
		*cp++ = IPOPT_LSRR;
	else
		*cp++ = IPOPT_SSRR;
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
	if ((sp = getservbyname(service, "udp")) == 0) {
		fprintf(stderr, "%s: unknown service\n", service);
		exit(10);
	}
	dst.sin_port = sp->s_port;
	sock = socket(AF_INET, SOCK_DGRAM, 0);
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
	signal(SIGALRM, send);
	send();

	for (;;) {
		if ((n = recvfrom(sock, &rbuf, sizeof(rbuf), 0, &from,
		    sizeof(from))) < 0) {
			if (errno != EINTR)
				perror("recvfrom");
		} else {
			gettimeofday(&now);
			recvd++;
			printf("%d bytes from %s: seq=%d. time=", n,
				inet_ntoa(from.sin_addr), rbuf.n);
			timevalsub(&now, &rbuf.t);
			if (now.tv_sec)
				printf("%d.%03d sec.\n", now.tv_sec,
					now.tv_usec/1000);
			else
				printf("%d.%d ms\n", now.tv_usec/1000,
					now.tv_usec/10000);
		}
	}
}

send()
{

	alarm(1);
	sbuf.n = sent++;
	gettimeofday(&sbuf.t);
	if (sendto(sock, &sbuf, sizeof(sbuf), 0, &dst, sizeof(dst)) < 0)
		perror("sendto");
}

timevalsub(t1, t2)
	struct timeval *t1, *t2;
{

	t1->tv_sec -= t2->tv_sec;
	t1->tv_usec -= t2->tv_usec;
	timevalfix(t1);
}

timevalfix(t1)
	struct timeval *t1;
{

	if (t1->tv_usec < 0) {
		t1->tv_sec--;
		t1->tv_usec += 1000000;
	}
	while (t1->tv_usec >= 1000000) {
		t1->tv_sec++;
		t1->tv_usec -= 1000000;
	}
}
