#include <stdio.h>
#include <ctype.h>
#include <netdb.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/errno.h>

#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>

int	nhops;
int	sock;
int	sendit();
int	sent, recvd;
int	conn;
extern	int errno;
char	*service = "echo";

struct {
	struct timeval t;
	int n;
	char sbuf[100];
} rbuf, sbuf = { {0}, 0, "Hi there"};
struct sockaddr_in dst;

main(argc, argv)
	char *argv[];
{
	int n, i;
	int options = 0, on = 1;
	struct hostent *hp;
	struct servent *sp;
	char buf[4096];
	struct sockaddr_in from;
	struct in_addr in;
	struct timeval now;
	char *cp;
	
	if (argc == 1) {
usage:
		fprintf(stderr, "usage: %s [ -c ] [service ] dest\n", argv[0]);
		exit(1);
	}
	argc--, argv++;
	if (argc > 1 && argv[0][0] == '-') {
		if (strcmp(argv[0], "-r") == 0) {
			options |= SO_DONTROUTE;
			argv++;
			argc--;
		}
		if (strcmp(argv[0], "-c") == 0) {
			conn = 1;
			argv++;
			argc--;
		}
	}
	if (argc > 1) {
		service = argv[0];
		argc--, argv++;
	}
	dst.sin_family = AF_INET;
	if ((dst.sin_addr.s_addr = inet_addr(*argv)) == -1) {
		hp = gethostbyname(*argv);
		if (hp == 0) {
			fprintf(stderr, "%s: unknown host\n", *argv);
			exit(1);
		}
		bcopy(hp->h_addr, &dst.sin_addr, sizeof(struct in_addr));
	}
	sp = getservbyname(service, "udp");
	if (sp)
		dst.sin_port = sp->s_port;
	else
		dst.sin_port = htons(atoi(service));
	if (dst.sin_port == 0) {
		fprintf(stderr, "%s: unknown service\n", service);
		exit(10);
	}
	sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (sock < 0) {
		perror("socket");
		exit(10);
	}
	if (options & SO_DONTROUTE &&
	    setsockopt(sock, SOL_SOCKET, SO_DONTROUTE, &on, sizeof(on)))
		perror("setsockopt SO_DONTROUTE");
		
	if (conn && connect(sock, &dst, sizeof(dst)) < 0) {
		perror("connect");
		exit(2);
	}
	signal(SIGALRM, sendit);
	sendit();

	for (;;) {
		if ((n = recvfrom(sock, &rbuf, sizeof(rbuf), 0, &from,
		    sizeof(from))) < 0) {
			if (errno != EINTR)
				perror("recvfrom");
		} else {
			gettimeofday(&now, 0);
			timevalsub(&now, &rbuf.t);
			recvd++;
			printf("%d bytes from %s: ", n,
				inet_ntoa(from.sin_addr));
			if ((unsigned)rbuf.n <= sent &&
			    (unsigned)now.tv_sec <= 1000 + sent) {
				printf("seq=%d. time=", rbuf.n);
				if (now.tv_sec)
					printf("%d.%03d sec.\n", now.tv_sec,
						now.tv_usec/1000);
				else
					printf("%d.%d ms\n", now.tv_usec/1000,
						now.tv_usec/10000);
			} else {
				cp = (char *)&rbuf;
				for (i = 0; i < n; i++)
					if (!isascii(cp[i]))
						break;
				if (i == n)
					printf("%.*s\n", n, cp);
				else {
					for (i = 0; i < n; i += sizeof(int))
						printf("%x ", *(int *)&cp[i]);
					printf("\n");
				}
			}
		}
	}
}

sendit()
{

	alarm(1);
	sbuf.n = sent++;
	gettimeofday(&sbuf.t);
	if (conn) {
		if (send(sock, &sbuf, sizeof(sbuf), 0) < 0)
			perror("sendto");
	} else {
		if (sendto(sock, &sbuf, sizeof(sbuf), 0, &dst, sizeof(dst)) < 0)
			perror("sendto");
	}
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
