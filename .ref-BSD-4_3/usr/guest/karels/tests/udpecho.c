#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <netdb.h>
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>

struct sockaddr_in sin;
jmp_buf	j;
int	catchsig();

main(argc, argv)
	char *argv[];
{
	int s, n;
	char buf[BUFSIZ];
	struct servent *sp;

	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("dgserver: socket");
		exit(1);
	}
	sp = getservbyname(argv[1], "udp");
	if (sp)
		sin.sin_port = sp->s_port;
	else
		sin.sin_port = htons(atoi(argv[1]));
	if (bind(s, &sin, sizeof(sin)) < 0) {
		perror("udpserver: bind");
		exit(1);
	}
	signal(SIGTERM, catchsig);
	signal(SIGPIPE, catchsig);
	if (setjmp(j)) {
		exit(1);
	}
	for (;;) {
		struct sockaddr_in from;
		int fromlen = sizeof (from);
		extern int errno;

		n = recvfrom(s, buf, sizeof (buf), 0, &from, &fromlen);
		if (n < 0) {
			if (errno != EINTR)
				perror("dgserver: recvfrom");
			continue;
		}
/*		printf("Server: message (%d) \"%.*s\"\n", n, n, buf); */
		n = sendto(s, buf, n, 0, &from, fromlen);
		if (n < 0 && errno != EINTR)
			perror("dgserver: sendto");
	}
}

catchsig(s)
	int s;
{

	psignal(s, "Server");
	longjmp(j, 1);
}
