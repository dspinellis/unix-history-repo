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
int	sock;

main(argc, argv)
	char *argv[];
{
	int s, n;
	char buf[BUFSIZ];
	struct servent *sp;

	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0) {
		perror("server: socket");
		exit(1);
	}
	sp = getservbyname(argv[1], "tcp");
	if (sp)
		sin.sin_port = sp->s_port;
	else
		sin.sin_port = htons(atoi(argv[1]));
	if (bind(s, &sin, sizeof(sin)) < 0) {
		perror("udpserver: bind");
		exit(1);
	}
	listen(s, 5);

	signal(SIGTERM, catchsig);
	signal(SIGPIPE, catchsig);
	if (setjmp(j)) {
		exit(1);
	}
	for (;;) {
		struct sockaddr_in from;
		int fromlen = sizeof (from);
		extern int errno;

		fromlen = sizeof(from);
		if ((sock = accept(s, &from, &fromlen)) < 0)
			perror("accept");
		if (fork() == 0)
		    for (;;) {
			n = recv(sock, buf, sizeof (buf), 0);
			if (n < 0) {
				if (errno != EINTR)
					perror("server: recv");
				exit(1);
			}
			if (n == 0)
				exit(0);
	/*		printf("Server: message (%d) \"%.*s\"\n", n, n, buf); */
			n = send(sock, buf, n, 0);
			if (n < 0 && errno != EINTR)
				perror("server: send");
		}
	}
}

catchsig(s)
	int s;
{

	psignal(s, "Server");
	longjmp(j, 1);
}
