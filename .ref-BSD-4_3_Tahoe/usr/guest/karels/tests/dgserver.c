#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>

struct	sockaddr_un sun;
jmp_buf	j;
int	catchsig();

main(argc, argv)
	char *argv[];
{
	int s, n;
	char buf[BUFSIZ];

	s = socket(AF_UNIX, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("dgserver: socket");
		exit(1);
	}
	sun.sun_family = AF_UNIX;
	strcpy(sun.sun_path, argv[1]);
	if (bind(s, &sun, strlen(argv[1]) + 2) < 0) {
		perror("dgserver: bind");
		exit(1);
	}
	signal(SIGTERM, catchsig);
	signal(SIGPIPE, catchsig);
	if (setjmp(j)) {
		unlink(argv[1]);
		exit(1);
	}
	for (;;) {
		struct sockaddr_un from;
		int fromlen = sizeof (from);
		extern int errno;

		n = recvfrom(s, buf, sizeof (buf), 0, &from, &fromlen);
		if (n < 0) {
			if (errno != EINTR)
				perror("dgserver: recvfrom");
			continue;
		}
		printf("Server: message \"%.*s\"\n", n, buf);
	}
}

catchsig(s)
	int s;
{

	psignal(s, "Server");
	longjmp(j, 1);
}
