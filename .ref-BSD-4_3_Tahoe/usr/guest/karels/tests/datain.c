#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>

#include <netinet/in.h>
#include <netdb.h>

#include <stdio.h>
#include <signal.h>
#include <errno.h>

int	catcher();
extern	int errno;

main(argc, argv)
	char *argv[];
{
	struct sockaddr_in sin;
	int s, debug = 0, readsize = 1024, bufsize = 0;
	char *buf, *malloc();
	struct hostent *hp;

	if (argc < 2) {
		printf("usage: datain [ -d ] port [ readsize ] [ bufsize ]\n");
		exit(1);
	}
	if (argc > 2 && !strcmp(argv[1], "-d")) {
		debug++;
		argc--;
		argv++;
	}
	if (argc > 2) {
		readsize = atoi(argv[2]);
		if (readsize <= 0) {
			printf("bad readsize %d\n", readsize);
			exit(2);
		}
	}
	if (argc > 3) {
		bufsize = atoi(argv[3]);
		if (bufsize <= 0) {
			printf("bad bufsize %d\n", bufsize);
			exit(2);
		}
	}
	buf = malloc(readsize);
	bzero(&sin, sizeof (sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = htons(atoi(argv[1]));
	signal(SIGPIPE, SIG_IGN);
	signal(SIGCHLD, catcher);
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	if (bind(s, &sin, sizeof (sin)) < 0) {
		perror("bind");
		exit(1);
	}
	listen(s, 3);
	for (;;) {
		int new = accept(s, 0, 0);

		if (new < 0) {
			if (errno != EINTR)
				perror("accept");
			continue;
		}
		if (fork() == 0) {
			int on = 1;

			close(s);
			if (debug)
				setsockopt(new, SOL_SOCKET, SO_DEBUG,
					&on, sizeof(on));
			if (bufsize)
				setsockopt(new, SOL_SOCKET, SO_RCVBUF,
					&bufsize, sizeof(bufsize));
			while (read(new, buf, readsize) > 0)
				;
			exit(0);
		}
		close(new);
	}
}

catcher()
{
	int stat;

	while (wait3(&stat, WNOHANG, 0) >= 0)
		;
}
