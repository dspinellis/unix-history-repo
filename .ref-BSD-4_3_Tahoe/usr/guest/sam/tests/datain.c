#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>

#include <netinet/in.h>

#include <signal.h>
#include <errno.h>

int	catcher();
extern	int errno;

main(argc, argv)
	char *argv[];
{
	struct sockaddr_in sin;
	char buf[1024];
	int s;

	if (argc < 2) {
		printf("usage: datain port\n");
		exit(1);
	}
	sin.sin_port = htons(atoi(argv[1]));
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_family = AF_INET;
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
			close(s);
			while (read(new, buf, sizeof (buf)) > 0)
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
