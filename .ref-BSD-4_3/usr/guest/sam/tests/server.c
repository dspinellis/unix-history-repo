#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/un.h>

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>

struct	sockaddr_un sun;
jmp_buf	j;
int	reapchild();
int	catchkill();

main(argc, argv)
	char *argv[];
{
	int s;

	s = socket(AF_UNIX, SOCK_STREAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	sun.sun_family = AF_UNIX;
	strcpy(sun.sun_path, argv[1]);
	if (bind(s, &sun, strlen(argv[1]) + 2) < 0) {
		perror("bind");
		exit(1);
	}
	if (listen(s, 2) < 0) {
		perror("listen");
		unlink(argv[1]);
		exit(1);
	}
	signal(SIGTERM, catchkill);
	signal(SIGCHLD, reapchild);
	if (setjmp(j)) {
		unlink(argv[1]);
		exit(1);
	}
	for (;;) {
		struct sockaddr_un from;
		int fromlen = sizeof (from), f;
		extern int errno;

		f = accept(s, &from, &fromlen);
		if (f < 0) {
			if (errno != EINTR)
				perror("accept");
			continue;
		}
		printf("connection");
		if (fromlen > 0)
			printf(" from %.*s\n", fromlen, from.sun_path);
		if (fork() == 0) {
			close(s);
			doit(f);
			exit(0);
		}
		close(f);
	}
}

reapchild()
{
	struct wait status;

	while (wait3(&status, WNOHANG, 0) > 0)
		;
}

catchkill()
{

	longjmp(j, 1);
}

doit(s)
	int s;
{
	char buf[BUFSIZ];
	int n;

	for (;;) {
		n = read(s, buf, sizeof (buf));
		if (n < 0) {
			perror("read");
			break;
		}
		if (n == 0)
			break;
		printf("read %d:\n", n);
		fflush(stdout);
		write(fileno(stdout), buf, n);
	}
	printf("done...\n");
}
