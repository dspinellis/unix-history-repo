#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

main(argc, argv)
	char *argv[];
{
	char buf[BUFSIZ];
	int sv[2], n;

	if (socketpair(AF_UNIX, SOCK_STREAM, 0, sv) < 0) {
		perror("socketpair");
		exit(1);
	}
	if (fork() == 0) {
		close(sv[0]);
		for (;;) {
			n = read(sv[1], buf, sizeof (buf));
			if (n < 0) {
				perror("read");
				break;
			}
			if (n == 0)
				break;
			printf("read %d:\n", n);
			write(1, buf, n);
		}
		printf("done...\n");
		exit(0);
	}
	close(sv[1]);
	while (fgets(buf, sizeof (buf), stdin) != NULL)
		write(sv[0], buf, strlen(buf));
	close(sv[0]);
	wait((int *)0);
}
