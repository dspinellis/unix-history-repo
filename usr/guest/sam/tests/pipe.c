#include <sys/types.h>
#include <sys/socket.h>
#include <signal.h>

#define	MEGABYTES	2

char buf[1024];
int p[2];

main(argc, argv)
	char *argv[];
{
	register int i, j, child;

	pipe(p);
	if (child = fork()) {
		sleep(2);
		for (i = 0; i < MEGABYTES; i++)
			for (j = 0; j < 1000; j++)
				if (write(p[1], buf, sizeof (buf)) < 0) {
					perror("write");
					kill(child, SIGKILL);
					exit(5);
				}
		close(p[1]);
		exit(0);
	}
	for (i = 0; i < MEGABYTES; i++)
		for (j = 0; j < 1000; j++)
			if (read(p[0], buf, sizeof (buf)) < 0) {
				perror("read");
				exit(1);
			}
}
