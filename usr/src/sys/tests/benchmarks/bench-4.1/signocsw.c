/*
 * Signal without context switch benchmark.
 */
#include <signal.h>

int	pid;
int	nsigs;
int	sigsub();

main(argc, argv)
	char *argv[];
{

	if (argc < 2) {
		printf("usage: %s nsignals\n", argv[0]);
		exit(1);
	}
	nsigs = atoi(argv[1]);
	signal(SIGALRM, sigsub);
	pid = getpid();
	kill(pid, SIGALRM);
}

sigsub()
{
	static int i = 0;

	signal(SIGALRM, sigsub);
	if (i++ < nsigs)
		kill(pid, SIGALRM);
}
