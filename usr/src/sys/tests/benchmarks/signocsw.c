/*
 * Signal without context switch benchmark.
 */
#include <signal.h>

int	pid;
int	nsigs;
void	sigsub();

main(argc, argv)
	char *argv[];
{
	register int i;

	if (argc < 2) {
		printf("usage: %s nsignals\n", argv[0]);
		exit(1);
	}
	nsigs = atoi(argv[1]);
	signal(SIGALRM, sigsub);
	pid = getpid();
	for (i = 0; i < nsigs; i++)
		kill(pid, SIGALRM);
}

void
sigsub()
{
	static mustreset = 1;
	void (*osig)();

	if (mustreset) {
		osig = signal(SIGALRM, sigsub);
		if (osig == sigsub)
			mustreset = 0;
	}
}
