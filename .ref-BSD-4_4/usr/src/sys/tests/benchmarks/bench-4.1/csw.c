/*
 * Context switching benchmark.
 *
 * Force system to context switch 2*nsigs
 * times by forking and exchanging signals.
 * To calculate system overhead for a context
 * switch, the signocsw program must be run
 * with nsigs.  Overhead is then estimated by
 *	t1 = time csw <n>
 *	t2 = time signocsw <n>
 *	overhead = t1 - 2 * t2;
 */
#include <signal.h>

int	sigsub();
int	otherpid;
int	nsigs;

main(argc, argv)
	char *argv[];
{
	int pid;

	if (argc < 2) {
		printf("usage: %s nsignals\n", argv[0]);
		exit(1);
	}
	nsigs = atoi(argv[1]);
	signal(SIGALRM, sigsub);
	otherpid = getpid();
	pid = fork();
	if (pid != 0) {
		otherpid = pid;
		kill(otherpid, SIGALRM);
	}
	for (;;)
		pause();
}

sigsub()
{

	signal(SIGALRM, sigsub);
	kill(otherpid, SIGALRM);
	if (--nsigs <= 0)
		exit(0);
}
