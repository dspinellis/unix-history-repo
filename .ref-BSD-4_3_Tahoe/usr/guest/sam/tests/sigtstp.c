#include <signal.h>

catch()
{

	printf("catch SIGTSTP\n");
	sigsetmask(0);
	signal(SIGTSTP, SIG_DFL);
	kill(getpid(), SIGTSTP);
	signal(SIGTSTP, catch);
}

main()
{

	signal(SIGTSTP, catch);
	for (;;)
		kill(getpid(), SIGTSTP);
}
