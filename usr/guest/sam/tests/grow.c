#include <signal.h>

catch(s, code, scp)
	int s, code;
	struct sigcontext *scp;
{

	sigsetmask(0);
	kill(getpid(), s);
}

main()
{

	signal(SIGINT, catch);
	for (;;)
		sigpause(0);
}
