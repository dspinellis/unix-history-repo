#include <signal.h>

main()
{
	int i, (*a)();
	extern char *sys_siglist[];

	if (sigblock(0))
		printf("blocked: %x\n", sigblock(0));
	for (i = 1; i < NSIG; i++) {
		a = signal(i, SIG_DFL); signal(i, a);
		if (a == SIG_DFL || a == BADSIG)
			continue;
		printf("%s: ", sys_siglist[i]);
		if (a == SIG_IGN)
			printf("SIG_IGN\n");
		else
			printf("%x\n", a);
	}
}
