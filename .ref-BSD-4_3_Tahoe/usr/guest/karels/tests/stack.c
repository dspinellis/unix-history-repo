#include <sys/types.h>
#include <sys/signal.h>
#include <sys/wait.h>

int nproc = 0;

main(ac, av)
char *av[];
{
	int i;
	union wait status;

	if (ac > 1)
		nproc = atoi(av[1]);
	for (i = 0; i == 0 || i < nproc; i++)
		stackgrow(ac, av);
	if (nproc > 0) for (;;) {
		i = wait(&status);
		if (i == -1)
			perror("wait");
		else
			stackgrow();
		
	}
	exit(0);
}

#define	INTSTK	1024
#define STACKINCR	(64000 - 20)		/* allow for frame */

int frames = 0;
int stacktop;
char stack[INTSTK];

stackgrow(ac, av)
	char *av[];
{
	struct sigvec vec;
	struct sigstack sstack;
	int segv();

	stacktop = (int)av[ac - 1];
	bzero((char *)&sstack, sizeof(sstack));
	sstack.ss_sp = &stack[INTSTK];
	sstack.ss_onstack = 0;
	if (sigstack(&sstack, 0) < 0)
		perror("sigstack");
	bzero((char *)&vec, sizeof(vec));
	vec.sv_handler = segv;
	vec.sv_flags = SV_ONSTACK;
	if (sigvec(SIGSEGV, &vec, 0) < 0)
		perror("sigvec");
	if (nproc <= 1 || fork() == 0)
		recurse();
}

recurse()
{
	char space[STACKINCR];

	frames++;
	recurse();
}

segv(sig, code, scp)
	int sig, code;
	struct sigcontext *scp;
{

	printf("%d frames, ended at 0x%x, stack size %d (0x%x)\n",
		frames, scp->sc_sp, stacktop - scp->sc_sp,
		stacktop - scp->sc_sp);
	exit(0);
}
