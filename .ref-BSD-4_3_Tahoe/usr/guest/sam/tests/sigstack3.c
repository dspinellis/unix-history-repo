#include <stdio.h>
#include <signal.h>

#define	MAXDEPTH	10
int	depth = 0;
int	maxdepth = MAXDEPTH;

onintr(signo, code, sc)
	struct sigcontext *sc;
{
	int x;

	printf("sig=%u, code=%u, onstack=%u, mask=%u, sp=%x, pc=%x, ps=%x\n",
	    signo, code, sc->sc_onstack, sc->sc_mask, sc->sc_sp, sc->sc_pc,
	    sc->sc_ps);
	printf("handler local var: &x=%x\n", &x);
	sigsetmask(0);
	if (++depth < maxdepth)
		kill(getpid(), SIGHUP);
	printf("onintr...return\n");
}

onhup(signo, code, sc)
	struct sigcontext *sc;
{
	int x;

	printf("sig=%u, code=%u, onstack=%u, mask=%u, sp=%x, pc=%x, ps=%x\n",
	    signo, code, sc->sc_onstack, sc->sc_mask, sc->sc_sp, sc->sc_pc,
	    sc->sc_ps);
	printf("handler local var: &x=%x\n", &x);
	sigsetmask(0);
	if (++depth < maxdepth)
		kill(getpid(), SIGINT);
	printf("onhup...return\n");
}

char	stack[1024];

main(argc, argv)
	char *argv[];
{
	struct sigvec sv;
	struct sigstack ss;

	if (argc > 1) {
		maxdepth = atoi(argv[1]);
		if (maxdepth <= 0)
			maxdepth = MAXDEPTH;
	}
	ss.ss_sp = stack + sizeof (stack) - 1;	/* grows down on vax */
	ss.ss_onstack = 0;
	if (sigstack(&ss, (struct sigstack *)0) < 0)
		perror("sigstack");
	sv.sv_handler = onintr;
	sv.sv_mask = 0;
#ifdef notdef
	sv.sv_onstack = argc > 1;
#else
	sv.sv_onstack = 0;
#endif
	if (sigvec(SIGINT, &sv, (struct sigvec *)0) < 0)
		perror("sigvec");
	sv.sv_handler = onhup;
	sv.sv_mask = 0;
	sv.sv_onstack = argc > 1;
	if (sigvec(SIGHUP, &sv, (struct sigvec *)0) < 0)
		perror("sigvec (SIGHUP)");
	for (;;) {
		depth = 0;
		sigpause(0);
	}
}
