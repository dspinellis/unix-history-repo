
#include <stdio.h>
#include <signal.h>

onintr(signo, code, sc)
	struct sigcontext *sc;
{
	int x;

	fprintf(stderr,
	    "sig=%u, code=%u, onstack=%u, mask=%u, sp=%x, pc=%x, ps=%x\n",
	    signo, code, sc->sc_onstack, sc->sc_mask, sc->sc_sp, sc->sc_pc,
	    sc->sc_ps);
	fprintf(stderr, "&x=%x\n", &x);
}

char	stack[1024];

main(argc)
{
	float x, y;
	struct sigvec sv;
	struct sigstack ss;

	sv.sv_handler = onintr;
	sv.sv_mask = 0;
	sv.sv_onstack = argc > 1;
	ss.ss_sp = 0/*stack + sizeof (stack) - 1*/;	/* grows down on vax */
	ss.ss_onstack = 0;
	if (sigstack(&ss, (struct sigstack *)0) < 0)
		perror("sigstack");
	if (sigvec(SIGFPE, &sv, (struct sigvec *)0) < 0)
		perror("sigvec");
	x = 1.; y = 0.; x = x / y;
}
