
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
	fprintf(stderr, "handler local var: &x=%x\n", &x);
}

char	stack[1024];

main(argc)
{
	int i;
	register int j;
	struct sigvec sv;
	struct sigstack ss;

	sv.sv_handler = onintr;
	sv.sv_mask = 0;
	sv.sv_onstack = argc > 1;
	ss.ss_sp = stack + sizeof (stack) - 1;	/* grows down on vax */
	ss.ss_onstack = 0;
	if (sigstack(&ss, (struct sigstack *)0) < 0)
		perror("sigstack");
	if (sigvec(SIGINT, &sv, (struct sigvec *)0) < 0)
		perror("sigvec");
	for (i = 0;; i++) {
		for (j = 0; j < 32767; j++)
			;
		printf("&i=%x, i=%d, j=%d\n", &i, i, j);
	}
}
