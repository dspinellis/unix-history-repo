/* STARTUP PROCEDURE FOR UNIX FORTRAN PROGRAMS */

#include <stdio.h>
#include <signal.h>

int	xargc;
char	**xargv;
int	_sigfdie(), _sigidie(), _sigqdie(), _sigindie(), _sigtdie();

main(argc, argv, arge)
	int argc;
	char **argv;
	char **arge;
{

	xargc = argc;
	xargv = argv;
	signal(SIGFPE, _sigfdie);	/* ignore underflow, enable overflow */
	signal(SIGIOT, _sigidie);
	if ((int)signal(SIGQUIT,_sigqdie) & 01)
		signal(SIGQUIT, SIG_IGN);
	if ((int)signal(SIGINT, _sigindie) & 01)
		signal(SIGINT, SIG_IGN);
	signal(SIGTERM,_sigtdie);

#ifdef pdp11
	ldfps(01200); /* detect overflow as an exception */
#endif
	f_init();
	MAIN__();
	f_exit();
}

#ifdef vax
static char *fpenames[] = {
	"unknown floating exception",
	"integer overflow",
	"integer divide by zero",
	"floating overflow",
	"floating/decimal divide by zero",
	"floating underflow",
	"decimal overflow",
	"subscript out of range",
	"floating overflow",
	"floating divide by zero",
	"floating underflow"
};

_sigfdie(sig, code)
	int sig, code;
{
	if (code < 0 || code >= sizeof fpenames/sizeof fpenames[0])
		code = 0;
	_sigdie(fpenames[code], 1);
}
#else
_sigfdie() { _sigdie("floating exception, 1); }
#endif

_sigidie() { _sigdie("IOT Trap", 1); }
_sigqdie() { _sigdie("Quit signal", 1); }
_sigindie() { _sigdie("Interrupt", 0); }
_sigtdie() { _sigdie("Killed", 0); }

_sigdie(s, kill)
	register char *s;
	int kill;
{

	/* print error message, then clear buffers */
	fflush(stderr);
	fprintf(stderr, "%s\n", s);
	f_exit();
	fflush(stderr);
	if (kill) {
		/* now get a core */
		signal(SIGIOT, 0);
		abort();
		/*NOTREACHED*/
	}
	exit(1);
}
