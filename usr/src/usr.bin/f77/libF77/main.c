/* STARTUP PROCEDURE FOR UNIX FORTRAN PROGRAMS */

#include <stdio.h>
#include <signal.h>

int xargc;
char **xargv;

main(argc, argv, arge)
int argc;
char **argv;
char **arge;
{
int sigfdie(), sigidie(), sigqdie(), sigindie(), sigtdie();
long int (*sigf)();

xargc = argc;
xargv = argv;
signal(SIGFPE, sigfdie);	/* ignore underflow, enable overflow */
signal(SIGIOT, sigidie);
if(sigf=signal(SIGQUIT, sigqdie) != SIG_DFL) signal(SIGQUIT, sigf);
if(sigf=signal(SIGINT, sigindie) != SIG_DFL) signal(SIGINT, sigf);
if(sigf=signal(SIGTERM, sigtdie) != SIG_DFL) signal(SIGTERM, sigf);

#ifdef pdp11
	ldfps(01200); /* detect overflow as an exception */
#endif

f_init();
MAIN__();
f_exit();
}


static sigfdie()
{
sigdie("Floating Exception", 1);
}



static sigidie()
{
sigdie("IOT Trap", 1);
}


static sigqdie()
{
sigdie("Quit signal", 1);
}



static sigindie()
{
sigdie("Interrupt!", 0);
}



static sigtdie()
{
sigdie("Killed", 0);
}



static sigdie(s, kill)
register char *s;
int kill;
{
/* print error message, then clear buffers */
fprintf(stderr, "%s\n", s);
f_exit();
_cleanup();

if(kill)
	{
	/* now get a core */
	signal(SIGIOT, 0);
	abort();
	}
else
	exit(1);
}
