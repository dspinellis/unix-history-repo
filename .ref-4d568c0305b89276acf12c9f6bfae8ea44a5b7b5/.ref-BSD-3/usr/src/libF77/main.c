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

xargc = argc;
xargv = argv;
signal(SIGFPE, sigfdie);	/* ignore underflow, enable overflow */
signal(SIGIOT, sigidie);
signal(SIGQUIT,sigqdie);
signal(SIGINT, sigindie);
signal(SIGTERM,sigtdie);

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
sigdie("Interrupt", 0);
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
fflush(stderr);
fprintf(stderr, "%s\n", s);
f_exit();
fflush(stderr);

if(kill)
	{
	/* now get a core */
	signal(SIGIOT, 0);
	abort();
	}
else
	exit(1);
}
