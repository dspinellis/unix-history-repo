/* STARTUP PROCEDURE FOR UNIX FORTRAN PROGRAMS */
char id_libF77[] = "@(#)main.c	2.2";

#include <stdio.h>
#include <signal.h>
#include "../libI77/fiodefs.h"

int xargc;
char **xargv;

main(argc, argv, arge)
int argc;
char **argv;
char **arge;
{
int sigfdie(), sigidie(), sigqdie(), sigindie(), sigtdie();
int sigildie(), sigedie(), sigbdie(), sigsdie();
long int (*sigf)();

xargc = argc;
xargv = argv;
signal(SIGFPE, sigfdie);	/* ignore underflow, enable overflow */
signal(SIGIOT, sigidie);
if(sigf=signal(SIGQUIT, sigqdie) != SIG_DFL) signal(SIGQUIT, sigf);
if(sigf=signal(SIGINT, sigindie) != SIG_DFL) signal(SIGINT, sigf);
if(sigf=signal(SIGTERM, sigtdie) != SIG_DFL) signal(SIGTERM, sigf);
if(sigf=signal(SIGILL, sigildie) != SIG_DFL) signal(SIGILL, sigf);
if(sigf=signal(SIGEMT, sigedie) != SIG_DFL) signal(SIGEMT, sigf);
if(sigf=signal(SIGBUS, sigbdie) != SIG_DFL) signal(SIGBUS, sigf);
if(sigf=signal(SIGSEGV, sigsdie) != SIG_DFL) signal(SIGSEGV, sigf);

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


static sigildie()
{
sigdie("Illegal instruction", 1);
}


static sigedie()
{
sigdie("EMT trap", 1);
}


static sigbdie()
{
sigdie("Bus error", 1);
}


static sigsdie()
{
sigdie("Segmentation violation", 1);
}


static sigdie(s, core)
register char *s;
int core;
{
extern unit units[];
/* clear buffers, then print error message */
f_exit();
fprintf(units[STDERR].ufd, "%s\n", s);
_cleanup();

if(core)
	{
	/* now get a core */
	signal(SIGIOT, 0);
	abort();
	}
else
	exit(1);
}
