/* STARTUP PROCEDURE FOR UNIX FORTRAN PROGRAMS */
char id_libF77[] = "@(#)main.c	2.3	%G%";

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
int sigdie();
long int (*sigf)();

xargc = argc;
xargv = argv;
signal(SIGFPE, sigdie);	/* ignore underflow, enable overflow */
signal(SIGIOT, sigdie);
if(sigf=signal(SIGQUIT, sigdie) != SIG_DFL) signal(SIGQUIT, sigf);
if(sigf=signal(SIGINT,  sigdie) != SIG_DFL) signal(SIGINT,  sigf);
if(sigf=signal(SIGTERM, sigdie) != SIG_DFL) signal(SIGTERM, sigf);
if(sigf=signal(SIGILL,  sigdie) != SIG_DFL) signal(SIGILL,  sigf);
if(sigf=signal(SIGEMT,  sigdie) != SIG_DFL) signal(SIGEMT,  sigf);
if(sigf=signal(SIGBUS,  sigdie) != SIG_DFL) signal(SIGBUS,  sigf);
if(sigf=signal(SIGSEGV, sigdie) != SIG_DFL) signal(SIGSEGV, sigf);

#ifdef pdp11
	ldfps(01200); /* detect overflow as an exception */
#endif

f_init();
MAIN__();
f_exit();
}

struct action {
	char *mesg;
	int   core;
} sig_act[16] = {
	{ 0, 0},			/* SIGHUP  */
	{"Interrupt!", 0},		/* SIGINT  */
	{"Quit!", 1},			/* SIGQUIT */
	{"Illegal instruction", 1},	/* SIGILL  */
	{ 0, 0},			/* SIGTRAP */
	{"IOT Trap", 1},		/* SIGIOT  */
	{"EMT trap", 1},		/* SIGEMT  */
	{"Floating Point Exception", 1},/* SIGFPE  */
	{ 0, 0},			/* SIGKILL */
	{"Bus error", 1},		/* SIGBUS  */
	{"Segmentation violation", 1},	/* SIGSEGV */
	{ 0, 0},			/* SIGSYS  */
	{ 0, 0},			/* SIGPIPE */
	{ 0, 0},			/* SIGALRM */
	{"Terminated", 0},		/* SIGTERM */
	{ 0, 0},			/* unassigned */
};


sigdie(s)
int s;
{
extern unit units[];
register struct action *act = &sig_act[s-1];
/* clear buffers, then print error message */
f_exit();
if (act->mesg) fprintf(units[STDERR].ufd, "%s\n", act->mesg);
_cleanup();

if(act->core)
	{
	/* now get a core */
	signal(SIGIOT, SIG_DFL);
	abort();
	}
exit(s);
}
