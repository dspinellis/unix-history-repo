/* STARTUP PROCEDURE FOR UNIX FORTRAN PROGRAMS */
char id_libF77[] = "@(#)main.c	2.6	%G%";

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
if((sigf=signal(SIGQUIT, sigdie)) != SIG_DFL) signal(SIGQUIT, sigf);
if((sigf=signal(SIGINT,  sigdie)) != SIG_DFL) signal(SIGINT,  sigf);
if((sigf=signal(SIGTERM, sigdie)) != SIG_DFL) signal(SIGTERM, sigf);
if((sigf=signal(SIGILL,  sigdie)) != SIG_DFL) signal(SIGILL,  sigf);
if((sigf=signal(SIGEMT,  sigdie)) != SIG_DFL) signal(SIGEMT,  sigf);
if((sigf=signal(SIGBUS,  sigdie)) != SIG_DFL) signal(SIGBUS,  sigf);
if((sigf=signal(SIGSEGV, sigdie)) != SIG_DFL) signal(SIGSEGV, sigf);

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
	{"EMT Trap", 1},		/* SIGEMT  */
#ifdef UCBVAX
	{"Arithmetic Exception", 1},	/* SIGFPE  */
#else
	{"Floating Point Exception", 1},/* SIGFPE  */
#endif
	{ 0, 0},			/* SIGKILL */
	{"Bus error", 1},		/* SIGBUS  */
	{"Segmentation violation", 1},	/* SIGSEGV */
	{ 0, 0},			/* SIGSYS  */
	{ 0, 0},			/* SIGPIPE */
	{ 0, 0},			/* SIGALRM */
	{"Terminated", 0},		/* SIGTERM */
	{ 0, 0},			/* unassigned */
};

#ifdef UCBVAX
struct action act_fpe[] = {
	{"Integer overflow", 1},
	{"Integer divide by 0", 1},
	{"Floating point overflow", 1},
	{"Floating divide by zero", 1},
	{"Floating point underflow", 1},
	{"Decimal overflow", 1},
	{"Subscript range", 1},
	{"Floating point overflow", 0},
	{"Floating divide by zero", 0},
	{"Floating point underflow", 0},
};
#endif

sigdie(s, t, pc)
int s; int t; long pc;
{
extern unit units[];
register struct action *act = &sig_act[s-1];
/* clear buffers, then print error message */
f_exit();
if (act->mesg)
	{
#ifdef UCBVAX
	fprintf(units[STDERR].ufd, "%s", act->mesg);
	if (s == SIGFPE)
		fprintf(units[STDERR].ufd, ": %s\n", act_fpe[t-1].mesg);
	else
		putc('\n', units[STDERR].ufd);
#else
	fprintf(units[STDERR].ufd, "%s\n", act->mesg);
#endif
	}
_cleanup();

if(act->core)
	{
	/* now get a core */
#ifdef VAX
	signal(SIGILL, SIG_DFL);
#else
	signal(SIGIOT, SIG_DFL);
#endif
	abort();
	}
exit(s);
}
