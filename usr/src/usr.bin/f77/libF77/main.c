/* STARTUP PROCEDURE FOR UNIX FORTRAN PROGRAMS */
char id_libF77[] = "@(#)main.c	2.10	%G%";

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
int signum;

xargc = argc;
xargv = argv;

for (signum=1; signum<=16; signum++)
{
	if((sigf=signal(signum, sigdie)) != SIG_DFL) signal(signum, sigf);
}

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
	{"Hangup", 0},			/* SIGHUP  */
	{"Interrupt!", 0},		/* SIGINT  */
	{"Quit!", 1},			/* SIGQUIT */
#ifdef UCBVAX
	{"Illegal ", 1},		/* SIGILL  */
#else
	{"Illegal instruction", 1},	/* SIGILL  */
#endif
	{"Trace Trap", 1},		/* SIGTRAP */
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
	{"Sys arg", 1},			/* SIGSYS  */
	{"Open pipe", 0},		/* SIGPIPE */
	{"Alarm", 0},			/* SIGALRM */
	{"Terminated", 0},		/* SIGTERM */
	{"Sig 16", 0},			/* unassigned */
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

struct action act_ill[] = {
	{"addr mode", 1},
	{"instruction", 1},
	{"operand", 0},
};
#endif

sigdie(s, t, pc)
int s; int t; long pc;
{
extern unit units[];
register struct action *act = &sig_act[s-1];
/* print error message, then flush buffers */

if (act->mesg)
	{
#ifdef UCBVAX
	fprintf(units[STDERR].ufd, "*** %s", act->mesg);
	if (s == SIGFPE)
		{
		if (t >= 1 && t <= 10)
			fprintf(units[STDERR].ufd, ": %s", act_fpe[t-1].mesg);
		else
			fprintf(units[STDERR].ufd, ": Type=%d?", t);
		}
	else if (s == SIGILL)
		{
		if (t == 4) t = 2;	/* 4.0bsd botch */
		if (t >= 0 && t <= 2)
			fprintf(units[STDERR].ufd, "%s", act_ill[t].mesg);
		else
			fprintf(units[STDERR].ufd, "compat mode: Code=%d", t);
		}
	putc('\n', units[STDERR].ufd);
#else
	fprintf(units[STDERR].ufd, "*** %s\n", act->mesg);
#endif
	}
f_exit();
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
