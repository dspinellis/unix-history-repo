/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <signal.h>
#include "../libI77/fiodefs.h"

extern int errno;
char *getenv();
int xargc;
char **xargv;

main(argc, argv, arge)
int argc;
char **argv;
char **arge;
{
void sigdie();
sig_t sigf;
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
MAIN_();
f_exit();
return 0;
}

struct action {
	char *mesg;
	int   core;
} sig_act[16] = {
	{"Hangup", 0},			/* SIGHUP  */
	{"Interrupt!", 0},		/* SIGINT  */
	{"Quit!", 1},			/* SIGQUIT */
	{"Illegal ", 1},		/* SIGILL  */
	{"Trace Trap", 1},		/* SIGTRAP */
	{"IOT Trap", 1},		/* SIGIOT  */
	{"EMT Trap", 1},		/* SIGEMT  */
	{"Arithmetic Exception", 1},	/* SIGFPE  */
	{ 0, 0},			/* SIGKILL */
	{"Bus error", 1},		/* SIGBUS  */
	{"Segmentation violation", 1},	/* SIGSEGV */
	{"Sys arg", 1},			/* SIGSYS  */
	{"Open pipe", 0},		/* SIGPIPE */
	{"Alarm", 0},			/* SIGALRM */
	{"Terminated", 0},		/* SIGTERM */
	{"Sig 16", 0},			/* unassigned */
};

#ifdef tahoe
/* The following arrays are defined & used assuming that signal codes are 
   1 to 5 for SIGFPE, and 0 to 3 for SIGILL. 
   Actually ILL_ALIGN_FAULT=14, and is mapped to 3. */

#define N_ACT_ILL 4			/* number of entries in act_ill[] */
#define N_ACT_FPE 5			/* number of entries in act_fpe[] */
#define ILL_ALIGN_FAULT 14

struct action act_fpe[] = {
	{"Integer overflow", 1},
	{"Integer divide by 0", 1},
	{"Floating divide by zero", 1},
	{"Floating point overflow", 1},
	{"Floating point underflow", 1},
};

#else vax || pdp11

struct action act_fpe[] = {
	{"Integer overflow", 1},
	{"Integer divide by 0", 1},
	{"Floating point overflow trap", 1},
	{"Floating divide by zero trap", 1},
	{"Floating point underflow trap", 1},
	{"Decimal overflow", 1},
	{"Subscript range", 1},
	{"Floating point overflow", 0},
	{"Floating divide by zero", 0},
	{"Floating point underflow", 0},
};
#endif vax || pdp11

struct action act_ill[] = {
	{"addr mode", 1},
	{"instruction", 1},
	{"operand", 0},
#ifdef tahoe
	{"alignment", 1},
#endif tahoe
};

#if (defined(vax) || defined(tahoe))
void
sigdie(s, t, sc)
int s; int t; struct sigcontext *sc;

#else	pdp11

void
sigdie(s, t, pc)
int s; int t; long pc;

#endif pdp11
{
extern unit units[];
register struct action *act = &sig_act[s-1];
/* print error message, then flush buffers */

if (s == SIGHUP || s == SIGINT || s == SIGQUIT)
	signal(s, SIG_IGN);	/* don't allow it again */
else
	signal(s, SIG_DFL);	/* shouldn't happen again, but ... */

if (act->mesg)
	{
	fprintf(units[STDERR].ufd, "*** %s", act->mesg);
	if (s == SIGFPE)
		{
#ifndef tahoe
		if (t >= 1 && t <= 10)
#else tahoe
		if ((t-1) >= 0 && t < N_ACT_FPE)
#endif tahoe
			fprintf(units[STDERR].ufd, ": %s", act_fpe[t-1].mesg);
		else
			fprintf(units[STDERR].ufd, ": Type=%d?", t);
		}
	else if (s == SIGILL)
		{
#ifndef tahoe
		if (t == 4) t = 2;	/* 4.0bsd botch */
		if (t >= 0 && t <= 2)
#else tahoe
		if (t == ILL_ALIGN_FAULT)	/* ILL_ALIGN_FAULT maps to last
			t = N_ACT_ILL-1;   	   entry in act_ill[] */
		if (t >= 0 && t < N_ACT_ILL)
#endif tahoe
			fprintf(units[STDERR].ufd, "%s", act_ill[t].mesg);
		else
			fprintf(units[STDERR].ufd, "compat mode: Code=%d", t);
		}
	putc('\n', units[STDERR].ufd);
	}
f77_abort( s, act->core );
}
