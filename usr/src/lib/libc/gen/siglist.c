/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)siglist.c	5.1 (Berkeley) %G%";
#endif not lint

#include <signal.h>

char	*sys_siglist[NSIG] = {
	"Signal 0",
	"Hangup",			/* SIGHUP */
	"Interrupt",			/* SIGINT */
	"Quit",				/* SIGQUIT */
	"Illegal instruction",		/* SIGILL */
	"Trace/BPT trap",		/* SIGTRAP */
	"IOT trap",			/* SIGIOT */
	"EMT trap",			/* SIGEMT */
	"Floating point exception",	/* SIGFPE */
	"Killed",			/* SIGKILL */
	"Bus error",			/* SIGBUS */
	"Segmentation fault",		/* SIGSEGV */
	"Bad system call",		/* SIGSYS */
	"Broken pipe",			/* SIGPIPE */
	"Alarm clock",			/* SIGALRM */
	"Terminated",			/* SIGTERM */
	"Urgent I/O condition",		/* SIGURG */
	"Stopped (signal)",		/* SIGSTOP */
	"Stopped",			/* SIGTSTP */
	"Continued",			/* SIGCONT */
	"Child exited",			/* SIGCHLD */
	"Stopped (tty input)",		/* SIGTTIN */
	"Stopped (tty output)",		/* SIGTTOU */
	"I/O possible",			/* SIGIO */
	"Cputime limit exceeded",	/* SIGXCPU */
	"Filesize limit exceeded",	/* SIGXFSZ */
	"Virtual timer expired",	/* SIGVTALRM */
	"Profiling timer expired",	/* SIGPROF */
	"Window size changes",		/* SIGWINCH */
	"Signal 29",
	"User defined signal 1",	/* SIGUSR1 */
	"User defined signal 2"		/* SIGUSR2 */
};
