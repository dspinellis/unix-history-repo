/*
 * Derived from:
 * static char sccsid[] = "@(#)siglist.c	8.1 (Berkeley) 6/4/93";
 */
#include <sys/cdefs.h>

#include <signal.h>

const char *const sys_signame[NSIG] = {
	"Signal 0",
	"hup",				/* SIGHUP */
	"int",				/* SIGINT */
	"quit",				/* SIGQUIT */
	"ill",				/* SIGILL */
	"trap",				/* SIGTRAP */
	"abrt",				/* SIGABRT */
	"emt",				/* SIGEMT */
	"fpe",				/* SIGFPE */
	"kill",				/* SIGKILL */
	"bus",				/* SIGBUS */
	"segv",				/* SIGSEGV */
	"sys",				/* SIGSYS */
	"pipe",				/* SIGPIPE */
	"alrm",				/* SIGALRM */
	"term",				/* SIGTERM */
	"usr1",				/* SIGUSR1 */
	"usr2",				/* SIGUSR2 */
	"chld",				/* SIGCHLD */
	"pwr",				/* SIGPWR */
	"vtalrm",			/* SIGVTALRM */
	"prof",				/* SIGPROF */
	"io",				/* SIGIO */
	"winch",			/* SIGWINCH */
	"stop",				/* SIGSTOP */
	"tstp",				/* SIGTSTP */
	"cont",				/* SIGCONT */
	"ttin",				/* SIGTTIN */
	"ttou",				/* SIGTTOU */
	"urg",				/* SIGURG */
	"lost",				/* SIGLOST */
};

const char *const sys_siglist[NSIG] = {
	"Signal 0",
	"Hangup",			/* SIGHUP */
	"Interrupt",			/* SIGINT */
	"Quit",				/* SIGQUIT */
	"Illegal instruction",		/* SIGILL */
	"Trace/BPT trap",		/* SIGTRAP */
	"Abort trap",			/* SIGABRT */
	"EMT trap",			/* SIGEMT */
	"Floating point exception",	/* SIGFPE */
	"Killed",			/* SIGKILL */
	"Bus error",			/* SIGBUS */
	"Segmentation fault",		/* SIGSEGV */
	"Bad system call",		/* SIGSYS */
	"Broken pipe",			/* SIGPIPE */
	"Alarm clock",			/* SIGALRM */
	"Terminated",			/* SIGTERM */
	"User defined signal 1",	/* SIGUSR1 */
	"User defined signal 2"		/* SIGUSR2 */
	"Child exited",			/* SIGCHLD */
	"Power failure",		/* SIGPWR */
	"Virtual timer expired",	/* SIGVTALRM */
	"Profiling timer expired",	/* SIGPROF */
	"I/O possible",			/* SIGIO */
	"Window size changes",		/* SIGWINCH */
	"Suspended (signal)",		/* SIGSTOP */
	"Suspended",			/* SIGTSTP */
	"Continued",			/* SIGCONT */
	"Stopped (tty input)",		/* SIGTTIN */
	"Stopped (tty output)",		/* SIGTTOU */
	"Urgent I/O condition",		/* SIGURG */
	"File lock lost",		/* SIGLOST */
};
