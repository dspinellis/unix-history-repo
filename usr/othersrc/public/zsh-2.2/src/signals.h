/* this file is created automatically by buildzsh */
/* if all this is wrong, blame csh ;-) */

#define SIGCOUNT       31

#ifdef GLOBALS

char *sigmsg[SIGCOUNT+2] = {
	"done",
	"hangup",
	"interrupt",
	"quit",
	"illegal instruction",
	"trace trap",
	"abort",
	"EMT instruction",
	"floating point exception",
	"killed",
	"bus error",
	"segmentation fault",
	"bad system call",
	"broken pipe",
	"SIGALRM",
	"terminated",
	"SIGURG",
#ifdef USE_SUSPENDED
	"suspended (signal)",
#else
	"stopped (signal)",
#endif
#ifdef USE_SUSPENDED
	"suspended",
#else
	"stopped",
#endif
	"continued",
	"SIGCHLD",
#ifdef USE_SUSPENDED
	"suspended (tty input)",
#else
	"stopped (tty input)",
#endif
#ifdef USE_SUSPENDED
	"suspended (tty output)",
#else
	"stopped (tty output)",
#endif
	"SIGIO",
	"cpu limit exceeded",
	"filesize limit exceeded",
	"virtual time alarm",
	"SIGPROF",
	"SIGWINCH",
	"SIGINFO",
	"SIGUSR1",
	"SIGUSR2",
	NULL
};

char *sigs[SIGCOUNT+4] = {
	"EXIT",
	"HUP",
	"INT",
	"QUIT",
	"ILL",
	"TRAP",
	"ABRT",
	"EMT",
	"FPE",
	"KILL",
	"BUS",
	"SEGV",
	"SYS",
	"PIPE",
	"ALRM",
	"TERM",
	"URG",
	"STOP",
	"TSTP",
	"CONT",
	"CHLD",
	"TTIN",
	"TTOU",
	"IO",
	"XCPU",
	"XFSZ",
	"VTALRM",
	"PROF",
	"WINCH",
	"INFO",
	"USR1",
	"USR2",
	"ERR",
	"DEBUG",
	NULL
};

#else

extern char *sigs[SIGCOUNT+4],*sigmsg[SIGCOUNT+2];

#endif
