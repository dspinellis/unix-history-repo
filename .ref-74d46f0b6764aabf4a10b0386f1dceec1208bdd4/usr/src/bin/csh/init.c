/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)init.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include "sh.h"

/*
 * C shell
 */

extern	int doalias();
extern	int dobg();
extern	int dobreak();
extern	int dochngd();
extern	int docontin();
extern	int dodirs();
extern	int doecho();
extern	int doelse();
extern	int doend();
extern	int doendif();
extern	int doendsw();
extern	int doeval();
extern	int doexit();
extern	int dofg();
extern	int doforeach();
extern	int doglob();
extern	int dogoto();
extern	int dohash();
extern	int dohist();
extern	int doif();
extern	int dojobs();
extern	int dokill();
extern	int dolet();
extern	int dolimit();
extern	int dologin();
extern	int dologout();
extern	int donice();
extern	int donotify();
extern	int donohup();
extern	int doonintr();
extern	int dopopd();
extern	int dopushd();
extern	int dorepeat();
extern	int doset();
extern	int dosetenv();
extern	int dosource();
extern	int dostop();
extern	int dosuspend();
extern	int doswbrk();
extern	int doswitch();
extern	int dotime();
extern	int dounlimit();
extern	int doumask();
extern	int dowait();
extern	int dowhile();
extern	int dozip();
extern	int execash();
extern	int goodbye();
#ifdef VFORK
extern	int hashstat();
#endif
extern	int shift();
extern	int showall();
extern	int unalias();
extern	int dounhash();
extern	int unset();
extern	int dounsetenv();

#define	INF	1000

struct biltins bfunc[] = {
	"@",		dolet,		0,	INF,
	"alias",	doalias,	0,	INF,
	"alloc",	showall,	0,	1,
	"bg",		dobg,		0,	INF,
	"break",	dobreak,	0,	0,
	"breaksw",	doswbrk,	0,	0,
	"case",		dozip,		0,	1,
	"cd",		dochngd,	0,	1,
	"chdir",	dochngd,	0,	1,
	"continue",	docontin,	0,	0,
	"default",	dozip,		0,	0,
	"dirs",		dodirs,		0,	1,
	"echo",		doecho,		0,	INF,
	"else",		doelse,		0,	INF,
	"end",		doend,		0,	0,
	"endif",	dozip,		0,	0,
	"endsw",	dozip,		0,	0,
	"eval",		doeval,		0,	INF,
	"exec",		execash,	1,	INF,
	"exit",		doexit,		0,	INF,
	"fg",		dofg,		0,	INF,
	"foreach",	doforeach,	3,	INF,
	"glob",		doglob,		0,	INF,
	"goto",		dogoto,		1,	1,
#ifdef VFORK
	"hashstat",	hashstat,	0,	0,
#endif
	"history",	dohist,		0,	2,
	"if",		doif,		1,	INF,
	"jobs",		dojobs,		0,	1,
	"kill",		dokill,		1,	INF,
	"limit",	dolimit,	0,	3,
	"login",	dologin,	0,	1,
	"logout",	dologout,	0,	0,
	"nice",		donice,		0,	INF,
	"nohup",	donohup,	0,	INF,
	"notify",	donotify,	0,	INF,
	"onintr",	doonintr,	0,	2,
	"popd",		dopopd,		0,	1,
	"pushd",	dopushd,	0,	1,
	"rehash",	dohash,		0,	0,
	"repeat",	dorepeat,	2,	INF,
	"set",		doset,		0,	INF,
	"setenv",	dosetenv,	0,	2,
	"shift",	shift,		0,	1,
	"source",	dosource,	1,	2,
	"stop",		dostop,		1,	INF,
	"suspend",	dosuspend,	0,	0,
	"switch",	doswitch,	1,	INF,
	"time",		dotime,		0,	INF,
	"umask",	doumask,	0,	1,
	"unalias",	unalias,	1,	INF,
	"unhash",	dounhash,	0,	0,
	"unlimit",	dounlimit,	0,	INF,
	"unset",	unset,		1,	INF,
	"unsetenv",	dounsetenv,	1,	INF,
	"wait",		dowait,		0,	0,
	"while",	dowhile,	1,	INF,
};
int nbfunc = sizeof bfunc / sizeof *bfunc;

struct srch srchn[] = {
	"@",		T_LET,
	"break",	T_BREAK,
	"breaksw",	T_BRKSW,
	"case",		T_CASE,
	"default", 	T_DEFAULT,
	"else",		T_ELSE,
	"end",		T_END,
	"endif",	T_ENDIF,
	"endsw",	T_ENDSW,
	"exit",		T_EXIT,
	"foreach", 	T_FOREACH,
	"goto",		T_GOTO,
	"if",		T_IF,
	"label",	T_LABEL,
	"set",		T_SET,
	"switch",	T_SWITCH,
	"while",	T_WHILE,
};
int nsrchn = sizeof srchn / sizeof *srchn;

struct mesg mesg[] = {
	0,	0,
	"HUP",	"Hangup",
	"INT",	"Interrupt",	
	"QUIT",	"Quit",
	"ILL",	"Illegal instruction",
	"TRAP",	"Trace/BPT trap",
	"IOT",	"IOT trap",
	"EMT",	"EMT trap",
	"FPE",	"Floating exception",
	"KILL",	"Killed",
	"BUS",	"Bus error",
	"SEGV",	"Segmentation fault",
	"SYS",	"Bad system call",
	"PIPE",	"Broken pipe",
	"ALRM",	"Alarm clock",
	"TERM",	"Terminated",
	"URG",	"Urgent I/O condition",
	"STOP",	"Suspended (signal)",
	"TSTP",	"Suspended",
	"CONT",	"Continued",
	"CHLD",	"Child exited",
	"TTIN", "Suspended (tty input)",
	"TTOU", "Suspended (tty output)",
	"IO",	"I/O possible",
	"XCPU",	"Cputime limit exceeded",
	"XFSZ", "Filesize limit exceeded",
	"VTALRM","Virtual timer expired",
	"PROF",	"Profiling timer expired",
	"WINCH","Window size changed",
	0,	"Signal 29",
	"USR1",	"User defined signal 1",
	"USR2",	"User defined signal 2",
	0,	"Signal 32"
};
