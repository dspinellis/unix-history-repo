/* Copyright (c) 1979 Regents of the University of California */
#include "sh.local.h"
/*
 * C shell
 */

extern	int await();
extern	int chngd();
extern	int doalias();
extern	int dobreak();
extern	int docontin();
extern	int doecho();
extern	int doelse();
extern	int doend();
extern	int doendif();
extern	int doendsw();
extern	int doexit();
extern	int doforeach();
extern	int doglob();
extern	int dogoto();
extern	int dohist();
extern	int doif();
extern	int dolet();
extern	int dologout();
extern	int donice();
extern	int donohup();
extern	int doonintr();
extern	int dorepeat();
extern	int doset();
extern	int dosetenv();
extern	int dosource();
extern	int doswbrk();
extern	int doswitch();
extern	int dotime();
#ifndef V6
extern	int doumask();
#endif
extern	int dowhile();
extern	int dozip();
extern	int execash();
extern	int goodbye();
extern	int shift();
extern	int showall();
extern	int unalias();
extern	int unset();

#define INF	1000

struct	biltins {
	char	*bname;
	int	(*bfunct)();
	short	minargs, maxargs;
} bfunc[] = {
	"@",		dolet,		0,	INF,
	"alias",	doalias,	0,	INF,
#ifdef debug
	"alloc",	showall,	0,	1,
#endif
	"break",	dobreak,	0,	0,
	"breaksw",	doswbrk,	0,	0,
	"case",		dozip,		0,	1,
	"cd",		chngd,		0,	1,
	"chdir",	chngd,		0,	1,
	"continue",	docontin,	0,	0,
	"default",	dozip,		0,	0,
	"echo",		doecho,		0,	INF,
	"else",		doelse,		0,	INF,
	"end",		doend,		0,	0,
	"endif",	dozip,		0,	0,
	"endsw",	dozip,		0,	0,
	"exec",		execash,	1,	INF,
	"exit",		doexit,		0,	INF,
	"foreach",	doforeach,	3,	INF,
	"glob",		doglob,		0,	INF,
	"goto",		dogoto,		1,	1,
	"history",	dohist,		0,	0,
	"if",		doif,		1,	INF,
	"logout",	dologout,	0,	0,
	"nice",		donice,		0,	INF,
	"nohup",	donohup,	0,	INF,
	"onintr",	doonintr,	0,	2,
	"repeat",	dorepeat,	2,	INF,
	"set",		doset,		0,	INF,
#ifndef V6
	"setenv",	dosetenv,	2,	2,
#endif
	"shift",	shift,		0,	1,
	"source",	dosource,	1,	1,
	"switch",	doswitch,	1,	INF,
	"time",		dotime,		0,	INF,
#ifndef V6
	"umask",	doumask,	0,	1,
#endif
	"unalias",	unalias,	1,	INF,
	"unset",	unset,		1,	INF,
	"wait",		await,		0,	0,
	"while",	dowhile,	1,	INF,
	0,		0,		0,	0,
};

#define	ZBREAK		0
#define	ZBRKSW		1
#define	ZCASE		2
#define	ZDEFAULT 	3
#define	ZELSE		4
#define	ZEND		5
#define	ZENDIF		6
#define	ZENDSW		7
#define	ZEXIT		8
#define	ZFOREACH	9
#define	ZGOTO		10
#define	ZIF		11
#define	ZLABEL		12
#define	ZLET		13
#define	ZSET		14
#define	ZSWITCH		15
#define	ZTEST		16
#define	ZTHEN		17
#define	ZWHILE		18

struct srch {
	char	*s_name;
	short	s_value;
} srchn[] = {
	"@",		ZLET,
	"break",	ZBREAK,
	"breaksw",	ZBRKSW,
	"case",		ZCASE,
	"default", 	ZDEFAULT,
	"else",		ZELSE,
	"end",		ZEND,
	"endif",	ZENDIF,
	"endsw",	ZENDSW,
	"exit",		ZEXIT,
	"foreach", 	ZFOREACH,
	"goto",		ZGOTO,
	"if",		ZIF,
	"label",	ZLABEL,
	"set",		ZSET,
	"switch",	ZSWITCH,
	"while",	ZWHILE,
	0,		0,
};

char	*mesg[] = {
	0,
	"Hangup",
	0,
	"Quit",
	"Illegal instruction",
	"Trace/BPT trap",
	"IOT trap",
	"EMT trap",
	"Floating exception",
	"Killed",
	"Bus error",
	"Segmentation violation",
	"Bad system call",
	0,
	"Alarm clock",
	"Terminated",
};
