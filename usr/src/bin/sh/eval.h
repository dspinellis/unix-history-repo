/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)eval.h	8.2 (Berkeley) %G%
 */

extern char *commandname;	/* currently executing command */
extern int exitstatus;		/* exit status of last command */
extern struct strlist *cmdenviron;  /* environment for builtin command */


struct backcmd {		/* result of evalbackcmd */
	int fd;			/* file descriptor to read from */
	char *buf;		/* buffer */
	int nleft;		/* number of chars in buffer */
	struct job *jp;		/* job structure for command */
};

int evalcmd __P((int, char **));
void evalstring __P((char *));
union node;	/* BLETCH for ansi C */
void evaltree __P((union node *, int));
void evalbackcmd __P((union node *, struct backcmd *));
int bltincmd __P((int, char **));
int breakcmd __P((int, char **));
int returncmd __P((int, char **));
int falsecmd __P((int, char **));
int truecmd __P((int, char **));
int execcmd __P((int, char **));

/* in_function returns nonzero if we are currently evaluating a function */
#define in_function()	funcnest
extern int funcnest;
