/*
 * Copyright (c) 1986 by University of Toronto.
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley
 * by Henry Spencer.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)regexp.h	1.5 (Berkeley) %G%
 */

/*
 * Definitions etc. for regexp(3) routines.
 *
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 */
#define NSUBEXP  10
typedef struct regexp {
	char *startp[NSUBEXP];
	char *endp[NSUBEXP];
	char regstart;		/* Internal use only. */
	char reganch;		/* Internal use only. */
	char *regmust;		/* Internal use only. */
	int regmlen;		/* Internal use only. */
	char program[1];	/* Unwarranted chumminess with compiler. */
} regexp;

#if __STDC__ || c_plusplus
extern regexp *regcomp(const char *);
extern int regexec(const  regexp *, const char *);
extern void regsub(const  regexp *, const char *, char *);
extern void regerror(const char *);
#else
extern regexp *regcomp();
extern int regexec();
extern void regsub();
extern void regerror();
#endif
