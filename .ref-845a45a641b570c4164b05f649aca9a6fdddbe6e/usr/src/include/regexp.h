/*
 * Copyright (c) 1986 by University of Toronto.
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley
 * by Henry Spencer.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)regexp.h	8.1 (Berkeley) %G%
 */

#ifndef	_REGEXP_H_
#define	_REGEXP_H_

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

#include <sys/cdefs.h>

__BEGIN_DECLS
regexp *regcomp __P((const char *));
int regexec __P((const  regexp *, const char *));
void regsub __P((const  regexp *, const char *, char *));
void regerror __P((const char *));
__END_DECLS

#endif /* !_REGEXP_H_ */
