/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)expand.h	5.1 (Berkeley) %G%
 */

struct strlist {
	struct strlist *next;
	char *text;
};


struct arglist {
	struct strlist *list;
	struct strlist **lastp;
};

#ifdef __STDC__
union node;
void expandarg(union node *, struct arglist *, int);
void expandhere(union node *, int);
int patmatch(char *, char *);
void rmescapes(char *);
int casematch(union node *, char *);
#else
void expandarg();
void expandhere();
int patmatch();
void rmescapes();
int casematch();
#endif
