/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)expand.h	5.3 (Berkeley) %G%
 */

struct strlist {
	struct strlist *next;
	char *text;
};


struct arglist {
	struct strlist *list;
	struct strlist **lastp;
};

/*
 * expandarg() flags
 */
#define EXP_FULL	0x1	/* perform word splitting & file globbing */
#define EXP_TILDE	0x2	/* do normal tilde expansion */
#define	EXP_VARTILDE	0x4	/* expand tildes in an assignment */
#define	EXP_REDIR	0x8	/* file glob for a redirection (1 match only) */
#define EXP_CASE	0x10	/* keeps quotes around for CASE pattern */


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
