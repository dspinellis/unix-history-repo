/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)termcap.h	8.1 (Berkeley) %G%
 */

/*
 * termcap.h: I cannot find those in any include files...
 */
#ifndef _h_termcap
#define _h_termcap

int   tgetent	__P((char *, char *));
char *tgetstr	__P((char *, char **));
int   tgetflag	__P((char *));
int   tgetnum	__P((char *));
char *tgoto	__P((char *, int, int));
char *tputs	__P((char *, int, void (*)(int)));

#endif /* _h_termcap */
