/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tprintf.h	8.1 (Berkeley) %G%
 */

typedef struct session *tpr_t;

tpr_t	tprintf_open __P((struct proc *));
void	tprintf_close __P((tpr_t));

void	tprintf __P((tpr_t, const char *fmt, ...));
