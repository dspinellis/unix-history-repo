/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tprintf.h	7.2 (Berkeley) %G%
 */

typedef struct session *tpr_t;

tpr_t	tprintf_open __P((struct proc *));
void	tprintf_close __P((tpr_t));

void	tprintf __P((tpr_t, const char *fmt, ...));
