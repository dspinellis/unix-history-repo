/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

struct proc;
void	pr_attime __P((time_t *, time_t *));
void	pr_idle __P((time_t));
int	proc_compare __P((struct proc *, struct proc *));
