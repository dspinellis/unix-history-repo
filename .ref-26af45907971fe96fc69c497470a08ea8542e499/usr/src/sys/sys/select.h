/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)select.h	8.1 (Berkeley) %G%
 */

#ifndef _SELECT_H_
#define	_SELECT_H_

/*
 * Used to maintain information about processes that wish to be
 * notified when I/O becomes possible.
 */
struct selinfo {
	pid_t	si_pid;		/* process to be notified */
	short	si_flags;	/* see below */
};
#define	SI_COLL	0x0001		/* collision occurred */

#ifdef KERNEL
struct proc;

void	selrecord __P((struct proc *selector, struct selinfo *));
void	selwakeup __P((struct selinfo *));
#endif

#endif /* !_SELECT_H_ */
