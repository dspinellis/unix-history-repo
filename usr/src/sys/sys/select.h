/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)select.h	7.1 (Berkeley) %G%
 */

#ifdef KERNEL
/*
 * Used to maintain information about processes that wish to be
 * notified when I/O becomes possible.
 */
struct selinfo {
	pid_t	si_pid;		/* process to be notified */
	short	si_flags;	/* see below */
};
#define	SI_COLL	0x0001		/* collision occurred */

void	selrecord __P((struct proc *selector, struct selinfo *));
void	selwakeup __P((struct selinfo *));

#endif	/* KERNEL */
