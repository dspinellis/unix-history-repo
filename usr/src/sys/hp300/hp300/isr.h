/*
 * Copyright (c) 1982, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)isr.h	8.1 (Berkeley) %G%
 */

struct isr {
	struct	isr *isr_forw;
	struct	isr *isr_back;
	int	(*isr_intr)();
	int	isr_arg;
	int	isr_ipl;
};

#define	NISR		3
#define	ISRIPL(x)	((x) - 3)
