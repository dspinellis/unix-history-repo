/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pdma.h	7.2 (Berkeley) %G%
 */

struct pdma {
	struct	dzdevice *p_addr;
	char	*p_mem;
	char	*p_end;
	int	p_arg;
	int	(*p_fcn)();
};
