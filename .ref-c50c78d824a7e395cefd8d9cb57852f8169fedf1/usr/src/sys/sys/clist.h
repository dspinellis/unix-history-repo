/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)clist.h	7.2 (Berkeley) %G%
 */

/*
 * Raw structures for the character list routines.
 */
struct cblock {
	struct cblock *c_next;
	char	c_quote[CBQSIZE];
	char	c_info[CBSIZE];
};
#ifdef KERNEL
struct	cblock *cfree;
int	nclist;
struct	cblock *cfreelist;
int	cfreecount;
#endif
