/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)callout.h	8.2 (Berkeley) %G%
 */

struct callout {
	struct	callout *c_next;		/* next callout in queue */
	void	*c_arg;				/* function argument */
	void	(*c_func) __P((void *));	/* function to call */
	int	c_time;				/* ticks to the event */
};

#ifdef KERNEL
struct	callout *callfree, *callout, calltodo;
int	ncallout;
#endif
