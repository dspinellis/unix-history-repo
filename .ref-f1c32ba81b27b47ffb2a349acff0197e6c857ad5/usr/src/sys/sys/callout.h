/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)callout.h	7.2 (Berkeley) %G%
 */

struct callout {
	struct callout *c_next;		/* next callout in queue */
	caddr_t c_arg;			/* function argument */
	int (*c_func)();		/* function to call */
	int c_time;			/* ticks to the event */
};

#ifdef KERNEL
struct callout *callfree, *callout, calltodo;
int ncallout;
#endif
