/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)atexit.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stddef.h>
#include <stdlib.h>
#include "atexit.h"

/*
 * Register a function to be performed at exit.
 */
int
atexit(fn)
	void (*fn)();
{
	static struct atexit __atexit0;	/* one guaranteed table */
	register struct atexit *p;

	if ((p = __atexit) == NULL)
		__atexit = p = &__atexit0;
	if (p->ind >= ATEXIT_SIZE) {
		if ((p = malloc(sizeof(*p))) == NULL)
			return (-1);
		__atexit->next = p;
		__atexit = p;
	}
	p->fns[p->ind++] = fn;
	return (0);
}
