/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)exit.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>
#include <unistd.h>
#include "atexit.h"

void (*__cleanup)();

/*
 * Exit, flushing stdio buffers if necessary.
 */
void
exit(status)
	int status;
{
	register struct atexit *p;
	register int n;

	for (p = __atexit; p; p = p->next)
		for (n = p->ind; --n >= 0;)
			(*p->fns[n])();
	if (__cleanup)
		(*__cleanup)();
	_exit(status);
}
