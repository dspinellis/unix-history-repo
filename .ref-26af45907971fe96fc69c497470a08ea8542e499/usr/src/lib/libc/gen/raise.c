/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)raise.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <signal.h>
#include <unistd.h>

raise(s)
	int s;
{
	return(kill(getpid(), s));
}
