/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)pause.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <signal.h>
#include <unistd.h>

/*
 * Backwards compatible pause.
 */
int
pause()
{

	return sigpause(sigblock(0L));
}
