/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)killpg.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <signal.h>
#include <errno.h>

/*
 * Backwards-compatible killpg().
 */
#if __STDC__
killpg(pid_t pgid, int sig)
#else
killpg(pgid, sig)
	pid_t pgid;
	int sig;
#endif
{
	if (pgid == 1) {
		errno = ESRCH;
		return (-1);
	}
	return (kill(-pgid, sig));
}
