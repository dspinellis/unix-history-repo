/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)killpg.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/errno.h>

/*
 * Backwards-compatible killpg().
 */
killpg(pgid, sig)
	pid_t pgid;
	int sig;
{
	extern int errno;

	if (pgid == 1) {
		errno = ESRCH;
		return (-1);
	}
	return (kill(-pgid, sig));
}
