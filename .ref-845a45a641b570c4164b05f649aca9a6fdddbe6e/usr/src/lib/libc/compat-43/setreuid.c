/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setreuid.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <errno.h>

int
setreuid(ruid, euid)
	uid_t ruid, euid;
{
	static uid_t saveduid = -1;
	
	if (saveduid == -1)
		saveduid = geteuid();
	/*
	 * we assume that the intent here is to be able to
	 * get back ruid priviledge. So we make sure that
	 * we will be able to do so, but do not actually
	 * set the ruid.
	 */
	if (ruid != -1 && ruid != getuid() && ruid != saveduid) {
		errno = EPERM;
		return (-1);
	}
	if (euid != -1 && seteuid(euid) < 0)
		return (-1);
	return (0);
}
