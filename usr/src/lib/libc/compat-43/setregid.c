/*
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setregid.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <errno.h>

int
setregid(rgid, egid)
	gid_t rgid, egid;
{
	static gid_t savedgid = -1;
	
	if (savedgid == -1)
		savedgid = getegid();
	/*
	 * we assume that the intent here is to be able to
	 * get back rgid priviledge. So we make sure that
	 * we will be able to do so, but do not actually
	 * set the rgid.
	 */
	if (rgid != -1 && rgid != getgid() && rgid != savedgid) {
		errno = EPERM;
		return (-1);
	}
	if (egid != -1 && setegid(egid) < 0)
		return (-1);
	return (0);
}
