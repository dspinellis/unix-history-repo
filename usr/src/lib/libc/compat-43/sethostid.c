/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)sethostid.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/sysctl.h>

#if __STDC__
long
sethostid(long hostid)
#else
long
sethostid(hostid)
	long hostid;
#endif
{
	int mib[2];

	mib[0] = CTL_KERN;
	mib[1] = KERN_HOSTID;
	if (sysctl(mib, 2, NULL, NULL, &hostid, sizeof hostid) == -1)
		return (-1);
	return (0);
}
