/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)gethostid.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/sysctl.h>

#if __STDC__
long
gethostid(void)
#else
long
gethostid()
#endif
{
	int mib[2];
	size_t size;
	long value;

	mib[0] = CTL_KERN;
	mib[1] = KERN_HOSTID;
	size = sizeof value;
	if (sysctl(mib, 2, &value, &size, NULL, 0) == -1)
		return (-1);
	return (value);
}
