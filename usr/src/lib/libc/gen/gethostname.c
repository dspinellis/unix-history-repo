/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)gethostname.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/sysctl.h>

long
gethostname(name, namelen)
	char *name;
	int namelen;
{
	int mib[2];
	size_t size;

	mib[0] = CTL_KERN;
	mib[1] = KERN_HOSTNAME;
	size = namelen;
	if (sysctl(mib, 2, name, &size, NULL, 0) == -1)
		return (-1);
	return (0);
}
