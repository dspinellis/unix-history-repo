/*-
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)uname.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/utsname.h>

int
uname(name)
	struct utsname *name;
{
	int mib[2], rval;
	size_t len;
	char *p;

	rval = 0;

	mib[0] = CTL_KERN;
	mib[1] = KERN_OSTYPE;
	len = sizeof(name->sysname);
	if (sysctl(mib, 2, &name->sysname, &len, NULL, 0) == -1)
		rval = -1;

	mib[0] = CTL_KERN;
	mib[1] = KERN_HOSTNAME;
	len = sizeof(name->nodename);
	if (sysctl(mib, 2, &name->nodename, &len, NULL, 0) == -1)
		rval = -1;

	mib[0] = CTL_KERN;
	mib[1] = KERN_OSRELEASE;
	len = sizeof(name->release);
	if (sysctl(mib, 2, &name->release, &len, NULL, 0) == -1)
		rval = -1;

	/* The version may have newlines in it, turn them into spaces. */
	mib[0] = CTL_KERN;
	mib[1] = KERN_VERSION;
	len = sizeof(name->version);
	if (sysctl(mib, 2, &name->version, &len, NULL, 0) == -1)
		rval = -1;
	else
		for (p = name->version; len--; ++p)
			if (*p == '\n' || *p == '\t')
				if (len > 1)
					*p = ' ';
				else
					*p = '\0';

	mib[0] = CTL_HW;
	mib[1] = HW_MACHINE;
	len = sizeof(name->machine);
	if (sysctl(mib, 2, &name->machine, &len, NULL, 0) == -1)
		rval = -1;
	return (rval);
}
