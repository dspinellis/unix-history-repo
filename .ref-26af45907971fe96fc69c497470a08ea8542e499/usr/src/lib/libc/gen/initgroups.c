/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)initgroups.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>

#include <stdio.h>

int
initgroups(uname, agroup)
	const char *uname;
	int agroup;
{
	int groups[NGROUPS], ngroups;

	ngroups = NGROUPS;
	if (getgrouplist(uname, agroup, groups, &ngroups) < 0)
		warnx("%s is in too many groups, using first %d",
		    uname, ngroups);
	if (setgroups(ngroups, groups) < 0) {
		warn("setgroups");
		return (-1);
	}
	return (0);
}
