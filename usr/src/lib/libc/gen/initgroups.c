/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)initgroups.c	5.8 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * initgroups
 */
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
		fprintf(stderr,
		    "initgroups: %s is in too many groups, using first %d\n",
		    uname, ngroups);
	if (setgroups(ngroups, groups) < 0) {
		perror("setgroups");
		return (-1);
	}
	return (0);
}
