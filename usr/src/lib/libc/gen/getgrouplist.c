/*
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getgrouplist.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * get credential
 */
#include <sys/types.h>
#include <string.h>
#include <grp.h>

int
getgrouplist(uname, agroup, groups, grpcnt)
	const char *uname;
	int agroup;
	register int *groups;
	int *grpcnt;
{
	register struct group *grp;
	register struct passwd *pw;
	register int i, ngroups;
	int ret, maxgroups;

	ret = 0;
	ngroups = 0;
	maxgroups = *grpcnt;
	/*
	 * When installing primary group, duplicate it;
	 * the first element of groups is the effective gid
	 * and will be overwritten when a setgid file is executed.
	 */
	groups[ngroups++] = agroup;
	if (maxgroups > 1)
		groups[ngroups++] = agroup;
	/*
	 * Scan the group file to find additional groups.
	 */
	setgrent();
	while (grp = getgrent()) {
		if (grp->gr_gid == agroup)
			continue;
		if (ngroups >= maxgroups) {
			ret = -1;
			break;
		}
		for (i = 0; grp->gr_mem[i]; i++) {
			if (!strcmp(grp->gr_mem[i], uname)) {
				groups[ngroups++] = grp->gr_gid;
				break;
			}
		}
	}
	endgrent();
	*grpcnt = ngroups;
	return (ret);
}
