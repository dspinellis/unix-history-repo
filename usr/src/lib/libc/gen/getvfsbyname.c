/*
 * Copyright (c) 1995
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getvfsbyname.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/mount.h>
#include <sys/sysctl.h>
#include <errno.h>
#include <kvm.h>

int getvfsbyname __P((const char *, struct vfsconf *));

/*
 * Given a filesystem name, determine if it is resident in the kernel,
 * and if it is resident, return its vfsconf structure.
 */
getvfsbyname(fsname, vfcp)
	const char *fsname;
	struct vfsconf *vfcp;
{
	int name[4], maxtypenum, cnt;
	size_t buflen;

	name[0] = CTL_VFS;
	name[1] = VFS_GENERIC;
	name[2] = VFS_MAXTYPENUM;
	buflen = 4;
	if (sysctl(name, 3, &maxtypenum, &buflen, (void *)0, (size_t)0) < 0)
		return (-1);
	name[2] = VFS_CONF;
	buflen = sizeof *vfcp;
	for (cnt = 0; cnt < maxtypenum; cnt++) {
		name[3] = cnt;
		if (sysctl(name, 4, vfcp, &buflen, (void *)0, (size_t)0) < 0) {
			if (errno != EOPNOTSUPP)
				return (-1);
			continue;
		}
		if (!strcmp(fsname, vfcp->vfc_name))
			return (0);
	}
	errno = ENOENT;
	return (-1);
}
