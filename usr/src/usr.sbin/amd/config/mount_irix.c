/*
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mount_irix.c	5.4 (Berkeley) %G%
 *
 * $Id: mount_irix.c,v 5.2.2.1 1992/02/09 15:10:32 jsp beta $
 *
 */


/*
 * IRIX Mount helper
 */

#include "misc-irix.h"

/*
 * Map from conventional mount arguments
 * to IRIX style arguments.
 */
irix_mount(fsname, dir, flags, type, data)
char *fsname;
char *dir;
int flags;
int type;
void *data;
{
	int size;

#ifdef DEBUG
	dlog("irix_mount: fsname %s, dir %s, type %d", fsname, dir, type);
#endif /* DEBUG */

	if (type == MOUNT_TYPE_NFS) {

		size = sizeof (struct nfs_args);

		return mount(dir, dir, (MS_FSS|MS_DATA|flags),
			     type, (struct nfs_args *) data, size);

	} else if (type == MOUNT_TYPE_UFS) {

		return mount(fsname, dir, (MS_FSS|flags), type);

	} else {
		return EINVAL;
	}

}
