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
 *	@(#)mount_stellix.c	5.1 (Berkeley) %G%
 */

/*
 * IRIX Mount helper
 */

#include "misc-stellix.h"

/*
 * Map from conventional mount arguments
 * to IRIX style arguments.
 */
stellix_mount(fsname, dir, flags, type, data)
char *fsname;
char *dir;
int flags;
int type;
void *data;
{

#ifdef DEBUG
	dlog("stellix_mount: fsname %s, dir %s, type %d", fsname, dir, type);
#endif /* DEBUG */

	if (type == MOUNT_TYPE_NFS) {

		return mount(dir, dir, (MS_FSS|MS_NFS|flags),
			     type, (caddr_t) data );

	} else if (type == MOUNT_TYPE_UFS) {

		return mount(fsname, dir, (MS_FSS|flags), type);

	} else {
		return EINVAL;
	}

}
