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
 *	@(#)mtab_bsd.c	5.5 (Berkeley) %G%
 *
 * $Id: mtab_bsd.c,v 5.2.2.1 1992/02/09 15:10:13 jsp beta $
 *
 */

#include "am.h"

#ifdef READ_MTAB_BSD_STYLE

#include <sys/mount.h>

static struct mntent *mnt_dup(mp)
struct statfs *mp;
{
	struct mntent *new_mp = ALLOC(mntent);
	char *ty;

	new_mp->mnt_fsname = strdup(mp->f_mntfromname);
	new_mp->mnt_dir = strdup(mp->f_mntonname);
	switch (mp->f_type) {
	case MOUNT_UFS:  ty = MTAB_TYPE_UFS; break;
	case MOUNT_NFS:  ty = MTAB_TYPE_NFS; break;
	case MOUNT_MFS:  ty = MTAB_TYPE_MFS; break;
	default:  ty = "unknown"; break;
	}
	new_mp->mnt_type = strdup(ty);
	new_mp->mnt_opts = strdup("unset");
	new_mp->mnt_freq = 0;
	new_mp->mnt_passno = 0;

	return new_mp;
}

/*
 * Read a mount table into memory
 */
mntlist *read_mtab(fs)
char *fs;
{
	mntlist **mpp, *mhp;
	struct statfs *mntbufp, *mntp;

	int nloc = getmntinfo(&mntbufp, MNT_NOWAIT);

	if (nloc == 0) {
		plog(XLOG_ERROR, "Can't read mount table");
		return 0;
	}

	mpp = &mhp;
	for (mntp = mntbufp; mntp < mntbufp + nloc; mntp++) {
		/*
		 * Allocate a new slot
		 */
		*mpp = ALLOC(mntlist);

		/*
		 * Copy the data returned by getmntent
		 */
		(*mpp)->mnt = mnt_dup(mntp);

		/*
		 * Move to next pointer
		 */
		mpp = &(*mpp)->mnext;
	}

	/*
	 * Terminate the list
	 */
	*mpp = 0;

	return mhp;
}

#endif /* READ_MTAB_BSD_STYLE */
