/*
 * $Id: mtab_bsd.c,v 5.2 90/06/23 22:20:40 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)mtab_bsd.c	5.1 (Berkeley) 6/29/90
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

	return mhp;
}

#endif /* READ_MTAB_BSD_STYLE */
