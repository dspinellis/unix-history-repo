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
 *	@(#)mtab_aix.c	5.4 (Berkeley) %G%
 *
 * $Id: mtab_aix.c,v 5.2.2.1 1992/02/09 15:10:07 jsp beta $
 *
 */

#include "am.h"

#ifdef READ_MTAB_AIX3_STYLE

#include <sys/mntctl.h>
#include <sys/vmount.h>

static struct mntent *mnt_dup(mp)
struct vmount *mp;
{
	struct mntent *new_mp = ALLOC(mntent);

	char *ty;
	new_mp->mnt_fsname = strdup(vmt2dataptr(mp, VMT_OBJECT));
	new_mp->mnt_dir = strdup(vmt2dataptr(mp, VMT_STUB));
	new_mp->mnt_opts = strdup(vmt2dataptr(mp, VMT_ARGS));
	switch (mp->vmt_gfstype) {
	case MNT_JFS:  ty = MTAB_TYPE_UFS; break;
	case MNT_NFS:
		ty = MTAB_TYPE_NFS;
		new_mp->mnt_fsname = str3cat(new_mp->mnt_fsname,
				vmt2dataptr(mp, VMT_HOSTNAME),
				":", new_mp->mnt_fsname);
		break;
	default:  ty = "unknown"; break;
	}
	new_mp->mnt_type = strdup(ty);
	new_mp->mnt_passno = mp->vmt_vfsnumber;
	new_mp->mnt_freq = 0;

	return new_mp;
}

/*
 * Read a mount table into memory
 */
mntlist *read_mtab(fs)
char *fs;
{
	mntlist **mpp, *mhp;

	int i;
	char *mntinfo = 0, *cp;
	struct vmount *vp;
	int ret;

	/*
	 * First figure out size of mount table
	 * and allocate space for a copy...
	 * Then get mount table for real.
	 */
	ret = mntctl(MCTL_QUERY, sizeof(i), &i);
	if (ret == 0) {
		mntinfo = xmalloc(i);
		ret = mntctl(MCTL_QUERY, i, mntinfo);
	}

	if (ret <= 0) {
		plog(XLOG_ERROR, "mntctl: %m");
		goto out;
	}
#ifdef DEBUG
	/*dlog("mntctl returns %d structures", ret);*/
#endif /* DEBUG */

	mpp = &mhp;
	for (i = 0, cp = mntinfo; i < ret; i++, cp += vp->vmt_length) {
		vp = (struct vmount *) cp;

		/*
		 * Allocate a new slot
		 */
		*mpp = ALLOC(mntlist);

		/*
		 * Copy the data returned by mntctl
		 */
		(*mpp)->mnt = mnt_dup(vp);

		/*
		 * Move to next pointer
		 */
		mpp = &(*mpp)->mnext;
	}

	*mpp = 0;

out:
	if (mntinfo)
		free(mntinfo);
	return mhp;
}

#endif /* READ_MTAB_AIX3_STYLE */
