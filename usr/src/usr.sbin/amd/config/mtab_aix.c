/*
 * $Id: mtab_aix.c,v 5.2 90/06/23 22:20:36 jsp Rel $
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
 *	@(#)mtab_aix.c	5.1 (Berkeley) 6/29/90
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
	case MNT_NFS:  ty = MTAB_TYPE_NFS; break;
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
