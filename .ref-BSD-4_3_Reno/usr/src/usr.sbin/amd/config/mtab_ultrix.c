/*
 * $Id: mtab_ultrix.c,v 5.2 90/06/23 22:20:57 jsp Rel $
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
 *	@(#)mtab_ultrix.c	5.1 (Berkeley) 6/29/90
 */

#include "am.h"

#ifdef READ_MTAB_ULTRIX_STYLE

#include <sys/mount.h>
#include <sys/fs_types.h>

static struct mntent *mnt_dup(mp)
struct fs_data *mp;
{
	struct mntent *new_mp = ALLOC(mntent);

	new_mp->mnt_fsname = strdup(mp->fd_devname);
	new_mp->mnt_dir = strdup(mp->fd_path);
        if (mp->fd_fstype >= GT_NUMTYPES)
                mp->fd_fstype = GT_UNKWN;
        else if (gt_names[mp->fd_fstype] == 0)
                mp->fd_fstype = GT_UNKWN;
        new_mp->mnt_type = strdup(gt_names[mp->fd_fstype]);
	new_mp->mnt_opts = strdup("unset");

	new_mp->mnt_freq = 0;
	new_mp->mnt_passno = mp->fd_dev;

	return new_mp;
}

/*
 * Read a mount table into memory
 */
mntlist *read_mtab(fs)
char *fs;
{
	mntlist **mpp, *mhp;

/* From: Piete Brooks <pb@cl.cam.ac.uk> */

	int loc=0;
#undef	NMOUNT
#define	NMOUNT	20
	struct fs_data mountbuffer[NMOUNT], *fs_data;
	int ret;

	mpp = &mhp;
	while ((ret = getmountent(&loc, mountbuffer, NMOUNT)) > 0) {
	        for (fs_data = mountbuffer; fs_data < &mountbuffer[ret]; fs_data++) {
			/*
			 * Allocate a new slot
			 */
			*mpp = ALLOC(mntlist);

			/*
			 * Copy the data returned by getmntent
			 */
			(*mpp)->mnt = mnt_dup(fs_data);

			/*
			 * Move to next pointer
			 */
			mpp = &(*mpp)->mnext;
		}
	}
	if (ret < 0) {
		plog(XLOG_ERROR, "getmountent: %m");
		return 0;
	}
	*mpp = 0;

	return mhp;
}

#endif /* READ_MTAB_ULTRIX_STYLE */
