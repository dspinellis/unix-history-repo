/*
 * $Id: umount_fs.c,v 5.2 90/06/23 22:20:04 jsp Rel $
 *
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
 *	@(#)umount_fs.c	5.1 (Berkeley) %G%
 */

#include "am.h"

#ifdef NEED_UMOUNT_BSD

#include <sys/mount.h>		/* For MNT_NOFORCE */

int umount_fs(fs_name)
char *fs_name;
{
	int error;

eintr:
	error = unmount(fs_name, MNT_NOFORCE);
	if (error < 0)
		error = errno;

	switch (error) {
	case EINVAL:
	case ENOTBLK:
		plog(XLOG_WARNING, "unmount: %s is not mounted", fs_name);
		error = 0;	/* Not really an error */
		break;

	case ENOENT:
		plog(XLOG_ERROR, "mount point %s: %m", fs_name);
		break;

	case EINTR:
#ifdef DEBUG
		/* not sure why this happens, but it does.  ask kirk one day... */
		dlog("%s: unmount: %m", fs_name);
#endif /* DEBUG */
		goto eintr;

#ifdef DEBUG
	default:
		dlog("%s: unmount: %m", fs_name);
		break;
#endif /* DEBUG */
	}

	return error;
}

#endif /* NEED_UMOUNT_BSD */

#ifdef NEED_UMOUNT_FS

int umount_fs(fs_name)
char *fs_name;
{
	mntlist *mlist, *mp, *mp_save = 0;
	int error = 0;

	mp = mlist = read_mtab(fs_name);

	/*
	 * Search the mount table looking for
	 * the correct (ie last) matching entry
	 */
	while (mp) {
		if (strcmp(mp->mnt->mnt_fsname, fs_name) == 0 ||
				strcmp(mp->mnt->mnt_dir, fs_name) == 0)
			mp_save = mp;
		mp = mp->mnext;
	}

	if (mp_save) {
#ifdef DEBUG
		dlog("Trying unmount(%s)", mp_save->mnt->mnt_dir);
#endif /* DEBUG */
		if (UNMOUNT_TRAP(mp_save->mnt) < 0) {
			switch (error = errno) {
			case EINVAL:
			case ENOTBLK:
				plog(XLOG_WARNING, "unmount: %s is not mounted", mp_save->mnt->mnt_dir);
				error = 0;	/* Not really an error */
				break;

			case ENOENT:
				plog(XLOG_ERROR, "mount point %s: %m", mp_save->mnt->mnt_dir);
				break;

			default:
#ifdef DEBUG
				dlog("%s: unmount: %m", mp_save->mnt->mnt_dir);
#endif /* DEBUG */
				break;
			}
		}

#ifdef UPDATE_MTAB
		if (!error) {
			mnt_free(mp_save->mnt);
			mp_save->mnt = 0;

			rewrite_mtab(mlist);
		}
#endif /* UPDATE_MTAB */
	} else {
		plog(XLOG_ERROR, "Couldn't find how to unmount %s", fs_name);
		/*
		 * Assume it is already unmounted
		 */
		error = 0;
	}

	free_mntlist(mlist);

	return error;
}

#endif /* NEED_UMOUNT_FS */
