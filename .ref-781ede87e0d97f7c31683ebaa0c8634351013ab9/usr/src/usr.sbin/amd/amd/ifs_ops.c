/*
 * $Id: ifs_ops.c,v 5.2 90/06/23 22:19:28 jsp Rel $
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ifs_ops.c	5.1 (Berkeley) %G%
 */

#include "am.h"

#ifdef HAS_IFS

/*
 * Inheritance file system.
 * This implements a filesystem restart.
 *
 * This is a *gross* hack - it knows far too
 * much about the way other parts of the
 * sytem work.  See restart.c too.
 */
static char not_a_filesystem[] = "Attempting to inherit not-a-filesystem";
/*
 * This should never be called.
 */
static int ifs_match()
{
	plog(XLOG_FATAL, "ifs_match called!");
	return FALSE;
}

static int ifs_init(mf)
mntfs *mf;
{
	mntfs *mf_link = (mntfs *) mf->mf_private;
	if (mf_link == 0) {
		plog(XLOG_FATAL, not_a_filesystem);
		return EINVAL;
	}
	/*
	 * Fill in attribute fields
	 */
	mf_link->mf_fattr.type = NFLNK;
	mf_link->mf_fattr.mode = NFSMODE_LNK | 0777;
	mf_link->mf_fattr.nlink = 1;
	mf_link->mf_fattr.size = MAXPATHLEN / 4;
	if (mf_link->mf_ops->fs_init)
		return (*mf_link->mf_ops->fs_init)(mf_link);
	return 0;
}

/*ARGSUSED*/
static int ifs_mount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	/*
	 * Take the linked mount point and
	 * propogate.
	 */
	mntfs *mf_link = (mntfs *) mf->mf_private;
	if (mf_link == 0) {
		plog(XLOG_FATAL, not_a_filesystem);
		return EINVAL;	/*XXX*/
	}

	mf_link->mf_fo = mf->mf_fo;
	mf_link->mf_fattr.fileid = mf->mf_fattr.fileid;

	/*
	 * Discard the old map.
	 * Don't call am_unmounted since this
	 * node was never really mounted in the
	 * first place.
	 */
	mf->mf_private = 0;
	free_mntfs(mf);
	/*
	 * Free the dangling reference
	 * to the mount link.
	 */
	free_mntfs(mf_link);
	/*
	 * Get a hold of the other entry
	 */
	mp->am_mnt = mf = mf_link;
	mf->mf_flags &= ~MFF_RESTART;

	/* Say what happened */
	plog(XLOG_INFO, "restarting %s on %s", mf->mf_info, mf->mf_mount);

	/*
	 * XXX - must do the am_mounted call here
	 */
	if (mf->mf_ops->fs_flags & FS_MBACKGROUND)
		am_mounted(mp);

	new_ttl(mp);

	return 0;
}

/*ARGSUSED*/
static int ifs_umount(mp)
am_node *mp;
{
	/*
	 * Always succeed
	 */
	return 0;
}

/*
 * Ops structure
 */
am_ops ifs_ops = {
	"inherit",
	ifs_match,
	ifs_init,
	ifs_mount,
	ifs_umount,
	efs_lookuppn,
	efs_readdir,
	0, /* ifs_readlink */
	0, /* ifs_mounted */
	0, /* ifs_umounted */
	find_afs_srvr,
	FS_DISCARD
};

#endif /* HAS_IFS */
