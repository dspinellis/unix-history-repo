/*
 * $Id: ufs_ops.c,v 5.2 90/06/23 22:20:03 jsp Rel $
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
 *	@(#)ufs_ops.c	5.1 (Berkeley) %G%
 */

#include "am.h"

#ifdef HAS_UFS

#include <sys/stat.h>
#ifdef NFS_3
typedef nfs_fh fhandle_t;
#endif /* NFS_3 */
#include <sys/mount.h>

#ifdef UFS_HDR
#include UFS_HDR
#endif /* UFS_HDR */

/*
 * UN*X file system
 */

/*
 * UFS needs local filesystem and device.
 */
static int ufs_match(fo)
am_opts *fo;
{
	if (!fo->opt_dev) {
		plog(XLOG_USER, "ufs: no device specified");
		return 0;
	}
	/*
	 * Determine magic cookie to put in mtab
	 */
	fo->fs_mtab = strealloc(fo->fs_mtab, fo->opt_dev);
#ifdef DEBUG
	dlog("UFS: mounting device \"%s\" on \"%s\"",
		fo->opt_dev, fo->opt_fs);
#endif /* DEBUG */

	return 1;
}

static mount_ufs(dir, fs_name, opts)
char *dir;
char *fs_name;
char *opts;
{
	struct ufs_args ufs_args;
	struct mntent mnt;
	int flags;

	/*
	 * Figure out the name of the file system type.
	 */
#ifdef M_NEWTYPE
	char *type = MOUNT_TYPE_UFS;
#else
	int type = MOUNT_TYPE_UFS;
#endif /* M_NEWTYPE */

	bzero((voidp) &ufs_args, sizeof(ufs_args));	/* Paranoid */

	/*
	 * Fill in the mount structure
	 */
	mnt.mnt_dir = dir;
	mnt.mnt_fsname = fs_name;
	mnt.mnt_type = MTAB_TYPE_UFS;
	mnt.mnt_opts = opts;
	mnt.mnt_freq = 1;
	mnt.mnt_passno = 2;

	flags = compute_mount_flags(&mnt);

#ifdef ULTRIX_HACK
	ufs_args.ufs_flags = flags;
	ufs_args.ufs_pgthresh = 64; /* 64K - XXX */
	flags &= M_RDONLY;
#else
	ufs_args.fspec = fs_name;
#endif /* ULTRIX_HACK */

	/*
	 * Call generic mount routine
	 */
	return mount_fs(&mnt, flags, (caddr_t) &ufs_args, 0, type);
}

/*ARGSUSED*/
static int ufs_mount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	int error;

	error = mount_ufs(mf->mf_mount, mf->mf_info, mf->mf_fo->opt_opts);
	if (error) {
		errno = error;
		plog(XLOG_ERROR, "mount_ufs: %m");
		return error;
	}

	return 0;
}

static int ufs_umount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	return UMOUNT_FS(mf->mf_mount);
}

/*
 * Ops structure
 */
am_ops ufs_ops = {
	"ufs",
	ufs_match,
	0, /* ufs_init */
	ufs_mount,
	ufs_umount,
	efs_lookuppn,
	efs_readdir,
	0, /* ufs_readlink */
	0, /* ufs_mounted */
	0, /* ufs_umounted */
	find_afs_srvr,
#ifdef FLUSH_KERNEL_NAME_CACHE
	FS_MKMNT|FS_NOTIMEOUT|FS_UBACKGROUND|FS_AMQINFO
#else /* FLUSH_KERNEL_NAME_CACHE */
	FS_MKMNT|FS_NOTIMEOUT|FS_UBACKGROUND|FS_AMQINFO
#endif /* FLUSH_KERNEL_NAME_CACHE */
};

#endif /* HAS_UFS */
