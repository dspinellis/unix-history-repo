/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vfs_conf.c	7.4 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/mount.h>
#include <ufs/ffs/ffs_extern.h>

/*
 * This specifies the filesystem used to mount the root.
 * This specification should be done by /etc/config.
 */
int (*mountroot)() = ffs_mountroot;

/*
 * These define the root filesystem and device.
 */
struct mount *rootfs;
struct vnode *rootdir;

/*
 * Set up the filesystem operations for vnodes.
 * The types are defined in mount.h.
 */
extern	struct vfsops ufs_vfsops;

#ifdef LFS
extern	struct vfsops lfs_vfsops;
#endif

#ifdef MFS
extern	struct vfsops mfs_vfsops;
#endif

#ifdef NFS
extern	struct vfsops nfs_vfsops;
#endif

struct vfsops *vfssw[] = {
	NULL,			/* 0 = MOUNT_NONE */
	&ufs_vfsops,		/* 1 = MOUNT_UFS */
#ifdef NFS
	&nfs_vfsops,		/* 2 = MOUNT_NFS */
#else
	NULL,
#endif
#ifdef MFS
	&mfs_vfsops,		/* 3 = MOUNT_MFS */
#else
	NULL,
#endif
	NULL,			/* 4 = MOUNT_PC */
#ifdef LFS
	&lfs_vfsops,		/* 5 = MOUNT_LFS */
#else
	NULL,
#endif
};
