/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vfs_conf.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "mount.h"

/*
 * This specifies the filesystem used to mount the root.
 * This specification should be done by /etc/config.
 */
extern int ufs_mountroot();
int (*mountroot)() = ufs_mountroot;

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

#ifdef NFS
extern	struct vfsops nfs_vfsops;
#endif

#ifdef MFS
extern	struct vfsops mfs_vfsops;
#endif

struct vfsops *vfssw[] = {
	(struct vfsops *)0,	/* 0 = MOUNT_NONE */
	&ufs_vfsops,		/* 1 = MOUNT_UFS */
#ifdef NFS
	&nfs_vfsops,		/* 2 = MOUNT_NFS */
#else
	(struct vfsops *)0,
#endif
#ifdef MFS
	&mfs_vfsops,		/* 3 = MOUNT_MFS */
#else
	(struct vfsops *)0,
#endif
	(struct vfsops *)0,	/* 4 = MOUNT_PC */
};
