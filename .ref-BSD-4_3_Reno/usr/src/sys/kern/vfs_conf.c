/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vfs_conf.c	7.3 (Berkeley) 6/28/90
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
