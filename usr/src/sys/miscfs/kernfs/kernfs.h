/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kernfs.h	8.3 (Berkeley) %G%
 */

#define	_PATH_KERNFS	"/kern"		/* Default mountpoint */

#ifdef KERNEL
struct kernfs_mount {
	struct vnode	*kf_root;	/* Root node */
};

struct kernfs_node {
	struct kern_target *kf_kt;
};

#define VFSTOKERNFS(mp)	((struct kernfs_mount *)((mp)->mnt_data))
#define	VTOKERN(vp) ((struct kernfs_node *)(vp)->v_data)

extern int (**kernfs_vnodeop_p)();
extern struct vfsops kernfs_vfsops;
extern struct vnode *rrootvp;
#endif /* KERNEL */
