/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kernfs.h	8.6 (Berkeley) %G%
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

#define kernfs_fhtovp ((int (*) __P((struct mount *, struct fid *, \
	    struct mbuf *, struct vnode **, int *, struct ucred **)))eopnotsupp)
#define kernfs_quotactl ((int (*) __P((struct mount *, int, uid_t, caddr_t, \
	    struct proc *)))eopnotsupp)
#define kernfs_sync ((int (*) __P((struct mount *, int, struct ucred *, \
	    struct proc *)))nullop)
#define kernfs_sysctl ((int (*) __P((int *, u_int, void *, size_t *, void *, \
	    size_t, struct proc *)))eopnotsupp)
#define kernfs_vget ((int (*) __P((struct mount *, ino_t, struct vnode **))) \
	    eopnotsupp)
#define kernfs_vptofh ((int (*) __P((struct vnode *, struct fid *)))eopnotsupp)
extern int (**kernfs_vnodeop_p)();
extern struct vfsops kernfs_vfsops;
extern dev_t rrootdev;
#endif /* KERNEL */
