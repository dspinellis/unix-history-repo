/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)null.h	8.2 (Berkeley) %G%
 *
 * $Id: lofs.h,v 1.8 1992/05/30 10:05:43 jsp Exp jsp $
 */

struct null_args {
	char		*target;	/* Target of loopback  */
};

struct null_mount {
	struct mount	*nullm_vfs;
	struct vnode	*nullm_rootvp;	/* Reference to root null_node */
};

#ifdef KERNEL
/*
 * A cache of vnode references
 */
struct null_node {
	struct null_node	*null_forw;	/* Hash chain */
	struct null_node	*null_back;
	struct vnode	        *null_lowervp;	/* VREFed once */
	struct vnode		*null_vnode;	/* Back pointer */
};

extern int null_node_create __P((struct mount *mp, struct vnode *target, struct vnode **vpp));

#define	MOUNTTONULLMOUNT(mp) ((struct null_mount *)((mp)->mnt_data))
#define	VTONULL(vp) ((struct null_node *)(vp)->v_data)
#define	NULLTOV(xp) ((xp)->null_vnode)
#ifdef NULLFS_DIAGNOSTIC
extern struct vnode *null_checkvp __P((struct vnode *vp, char *fil, int lno));
#define	NULLVPTOLOWERVP(vp) null_checkvp((vp), __FILE__, __LINE__)
#else
#define	NULLVPTOLOWERVP(vp) (VTONULL(vp)->null_lowervp)
#endif

extern int (**null_vnodeop_p)();
extern struct vfsops null_vfsops;
#endif /* KERNEL */
