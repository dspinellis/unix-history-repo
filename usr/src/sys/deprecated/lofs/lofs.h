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
 *	@(#)lofs.h	8.4 (Berkeley) %G%
 *
 * $Id: lofs.h,v 1.8 1992/05/30 10:05:43 jsp Exp jsp $
 */

struct lofs_args {
	char		*target;	/* Target of loopback  */
};

struct lofsmount {
	struct mount	*looped_vfs;
	struct vnode	*rootvp;	/* Reference to root lofsnode */
};

#ifdef KERNEL
/*
 * A cache of vnode references
 */
struct lofsnode {
	struct lofsnode	*a_forw;	/* Hash chain */
	struct lofsnode	*a_back;
	struct vnode	*a_lofsvp;	/* Aliased vnode - VREFed once */
	struct vnode	*a_vnode;	/* Back pointer to vnode/lofsnode */
};

extern int lofs_init __P((void));
extern int make_lofs __P((struct mount *mp, struct vnode **vpp));

#define	VFSTOLOFS(mp) ((struct lofsmount *)((mp)->mnt_data))
#define	LOFSP(vp) ((struct lofsnode *)(vp)->v_data)
#define	LOFSVP(vp) (LOFSP(vp)->a_lofsvp)

extern int (**lofs_vnodeop_p)();
extern struct vfsops lofs_vfsops;
#endif /* KERNEL */
