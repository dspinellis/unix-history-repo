/*
 * Copyright (c) 1992 The Regents of the University of California
 * Copyright (c) 1990, 1992 Jan-Simon Pendry
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fdesc.h	7.1 (Berkeley) %G%
 *
 * $Id: fdesc.h,v 1.5 1992/05/30 10:05:34 jsp Exp jsp $
 */

#ifdef KERNEL
struct fdescmount {
	struct vnode	*f_root;	/* Root node */
};

struct fdescnode {
	unsigned	f_fd;			/* Fd to be dup'ed */
};

#define VFSTOFDESC(mp)	((struct fdescmount *)((mp)->mnt_data))
#define	VTOFDESC(vp) ((struct fdescnode *)(vp)->v_data)

extern int (**fdesc_vnodeop_p)();
extern struct vfsops fdesc_vfsops;
#endif /* KERNEL */
