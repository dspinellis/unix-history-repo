/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)dead_vnops.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "time.h"
#include "vnode.h"
#include "errno.h"
#include "namei.h"
#include "buf.h"

int	dead_lookup(),
	dead_open(),
	dead_read(),
	dead_write(),
	dead_strategy(),
	dead_ioctl(),
	dead_select(),
	dead_lock(),
	dead_bmap(),
	dead_ebadf(),
	dead_badop(),
	dead_nullop();

struct vnodeops dead_vnodeops = {
	dead_lookup,	/* lookup */
	dead_badop,	/* create */
	dead_badop,	/* mknod */
	dead_open,	/* open */
	dead_nullop,	/* close */
	dead_ebadf,	/* access */
	dead_ebadf,	/* getattr */
	dead_ebadf,	/* setattr */
	dead_read,	/* read */
	dead_write,	/* write */
	dead_ioctl,	/* ioctl */
	dead_select,	/* select */
	dead_badop,	/* mmap */
	dead_nullop,	/* fsync */
	dead_nullop,	/* seek */
	dead_badop,	/* remove */
	dead_badop,	/* link */
	dead_badop,	/* rename */
	dead_badop,	/* mkdir */
	dead_badop,	/* rmdir */
	dead_badop,	/* symlink */
	dead_ebadf,	/* readdir */
	dead_ebadf,	/* readlink */
	dead_badop,	/* abortop */
	dead_nullop,	/* inactive */
	dead_nullop,	/* reclaim */
	dead_lock,	/* lock */
	dead_nullop,	/* unlock */
	dead_bmap,	/* bmap */
	dead_strategy,	/* strategy */
};

/*
 * Trivial lookup routine that always fails.
 */
dead_lookup(vp, ndp)
	struct vnode *vp;
	struct nameidata *ndp;
{

	ndp->ni_dvp = vp;
	ndp->ni_vp = NULL;
	return (ENOTDIR);
}

/*
 * Open always fails as if device did not exist.
 */
/* ARGSUSED */
dead_open(vp, mode, cred)
	struct vnode *vp;
	int mode;
	struct ucred *cred;
{

	return (ENXIO);
}

/*
 * Vnode op for read
 */
dead_read(vp, uio, offp, ioflag, cred)
	struct vnode *vp;
	struct uio *uio;
	off_t *offp;
	int ioflag;
	struct ucred *cred;
{
	int locked = 0;

	/*
	 * We have to wait during times when the vnode is
	 * in a state of change.
	 */
	while (vp->v_flag & VXLOCK) {
		vp->v_flag |= VXWANT;
		sleep((caddr_t)vp, PINOD);
		locked = 1;
	}
	if (!locked)
		return (EIO);
	return (VOP_READ(vp, uio, offp, ioflag, cred));
}

/*
 * Vnode op for write
 */
dead_write(vp, uio, offp, ioflag, cred)
	register struct vnode *vp;
	struct uio *uio;
	off_t *offp;
	int ioflag;
	struct ucred *cred;
{
	int locked = 0;

	/*
	 * We have to wait during times when the vnode is
	 * in a state of change.
	 */
	while (vp->v_flag & VXLOCK) {
		vp->v_flag |= VXWANT;
		sleep((caddr_t)vp, PINOD);
		locked = 1;
	}
	if (!locked)
		return (EIO);
	return (VOP_WRITE(vp, uio, offp, ioflag, cred));
}

/*
 * Device ioctl operation.
 */
/* ARGSUSED */
dead_ioctl(vp, com, data, fflag, cred)
	struct vnode *vp;
	register int com;
	caddr_t data;
	int fflag;
	struct ucred *cred;
{
	int locked = 0;

	/*
	 * We have to wait during times when the vnode is
	 * in a state of change.
	 */
	while (vp->v_flag & VXLOCK) {
		vp->v_flag |= VXWANT;
		sleep((caddr_t)vp, PINOD);
		locked = 1;
	}
	if (!locked)
		return (EBADF);
	return (VOP_IOCTL(vp, com, data, fflag, cred));
}

/* ARGSUSED */
dead_select(vp, which, cred)
	struct vnode *vp;
	int which;
	struct ucred *cred;
{

	/*
	 * Let the user find out that the descriptor is gone.
	 */
	return (1);
}

/*
 * Just call the device strategy routine
 */
dead_strategy(bp)
	register struct buf *bp;
{
	int locked = 0;

	/*
	 * We have to wait during times when the vnode is
	 * in a state of change.
	 */
	while (bp->b_vp->v_flag & VXLOCK) {
		bp->b_vp->v_flag |= VXWANT;
		sleep((caddr_t)bp->b_vp, PINOD);
		locked = 1;
	}
	if (!locked)
		return (EIO);
	return (VOP_STRATEGY(bp));
}

/*
 * Wait until the vnode has finished changing state.
 */
dead_lock(vp)
	struct vnode *vp;
{
	int locked = 0;

	/*
	 * We have to wait during times when the vnode is
	 * in a state of change.
	 */
	while (vp->v_flag & VXLOCK) {
		vp->v_flag |= VXWANT;
		sleep((caddr_t)vp, PINOD);
		locked = 1;
	}
	if (!locked)
		return (0);
	return (VOP_LOCK(vp));
}

/*
 * Wait until the vnode has finished changing state.
 */
dead_bmap(vp, bn, vpp, bnp)
	struct vnode *vp;
	daddr_t bn;
	struct vnode **vpp;
	daddr_t *bnp;
{
	int locked = 0;

	/*
	 * We have to wait during times when the vnode is
	 * in a state of change.
	 */
	while (vp->v_flag & VXLOCK) {
		vp->v_flag |= VXWANT;
		sleep((caddr_t)vp, PINOD);
		locked = 1;
	}
	if (!locked)
		return (EIO);
	return (VOP_BMAP(vp, bn, vpp, bnp));
}

/*
 * Empty vnode failed operation
 */
dead_ebadf()
{

	return (EBADF);
}

/*
 * Empty vnode bad operation
 */
dead_badop()
{

	panic("dead_badop called");
	/* NOTREACHED */
}

/*
 * Empty vnode null operation
 */
dead_nullop()
{

	return (0);
}
