/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dead_vnops.c	7.11 (Berkeley) %G%
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
	dead_print(),
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
	dead_print,	/* print */
	dead_nullop,	/* islocked */
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
/* ARGSUSED */
dead_read(vp, uio, ioflag, cred)
	struct vnode *vp;
	struct uio *uio;
	int ioflag;
	struct ucred *cred;
{

	if (chkvnlock(vp))
		panic("dead_read: lock");
	/*
	 * Return EOF for character devices, EIO for others
	 */
	if (vp->v_type != VCHR)
		return (EIO);
	return (0);
}

/*
 * Vnode op for write
 */
/* ARGSUSED */
dead_write(vp, uio, ioflag, cred)
	register struct vnode *vp;
	struct uio *uio;
	int ioflag;
	struct ucred *cred;
{

	if (chkvnlock(vp))
		panic("dead_write: lock");
	return (EIO);
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

	if (!chkvnlock(vp))
		return (EBADF);
	return (VOP_IOCTL(vp, com, data, fflag, cred));
}

/* ARGSUSED */
dead_select(vp, which, fflags, cred)
	struct vnode *vp;
	int which, fflags;
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

	if (bp->b_vp == NULL || !chkvnlock(bp->b_vp)) {
		bp->b_flags |= B_ERROR;
		biodone(bp);
		return (EIO);
	}
	return (VOP_STRATEGY(bp));
}

/*
 * Wait until the vnode has finished changing state.
 */
dead_lock(vp)
	struct vnode *vp;
{

	if (!chkvnlock(vp))
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

	if (!chkvnlock(vp))
		return (EIO);
	return (VOP_BMAP(vp, bn, vpp, bnp));
}

/*
 * Print out the contents of a dead vnode.
 */
/* ARGSUSED */
dead_print(vp)
	struct vnode *vp;
{

	printf("tag VT_NON, dead vnode\n");
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

/*
 * We have to wait during times when the vnode is
 * in a state of change.
 */
chkvnlock(vp)
	register struct vnode *vp;
{
	int locked = 0;

	while (vp->v_flag & VXLOCK) {
		vp->v_flag |= VXWANT;
		sleep((caddr_t)vp, PINOD);
		locked = 1;
	}
	return (locked);
}
