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
 *	@(#)spec_vnops.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "kernel.h"
#include "conf.h"
#include "buf.h"
#include "vnode.h"
#include "../ufs/inode.h"
#include "stat.h"
#include "errno.h"
#include "malloc.h"

int	blk_open(),
	blk_read(),
	blk_write(),
	blk_strategy(),
	blk_ioctl(),
	blk_select(),
	blk_lock(),
	blk_unlock(),
	blk_close(),
	blk_badop(),
	blk_nullop();

int	ufs_getattr(),
	ufs_setattr(),
	ufs_access(),
	ufs_inactive();

struct vnodeops blk_vnodeops = {
	blk_badop,
	blk_badop,
	blk_badop,
	blk_open,
	blk_close,
	ufs_access,
	ufs_getattr,
	ufs_setattr,
	blk_read,
	blk_write,
	blk_ioctl,
	blk_select,
	blk_badop,
	blk_nullop,
	blk_badop,
	blk_badop,
	blk_badop,
	blk_badop,
	blk_badop,
	blk_badop,
	blk_badop,
	blk_badop,
	blk_badop,
	blk_badop,
	ufs_inactive,
	blk_lock,
	blk_unlock,
	blk_badop,
	blk_strategy,
};

/*
 * Open called to allow handler
 * of special files to initialize and
 * validate before actual IO.
 */
/* ARGSUSED */
blk_open(vp, mode, cred)
	register struct vnode *vp;
	int mode;
	struct ucred *cred;
{
	dev_t dev = (dev_t)vp->v_rdev;
	register int maj = major(dev);

	switch (vp->v_type) {

	case VCHR:
		if ((u_int)maj >= nchrdev)
			return (ENXIO);
		return ((*cdevsw[maj].d_open)(dev, mode, S_IFCHR));

	case VBLK:
		if ((u_int)maj >= nblkdev)
			return (ENXIO);
		return ((*bdevsw[maj].d_open)(dev, mode, S_IFBLK));
	}
	return (0);
}

/*
 * Check access permissions for a block device.
 */
blk_access(vp, mode, cred)
	struct vnode *vp;
	int mode;
	struct ucred *cred;
{

	return (iaccess(VTOI(vp), mode, cred));
}

/*
 * Vnode op for read
 */
blk_read(vp, uio, offp, ioflag, cred)
	register struct vnode *vp;
	struct uio *uio;
	off_t *offp;
	int ioflag;
	struct ucred *cred;
{
	int count, error;

	if (vp->v_type == VBLK && vp->v_data)
		VOP_LOCK(vp);
	uio->uio_offset = *offp;
	count = uio->uio_resid;
	error = readblkvp(vp, uio, cred, ioflag);
	*offp += count - uio->uio_resid;
	if (vp->v_type == VBLK && vp->v_data)
		VOP_UNLOCK(vp);
	return (error);
}

/*
 * Vnode op for write
 */
blk_write(vp, uio, offp, ioflag, cred)
	register struct vnode *vp;
	struct uio *uio;
	off_t *offp;
	int ioflag;
	struct ucred *cred;
{
	int count, error;

	if (vp->v_type == VBLK && vp->v_data)
		VOP_LOCK(vp);
	uio->uio_offset = *offp;
	count = uio->uio_resid;
	error = writeblkvp(vp, uio, cred, ioflag);
	*offp += count - uio->uio_resid;
	if (vp->v_type == VBLK && vp->v_data)
		VOP_UNLOCK(vp);
	return (error);
}

/*
 * Device ioctl operation.
 */
/* ARGSUSED */
blk_ioctl(vp, com, data, fflag, cred)
	struct vnode *vp;
	register int com;
	caddr_t data;
	int fflag;
	struct ucred *cred;
{
	dev_t dev = vp->v_rdev;

	switch (vp->v_type) {

	case VCHR:
		return ((*cdevsw[major(dev)].d_ioctl)(dev, com, data, fflag));

	case VBLK:
		return ((*bdevsw[major(dev)].d_ioctl)(dev, com, data, fflag));

	default:
		panic("blk_ioctl");
		/* NOTREACHED */
	}
}

/* ARGSUSED */
blk_select(vp, which, cred)
	struct vnode *vp;
	int which;
	struct ucred *cred;
{
	register dev_t dev;

	switch (vp->v_type) {

	default:
		return (1);		/* XXX */

	case VCHR:
		dev = vp->v_rdev;
		return (*cdevsw[major(dev)].d_select)(dev, which);
	}
}

/*
 * Just call the device strategy routine
 */
blk_strategy(bp)
	register struct buf *bp;
{
	(*bdevsw[major(bp->b_dev)].d_strategy)(bp);
	return (0);
}

blk_lock(vp)
	struct vnode *vp;
{
	register struct inode *ip = VTOI(vp);

	if (ip)
		ILOCK(ip);
	return (0);
}

blk_unlock(vp)
	struct vnode *vp;
{
	register struct inode *ip = VTOI(vp);

	if (ip)
		IUNLOCK(ip);
	return (0);
}

/*
 * Device close routine
 */
/* ARGSUSED */
blk_close(vp, flag, cred)
	register struct vnode *vp;
	int flag;
	struct ucred *cred;
{
	register struct inode *ip = VTOI(vp);
	dev_t dev = vp->v_rdev;
	int (*cfunc)();
	int error, mode;

	if (vp->v_count > 1 && !(ip->i_flag & ILOCKED))
		ITIMES(ip, &time, &time);

	switch (vp->v_type) {

	case VCHR:
		if (vp->v_count > 1)
			return (0);
		cfunc = cdevsw[major(dev)].d_close;
		mode = IFCHR;
		break;

	case VBLK:
		/*
		 * On last close of a block device (that isn't mounted)
		 * we must invalidate any in core blocks, so that
		 * we can, for instance, change floppy disks.
		 */
		bflush(dev);
		binval(dev);
		/*
		 * We don't want to really close the device if it is still
		 * in use. Since every use (buffer, inode, swap, cmap)
		 * holds a reference to the vnode, and because we ensure
		 * that there cannot be more than one vnode per device,
		 * we need only check that we are down to the last
		 * reference before closing.
		 */
		if (vp->v_count > 1)
			return (0);
		cfunc = bdevsw[major(dev)].d_close;
		mode = IFBLK;
		break;

	default:
		panic("blk_close: not special");
	}

	/* XXX what is this doing below the vnode op call */
	if (setjmp(&u.u_qsave)) {
		/*
		 * If device close routine is interrupted,
		 * must return so closef can clean up.
		 */
		error = EINTR;
	} else
		error = (*cfunc)(dev, flag, mode);
	/*
	 * Most device close routines don't return errors,
	 * and dup2() doesn't work right on error.
	 */
	error = 0;		/* XXX */
	return (error);
}

/*
 * Block device bad operation
 */
blk_badop()
{

	printf("blk_badop called\n");
	return (ENXIO);
}

/*
 * Block device null operation
 */
blk_nullop()
{

	return (0);
}
