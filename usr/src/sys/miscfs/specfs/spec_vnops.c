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
 *	@(#)spec_vnops.c	7.2 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "time.h"
#include "conf.h"
#include "buf.h"
#include "vnode.h"
#include "../ufs/inode.h"
#include "stat.h"
#include "uio.h"
#include "errno.h"
#include "malloc.h"

int	blk_open(),
	blk_access(),
	blk_read(),
	blk_write(),
	blk_strategy(),
	blk_ioctl(),
	blk_select(),
	blk_inactive(),
	blk_lock(),
	blk_unlock(),
	blk_close(),
	blk_badop(),
	blk_nullop();

int	ufs_getattr(),
	ufs_setattr();

struct vnodeops blk_vnodeops = {
	blk_badop,
	blk_badop,
	blk_badop,
	blk_open,
	blk_close,
	blk_access,
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
	blk_inactive,
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
	register struct inode *ip = VTOI(vp);
	int error;

	if ((ip->i_flag & ILOCKED) == 0)
		printf("access called with %d not locked\n", ip->i_number);
	error = iaccess(ip, mode, cred);
	return (error);
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
	register struct inode *ip = VTOI(vp);
	int count, error;

	if (vp->v_type == VBLK && ip)
		ILOCK(ip);
	uio->uio_offset = *offp;
	count = uio->uio_resid;
	error = readblkvp(vp, uio, cred);
	*offp += count - uio->uio_resid;
	if (vp->v_type == VBLK && ip)
		IUNLOCK(ip);
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
	register struct inode *ip = VTOI(vp);
	int count, error;

	if (vp->v_type == VBLK && ip)
		ILOCK(ip);
	uio->uio_offset = *offp;
	count = uio->uio_resid;
	error = writeblkvp(vp, uio, cred);
	*offp += count - uio->uio_resid;
	if (vp->v_type == VBLK && ip)
		IUNLOCK(ip);
	return (error);
}

/*
 * Device ioctl operation.
 */
blk_ioctl(vp, com, data, fflag, cred)
	struct vnode *vp;
	register int com;
	caddr_t data;
	int fflag;
	struct ucred *cred;
{
	register struct inode *ip = VTOI(vp);
	dev_t dev = ip->i_rdev;

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

blk_select(vp, which, cred)
	struct vnode *vp;
	int which;
	struct ucred *cred;
{
	register struct inode *ip = VTOI(vp);
	register dev_t dev;

	switch (vp->v_type) {

	default:
		return (1);		/* XXX */

	case VCHR:
		dev = ip->i_rdev;
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

blk_inactive(vp)
	struct vnode *vp;
{
	struct inode *ip = VTOI(vp);
	struct vnode *devvp = 0;
	int error;

	if (vp->v_count > 0)
		return (0);
	return (irele(ip));
}

/*
 * Device close routine
 */
blk_close(vp, flag, cred)
	struct vnode *vp;
	int flag;
	struct ucred *cred;
{
	dev_t dev = vp->v_rdev;
	int type = vp->v_type;

	return (closei(dev, type, flag));
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
