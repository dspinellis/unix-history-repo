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
 *	@(#)spec_vnops.c	7.12 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "kernel.h"
#include "conf.h"
#include "buf.h"
#include "mount.h"
#include "vnode.h"
#include "stat.h"
#include "errno.h"

int	spec_lookup(),
	spec_open(),
	spec_read(),
	spec_write(),
	spec_strategy(),
	spec_ioctl(),
	spec_select(),
	spec_lock(),
	spec_unlock(),
	spec_close(),
	spec_badop(),
	spec_nullop();

struct vnodeops spec_vnodeops = {
	spec_lookup,
	spec_badop,
	spec_badop,
	spec_open,
	spec_close,
	spec_badop,
	spec_badop,
	spec_badop,
	spec_read,
	spec_write,
	spec_ioctl,
	spec_select,
	spec_badop,
	spec_nullop,
	spec_badop,
	spec_badop,
	spec_badop,
	spec_badop,
	spec_badop,
	spec_badop,
	spec_badop,
	spec_badop,
	spec_badop,
	spec_badop,
	spec_nullop,
	spec_nullop,
	spec_lock,
	spec_unlock,
	spec_badop,
	spec_strategy,
};

/*
 * Trivial lookup routine that always fails.
 */
spec_lookup(vp, ndp)
	struct vnode *vp;
	struct nameidata *ndp;
{

	ndp->ni_dvp = vp;
	ndp->ni_vp = NULL;
	return (ENOTDIR);
}

/*
 * Open called to allow handler
 * of special files to initialize and
 * validate before actual IO.
 */
/* ARGSUSED */
spec_open(vp, mode, cred)
	register struct vnode *vp;
	int mode;
	struct ucred *cred;
{
	dev_t dev = (dev_t)vp->v_rdev;
	register int maj = major(dev);

	if (vp->v_mount && (vp->v_mount->m_flag & M_NODEV))
		return (ENXIO);

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
 * Vnode op for read
 */
spec_read(vp, uio, offp, ioflag, cred)
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
spec_write(vp, uio, offp, ioflag, cred)
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
spec_ioctl(vp, com, data, fflag, cred)
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
		panic("spec_ioctl");
		/* NOTREACHED */
	}
}

/* ARGSUSED */
spec_select(vp, which, cred)
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
spec_strategy(bp)
	register struct buf *bp;
{
	(*bdevsw[major(bp->b_dev)].d_strategy)(bp);
	return (0);
}

/*
 * At the moment we do not do any locking.
 */
spec_lock(vp)
	struct vnode *vp;
{

	return (0);
}

spec_unlock(vp)
	struct vnode *vp;
{

	return (0);
}

/*
 * Device close routine
 */
/* ARGSUSED */
spec_close(vp, flag, cred)
	register struct vnode *vp;
	int flag;
	struct ucred *cred;
{
	dev_t dev = vp->v_rdev;
	int (*cfunc)();
	int error, mode;

	switch (vp->v_type) {

	case VCHR:
		/*
		 * If the vnode is locked, then we are in the midst
		 * of forcably closing the device, otherwise we only
		 * close on last reference.
		 */
		if (vp->v_count > 1 && (vp->v_flag & VXLOCK) == 0)
			return (0);
		cfunc = cdevsw[major(dev)].d_close;
		mode = S_IFCHR;
		break;

	case VBLK:
		/*
		 * On last close of a block device (that isn't mounted)
		 * we must invalidate any in core blocks, so that
		 * we can, for instance, change floppy disks.
		 */
		bflush(vp->v_mount);
		if (binval(vp->v_mount))
			return (0);
		/*
		 * We do not want to really close the device if it
		 * is still in use unless we are trying to close it
		 * forcibly. Since every use (buffer, vnode, swap, cmap)
		 * holds a reference to the vnode, and because we ensure
		 * that there cannot be more than one vnode per device,
		 * we need only check that we are down to the last
		 * reference to detect last close.
		 */
		if (vp->v_count > 1 && (vp->v_flag & VXLOCK) == 0)
			return (0);
		cfunc = bdevsw[major(dev)].d_close;
		mode = S_IFBLK;
		break;

	default:
		panic("spec_close: not special");
	}

	if (setjmp(&u.u_qsave)) {
		/*
		 * If device close routine is interrupted,
		 * must return so closef can clean up.
		 */
		error = EINTR;
	} else
		error = (*cfunc)(dev, flag, mode);
	return (error);
}

/*
 * Block device bad operation
 */
spec_badop()
{

	panic("spec_badop called");
	/* NOTREACHED */
}

/*
 * Block device null operation
 */
spec_nullop()
{

	return (0);
}
