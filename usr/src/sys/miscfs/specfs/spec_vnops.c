/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)spec_vnops.c	7.29 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "kernel.h"
#include "conf.h"
#include "buf.h"
#include "mount.h"
#include "vnode.h"
#include "specdev.h"
#include "stat.h"
#include "errno.h"
#include "ioctl.h"
#include "file.h"
#include "disklabel.h"

/* symbolic sleep message strings for devices */
char	devopn[] = "devopn";
char	devio[] = "devio";
char	devwait[] = "devwait";
char	devin[] = "devin";
char	devout[] = "devout";
char	devioc[] = "devioc";
char	devcls[] = "devcls";

int	spec_lookup(),
	spec_open(),
	spec_read(),
	spec_write(),
	spec_strategy(),
	spec_bmap(),
	spec_ioctl(),
	spec_select(),
	spec_lock(),
	spec_unlock(),
	spec_close(),
	spec_print(),
	spec_ebadf(),
	spec_badop(),
	spec_nullop();

struct vnodeops spec_vnodeops = {
	spec_lookup,		/* lookup */
	spec_badop,		/* create */
	spec_badop,		/* mknod */
	spec_open,		/* open */
	spec_close,		/* close */
	spec_ebadf,		/* access */
	spec_ebadf,		/* getattr */
	spec_ebadf,		/* setattr */
	spec_read,		/* read */
	spec_write,		/* write */
	spec_ioctl,		/* ioctl */
	spec_select,		/* select */
	spec_badop,		/* mmap */
	spec_nullop,		/* fsync */
	spec_badop,		/* seek */
	spec_badop,		/* remove */
	spec_badop,		/* link */
	spec_badop,		/* rename */
	spec_badop,		/* mkdir */
	spec_badop,		/* rmdir */
	spec_badop,		/* symlink */
	spec_badop,		/* readdir */
	spec_badop,		/* readlink */
	spec_badop,		/* abortop */
	spec_nullop,		/* inactive */
	spec_nullop,		/* reclaim */
	spec_lock,		/* lock */
	spec_unlock,		/* unlock */
	spec_bmap,		/* bmap */
	spec_strategy,		/* strategy */
	spec_print,		/* print */
	spec_nullop,		/* islocked */
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
	int error;

	if (vp->v_mount && (vp->v_mount->mnt_flag & MNT_NODEV))
		return (ENXIO);

	switch (vp->v_type) {

	case VCHR:
		if ((u_int)maj >= nchrdev)
			return (ENXIO);
		return ((*cdevsw[maj].d_open)(dev, mode, S_IFCHR));

	case VBLK:
		if ((u_int)maj >= nblkdev)
			return (ENXIO);
		if (error = mountedon(vp))
			return (error);
		return ((*bdevsw[maj].d_open)(dev, mode, S_IFBLK));
	}
	return (0);
}

/*
 * Vnode op for read
 */
/* ARGSUSED */
spec_read(vp, uio, ioflag, cred)
	register struct vnode *vp;
	register struct uio *uio;
	int ioflag;
	struct ucred *cred;
{
	struct buf *bp;
	daddr_t bn;
	long bsize, bscale;
	struct partinfo dpart;
	register int n, on;
	int error = 0;
	extern int mem_no;

	if (uio->uio_rw != UIO_READ)
		panic("spec_read mode");
	if (uio->uio_resid == 0)
		return (0);

	switch (vp->v_type) {

	case VCHR:
		/*
		 * Negative offsets allowed only for /dev/kmem
		 */
		if (uio->uio_offset < 0 && major(vp->v_rdev) != mem_no)
			return (EINVAL);
		VOP_UNLOCK(vp);
		error = (*cdevsw[major(vp->v_rdev)].d_read)
			(vp->v_rdev, uio, ioflag);
		VOP_LOCK(vp);
		return (error);

	case VBLK:
		if (uio->uio_offset < 0)
			return (EINVAL);
		bsize = BLKDEV_IOSIZE;
		if ((*bdevsw[major(vp->v_rdev)].d_ioctl)(vp->v_rdev, DIOCGPART,
		    (caddr_t)&dpart, FREAD) == 0) {
			if (dpart.part->p_fstype == FS_BSDFFS &&
			    dpart.part->p_frag != 0 && dpart.part->p_fsize != 0)
				bsize = dpart.part->p_frag *
				    dpart.part->p_fsize;
		}
		bscale = bsize / DEV_BSIZE;
		do {
			bn = (uio->uio_offset / DEV_BSIZE) &~ (bscale - 1);
			on = uio->uio_offset % bsize;
			n = MIN((unsigned)(bsize - on), uio->uio_resid);
			if (vp->v_lastr + bscale == bn)
				error = breada(vp, bn, (int)bsize, bn + bscale,
					(int)bsize, NOCRED, &bp);
			else
				error = bread(vp, bn, (int)bsize, NOCRED, &bp);
			vp->v_lastr = bn;
			n = MIN(n, bsize - bp->b_resid);
			if (error) {
				brelse(bp);
				return (error);
			}
			error = uiomove(bp->b_un.b_addr + on, n, uio);
			if (n + on == bsize)
				bp->b_flags |= B_AGE;
			brelse(bp);
		} while (error == 0 && uio->uio_resid > 0 && n != 0);
		return (error);

	default:
		panic("spec_read type");
	}
	/* NOTREACHED */
}

/*
 * Vnode op for write
 */
/* ARGSUSED */
spec_write(vp, uio, ioflag, cred)
	register struct vnode *vp;
	register struct uio *uio;
	int ioflag;
	struct ucred *cred;
{
	struct buf *bp;
	daddr_t bn;
	int bsize, blkmask;
	struct partinfo dpart;
	register int n, on;
	int error = 0;
	extern int mem_no;

	if (uio->uio_rw != UIO_WRITE)
		panic("spec_write mode");

	switch (vp->v_type) {

	case VCHR:
		/*
		 * Negative offsets allowed only for /dev/kmem
		 */
		if (uio->uio_offset < 0 && major(vp->v_rdev) != mem_no)
			return (EINVAL);
		VOP_UNLOCK(vp);
		error = (*cdevsw[major(vp->v_rdev)].d_write)
			(vp->v_rdev, uio, ioflag);
		VOP_LOCK(vp);
		return (error);

	case VBLK:
		if (uio->uio_resid == 0)
			return (0);
		if (uio->uio_offset < 0)
			return (EINVAL);
		bsize = BLKDEV_IOSIZE;
		if ((*bdevsw[major(vp->v_rdev)].d_ioctl)(vp->v_rdev, DIOCGPART,
		    (caddr_t)&dpart, FREAD) == 0) {
			if (dpart.part->p_fstype == FS_BSDFFS &&
			    dpart.part->p_frag != 0 && dpart.part->p_fsize != 0)
				bsize = dpart.part->p_frag *
				    dpart.part->p_fsize;
		}
		blkmask = (bsize / DEV_BSIZE) - 1;
		do {
			bn = (uio->uio_offset / DEV_BSIZE) &~ blkmask;
			on = uio->uio_offset % bsize;
			n = MIN((unsigned)(bsize - on), uio->uio_resid);
			if (n == bsize)
				bp = getblk(vp, bn, bsize);
			else
				error = bread(vp, bn, bsize, NOCRED, &bp);
			n = MIN(n, bsize - bp->b_resid);
			if (error) {
				brelse(bp);
				return (error);
			}
			error = uiomove(bp->b_un.b_addr + on, n, uio);
			if (n + on == bsize) {
				bp->b_flags |= B_AGE;
				bawrite(bp);
			} else
				bdwrite(bp);
		} while (error == 0 && uio->uio_resid > 0 && n != 0);
		return (error);

	default:
		panic("spec_write type");
	}
	/* NOTREACHED */
}

/*
 * Device ioctl operation.
 */
/* ARGSUSED */
spec_ioctl(vp, com, data, fflag, cred)
	struct vnode *vp;
	int com;
	caddr_t data;
	int fflag;
	struct ucred *cred;
{
	dev_t dev = vp->v_rdev;

	switch (vp->v_type) {

	case VCHR:
		return ((*cdevsw[major(dev)].d_ioctl)(dev, com, data, fflag));

	case VBLK:
		if (com == 0 && (int)data == B_TAPE)
			if (bdevsw[major(dev)].d_flags & B_TAPE)
				return (0);
			else
				return (1);
		return ((*bdevsw[major(dev)].d_ioctl)(dev, com, data, fflag));

	default:
		panic("spec_ioctl");
		/* NOTREACHED */
	}
}

/* ARGSUSED */
spec_select(vp, which, fflags, cred)
	struct vnode *vp;
	int which, fflags;
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
 * This is a noop, simply returning what one has been given.
 */
spec_bmap(vp, bn, vpp, bnp)
	struct vnode *vp;
	daddr_t bn;
	struct vnode **vpp;
	daddr_t *bnp;
{

	if (vpp != NULL)
		*vpp = vp;
	if (bnp != NULL)
		*bnp = bn;
	return (0);
}

/*
 * At the moment we do not do any locking.
 */
/* ARGSUSED */
spec_lock(vp)
	struct vnode *vp;
{

	return (0);
}

/* ARGSUSED */
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
	int mode;

	switch (vp->v_type) {

	case VCHR:
		/*
		 * If the vnode is locked, then we are in the midst
		 * of forcably closing the device, otherwise we only
		 * close on last reference.
		 */
		if (vcount(vp) > 1 && (vp->v_flag & VXLOCK) == 0)
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
		vflushbuf(vp, 0);
		if (vinvalbuf(vp, 1))
			return (0);
		/*
		 * We do not want to really close the device if it
		 * is still in use unless we are trying to close it
		 * forcibly. Since every use (buffer, vnode, swap, cmap)
		 * holds a reference to the vnode, and because we mark
		 * any other vnodes that alias this device, when the
		 * sum of the reference counts on all the aliased
		 * vnodes descends to one, we are on last close.
		 */
		if (vcount(vp) > 1 && (vp->v_flag & VXLOCK) == 0)
			return (0);
		cfunc = bdevsw[major(dev)].d_close;
		mode = S_IFBLK;
		break;

	default:
		panic("spec_close: not special");
	}

	return ((*cfunc)(dev, flag, mode));
}

/*
 * Print out the contents of a special device vnode.
 */
spec_print(vp)
	struct vnode *vp;
{

	printf("tag VT_NON, dev %d, %d\n", major(vp->v_rdev),
		minor(vp->v_rdev));
}

/*
 * Special device failed operation
 */
spec_ebadf()
{

	return (EBADF);
}

/*
 * Special device bad operation
 */
spec_badop()
{

	panic("spec_badop called");
	/* NOTREACHED */
}

/*
 * Special device null operation
 */
spec_nullop()
{

	return (0);
}
