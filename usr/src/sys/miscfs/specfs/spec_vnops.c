/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)spec_vnops.c	7.44 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/conf.h>
#include <sys/buf.h>
#include <sys/mount.h>
#include <sys/namei.h>
#include <sys/vnode.h>
#include <sys/specdev.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sys/disklabel.h>

/* symbolic sleep message strings for devices */
char	devopn[] = "devopn";
char	devio[] = "devio";
char	devwait[] = "devwait";
char	devin[] = "devin";
char	devout[] = "devout";
char	devioc[] = "devioc";
char	devcls[] = "devcls";

int (**spec_vnodeop_p)();
struct vnodeopv_entry_desc spec_vnodeop_entries[] = {
	{ &vop_default_desc, vn_default_error },
	{ &vop_lookup_desc, spec_lookup },		/* lookup */
	{ &vop_create_desc, spec_create },		/* create */
	{ &vop_mknod_desc, spec_mknod },		/* mknod */
	{ &vop_open_desc, spec_open },		/* open */
	{ &vop_close_desc, spec_close },		/* close */
	{ &vop_access_desc, spec_access },		/* access */
	{ &vop_getattr_desc, spec_getattr },		/* getattr */
	{ &vop_setattr_desc, spec_setattr },		/* setattr */
	{ &vop_read_desc, spec_read },		/* read */
	{ &vop_write_desc, spec_write },		/* write */
	{ &vop_ioctl_desc, spec_ioctl },		/* ioctl */
	{ &vop_select_desc, spec_select },		/* select */
	{ &vop_mmap_desc, spec_mmap },		/* mmap */
	{ &vop_fsync_desc, spec_fsync },		/* fsync */
	{ &vop_seek_desc, spec_seek },		/* seek */
	{ &vop_remove_desc, spec_remove },		/* remove */
	{ &vop_link_desc, spec_link },		/* link */
	{ &vop_rename_desc, spec_rename },		/* rename */
	{ &vop_mkdir_desc, spec_mkdir },		/* mkdir */
	{ &vop_rmdir_desc, spec_rmdir },		/* rmdir */
	{ &vop_symlink_desc, spec_symlink },		/* symlink */
	{ &vop_readdir_desc, spec_readdir },		/* readdir */
	{ &vop_readlink_desc, spec_readlink },		/* readlink */
	{ &vop_abortop_desc, spec_abortop },		/* abortop */
	{ &vop_inactive_desc, spec_inactive },		/* inactive */
	{ &vop_reclaim_desc, spec_reclaim },		/* reclaim */
	{ &vop_lock_desc, spec_lock },		/* lock */
	{ &vop_unlock_desc, spec_unlock },		/* unlock */
	{ &vop_bmap_desc, spec_bmap },		/* bmap */
	{ &vop_strategy_desc, spec_strategy },		/* strategy */
	{ &vop_print_desc, spec_print },		/* print */
	{ &vop_islocked_desc, spec_islocked },		/* islocked */
	{ &vop_advlock_desc, spec_advlock },		/* advlock */
	{ &vop_blkatoff_desc, spec_blkatoff },		/* blkatoff */
	{ &vop_vget_desc, spec_vget },		/* vget */
	{ &vop_valloc_desc, spec_valloc },		/* valloc */
	{ &vop_vfree_desc, spec_vfree },		/* vfree */
	{ &vop_truncate_desc, spec_truncate },		/* truncate */
	{ &vop_update_desc, spec_update },		/* update */
	{ &vop_bwrite_desc, spec_bwrite },		/* bwrite */
	{ (struct vnodeop_desc*)NULL, (int(*)())NULL }
};
struct vnodeopv_desc spec_vnodeop_opv_desc =
	{ &spec_vnodeop_p, spec_vnodeop_entries };

/*
 * Trivial lookup routine that always fails.
 */
int
spec_lookup (ap)
	struct vop_lookup_args *ap;
{

	*ap->a_vpp = NULL;
	return (ENOTDIR);
}

/*
 * Open a special file: Don't allow open if fs is mounted -nodev,
 * and don't allow opens of block devices that are currently mounted.
 * Otherwise, call device driver open function.
 */
/* ARGSUSED */
spec_open (ap)
	struct vop_open_args *ap;
{
	USES_VOP_LOCK;
	USES_VOP_UNLOCK;
	dev_t dev = (dev_t)ap->a_vp->v_rdev;
	register int maj = major(dev);
	int error;

	if (ap->a_vp->v_mount && (ap->a_vp->v_mount->mnt_flag & MNT_NODEV))
		return (ENXIO);

	switch (ap->a_vp->v_type) {

	case VCHR:
		if ((u_int)maj >= nchrdev)
			return (ENXIO);
		VOP_UNLOCK(ap->a_vp);
		error = (*cdevsw[maj].d_open)(dev, ap->a_mode, S_IFCHR, ap->a_p);
		VOP_LOCK(ap->a_vp);
		return (error);

	case VBLK:
		if ((u_int)maj >= nblkdev)
			return (ENXIO);
		if (error = ufs_mountedon(ap->a_vp))
			return (error);
		return ((*bdevsw[maj].d_open)(dev, ap->a_mode, S_IFBLK, ap->a_p));
	}
	return (0);
}

/*
 * Vnode op for read
 */
/* ARGSUSED */
spec_read (ap)
	struct vop_read_args *ap;
{
	USES_VOP_LOCK;
	USES_VOP_UNLOCK;
	struct proc *p = ap->a_uio->uio_procp;
	struct buf *bp;
	daddr_t bn, nextbn;
	long bsize, bscale;
	struct partinfo dpart;
	register int n, on;
	int error = 0;

#ifdef DIAGNOSTIC
	if (ap->a_uio->uio_rw != UIO_READ)
		panic("spec_read mode");
	if (ap->a_uio->uio_segflg == UIO_USERSPACE && ap->a_uio->uio_procp != curproc)
		panic("spec_read proc");
#endif
	if (ap->a_uio->uio_resid == 0)
		return (0);

	switch (ap->a_vp->v_type) {

	case VCHR:
		VOP_UNLOCK(ap->a_vp);
		error = (*cdevsw[major(ap->a_vp->v_rdev)].d_read)
			(ap->a_vp->v_rdev, ap->a_uio, ap->a_ioflag);
		VOP_LOCK(ap->a_vp);
		return (error);

	case VBLK:
		if (ap->a_uio->uio_offset < 0)
			return (EINVAL);
		bsize = BLKDEV_IOSIZE;
		if ((*bdevsw[major(ap->a_vp->v_rdev)].d_ioctl)(ap->a_vp->v_rdev, DIOCGPART,
		    (caddr_t)&dpart, FREAD, p) == 0) {
			if (dpart.part->p_fstype == FS_BSDFFS &&
			    dpart.part->p_frag != 0 && dpart.part->p_fsize != 0)
				bsize = dpart.part->p_frag *
				    dpart.part->p_fsize;
		}
		bscale = bsize / DEV_BSIZE;
		do {
			bn = (ap->a_uio->uio_offset / DEV_BSIZE) &~ (bscale - 1);
			on = ap->a_uio->uio_offset % bsize;
			n = MIN((unsigned)(bsize - on), ap->a_uio->uio_resid);
			if (ap->a_vp->v_lastr + bscale == bn) {
				nextbn = bn + bscale;
				error = breadn(ap->a_vp, bn, (int)bsize, &nextbn,
					(int *)&bsize, 1, NOCRED, &bp);
			} else
				error = bread(ap->a_vp, bn, (int)bsize, NOCRED, &bp);
			ap->a_vp->v_lastr = bn;
			n = MIN(n, bsize - bp->b_resid);
			if (error) {
				brelse(bp);
				return (error);
			}
			error = uiomove(bp->b_un.b_addr + on, n, ap->a_uio);
			if (n + on == bsize)
				bp->b_flags |= B_AGE;
			brelse(bp);
		} while (error == 0 && ap->a_uio->uio_resid > 0 && n != 0);
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
spec_write (ap)
	struct vop_write_args *ap;
{
	USES_VOP_LOCK;
	USES_VOP_UNLOCK;
	struct proc *p = ap->a_uio->uio_procp;
	struct buf *bp;
	daddr_t bn;
	int bsize, blkmask;
	struct partinfo dpart;
	register int n, on;
	int error = 0;

#ifdef DIAGNOSTIC
	if (ap->a_uio->uio_rw != UIO_WRITE)
		panic("spec_write mode");
	if (ap->a_uio->uio_segflg == UIO_USERSPACE && ap->a_uio->uio_procp != curproc)
		panic("spec_write proc");
#endif

	switch (ap->a_vp->v_type) {

	case VCHR:
		VOP_UNLOCK(ap->a_vp);
		error = (*cdevsw[major(ap->a_vp->v_rdev)].d_write)
			(ap->a_vp->v_rdev, ap->a_uio, ap->a_ioflag);
		VOP_LOCK(ap->a_vp);
		return (error);

	case VBLK:
		if (ap->a_uio->uio_resid == 0)
			return (0);
		if (ap->a_uio->uio_offset < 0)
			return (EINVAL);
		bsize = BLKDEV_IOSIZE;
		if ((*bdevsw[major(ap->a_vp->v_rdev)].d_ioctl)(ap->a_vp->v_rdev, DIOCGPART,
		    (caddr_t)&dpart, FREAD, p) == 0) {
			if (dpart.part->p_fstype == FS_BSDFFS &&
			    dpart.part->p_frag != 0 && dpart.part->p_fsize != 0)
				bsize = dpart.part->p_frag *
				    dpart.part->p_fsize;
		}
		blkmask = (bsize / DEV_BSIZE) - 1;
		do {
			bn = (ap->a_uio->uio_offset / DEV_BSIZE) &~ blkmask;
			on = ap->a_uio->uio_offset % bsize;
			n = MIN((unsigned)(bsize - on), ap->a_uio->uio_resid);
			if (n == bsize)
				bp = getblk(ap->a_vp, bn, bsize);
			else
				error = bread(ap->a_vp, bn, bsize, NOCRED, &bp);
			n = MIN(n, bsize - bp->b_resid);
			if (error) {
				brelse(bp);
				return (error);
			}
			error = uiomove(bp->b_un.b_addr + on, n, ap->a_uio);
			if (n + on == bsize) {
				bp->b_flags |= B_AGE;
				bawrite(bp);
			} else
				bdwrite(bp);
		} while (error == 0 && ap->a_uio->uio_resid > 0 && n != 0);
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
spec_ioctl (ap)
	struct vop_ioctl_args *ap;
{
	dev_t dev = ap->a_vp->v_rdev;

	switch (ap->a_vp->v_type) {

	case VCHR:
		return ((*cdevsw[major(dev)].d_ioctl)(dev, ap->a_command, ap->a_data,
		    ap->a_fflag, ap->a_p));

	case VBLK:
		if (ap->a_command == 0 && (int)ap->a_data == B_TAPE)
			if (bdevsw[major(dev)].d_flags & B_TAPE)
				return (0);
			else
				return (1);
		return ((*bdevsw[major(dev)].d_ioctl)(dev, ap->a_command, ap->a_data,
		   ap->a_fflag, ap->a_p));

	default:
		panic("spec_ioctl");
		/* NOTREACHED */
	}
}

/* ARGSUSED */
spec_select (ap)
	struct vop_select_args *ap;
{
	register dev_t dev;

	switch (ap->a_vp->v_type) {

	default:
		return (1);		/* XXX */

	case VCHR:
		dev = ap->a_vp->v_rdev;
		return (*cdevsw[major(dev)].d_select)(dev, ap->a_which, ap->a_p);
	}
}

/*
 * Just call the device strategy routine
 */
spec_strategy (ap)
	struct vop_strategy_args *ap;
{

	(*bdevsw[major(ap->a_bp->b_dev)].d_strategy)(ap->a_bp);
	return (0);
}

/*
 * This is a noop, simply returning what one has been given.
 */
spec_bmap (ap)
	struct vop_bmap_args *ap;
{

	if (ap->a_vpp != NULL)
		*ap->a_vpp = ap->a_vp;
	if (ap->a_bnp != NULL)
		*ap->a_bnp = ap->a_bn;
	return (0);
}

/*
 * At the moment we do not do any locking.
 */
/* ARGSUSED */
spec_lock (ap)
	struct vop_lock_args *ap;
{

	return (0);
}

/* ARGSUSED */
spec_unlock (ap)
	struct vop_unlock_args *ap;
{

	return (0);
}

/*
 * Device close routine
 */
/* ARGSUSED */
spec_close (ap)
	struct vop_close_args *ap;
{
	dev_t dev = ap->a_vp->v_rdev;
	int (*devclose) __P((dev_t, int, int, struct proc *));
	int mode;

	switch (ap->a_vp->v_type) {

	case VCHR:
		/*
		 * If the vnode is locked, then we are in the midst
		 * of forcably closing the device, otherwise we only
		 * close on last reference.
		 */
		if (vcount(ap->a_vp) > 1 && (ap->a_vp->v_flag & VXLOCK) == 0)
			return (0);
		devclose = cdevsw[major(dev)].d_close;
		mode = S_IFCHR;
		break;

	case VBLK:
		/*
		 * On last close of a block device (that isn't mounted)
		 * we must invalidate any in core blocks, so that
		 * we can, for instance, change floppy disks.
		 */
		vflushbuf(ap->a_vp, 0);
		if (vinvalbuf(ap->a_vp, 1))
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
		if (vcount(ap->a_vp) > 1 && (ap->a_vp->v_flag & VXLOCK) == 0)
			return (0);
		devclose = bdevsw[major(dev)].d_close;
		mode = S_IFBLK;
		break;

	default:
		panic("spec_close: not special");
	}

	return ((*devclose)(dev, ap->a_fflag, mode, ap->a_p));
}

/*
 * Print out the contents of a special device vnode.
 */
spec_print (ap)
	struct vop_print_args *ap;
{

	printf("tag VT_NON, dev %d, %d\n", major(ap->a_vp->v_rdev),
		minor(ap->a_vp->v_rdev));
}

/*
 * Special device advisory byte-level locks.
 */
/* ARGSUSED */
spec_advlock (ap)
	struct vop_advlock_args *ap;
{

	return (EOPNOTSUPP);
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
