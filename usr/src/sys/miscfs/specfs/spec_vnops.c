/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)spec_vnops.c	7.43 (Berkeley) %G%
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
#define dvp (ap->a_dvp)
#define vpp (ap->a_vpp)
#define cnp (ap->a_cnp)
{

	*vpp = NULL;
	return (ENOTDIR);
}
#undef dvp
#undef vpp
#undef cnp

/*
 * Open a special file: Don't allow open if fs is mounted -nodev,
 * and don't allow opens of block devices that are currently mounted.
 * Otherwise, call device driver open function.
 */
/* ARGSUSED */
spec_open (ap)
	struct vop_open_args *ap;
#define vp (ap->a_vp)
#define mode (ap->a_mode)
#define cred (ap->a_cred)
#define p (ap->a_p)
{
	USES_VOP_LOCK;
	USES_VOP_UNLOCK;
	dev_t dev = (dev_t)vp->v_rdev;
	register int maj = major(dev);
	int error;

	if (vp->v_mount && (vp->v_mount->mnt_flag & MNT_NODEV))
		return (ENXIO);

	switch (vp->v_type) {

	case VCHR:
		if ((u_int)maj >= nchrdev)
			return (ENXIO);
		VOP_UNLOCK(vp);
		error = (*cdevsw[maj].d_open)(dev, mode, S_IFCHR, p);
		VOP_LOCK(vp);
		return (error);

	case VBLK:
		if ((u_int)maj >= nblkdev)
			return (ENXIO);
		if (error = ufs_mountedon(vp))
			return (error);
		return ((*bdevsw[maj].d_open)(dev, mode, S_IFBLK, p));
	}
	return (0);
}
#undef vp
#undef mode
#undef cred
#undef p

/*
 * Vnode op for read
 */
/* ARGSUSED */
spec_read (ap)
	struct vop_read_args *ap;
#define vp (ap->a_vp)
#define uio (ap->a_uio)
#define ioflag (ap->a_ioflag)
#define cred (ap->a_cred)
{
	USES_VOP_LOCK;
	USES_VOP_UNLOCK;
	struct proc *p = uio->uio_procp;
	struct buf *bp;
	daddr_t bn, nextbn;
	long bsize, bscale;
	struct partinfo dpart;
	register int n, on;
	int error = 0;

#ifdef DIAGNOSTIC
	if (uio->uio_rw != UIO_READ)
		panic("spec_read mode");
	if (uio->uio_segflg == UIO_USERSPACE && uio->uio_procp != curproc)
		panic("spec_read proc");
#endif
	if (uio->uio_resid == 0)
		return (0);

	switch (vp->v_type) {

	case VCHR:
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
		    (caddr_t)&dpart, FREAD, p) == 0) {
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
			if (vp->v_lastr + bscale == bn) {
				nextbn = bn + bscale;
				error = breadn(vp, bn, (int)bsize, &nextbn,
					(int *)&bsize, 1, NOCRED, &bp);
			} else
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
#undef vp
#undef uio
#undef ioflag
#undef cred

/*
 * Vnode op for write
 */
/* ARGSUSED */
spec_write (ap)
	struct vop_write_args *ap;
#define vp (ap->a_vp)
#define uio (ap->a_uio)
#define ioflag (ap->a_ioflag)
#define cred (ap->a_cred)
{
	USES_VOP_LOCK;
	USES_VOP_UNLOCK;
	struct proc *p = uio->uio_procp;
	struct buf *bp;
	daddr_t bn;
	int bsize, blkmask;
	struct partinfo dpart;
	register int n, on;
	int error = 0;

#ifdef DIAGNOSTIC
	if (uio->uio_rw != UIO_WRITE)
		panic("spec_write mode");
	if (uio->uio_segflg == UIO_USERSPACE && uio->uio_procp != curproc)
		panic("spec_write proc");
#endif

	switch (vp->v_type) {

	case VCHR:
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
		    (caddr_t)&dpart, FREAD, p) == 0) {
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
#undef vp
#undef uio
#undef ioflag
#undef cred

/*
 * Device ioctl operation.
 */
/* ARGSUSED */
spec_ioctl (ap)
	struct vop_ioctl_args *ap;
#define vp (ap->a_vp)
#define com (ap->a_command)
#define data (ap->a_data)
#define fflag (ap->a_fflag)
#define cred (ap->a_cred)
#define p (ap->a_p)
{
	dev_t dev = vp->v_rdev;

	switch (vp->v_type) {

	case VCHR:
		return ((*cdevsw[major(dev)].d_ioctl)(dev, com, data,
		    fflag, p));

	case VBLK:
		if (com == 0 && (int)data == B_TAPE)
			if (bdevsw[major(dev)].d_flags & B_TAPE)
				return (0);
			else
				return (1);
		return ((*bdevsw[major(dev)].d_ioctl)(dev, com, data,
		   fflag, p));

	default:
		panic("spec_ioctl");
		/* NOTREACHED */
	}
}
#undef vp
#undef com
#undef data
#undef fflag
#undef cred
#undef p

/* ARGSUSED */
spec_select (ap)
	struct vop_select_args *ap;
#define vp (ap->a_vp)
#define which (ap->a_which)
#define fflags (ap->a_fflags)
#define cred (ap->a_cred)
#define p (ap->a_p)
{
	register dev_t dev;

	switch (vp->v_type) {

	default:
		return (1);		/* XXX */

	case VCHR:
		dev = vp->v_rdev;
		return (*cdevsw[major(dev)].d_select)(dev, which, p);
	}
}
#undef vp
#undef which
#undef fflags
#undef cred
#undef p

/*
 * Just call the device strategy routine
 */
spec_strategy (ap)
	struct vop_strategy_args *ap;
#define bp (ap->a_bp)
{

	(*bdevsw[major(bp->b_dev)].d_strategy)(bp);
	return (0);
}
#undef bp

/*
 * This is a noop, simply returning what one has been given.
 */
spec_bmap (ap)
	struct vop_bmap_args *ap;
#define vp (ap->a_vp)
#define bn (ap->a_bn)
#define vpp (ap->a_vpp)
#define bnp (ap->a_bnp)
{

	if (vpp != NULL)
		*vpp = vp;
	if (bnp != NULL)
		*bnp = bn;
	return (0);
}
#undef vp
#undef bn
#undef vpp
#undef bnp

/*
 * At the moment we do not do any locking.
 */
/* ARGSUSED */
spec_lock (ap)
	struct vop_lock_args *ap;
#define vp (ap->a_vp)
{

	return (0);
}
#undef vp

/* ARGSUSED */
spec_unlock (ap)
	struct vop_unlock_args *ap;
#define vp (ap->a_vp)
{

	return (0);
}
#undef vp

/*
 * Device close routine
 */
/* ARGSUSED */
spec_close (ap)
	struct vop_close_args *ap;
#define vp (ap->a_vp)
#define flag (ap->a_fflag)
#define cred (ap->a_cred)
#define p (ap->a_p)
{
	dev_t dev = vp->v_rdev;
	int (*devclose) __P((dev_t, int, int, struct proc *));
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
		devclose = cdevsw[major(dev)].d_close;
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
		devclose = bdevsw[major(dev)].d_close;
		mode = S_IFBLK;
		break;

	default:
		panic("spec_close: not special");
	}

	return ((*devclose)(dev, flag, mode, p));
}
#undef vp
#undef flag
#undef cred
#undef p

/*
 * Print out the contents of a special device vnode.
 */
spec_print (ap)
	struct vop_print_args *ap;
#define vp (ap->a_vp)
{

	printf("tag VT_NON, dev %d, %d\n", major(vp->v_rdev),
		minor(vp->v_rdev));
}
#undef vp

/*
 * Special device advisory byte-level locks.
 */
/* ARGSUSED */
spec_advlock (ap)
	struct vop_advlock_args *ap;
#define vp (ap->a_vp)
#define id (ap->a_id)
#define op (ap->a_op)
#define fl (ap->a_fl)
#define flags (ap->a_flags)
{

	return (EOPNOTSUPP);
}
#undef vp
#undef id
#undef op
#undef fl
#undef flags

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
