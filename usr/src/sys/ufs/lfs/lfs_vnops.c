/*
 * Copyright (c) 1986, 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_vnops.c	7.70 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/namei.h>
#include <sys/resourcevar.h>
#include <sys/kernel.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/conf.h>
#include <sys/mount.h>
#include <sys/vnode.h>
#include <sys/specdev.h>
#include <sys/fifo.h>
#include <sys/malloc.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/dir.h>
#include <ufs/ufs/ufs_extern.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

/* Global vfs data structures for lfs. */
struct vnodeops lfs_vnodeops = {
	ufs_lookup,		/* lookup */
	ufs_create,		/* create */
	ufs_mknod,		/* mknod */
	ufs_open,		/* open */
	ufs_close,		/* close */
	ufs_access,		/* access */
	ufs_getattr,		/* getattr */
	ufs_setattr,		/* setattr */
	lfs_read,		/* read */
	lfs_write,		/* write */
	ufs_ioctl,		/* ioctl */
	ufs_select,		/* select */
	ufs_mmap,		/* mmap */
	lfs_fsync,		/* fsync */
	ufs_seek,		/* seek */
	ufs_remove,		/* remove */
	ufs_link,		/* link */
	ufs_rename,		/* rename */
	ufs_mkdir,		/* mkdir */
	ufs_rmdir,		/* rmdir */
	ufs_symlink,		/* symlink */
	ufs_readdir,		/* readdir */
	ufs_readlink,		/* readlink */
	ufs_abortop,		/* abortop */
	ufs_inactive,		/* inactive */
	ufs_reclaim,		/* reclaim */
	ufs_lock,		/* lock */
	ufs_unlock,		/* unlock */
	ufs_bmap,		/* bmap */
	ufs_strategy,		/* strategy */
	ufs_print,		/* print */
	ufs_islocked,		/* islocked */
	ufs_advlock,		/* advlock */
};

/*
 * Vnode op for reading.
 */
/* ARGSUSED */
lfs_read(vp, uio, ioflag, cred)
	struct vnode *vp;
	register struct uio *uio;
	int ioflag;
	struct ucred *cred;
{
	register struct inode *ip = VTOI(vp);
	register struct lfs *fs;				/* LFS */
	struct buf *bp;
	daddr_t lbn, bn, rablock;
	int size, diff, error = 0;
	long n, on, type;

printf("lfs_read: ino %d\n", ip->i_number);
#ifdef DIAGNOSTIC
	if (uio->uio_rw != UIO_READ)
		panic("ufs_read mode");
	type = ip->i_mode & IFMT;
	if (type != IFDIR && type != IFREG && type != IFLNK)
		panic("ufs_read type");
#endif
	if (uio->uio_resid == 0)
		return (0);
	if (uio->uio_offset < 0)
		return (EINVAL);
	ip->i_flag |= IACC;

	fs = ip->i_lfs;						/* LFS */
	do {
		lbn = lblkno(fs, uio->uio_offset);
		on = blkoff(fs, uio->uio_offset);
		n = MIN((unsigned)(fs->lfs_bsize - on), uio->uio_resid);
		diff = ip->i_size - uio->uio_offset;
		if (diff <= 0)
			return (0);
		if (diff < n)
			n = diff;
		size = blksize(fs);				/* LFS */
		rablock = lbn + 1;
		if (vp->v_lastr + 1 == lbn &&
		    lblktosize(fs, rablock) < ip->i_size)
			error = breada(ITOV(ip), lbn, size, rablock,
				blksize(fs), NOCRED, &bp);
		else
			error = bread(ITOV(ip), lbn, size, NOCRED, &bp);
		vp->v_lastr = lbn;
		n = MIN(n, size - bp->b_resid);
		if (error) {
			brelse(bp);
			return (error);
		}
		error = uiomove(bp->b_un.b_addr + on, (int)n, uio);
		if (n + on == fs->lfs_bsize || uio->uio_offset == ip->i_size)
			bp->b_flags |= B_AGE;
		brelse(bp);
	} while (error == 0 && uio->uio_resid > 0 && n != 0);
	return (error);
}

/*
 * Vnode op for writing.
 */
lfs_write(vp, uio, ioflag, cred)
	register struct vnode *vp;
	struct uio *uio;
	int ioflag;
	struct ucred *cred;
{
	struct proc *p = uio->uio_procp;
	register struct inode *ip = VTOI(vp);
	register struct lfs *fs;				/* LFS */
	struct buf *bp;
	daddr_t lbn, bn;
	u_long osize;
	int n, on, flags;
	int size, resid, error = 0;

printf("lfs_write ino %d\n", ip->i_number);
#ifdef DIAGNOSTIC
	if (uio->uio_rw != UIO_WRITE)
		panic("ufs_write mode");
#endif
	switch (vp->v_type) {
	case VREG:
		if (ioflag & IO_APPEND)
			uio->uio_offset = ip->i_size;
		/* fall through */
	case VLNK:
		break;

	case VDIR:
		if ((ioflag & IO_SYNC) == 0)
			panic("ufs_write nonsync dir write");
		break;

	default:
		panic("ufs_write type");
	}
	if (uio->uio_offset < 0)
		return (EINVAL);
	if (uio->uio_resid == 0)
		return (0);
	/*
	 * Maybe this should be above the vnode op call, but so long as
	 * file servers have no limits, i don't think it matters
	 */
	if (vp->v_type == VREG && p &&
	    uio->uio_offset + uio->uio_resid >
	      p->p_rlimit[RLIMIT_FSIZE].rlim_cur) {
		psignal(p, SIGXFSZ);
		return (EFBIG);
	}
	resid = uio->uio_resid;
	osize = ip->i_size;
	fs = ip->i_lfs;						/* LFS */
	flags = 0;
#ifdef NOTLFS
	if (ioflag & IO_SYNC)
		flags = B_SYNC;
#endif
	do {
		lbn = lblkno(fs, uio->uio_offset);
		on = blkoff(fs, uio->uio_offset);		/* LFS */
		n = MIN((unsigned)(fs->lfs_bsize - on), uio->uio_resid);
		if (n < fs->lfs_bsize)				/* LFS */
			flags |= B_CLRBUF;
		else
			flags &= ~B_CLRBUF;			/* LFS */
		if (error = bread(vp, lbn, fs->lfs_bsize, NOCRED, &bp))
			break;
		bn = bp->b_blkno;
		if (uio->uio_offset + n > ip->i_size) {
			ip->i_size = uio->uio_offset + n;
			vnode_pager_setsize(vp, ip->i_size);
		}
		size = blksize(fs);
		(void) vnode_pager_uncache(vp);
		n = MIN(n, size - bp->b_resid);
		error = uiomove(bp->b_un.b_addr + on, n, uio);
#ifdef NOTLFS							/* LFS */
		if (ioflag & IO_SYNC)
			(void) bwrite(bp);
		else if (n + on == fs->fs_bsize) {
			bp->b_flags |= B_AGE;
			bawrite(bp);
		} else
			bdwrite(bp);
#else
		/*
		 * Update segment usage information; call segment
		 * writer if necessary.
		 */
		lfs_bwrite(bp);
#endif
		ip->i_flag |= IUPD|ICHG;
		if (cred->cr_uid != 0)
			ip->i_mode &= ~(ISUID|ISGID);
	} while (error == 0 && uio->uio_resid > 0 && n != 0);
	if (error && (ioflag & IO_UNIT)) {
#ifdef NOTLFS
	/* This just doesn't work... */
		(void) lfs_itrunc(ip, osize, ioflag & IO_SYNC);
#endif
		uio->uio_offset -= resid - uio->uio_resid;
		uio->uio_resid = resid;
	}
	if (!error && (ioflag & IO_SYNC))
		ITIMES(ip, &time, &time);			/* LFS */
	return (error);
}

/*
 * Synch an open file.
 */
/* ARGSUSED */
lfs_fsync(vp, fflags, cred, waitfor, p)
	struct vnode *vp;
	int fflags;
	struct ucred *cred;
	int waitfor;
	struct proc *p;
{
	struct inode *ip = VTOI(vp);

	if (fflags & FWRITE)
		ip->i_flag |= ICHG;
	/*
	 * XXX
	 * Sync the mounted file system associated with the file
	 * descriptor.
	 */
	ITIMES(ip, &time, &time);				/* LFS */
	return (0);
}
