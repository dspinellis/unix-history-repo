/*
 *
 *
 * PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
 * --------------------         -----   ----------------------
 * CURRENT PATCH LEVEL:         1       00040
 * --------------------         -----   ----------------------
 *
 * 10 Aug 92	Scott Burris		Fixed "delete from CD-ROM" bug
 */
#include "param.h"
#include "systm.h"
#include "namei.h"
#include "resourcevar.h"
#include "kernel.h"
#include "file.h"
#include "stat.h"
#include "buf.h"
#include "proc.h"
#include "conf.h"
#include "mount.h"
#include "vnode.h"
#include "specdev.h"
#include "fifo.h"
#include "malloc.h"
#include "dir.h"

#include "iso.h"
#include "isofs_node.h"

/*
 * Open called.
 *
 * Nothing to do.
 */
/* ARGSUSED */
isofs_open(vp, mode, cred, p)
	struct vnode *vp;
	int mode;
	struct ucred *cred;
	struct proc *p;
{
	return (0);
}

/*
 * Close called
 *
 * Update the times on the inode on writeable file systems.
 */
/* ARGSUSED */
isofs_close(vp, fflag, cred, p)
	struct vnode *vp;
	int fflag;
	struct ucred *cred;
	struct proc *p;
{
	return (0);
}

/*
 * Check mode permission on inode pointer. Mode is READ, WRITE or EXEC.
 * The mode is shifted to select the owner/group/other fields. The
 * super user is granted all permissions.
 */
isofs_access(vp, mode, cred, p)
	struct vnode *vp;
	register int mode;
	struct ucred *cred;
	struct proc *p;
{
	return (0);
}

/* ARGSUSED */
isofs_getattr(vp, vap, cred, p)
	struct vnode *vp;
	register struct vattr *vap;
	struct ucred *cred;
	struct proc *p;
{
	register struct iso_node *ip = VTOI(vp);
	int year, month, day, hour ,minute, second, tz;
	int crtime, days;
	int i;

	year = ip->iso_time[0] - 70;
	month = ip->iso_time[1];
	day = ip->iso_time[2];
	hour = ip->iso_time[3];
	minute = ip->iso_time[4];
	second = ip->iso_time[5];
	tz = ip->iso_time[6];
	
	if (year < 0) {
		crtime = 0;
	} else {
		int monlen[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
		days = year * 365;
		if (year > 2)
			days += (year+2) / 4;
		for (i = 1; i < month; i++)
			days += monlen[i-1];
		if (((year+2) % 4) == 0 && month > 2)
			days++;
		days += day - 1;
		crtime = ((((days * 24) + hour) * 60 + minute) * 60)
			+ second;

		/* sign extend */
		if (tz & 0x80)
			tz |= (-1 << 8);

		/* timezone offset is unreliable on some disks */
		if (-48 <= tz && tz <= 52)
			crtime += tz * 15 * 60;
	}

	vap->va_fsid = ip->i_dev;
	vap->va_fileid = ip->i_number;
	vap->va_mode = VREAD|VEXEC;
	vap->va_mode |= (vap->va_mode >> 3) | (vap->va_mode >> 6);
	if (vp->v_type == VDIR)
		vap->va_nlink = 2;
	else
		vap->va_nlink = 1;
	vap->va_uid = 0;
	vap->va_gid = 0;
	vap->va_rdev = 0;
	vap->va_size = ip->i_size;
	vap->va_size_rsv = 0;
	vap->va_atime.tv_sec = crtime;
	vap->va_atime.tv_usec = 0;
	vap->va_mtime.tv_sec = crtime;
	vap->va_mtime.tv_usec = 0;
	vap->va_ctime.tv_sec = crtime;
	vap->va_ctime.tv_usec = 0;
	vap->va_flags = 0;
	vap->va_gen = 1;
	vap->va_blocksize = ip->i_mnt->logical_block_size;
	vap->va_bytes = ip->i_size;
	vap->va_bytes_rsv = 0;
	vap->va_type = vp->v_type;
	return (0);
}

/*
 * Vnode op for reading.
 */
/* ARGSUSED */
isofs_read(vp, uio, ioflag, cred)
	struct vnode *vp;
	register struct uio *uio;
	int ioflag;
	struct ucred *cred;
{
	register struct iso_node *ip = VTOI(vp);
	register struct iso_mnt *imp;
	struct buf *bp;
	daddr_t lbn, bn, rablock;
	int size, diff, error = 0;
	long n, on, type;

#ifdef DIAGNOSTICx
	if (uio->uio_rw != UIO_READ)
		panic("isofs_read mode");
	type = ip->i_mode & IFMT;
	if (type != IFDIR && type != IFREG && type != IFLNK)
		panic("isofs_read type");
#endif
	if (uio->uio_resid == 0)
		return (0);
	if (uio->uio_offset < 0)
		return (EINVAL);
	ip->i_flag |= IACC;
	imp = ip->i_mnt;
	do {
		lbn = iso_lblkno(imp, uio->uio_offset);
		on = iso_blkoff(imp, uio->uio_offset);
		n = MIN((unsigned)(imp->im_bsize - on), uio->uio_resid);
		diff = ip->i_size - uio->uio_offset;
		if (diff <= 0)
			return (0);
		if (diff < n)
			n = diff;
		size = iso_blksize(imp, ip, lbn);
		rablock = lbn + 1;
		if (vp->v_lastr + 1 == lbn &&
		    iso_lblktosize(imp, rablock) < ip->i_size)
			error = breada(ITOV(ip), lbn, size, rablock,
				iso_blksize(imp, ip, rablock), NOCRED, &bp);
		else
			error = bread(ITOV(ip), lbn, size, NOCRED, &bp);
		vp->v_lastr = lbn;
		n = MIN(n, size - bp->b_resid);
		if (error) {
			brelse(bp);
			return (error);
		}

		error = uiomove(bp->b_un.b_addr + on, (int)n, uio);
		if (n + on == imp->im_bsize || uio->uio_offset == ip->i_size)
			bp->b_flags |= B_AGE;
		brelse(bp);
	} while (error == 0 && uio->uio_resid > 0 && n != 0);
	return (error);
}

/* ARGSUSED */
isofs_ioctl(vp, com, data, fflag, cred, p)
	struct vnode *vp;
	int com;
	caddr_t data;
	int fflag;
	struct ucred *cred;
	struct proc *p;
{
	return (ENOTTY);
}

/* ARGSUSED */
isofs_select(vp, which, fflags, cred, p)
	struct vnode *vp;
	int which, fflags;
	struct ucred *cred;
	struct proc *p;
{

	/*
	 * We should really check to see if I/O is possible.
	 */
	return (1);
}

/*
 * Mmap a file
 *
 * NB Currently unsupported.
 */
/* ARGSUSED */
isofs_mmap(vp, fflags, cred, p)
	struct vnode *vp;
	int fflags;
	struct ucred *cred;
	struct proc *p;
{

	return (EINVAL);
}

/*
 * Seek on a file
 *
 * Nothing to do, so just return.
 */
/* ARGSUSED */
isofs_seek(vp, oldoff, newoff, cred)
	struct vnode *vp;
	off_t oldoff, newoff;
	struct ucred *cred;
{

	return (0);
}

/*
 * Vnode op for readdir
 */
isofs_readdir(vp, uio, cred, eofflagp)
	struct vnode *vp;
	register struct uio *uio;
	struct ucred *cred;
	int *eofflagp;
{
	struct dirent dirent;
	int iso_offset;
	int entryoffsetinblock;
	int error = 0;
	int endsearch;
	struct iso_directory_record *ep;
	int reclen;
	struct iso_mnt *imp;
	struct iso_node *ip;
	struct buf *bp = NULL;
	int i;
	int end_flag = 0;

	ip = VTOI (vp);
	imp = ip->i_mnt;

	iso_offset = uio->uio_offset;

	entryoffsetinblock = iso_blkoff(imp, iso_offset);
	if (entryoffsetinblock != 0) {
		if (error = iso_blkatoff(ip, iso_offset, (char **)0, &bp))
			return (error);
	}

	endsearch = ip->i_size;

	while (iso_offset < endsearch && uio->uio_resid > 0) {
		/*
		 * If offset is on a block boundary,
		 * read the next directory block.
		 * Release previous if it exists.
		 */

		if (iso_blkoff(imp, iso_offset) == 0) {
			if (bp != NULL)
				brelse(bp);
			if (error = iso_blkatoff(ip, iso_offset,
						 (char **)0, &bp))
				return (error);
			entryoffsetinblock = 0;
		}
		/*
		 * Get pointer to next entry.
		 */

		ep = (struct iso_directory_record *)
			(bp->b_un.b_addr + entryoffsetinblock);

		reclen = isonum_711 (ep->length);
		if (reclen == 0) {
			/* skip to next block, if any */
			iso_offset = roundup (iso_offset,
					      imp->logical_block_size);
			continue;
		}

		if (reclen < sizeof (struct iso_directory_record))
			/* illegal entry, stop */
			break;

/* 10 Aug 92*/	if (entryoffsetinblock + reclen -1 >= imp->logical_block_size)
			/* illegal directory, so stop looking */
			break;

		dirent.d_fileno = isonum_733 (ep->extent);
		dirent.d_namlen = isonum_711 (ep->name_len);

		if (reclen < sizeof (struct iso_directory_record)
		    + dirent.d_namlen)
			/* illegal entry, stop */
			break;

		/*bcopy (ep->name, dirent.d_name, dirent.d_namlen);*/
		isofntrans(ep->name, dirent.d_namlen,
			dirent.d_name, &dirent.d_namlen);
		if (dirent.d_namlen == 1) {
			switch (dirent.d_name[0]) {
			case 0:
				dirent.d_name[0] = '.';
				break;
			case 1:
				dirent.d_name[0] = '.';
				dirent.d_name[1] = '.';
				dirent.d_namlen = 2;
			}
		}
		dirent.d_name[dirent.d_namlen] = 0;
		dirent.d_reclen = DIRSIZ (&dirent);

		if (uio->uio_resid < dirent.d_reclen)
			break;

		if (error = uiomove (&dirent, dirent.d_reclen, uio))
			break;

		iso_offset += reclen;
		entryoffsetinblock += reclen;
	}
			
	if (bp)
		brelse (bp);

	if (end_flag || (VTOI(vp)->i_size - iso_offset) <= 0)
		*eofflagp = 1;
	else
		*eofflagp = 0;

	uio->uio_offset = iso_offset;

	return (error);
}

/*
 * Ufs abort op, called after namei() when a CREATE/DELETE isn't actually
 * done. If a buffer has been saved in anticipation of a CREATE, delete it.
 */
/* ARGSUSED */
isofs_abortop(ndp)
	struct nameidata *ndp;
{

	if ((ndp->ni_nameiop & (HASBUF | SAVESTART)) == HASBUF)
		FREE(ndp->ni_pnbuf, M_NAMEI);
	return (0);
}

/*
 * Lock an inode.
 */
isofs_lock(vp)
	struct vnode *vp;
{
	register struct iso_node *ip = VTOI(vp);

	ISO_ILOCK(ip);
	return (0);
}

/*
 * Unlock an inode.
 */
isofs_unlock(vp)
	struct vnode *vp;
{
	register struct iso_node *ip = VTOI(vp);

	if (!(ip->i_flag & ILOCKED))
		panic("isofs_unlock NOT LOCKED");
	ISO_IUNLOCK(ip);
	return (0);
}

/*
 * Check for a locked inode.
 */
isofs_islocked(vp)
	struct vnode *vp;
{

	if (VTOI(vp)->i_flag & ILOCKED)
		return (1);
	return (0);
}

/*
 * Calculate the logical to physical mapping if not done already,
 * then call the device strategy routine.
 */

isofs_strategy(bp)
	register struct buf *bp;
{
	register struct iso_node *ip = VTOI(bp->b_vp);
	struct vnode *vp;
	int error;

	if (bp->b_vp->v_type == VBLK || bp->b_vp->v_type == VCHR)
		panic("isofs_strategy: spec");
	if (bp->b_blkno == bp->b_lblkno) {
		if (error = iso_bmap(ip, bp->b_lblkno, &bp->b_blkno))
			return (error);
		if ((long)bp->b_blkno == -1)
			clrbuf(bp);
	}
	if ((long)bp->b_blkno == -1) {
		biodone(bp);
		return (0);
	}
	vp = ip->i_devvp;
	bp->b_dev = vp->v_rdev;
	(*(vp->v_op->vop_strategy))(bp);
	return (0);
}

/*
 * Print out the contents of an inode.
 */
isofs_print(vp)
	struct vnode *vp;
{
	printf ("isoprint\n");
}

extern int enodev ();

/*
 * Global vfs data structures for isofs
 */
struct vnodeops isofs_vnodeops = {
	isofs_lookup,		/* lookup */
	(void *)enodev,		/* create */
	(void *)enodev,		/* mknod */
	isofs_open,		/* open */
	isofs_close,		/* close */
	isofs_access,		/* access */
	isofs_getattr,		/* getattr */
	(void *)enodev,		/* setattr */
	isofs_read,		/* read */
	(void *)enodev,		/* write */
	isofs_ioctl,		/* ioctl */
	isofs_select,		/* select */
	isofs_mmap,		/* mmap */
	(void *)nullop,		/* fsync */
	isofs_seek,		/* seek */
	(void *)enodev,		/* remove */
	(void *)enodev,		/* link */
	(void *)enodev,		/* rename */
	(void *)enodev,		/* mkdir */
	(void *)enodev,		/* rmdir */
	(void *)enodev,		/* symlink */
	isofs_readdir,		/* readdir */
	(void *)enodev,		/* readlink */
	isofs_abortop,		/* abortop */
	isofs_inactive,		/* inactive */
	isofs_reclaim,		/* reclaim */
	isofs_lock,		/* lock */
	isofs_unlock,		/* unlock */
	(void *)enodev,		/* bmap */
	isofs_strategy,		/* strategy */
	isofs_print,		/* print */
	isofs_islocked,		/* islocked */
	(void *)enodev,		/* advlock */
};


