/*
 * Copyright (c) 1982, 1986, 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_vnops.c	7.67 (Berkeley) %G%
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

#include "../ufs/lockf.h"
#include "../ufs/quota.h"
#include "../ufs/inode.h"
#include "../ufs/dir.h"
#include "lfs.h"
#include "lfs_extern.h"

static int	chmod1 __P((struct vnode *, int, struct proc *));
static int	chown1 __P((struct vnode *, uid_t, gid_t, struct proc *));
static int	maknode __P((int, struct nameidata *, struct inode **));

/*
 * Create a regular file
 */
lfs_create(ndp, vap, p)
	struct nameidata *ndp;
	struct vattr *vap;
	struct proc *p;
{
	struct inode *ip;
	int error;

printf("lfs_create\n");
	if (error = maknode(MAKEIMODE(vap->va_type, vap->va_mode), ndp, &ip))
		return (error);
	ndp->ni_vp = ITOV(ip);
	return (0);
}

/*
 * Mknod vnode call
 */
/* ARGSUSED */
lfs_mknod(ndp, vap, cred, p)
	struct nameidata *ndp;
	struct ucred *cred;
	struct vattr *vap;
	struct proc *p;
{
	register struct vnode *vp;
	struct inode *ip;
	int error;

printf("lfs_mknod\n");
	if (error = maknode(MAKEIMODE(vap->va_type, vap->va_mode), ndp, &ip))
		return (error);
	ip->i_flag |= IACC|IUPD|ICHG;
	if (vap->va_rdev != VNOVAL) {
		/*
		 * Want to be able to use this to make badblock
		 * inodes, so don't truncate the dev number.
		 */
		ip->i_rdev = vap->va_rdev;
	}
	/*
	 * Remove inode so that it will be reloaded by iget and
	 * checked to see if it is an alias of an existing entry
	 * in the inode cache.
	 */
	vp = ITOV(ip);
	vput(vp);
	vp->v_type = VNON;
	vgone(vp);
	return (0);
}

/* ARGSUSED */
lfs_getattr(vp, vap, cred, p)
	struct vnode *vp;
	register struct vattr *vap;
	struct ucred *cred;
	struct proc *p;
{
	register struct inode *ip = VTOI(vp);

printf("lfs_getattr\n");
	ITIMES(ip, &time, &time);				/* LFS */
	/*
	 * Copy from inode table
	 */
	vap->va_fsid = ip->i_dev;
	vap->va_fileid = ip->i_number;
	vap->va_mode = ip->i_mode & ~IFMT;
	vap->va_nlink = ip->i_nlink;
	vap->va_uid = ip->i_uid;
	vap->va_gid = ip->i_gid;
	vap->va_rdev = (dev_t)ip->i_rdev;
#ifdef tahoe
	vap->va_size = ip->i_size;
	vap->va_size_rsv = 0;
#else
	vap->va_qsize = ip->i_din.di_qsize;
#endif
	vap->va_atime.tv_sec = ip->i_atime;
	vap->va_atime.tv_usec = 0;
	vap->va_mtime.tv_sec = ip->i_mtime;
	vap->va_mtime.tv_usec = 0;
	vap->va_ctime.tv_sec = ip->i_ctime;
	vap->va_ctime.tv_usec = 0;
	vap->va_flags = ip->i_flags;
	vap->va_gen = ip->i_gen;
	/* this doesn't belong here */
	if (vp->v_type == VBLK)
		vap->va_blocksize = BLKDEV_IOSIZE;
	else if (vp->v_type == VCHR)
		vap->va_blocksize = MAXBSIZE;
	else
		vap->va_blocksize = ip->i_lfs->lfs_bsize;	/* LFS */
	vap->va_bytes = dbtob(ip->i_blocks);
	vap->va_bytes_rsv = 0;
	vap->va_type = vp->v_type;
	return (0);
}

/*
 * Set attribute vnode op. called from several syscalls
 */
lfs_setattr(vp, vap, cred, p)
	register struct vnode *vp;
	register struct vattr *vap;
	register struct ucred *cred;
	struct proc *p;
{
	register struct inode *ip = VTOI(vp);
	int error = 0;

printf("lfs_setattr\n");
	/*
	 * Check for unsettable attributes.
	 */
	if ((vap->va_type != VNON) || (vap->va_nlink != VNOVAL) ||
	    (vap->va_fsid != VNOVAL) || (vap->va_fileid != VNOVAL) ||
	    (vap->va_blocksize != VNOVAL) || (vap->va_rdev != VNOVAL) ||
	    ((int)vap->va_bytes != VNOVAL) || (vap->va_gen != VNOVAL)) {
		return (EINVAL);
	}
	/*
	 * Go through the fields and update iff not VNOVAL.
	 */
	if (vap->va_uid != (u_short)VNOVAL || vap->va_gid != (u_short)VNOVAL)
		if (error = chown1(vp, vap->va_uid, vap->va_gid, p))
			return (error);
	if (vap->va_size != VNOVAL) {
		if (vp->v_type == VDIR)
			return (EISDIR);			/* LFS */
		if (error = lfs_itrunc(ip, vap->va_size, 0)) /* XXX IO_SYNC? */
			return (error);
	}
	if (vap->va_atime.tv_sec != VNOVAL || vap->va_mtime.tv_sec != VNOVAL) {
		if (cred->cr_uid != ip->i_uid &&
		    (error = suser(cred, &p->p_acflag)))
			return (error);
		if (vap->va_atime.tv_sec != VNOVAL)
			ip->i_flag |= IACC;
		if (vap->va_mtime.tv_sec != VNOVAL)
			ip->i_flag |= IUPD;
		ip->i_flag |= ICHG;				/* LFS */
		ITIMES(ip, &vap->va_atime, &vap->va_mtime);	/* LFS */
	}
	if (vap->va_mode != (u_short)VNOVAL)
		error = chmod1(vp, (int)vap->va_mode, p);
	if (vap->va_flags != VNOVAL) {
		if (cred->cr_uid != ip->i_uid &&
		    (error = suser(cred, &p->p_acflag)))
			return (error);
		if (cred->cr_uid == 0) {
			ip->i_flags = vap->va_flags;
		} else {
			ip->i_flags &= 0xffff0000;
			ip->i_flags |= (vap->va_flags & 0xffff);
		}
		ip->i_flag |= ICHG;
	}
	return (error);
}

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
	register LFS *fs;					/* LFS */
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
	register LFS *fs;					/* LFS */
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

printf("lfs_sync: ino %d\n", ip->i_number);
	if (fflags & FWRITE)
		ip->i_flag |= ICHG;
	ITIMES(ip, &time, &time);				/* LFS */
	vflushbuf(vp, waitfor == MNT_WAIT ? B_SYNC : 0);	/* LFS */
	return (0);
}

/*
 * ufs remove
 * Hard to avoid races here, especially
 * in unlinking directories.
 */
lfs_remove(ndp, p)
	struct nameidata *ndp;
	struct proc *p;
{
	register struct inode *ip, *dp;
	int error;

printf("lfs_remove\n");
	ip = VTOI(ndp->ni_vp);
	dp = VTOI(ndp->ni_dvp);
	error = lfs_dirremove(ndp);				/* LFS */
	if (!error) {
		ip->i_nlink--;
		ip->i_flag |= ICHG;
	}
	if (dp == ip)
		vrele(ITOV(ip));
	else
		iput(ip);
	iput(dp);
	return (error);
}
/*
 * link vnode call
 */
lfs_link(vp, ndp, p)
	register struct vnode *vp;
	register struct nameidata *ndp;
	struct proc *p;
{
	register struct inode *ip = VTOI(vp);
	int error;

printf("lfs_link\n");
#ifdef DIANOSTIC
	if ((ndp->ni_nameiop & HASBUF) == 0)
		panic("ufs_link: no name");
#endif
	if ((unsigned short)ip->i_nlink >= LINK_MAX) {
		free(ndp->ni_pnbuf, M_NAMEI);
		return (EMLINK);
	}
	if (ndp->ni_dvp != vp)
		ILOCK(ip);
	ip->i_nlink++;
	ip->i_flag |= ICHG;
	ITIMES(ip, &time, &time);				/* LFS */
	error = lfs_direnter(ip, ndp);				/* LFS */
	if (ndp->ni_dvp != vp)
		IUNLOCK(ip);
	FREE(ndp->ni_pnbuf, M_NAMEI);
	vput(ndp->ni_dvp);
	if (error) {
		ip->i_nlink--;
		ip->i_flag |= ICHG;
	}
	return (error);
}

/*
 * Rename system call.
 * 	rename("foo", "bar");
 * is essentially
 *	unlink("bar");
 *	link("foo", "bar");
 *	unlink("foo");
 * but ``atomically''.  Can't do full commit without saving state in the
 * inode on disk which isn't feasible at this time.  Best we can do is
 * always guarantee the target exists.
 *
 * Basic algorithm is:
 *
 * 1) Bump link count on source while we're linking it to the
 *    target.  This also ensure the inode won't be deleted out
 *    from underneath us while we work (it may be truncated by
 *    a concurrent `trunc' or `open' for creation).
 * 2) Link source to destination.  If destination already exists,
 *    delete it first.
 * 3) Unlink source reference to inode if still around. If a
 *    directory was moved and the parent of the destination
 *    is different from the source, patch the ".." entry in the
 *    directory.
 */
lfs_rename(fndp, tndp, p)
	register struct nameidata *fndp, *tndp;
	struct proc *p;
{
	register struct inode *ip, *xp, *dp;
	struct dirtemplate dirbuf;
	int doingdirectory = 0, oldparent = 0, newparent = 0;
	int error = 0;

printf("lfs_rename\n");
#ifdef DIANOSTIC
	if ((tndp->ni_nameiop & HASBUF) == 0 ||
	    (fndp->ni_nameiop & HASBUF) == 0)
		panic("ufs_rename: no name");
#endif
	dp = VTOI(fndp->ni_dvp);
	ip = VTOI(fndp->ni_vp);
	/*
	 * Check if just deleting a link name.
	 */
	if (fndp->ni_vp == tndp->ni_vp) {
		VOP_ABORTOP(tndp);
		vput(tndp->ni_dvp);
		vput(tndp->ni_vp);
		vrele(fndp->ni_dvp);
		if ((ip->i_mode&IFMT) == IFDIR) {
			VOP_ABORTOP(fndp);
			vrele(fndp->ni_vp);
			return (EINVAL);
		}
		doingdirectory = 0;
		goto unlinkit;
	}
	ILOCK(ip);
	if ((ip->i_mode&IFMT) == IFDIR) {
		/*
		 * Avoid ".", "..", and aliases of "." for obvious reasons.
		 */
		if ((fndp->ni_namelen == 1 && fndp->ni_ptr[0] == '.') ||
		    dp == ip || fndp->ni_isdotdot || (ip->i_flag & IRENAME)) {
			VOP_ABORTOP(tndp);
			vput(tndp->ni_dvp);
			if (tndp->ni_vp)
				vput(tndp->ni_vp);
			VOP_ABORTOP(fndp);
			vrele(fndp->ni_dvp);
			vput(fndp->ni_vp);
			return (EINVAL);
		}
		ip->i_flag |= IRENAME;
		oldparent = dp->i_number;
		doingdirectory++;
	}
	vrele(fndp->ni_dvp);

	/*
	 * 1) Bump link count while we're moving stuff
	 *    around.  If we crash somewhere before
	 *    completing our work, the link count
	 *    may be wrong, but correctable.
	 */
	ip->i_nlink++;
	ip->i_flag |= ICHG;
	ITIMES(ip, &time, &time);				/* LFS */
	IUNLOCK(ip);

	/*
	 * When the target exists, both the directory
	 * and target vnodes are returned locked.
	 */
	dp = VTOI(tndp->ni_dvp);
	xp = NULL;
	if (tndp->ni_vp)
		xp = VTOI(tndp->ni_vp);
	/*
	 * If ".." must be changed (ie the directory gets a new
	 * parent) then the source directory must not be in the
	 * directory heirarchy above the target, as this would
	 * orphan everything below the source directory. Also
	 * the user must have write permission in the source so
	 * as to be able to change "..". We must repeat the call 
	 * to namei, as the parent directory is unlocked by the
	 * call to checkpath().
	 */
	if (oldparent != dp->i_number)
		newparent = dp->i_number;
	if (doingdirectory && newparent) {
		VOP_LOCK(fndp->ni_vp);
		error = ufs_access(fndp->ni_vp, VWRITE, tndp->ni_cred, p);
		VOP_UNLOCK(fndp->ni_vp);
		if (error)
			goto bad;
		if (xp != NULL)
			iput(xp);				/* LFS */
		if (error = lfs_checkpath(ip, dp, tndp->ni_cred))
			goto out;
		if ((tndp->ni_nameiop & SAVESTART) == 0)
			panic("ufs_rename: lost to startdir");
		p->p_spare[1]--;
		if (error = lookup(tndp, p))
			goto out;
		dp = VTOI(tndp->ni_dvp);
		xp = NULL;
		if (tndp->ni_vp)
			xp = VTOI(tndp->ni_vp);
	}
	/*
	 * 2) If target doesn't exist, link the target
	 *    to the source and unlink the source. 
	 *    Otherwise, rewrite the target directory
	 *    entry to reference the source inode and
	 *    expunge the original entry's existence.
	 */
	if (xp == NULL) {
		if (dp->i_dev != ip->i_dev)
			panic("rename: EXDEV");
		/*
		 * Account for ".." in new directory.
		 * When source and destination have the same
		 * parent we don't fool with the link count.
		 */
		if (doingdirectory && newparent) {
			if ((unsigned short)dp->i_nlink >= LINK_MAX) {
				error = EMLINK;
				goto bad;
			}
			dp->i_nlink++;
			dp->i_flag |= ICHG;			/* LFS */
			ITIMES(dp, &time, &time);		/* LFS */
		}
		if (error = lfs_direnter(ip, tndp)) {
			if (doingdirectory && newparent) {
				dp->i_nlink--;
				dp->i_flag |= ICHG;		/* LFS */
				ITIMES(dp, &time, &time);	/* LFS */
			}
			goto bad;
		}
		iput(dp);
	} else {
		if (xp->i_dev != dp->i_dev || xp->i_dev != ip->i_dev)
			panic("rename: EXDEV");
		/*
		 * Short circuit rename(foo, foo).
		 */
		if (xp->i_number == ip->i_number)
			panic("rename: same file");
		/*
		 * If the parent directory is "sticky", then the user must
		 * own the parent directory, or the destination of the rename,
		 * otherwise the destination may not be changed (except by
		 * root). This implements append-only directories.
		 */
		if ((dp->i_mode & ISVTX) && tndp->ni_cred->cr_uid != 0 &&
		    tndp->ni_cred->cr_uid != dp->i_uid &&
		    xp->i_uid != tndp->ni_cred->cr_uid) {
			error = EPERM;
			goto bad;
		}
		/*
		 * Target must be empty if a directory and have no links
		 * to it. Also, ensure source and target are compatible
		 * (both directories, or both not directories).
		 */
		if ((xp->i_mode&IFMT) == IFDIR) {
			if (!dirempty(xp, dp->i_number, tndp->ni_cred) || 
			    xp->i_nlink > 2) {
				error = ENOTEMPTY;
				goto bad;
			}
			if (!doingdirectory) {
				error = ENOTDIR;
				goto bad;
			}
			cache_purge(ITOV(dp));
		} else if (doingdirectory) {
			error = EISDIR;
			goto bad;
		}
		if (error = dirrewrite(dp, ip, tndp))
			goto bad;
		/*
		 * If the target directory is in the same
		 * directory as the source directory,
		 * decrement the link count on the parent
		 * of the target directory.
		 */
		 if (doingdirectory && !newparent) {
			dp->i_nlink--;
			dp->i_flag |= ICHG;
		}
		vput(ITOV(dp));
		/*
		 * Adjust the link count of the target to
		 * reflect the dirrewrite above.  If this is
		 * a directory it is empty and there are
		 * no links to it, so we can squash the inode and
		 * any space associated with it.  We disallowed
		 * renaming over top of a directory with links to
		 * it above, as the remaining link would point to
		 * a directory without "." or ".." entries.
		 */
		xp->i_nlink--;
		if (doingdirectory) {
			if (--xp->i_nlink != 0)			/* LFS */
				panic("rename: linked directory");
			error = lfs_itrunc(xp, (u_long)0, IO_SYNC);
		}
		xp->i_flag |= ICHG;
		iput(xp);
		xp = NULL;
	}

	/*
	 * 3) Unlink the source.
	 */
unlinkit:
	fndp->ni_nameiop &= ~MODMASK;
	fndp->ni_nameiop |= LOCKPARENT | LOCKLEAF;
	if ((fndp->ni_nameiop & SAVESTART) == 0)
		panic("ufs_rename: lost from startdir");
	p->p_spare[1]--;
	(void) lookup(fndp, p);
	if (fndp->ni_vp != NULL) {
		xp = VTOI(fndp->ni_vp);
		dp = VTOI(fndp->ni_dvp);
	} else {
		/*
		 * From name has disappeared.
		 */
		if (doingdirectory)
			panic("rename: lost dir entry");
		vrele(ITOV(ip));
		return (0);
	}
	/*
	 * Ensure that the directory entry still exists and has not
	 * changed while the new name has been entered. If the source is
	 * a file then the entry may have been unlinked or renamed. In
	 * either case there is no further work to be done. If the source
	 * is a directory then it cannot have been rmdir'ed; its link
	 * count of three would cause a rmdir to fail with ENOTEMPTY.
	 * The IRENAME flag ensures that it cannot be moved by another
	 * rename.
	 */
	if (xp != ip) {
		if (doingdirectory)
			panic("rename: lost dir entry");
	} else {
		/*
		 * If the source is a directory with a
		 * new parent, the link count of the old
		 * parent directory must be decremented
		 * and ".." set to point to the new parent.
		 */
		if (doingdirectory && newparent) {
			dp->i_nlink--;
			dp->i_flag |= ICHG;
			error = vn_rdwr(UIO_READ, ITOV(xp), (caddr_t)&dirbuf,
				sizeof (struct dirtemplate), (off_t)0,
				UIO_SYSSPACE, IO_NODELOCKED, 
				tndp->ni_cred, (int *)0, (struct proc *)0);
			if (error == 0) {
				if (dirbuf.dotdot_namlen != 2 ||
				    dirbuf.dotdot_name[0] != '.' ||
				    dirbuf.dotdot_name[1] != '.') {
					lfs_dirbad(xp, 12,
					    "rename: mangled dir");
				} else {
					dirbuf.dotdot_ino = newparent;
					(void) vn_rdwr(UIO_WRITE, ITOV(xp),
					    (caddr_t)&dirbuf,
					    sizeof (struct dirtemplate),
					    (off_t)0, UIO_SYSSPACE,
					    IO_NODELOCKED|IO_SYNC,
					    tndp->ni_cred, (int *)0,
					    (struct proc *)0);
					cache_purge(ITOV(dp));
				}
			}
		}
		error = lfs_dirremove(fndp);			/* LFS */
		if (!error) {
			xp->i_nlink--;
			xp->i_flag |= ICHG;
		}
		xp->i_flag &= ~IRENAME;
	}
	if (dp)
		vput(ITOV(dp));
	if (xp)
		vput(ITOV(xp));
	vrele(ITOV(ip));
	return (error);

bad:
	if (xp)
		vput(ITOV(xp));
	vput(ITOV(dp));
out:
	ip->i_nlink--;
	ip->i_flag |= ICHG;
	vrele(ITOV(ip));
	return (error);
}

/*
 * A virgin directory (no blushing please).
 */
static struct dirtemplate mastertemplate = {
	0, 12, 1, ".",
	0, DIRBLKSIZ - 12, 2, ".."
};

/*
 * Mkdir system call
 */
lfs_mkdir(ndp, vap, p)
	struct nameidata *ndp;
	struct vattr *vap;
	struct proc *p;
{
	register struct inode *ip, *dp;
	struct inode *tip;
	struct vnode *dvp;
	struct dirtemplate dirtemplate;
	int error;
	int dmode;

printf("lfs_mkdir\n");
#ifdef DIANOSTIC
	if ((ndp->ni_nameiop & HASBUF) == 0)
		panic("ufs_mkdir: no name");
#endif
	dvp = ndp->ni_dvp;
	dp = VTOI(dvp);
	if ((unsigned short)dp->i_nlink >= LINK_MAX) {
		free(ndp->ni_pnbuf, M_NAMEI);
		iput(dp);
		return (EMLINK);
	}
	dmode = vap->va_mode&0777;
	dmode |= IFDIR;
	/*
	 * Must simulate part of maknode here to acquire the inode, but
	 * not have it entered in the parent directory. The entry is made
	 * later after writing "." and ".." entries.
	 */
#ifdef NOTLFS							/* LFS */
	if (error = ialloc(dp, dirpref(dp->i_fs), dmode, ndp->ni_cred, &tip)) {
#else
	if (error = lfs_ialloc(dp->i_lfs, dp, &tip, ndp->ni_cred)) {
#endif
		free(ndp->ni_pnbuf, M_NAMEI);
		iput(dp);
		return (error);
	}
	ip = tip;
	ip->i_uid = ndp->ni_cred->cr_uid;
	ip->i_gid = dp->i_gid;
#ifdef QUOTA
	if ((error = getinoquota(ip)) ||
	    (error = chkiq(ip, 1, ndp->ni_cred, 0))) {
		free(ndp->ni_pnbuf, M_NAMEI);
#ifdef NOTLFS							/* LFS */
		ifree(ip, ip->i_number, dmode);
#else
		lfs_ifree(ip);
#endif
		iput(ip);
		iput(dp);
		return (error);
	}
#endif
	ip->i_flag |= IACC|IUPD|ICHG;
	ip->i_mode = dmode;
	ITOV(ip)->v_type = VDIR;	/* Rest init'd in iget() */
	ip->i_nlink = 2;
	ITIMES(ip, &time, &time);				/* LFS */

	/*
	 * Bump link count in parent directory
	 * to reflect work done below.  Should
	 * be done before reference is created
	 * so reparation is possible if we crash.
	 */
	dp->i_nlink++;
	dp->i_flag |= ICHG;
	ITIMES(dp, &time, &time);				/* LFS */

	/*
	 * Initialize directory with "."
	 * and ".." from static template.
	 */
	dirtemplate = mastertemplate;
	dirtemplate.dot_ino = ip->i_number;
	dirtemplate.dotdot_ino = dp->i_number;
	error = vn_rdwr(UIO_WRITE, ITOV(ip), (caddr_t)&dirtemplate,
	    sizeof (dirtemplate), (off_t)0, UIO_SYSSPACE,
	    IO_NODELOCKED|IO_SYNC, ndp->ni_cred, (int *)0, (struct proc *)0);
	if (error) {
		dp->i_nlink--;
		dp->i_flag |= ICHG;
		goto bad;
	}
	if (DIRBLKSIZ > dp->i_lfs->lfs_fsize) {
		panic("mkdir: blksize");     /* XXX - should grow w/balloc() */
	} else {
		ip->i_size = DIRBLKSIZ;
		ip->i_flag |= ICHG;
	}
	/*
	 * Directory all set up, now
	 * install the entry for it in
	 * the parent directory.
	 */
	if (error = lfs_direnter(ip, ndp)) {			/* LFS */
		dp->i_nlink--;
		dp->i_flag |= ICHG;
	}
bad:
	/*
	 * No need to do an explicit lfs_itrunc here,
	 * vrele will do this for us because we set
	 * the link count to 0.
	 */
	if (error) {
		ip->i_nlink = 0;
		ip->i_flag |= ICHG;
		iput(ip);
	} else
		ndp->ni_vp = ITOV(ip);
	FREE(ndp->ni_pnbuf, M_NAMEI);
	iput(dp);
	return (error);
}

/*
 * Rmdir system call.
 */
lfs_rmdir(ndp, p)
	register struct nameidata *ndp;
	struct proc *p;
{
	register struct inode *ip, *dp;
	int error = 0;

printf("lfs_rmdir\n");
	ip = VTOI(ndp->ni_vp);
	dp = VTOI(ndp->ni_dvp);
	/*
	 * No rmdir "." please.
	 */
	if (dp == ip) {
		vrele(ITOV(dp));
		iput(ip);
		return (EINVAL);
	}
	/*
	 * Verify the directory is empty (and valid).
	 * (Rmdir ".." won't be valid since
	 *  ".." will contain a reference to
	 *  the current directory and thus be
	 *  non-empty.)
	 */
	if (ip->i_nlink != 2 || !dirempty(ip, dp->i_number, ndp->ni_cred)) {
		error = ENOTEMPTY;
		goto out;
	}
	/*
	 * Delete reference to directory before purging
	 * inode.  If we crash in between, the directory
	 * will be reattached to lost+found,
	 */
	if (error = lfs_dirremove(ndp))				/* LFS */
		goto out;
	dp->i_nlink--;
	dp->i_flag |= ICHG;
	cache_purge(ITOV(dp));
	iput(dp);
	ndp->ni_dvp = NULL;
	/*
	 * Truncate inode.  The only stuff left
	 * in the directory is "." and "..".  The
	 * "." reference is inconsequential since
	 * we're quashing it.  The ".." reference
	 * has already been adjusted above.  We've
	 * removed the "." reference and the reference
	 * in the parent directory, but there may be
	 * other hard links so decrement by 2 and
	 * worry about them later.
	 */
	ip->i_nlink -= 2;
	error = lfs_itrunc(ip, (u_long)0, IO_SYNC);		/* LFS */
	cache_purge(ITOV(ip));
out:
	if (ndp->ni_dvp)
		iput(dp);
	iput(ip);
	return (error);
}

/*
 * symlink -- make a symbolic link
 */
lfs_symlink(ndp, vap, target, p)
	struct nameidata *ndp;
	struct vattr *vap;
	char *target;
	struct proc *p;
{
	struct inode *ip;
	int error;

printf("lfs_symlink\n");
	error = maknode(IFLNK | vap->va_mode, ndp, &ip);
	if (error)
		return (error);
	error = vn_rdwr(UIO_WRITE, ITOV(ip), target, strlen(target), (off_t)0,
		UIO_SYSSPACE, IO_NODELOCKED, ndp->ni_cred, (int *)0,
		(struct proc *)0);
	iput(ip);
	return (error);
}

/*
 * Vnode op for read and write
 */
lfs_readdir(vp, uio, cred, eofflagp)
	struct vnode *vp;
	register struct uio *uio;
	struct ucred *cred;
	int *eofflagp;
{
	int count, lost, error;

printf("lfs_readdir\n");
	count = uio->uio_resid;
	count &= ~(DIRBLKSIZ - 1);
	lost = uio->uio_resid - count;
	if (count < DIRBLKSIZ || (uio->uio_offset & (DIRBLKSIZ -1)))
		return (EINVAL);
	uio->uio_resid = count;
	uio->uio_iov->iov_len = count;
	error = lfs_read(vp, uio, 0, cred);			/* LFS */
	uio->uio_resid += lost;
	if ((VTOI(vp)->i_size - uio->uio_offset) <= 0)
		*eofflagp = 1;
	else
		*eofflagp = 0;
	return (error);
}

/*
 * Return target name of a symbolic link
 */
lfs_readlink(vp, uiop, cred)
	struct vnode *vp;
	struct uio *uiop;
	struct ucred *cred;
{

printf("lfs_readlink\n");
	return (lfs_read(vp, uiop, 0, cred));			/* LFS */
}

/*
 * Get access to bmap
 */
lfs_vbmap(vp, bn, vpp, bnp)
	struct vnode *vp;
	daddr_t bn;
	struct vnode **vpp;
	daddr_t *bnp;
{
	struct inode *ip = VTOI(vp);

printf("lfs_vbmap\n");
	if (vpp != NULL)
		*vpp = ip->i_devvp;
	if (bnp == NULL)
		return (0);
	return (lfs_bmap(ip, bn, bnp));				/* LFS */
}

/*
 * Calculate the logical to physical mapping if not done already,
 * then call the device strategy routine.
 */
lfs_strategy(bp)
	register struct buf *bp;
{
	register struct inode *ip = VTOI(bp->b_vp);
	struct vnode *vp;
	int error;

printf("lfs_strategy: type: %d lblk %d pblk %d\n", bp->b_vp->v_type,
	bp->b_lblkno, bp->b_blkno);
	if (bp->b_vp->v_type == VBLK || bp->b_vp->v_type == VCHR)
		panic("ufs_strategy: spec");
	if (bp->b_blkno == bp->b_lblkno) {			/* LFS */
		if (error = lfs_bmap(ip, bp->b_lblkno, &bp->b_blkno))
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
 * Allocate a new inode.
 */
static int
maknode(mode, ndp, ipp)
	int mode;
	register struct nameidata *ndp;
	struct inode **ipp;
{
	register struct inode *ip;
	struct inode *tip;
	register struct inode *pdir = VTOI(ndp->ni_dvp);
	ino_t ipref;
	int error;

printf("maknode\n");
#ifdef DIANOSTIC
	if ((ndp->ni_nameiop & HASBUF) == 0)
		panic("maknode: no name");
#endif
	*ipp = 0;
	if ((mode & IFMT) == 0)
		mode |= IFREG;
#ifdef NOTLFS							/* LFS */
	if ((mode & IFMT) == IFDIR)
		ipref = dirpref(pdir->i_fs);
	else
		ipref = pdir->i_number;
	if (error = ialloc(pdir, ipref, mode, ndp->ni_cred, &tip)) {
#else
	if (error = lfs_ialloc(pdir->i_lfs, pdir, &tip, ndp->ni_cred)) {
#endif
		free(ndp->ni_pnbuf, M_NAMEI);
		iput(pdir);
		return (error);
	}
	ip = tip;
	ip->i_uid = ndp->ni_cred->cr_uid;
	ip->i_gid = pdir->i_gid;
#ifdef QUOTA
	if ((error = getinoquota(ip)) ||
	    (error = chkiq(ip, 1, ndp->ni_cred, 0))) {
		free(ndp->ni_pnbuf, M_NAMEI);
#ifdef NOTLFS							/* LFS */
		ifree(ip, ip->i_number, mode);
#else
		lfs_ifree(ip);
#endif
		iput(ip);
		iput(pdir);
		return (error);
	}
#endif
	ip->i_flag |= IACC|IUPD|ICHG;
	ip->i_mode = mode;
	ITOV(ip)->v_type = IFTOVT(mode);	/* Rest init'd in iget() */
	ip->i_nlink = 1;
	if ((ip->i_mode & ISGID) && !groupmember(ip->i_gid, ndp->ni_cred) &&
	    suser(ndp->ni_cred, NULL))
		ip->i_mode &= ~ISGID;

	ITIMES(ip, &time, &time);				/* LFS */
	if (error = lfs_direnter(ip, ndp))			/* LFS */
		goto bad;
	if ((ndp->ni_nameiop & SAVESTART) == 0)
		FREE(ndp->ni_pnbuf, M_NAMEI);
	iput(pdir);
	*ipp = ip;
	return (0);

bad:
	/*
	 * Write error occurred trying to update the inode
	 * or the directory so must deallocate the inode.
	 */
	free(ndp->ni_pnbuf, M_NAMEI);
	iput(pdir);
	ip->i_nlink = 0;
	ip->i_flag |= ICHG;
	iput(ip);
	return (error);
}

/*
 * Global vfs data structures for lfs
 */
struct vnodeops lfs_vnodeops = {
	lfs_lookup,		/* lookup */
	lfs_create,		/* create */
	lfs_mknod,		/* mknod */
	ufs_open,		/* open */
	ufs_close,		/* close */
	ufs_access,		/* access */
	lfs_getattr,		/* getattr */
	lfs_setattr,		/* setattr */
	lfs_read,		/* read */
	lfs_write,		/* write */
	ufs_ioctl,		/* ioctl */
	ufs_select,		/* select */
	ufs_mmap,		/* mmap */
	lfs_fsync,		/* fsync */
	ufs_seek,		/* seek */
	lfs_remove,		/* remove */
	lfs_link,		/* link */
	lfs_rename,		/* rename */
	lfs_mkdir,		/* mkdir */
	lfs_rmdir,		/* rmdir */
	lfs_symlink,		/* symlink */
	lfs_readdir,		/* readdir */
	lfs_readlink,		/* readlink */
	ufs_abortop,		/* abortop */
	lfs_inactive,		/* inactive */
	ufs_reclaim,		/* reclaim */
	ufs_lock,		/* lock */
	ufs_unlock,		/* unlock */
	lfs_vbmap,		/* bmap */
	lfs_strategy,		/* strategy */
	ufs_print,		/* print */
	ufs_islocked,		/* islocked */
	ufs_advlock,		/* advlock */
};
