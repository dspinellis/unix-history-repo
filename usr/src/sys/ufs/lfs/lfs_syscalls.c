/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_syscalls.c	7.17 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/mount.h>
#include <sys/vnode.h>
#include <sys/malloc.h>
#include <sys/kernel.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufsmount.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

/*
 * lfs_markv:
 *
 * This will mark inodes and blocks dirty, so they are written into the log.
 * It will block until all the blocks have been written.  The segment create
 * time passed in the block_info and inode_info structures is used to decide
 * if the data is valid for each block (in case some process dirtied a block
 * or inode that is being cleaned between the determination that a block is
 * live and the lfs_markv call).
 *
 *  0 on success
 * -1/errno is return on error.
 */
int
lfs_markv(p, uap, retval)
	struct proc *p;
	struct args {
		fsid_t fsid;		/* file system */
		BLOCK_INFO *blkiov;	/* block array */
		int blkcnt;		/* count of block array entries */
		INODE_INFO *inoiov;	/* inode array */
		int inocnt;		/* count of inode array entries */
	} *uap;
	int *retval;
{
	BLOCK_INFO *blkp;
	IFILE *ifp;
	INODE_INFO *inop;
	struct buf *bp;
	struct inode *ip;
	struct lfs *fs;
	struct mount *mntp;
	struct vnode *vp;
	void *start;
	ino_t lastino;
	daddr_t daddr;
	u_long bsize;
	int cnt, error;

#ifdef VERBOSE
	printf("lfs_markv\n");
#endif
	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);

	if ((mntp = getvfs(&uap->fsid)) == NULL)
		return (EINVAL);

	cnt = uap->blkcnt;
	start = malloc(cnt * sizeof(BLOCK_INFO), M_SEGMENT, M_WAITOK);
	if (error = copyin(uap->blkiov, start, cnt * sizeof(BLOCK_INFO))) {
		free(start, M_SEGMENT);
		return (error);
	}

	/*
	 * Mark blocks/inodes dirty.  Note that errors are mostly ignored.  If
	 * we can't get the info, the block is probably not all that useful,
	 * and hopefully subsequent calls from the cleaner will fix everything.
	 */
	fs = VFSTOUFS(mntp)->um_lfs;
	bsize = fs->lfs_bsize;
	for (lastino = LFS_UNUSED_INUM, blkp = start; cnt--; ++blkp) {
		/*
		 * Get the IFILE entry (only once) and see if the file still
		 * exists.
		 */
		if (lastino != blkp->bi_inode) {
			lastino = blkp->bi_inode;
			LFS_IENTRY(ifp, fs, blkp->bi_inode, bp);
			daddr = ifp->if_daddr;
			brelse(bp);
			if (daddr == LFS_UNUSED_DADDR)
				continue;
		}

		/*
		 * Get the vnode/inode.  If the inode modification time is
		 * earlier than the segment in which the block was found then
		 * they have to be valid, skip other checks.
		 */
		if (VFS_VGET(mntp, blkp->bi_inode, &vp))
			continue;
		ip = VTOI(vp);

		/*
		 * If modify time later than segment create time, see if the
		 * block has been replaced.
		 */
		if (ip->i_mtime.ts_sec > blkp->bi_segcreate &&
		    (VOP_BMAP(vp, blkp->bi_lbn, NULL, &daddr) ||
		    daddr != blkp->bi_daddr)) {
			vput(vp);
			continue;
		}

		/* Get the block (from core or the cleaner) and write it. */
		bp = getblk(vp, blkp->bi_lbn, bsize);
		vput(vp);
		if (!(bp->b_flags & B_CACHE) &&
		    (error = copyin(blkp->bi_bp, bp->b_un.b_addr, bsize))) {
			brelse(bp);
			free(start, M_SEGMENT);
			return (error);
		}
		VOP_BWRITE(bp);
	}
	free(start, M_SEGMENT);

	cnt = uap->inocnt;
	start = malloc(cnt * sizeof(INODE_INFO), M_SEGMENT, M_WAITOK);
	if (error = copyin(uap->inoiov, start, cnt * sizeof(INODE_INFO))) {
		free(start, M_SEGMENT);
		return (error);
	}

	for (inop = start; cnt--; ++inop) {
		if (inop->ii_inode == LFS_IFILE_INUM)
			daddr = fs->lfs_idaddr;
		else {
			LFS_IENTRY(ifp, fs, inop->ii_inode, bp);
			daddr = ifp->if_daddr;
			brelse(bp);
		}

		if (daddr != inop->ii_daddr)
			continue;
		/*
		 * XXX
		 * This is grossly inefficient since the cleaner just handed
		 * us a copy of the inode and we're going to have to seek
		 * to get our own.  The fix requires creating a version of
		 * lfs_vget that takes the copy and uses it instead of reading
		 * from disk, if it's not already in the cache.
		 */
		if (!VFS_VGET(mntp, inop->ii_inode, &vp)) {
			VTOI(vp)->i_flag |= IMOD;
			vput(vp);
		}	
	}
	free(start, M_SEGMENT);
	return (lfs_segwrite(mntp, 1));
}

/*
 * lfs_bmapv:
 *
 * This will fill in the current disk address for arrays of blocks.
 *
 *  0 on success
 * -1/errno is return on error.
 */
int
lfs_bmapv(p, uap, retval)
	struct proc *p;
	struct args {
		fsid_t fsid;		/* file system */
		BLOCK_INFO *blkiov;	/* block array */
		int blkcnt;		/* count of block array entries */
	} *uap;
	int *retval;
{
	BLOCK_INFO *blkp;
	struct mount *mntp;
	struct vnode *vp;
	void *start;
	daddr_t daddr;
	int cnt, error, step;

#ifdef VERBOSE
	printf("lfs_bmapv\n");
#endif
	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);

	if ((mntp = getvfs(&uap->fsid)) == NULL)
		return (EINVAL);

	cnt = uap->blkcnt;
	start = blkp = malloc(cnt * sizeof(BLOCK_INFO), M_SEGMENT, M_WAITOK);
	if (error = copyin(uap->blkiov, blkp, cnt * sizeof(BLOCK_INFO))) {
		free(blkp, M_SEGMENT);
		return (error);
	}

	for (step = cnt; step--; ++blkp) {
		if (VFS_VGET(mntp, blkp->bi_inode, &vp))
			daddr = LFS_UNUSED_DADDR;
		else {
			if (VOP_BMAP(vp, blkp->bi_lbn, NULL, &daddr))
				daddr = LFS_UNUSED_DADDR;
			vput(vp);
		}
		blkp->bi_daddr = daddr;
        }
	copyout(start, uap->blkiov, cnt * sizeof(BLOCK_INFO));
	free(start, M_SEGMENT);
	return (0);
}

/*
 * lfs_segclean:
 *
 * Mark the segment clean.
 *
 *  0 on success
 * -1/errno is return on error.
 */
int
lfs_segclean(p, uap, retval)
	struct proc *p;
	struct args {
		fsid_t fsid;		/* file system */
		u_long segment;		/* segment number */
	} *uap;
	int *retval;
{
	CLEANERINFO *cip;
	SEGUSE *sup;
	struct buf *bp;
	struct mount *mntp;
	struct lfs *fs;
	int error;

#ifdef VERBOSE
	printf("lfs_segclean\n");
#endif
	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);

	if ((mntp = getvfs(&uap->fsid)) == NULL)
		return (EINVAL);

	fs = VFSTOUFS(mntp)->um_lfs;

	LFS_SEGENTRY(sup, fs, uap->segment, bp);
	fs->lfs_bfree += (sup->su_nsums * LFS_SUMMARY_SIZE / DEV_BSIZE) +
	    sup->su_ninos * btodb(fs->lfs_bsize);
	sup->su_flags &= ~SEGUSE_DIRTY;
	sup->su_nbytes -= sup->su_nsums * LFS_SUMMARY_SIZE;
	sup->su_ninos = 0;
	sup->su_nsums = 0;
	LFS_UBWRITE(bp);

	LFS_CLEANERINFO(cip, fs, bp);
	++cip->clean;
	--cip->dirty;
	LFS_UBWRITE(bp);
	return (0);
}

/*
 * lfs_segwait:
 *
 * This will block until a segment in file system fsid is written.  A timeout
 * in milliseconds may be specified which will awake the cleaner automatically.
 * An fsid of -1 means any file system, and a timeout of 0 means forever.
 *
 *  0 on success
 *  1 on timeout
 * -1/errno is return on error.
 */
int
lfs_segwait(p, uap, retval)
	struct proc *p;
	struct args {
		fsid_t fsid;		/* file system */
		struct timeval *tv;	/* timeout */
	} *uap;
	int *retval;
{
	extern int lfs_allclean_wakeup;
	struct mount *mntp;
	struct timeval atv;
	void *addr;
	u_long timeout;
	int error, s;

#ifdef VERBOSE
	printf("lfs_segwait\n");
#endif
	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);

#ifdef WHEN_QUADS_WORK
	if (uap->fsid == (fsid_t)-1)
		addr = &lfs_allclean_wakeup;
	else {
		if ((mntp = getvfs(&uap->fsid)) == NULL)
			return (EINVAL);
		addr = &VFSTOUFS(mntp)->um_lfs->lfs_nextseg;
	}
#else
	if ((mntp = getvfs(&uap->fsid)) == NULL)
		addr = &lfs_allclean_wakeup;
	else
		addr = &VFSTOUFS(mntp)->um_lfs->lfs_nextseg;
#endif

	if (uap->tv) {
		if (error = copyin(uap->tv, &atv, sizeof(struct timeval)))
			return (error);
		if (itimerfix(&atv))
			return (EINVAL);
		s = splclock();
		timevaladd(&atv, (struct timeval *)&time);
		timeout = hzto(&atv);
		splx(s);
	} else
		timeout = 0;

	error = tsleep(addr, PCATCH | PUSER, "segment", timeout);
	return (error == ERESTART ? EINTR : 0);
}
