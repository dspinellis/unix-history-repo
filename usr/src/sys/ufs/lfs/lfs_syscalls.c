/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_syscalls.c	7.2 (Berkeley) %G%
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
	struct lfs *fs;
	struct mount *mntp;
	struct vnode *vp;
	daddr_t daddr;
	u_long bsize;
	int cnt, error;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);

	if ((mntp = getvfs(&uap->fsid)) == NULL)
		return (EINVAL);

	cnt = uap->blkcnt;
	blkp = malloc(cnt * sizeof(BLOCK_INFO), M_SEGMENT, M_WAITOK);
	if (error = copyin(uap->blkiov, blkp, cnt * sizeof(BLOCK_INFO))) {
		free(blkp, M_SEGMENT);
		return (error);
	}

	/*
	 * Mark blocks/inodes dirty.  For blocks, we get the vnode, and check
	 * to see if the modified or disk address is newer than the cleaner
	 * thinks.  If so, we're done.  Otherwise, we get the block, from core
	 * if we have it, otherwise from the cleaner, and write it.  Note that
	 * errors are mostly ignored.  If we can't get the info, the block is
	 * probably not all that useful, and hopefully subsequent calls from
	 * the cleaner will fix everything.
	 */
	bsize = VFSTOUFS(mntp)->um_lfs->lfs_bsize;
	for (; cnt--; ++blkp) {
		if (lfs_vget(mntp, blkp->bi_inode, &vp) ||
		    VTOI(vp)->i_mtime >= blkp->bi_segcreate ||
		    lfs_bmap(vp, blkp->bi_lbn, NULL, &daddr) ||
		    daddr != blkp->bi_daddr)
			continue;
		bp = getblk(vp, blkp->bi_lbn, bsize);
		if (!(bp->b_flags & B_CACHE) &&
		    (error = copyin(blkp->bi_bp, bp->b_un.b_daddr, bsize))) {
			brelse(bp);
			free(blkp, M_SEGMENT);
			return (error);
		}
		lfs_bwrite(bp);
		brelse(bp);
	}
	free(blkp, M_SEGMENT);

	cnt = uap->inocnt;
	inop = malloc(cnt * sizeof(INODE_INFO), M_SEGMENT, M_WAITOK);
	if (error = copyin(uap->inoiov, inop, cnt * sizeof(INODE_INFO))) {
		free(inop, M_SEGMENT);
		return (error);
	}

	fs = VFSTOUFS(mntp)->um_lfs;
	for (; cnt--; ++inop) {
		LFS_IENTRY(ifp, fs, inop->ii_inode, bp);
		daddr = ifp->if_daddr;
		brelse(bp);
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
		if (!lfs_vget(mntp, inop->ii_inode, &vp))
			VTOI(vp)->i_flag |= IMOD;
	}
	free(inop, M_SEGMENT);
	return (lfs_segwrite(mntp, 1));
}

/*
 * lfs_bmapv:
 *
 * This will fill in the current disk address for arrays of inodes and blocks.
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
	daddr_t daddr;
	int cnt, error;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);

	if ((mntp = getvfs(&uap->fsid)) == NULL)
		return (EINVAL);

	cnt = uap->blkcnt;
	blkp = malloc(cnt * sizeof(BLOCK_INFO), M_SEGMENT, M_WAITOK);
	if (error = copyin(uap->blkiov, blkp, cnt * sizeof(BLOCK_INFO))) {
		free(blkp, M_SEGMENT);
		return (error);
	}

	for (; cnt--; ++blkp)
		blkp->bi_daddr = 
		    lfs_vget(mntp, blkp->bi_inode, &vp) ||
		    lfs_bmap(vp, blkp->bi_lbn, NULL, &daddr) ?
			LFS_UNUSED_DADDR : daddr;
	free(blkp, M_SEGMENT);
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

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);

	if ((mntp = getvfs(&uap->fsid)) == NULL)
		return (EINVAL);

	fs = VFSTOUFS(mntp)->um_lfs;

	LFS_SEGENTRY(sup, fs, uap->segment, bp);
	sup->su_flags &= ~SEGUSE_DIRTY;
	brelse(bp);

	LFS_CLEANERINFO(cip, fs, bp);
	++cip->clean;
	--cip->dirty;
	brelse(bp);

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
		s = splhigh(); timevaladd(&atv, &time); splx(s);
		timeout = hzto(&atv);
	} else
		timeout = 0;

	error = tsleep(addr, PCATCH | PUSER, "segment", timeout);
	return (error == ERESTART ? EINTR : 0);
}
