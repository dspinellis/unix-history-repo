/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_syscalls.c	7.19 (Berkeley) %G%
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
#include <ufs/ufs/ufs_extern.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

struct buf *lfs_fakebuf __P((struct vnode *, int, size_t, caddr_t));

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
	} *uap;
	int *retval;
{
	struct segment *sp;
	BLOCK_INFO *blkp;
	IFILE *ifp;
	struct buf *bp, **bpp;
	struct inode *ip;
	struct lfs *fs;
	struct mount *mntp;
	struct vnode *vp;
	void *start;
	ino_t lastino;
	daddr_t b_daddr, v_daddr;
	u_long bsize;
	int cnt, error;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	if ((mntp = getvfs(&uap->fsid)) == NULL)
		return (EINVAL);
	/* Initialize a segment. */
	sp = malloc(sizeof(struct segment), M_SEGMENT, M_WAITOK);
	sp->bpp = malloc(((LFS_SUMMARY_SIZE - sizeof(SEGSUM)) /
	    sizeof(daddr_t) + 1) * sizeof(struct buf *), M_SEGMENT, M_WAITOK);
	sp->seg_flags = SEGM_CKP;

	cnt = uap->blkcnt;
	start = malloc(cnt * sizeof(BLOCK_INFO), M_SEGMENT, M_WAITOK);
	if (error = copyin(uap->blkiov, start, cnt * sizeof(BLOCK_INFO)))
		goto err1;

	/* Mark blocks/inodes dirty.  */
	fs = VFSTOUFS(mntp)->um_lfs;
	bsize = fs->lfs_bsize;
	error = 0;

	lfs_seglock(fs);
	lfs_initseg(fs, sp);
	sp->seg_flags |= SEGM_CLEAN;
	for (v_daddr = LFS_UNUSED_DADDR, lastino = LFS_UNUSED_INUM,
	    blkp = start; cnt--; ++blkp) {
		/*
		 * Get the IFILE entry (only once) and see if the file still
		 * exists.
		 */
		if (lastino != blkp->bi_inode) {
			if (lastino != LFS_UNUSED_INUM) {
				lfs_updatemeta(sp, vp);
				lfs_writeinode(fs, sp, ip);
				vput(vp);
			}
			lastino = blkp->bi_inode;
			LFS_IENTRY(ifp, fs, blkp->bi_inode, bp);
			v_daddr = ifp->if_daddr;
			brelse(bp);
			if (v_daddr == LFS_UNUSED_DADDR)
				continue;
			/* Get the vnode/inode. */
			if (lfs_fastvget(mntp, blkp->bi_inode, v_daddr, &vp,
			    blkp->bi_lbn == LFS_UNUSED_LBN ? NULL :
			    blkp->bi_bp)) {
#ifdef DIAGNOSTIC
				printf("lfs_markv: VFS_VGET failed (%d)\n",
				    blkp->bi_inode);
#endif
				v_daddr == LFS_UNUSED_DADDR;
				continue;
			}
			ip = VTOI(vp);
		} else if (v_daddr == LFS_UNUSED_DADDR)
			continue;

		/* If this BLOCK_INFO didn't contain a block, keep going. */
		if (blkp->bi_lbn == LFS_UNUSED_LBN)
			continue;
		/*
		 * If modify time later than segment create time, see if the
		 * block has been replaced.
		 */
		if (ip->i_mtime.ts_sec > blkp->bi_segcreate &&
		    (VOP_BMAP(vp, blkp->bi_lbn, NULL, &b_daddr) ||
		    b_daddr != blkp->bi_daddr))
			continue;
		/*
		 * If we got to here, then we are keeping the block.  If it
		 * is an indirect block, we want to actually put it in the
		 * buffer cache so that it can be updated in the finish_meta
		 * section.  If it's not, we need to allocate a fake buffer
		 * so that writeseg can perform the copyin and write the buffer.
		 */
		if (blkp->bi_lbn >= 0)	/* Data Block */
			bp = lfs_fakebuf(vp, blkp->bi_lbn, bsize,
			    blkp->bi_bp);
		else {
			bp = getblk(vp, blkp->bi_lbn, bsize);
			if (!(bp->b_flags & B_CACHE) &&
			    (error = copyin(blkp->bi_bp, bp->b_un.b_addr,
			    bsize)))
				goto err2;
			if (error = VOP_BWRITE(bp))
				goto err2;
		}
		lfs_gatherblock(sp, bp, NULL);
	}
	lfs_updatemeta(sp, vp);
	lfs_writeinode(fs, sp, ip);
	vput(vp);
	(void) lfs_writeseg(fs, sp);
	lfs_segunlock(fs);
	free(start, M_SEGMENT);
	free(sp->bpp, M_SEGMENT);
	free(sp, M_SEGMENT);
	return (error);
/*
 * XXX If we come in to error 2, we might have indirect blocks that were
 * updated and now have bad block pointers.  I don't know what to do
 * about this.
 */

err2:	vput(vp);
	/* Free up fakebuffers */
	for (bpp = --sp->cbpp; bpp >= sp->bpp; --bpp)
		if ((*bpp)->b_flags & B_CALL) {
			brelvp(*bpp);
			free(*bpp, M_SEGMENT);
		} else
			brelse(*bpp);
	lfs_segunlock(fs);
err1:
	free(sp->bpp, M_SEGMENT);
	free(sp, M_SEGMENT);
	free(start, M_SEGMENT);
	return(error);
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

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	if ((mntp = getvfs(&uap->fsid)) == NULL)
		return (EINVAL);

	fs = VFSTOUFS(mntp)->um_lfs;

	LFS_SEGENTRY(sup, fs, uap->segment, bp);
	fs->lfs_avail += fsbtodb(fs, fs->lfs_ssize) - 1;
	fs->lfs_bfree += (sup->su_nsums * LFS_SUMMARY_SIZE / DEV_BSIZE) +
	    sup->su_ninos * btodb(fs->lfs_bsize);
	sup->su_flags &= ~SEGUSE_DIRTY;
	sup->su_nbytes -= sup->su_nsums * LFS_SUMMARY_SIZE;
	sup->su_ninos = 0;
	sup->su_nsums = 0;
	(void) VOP_BWRITE(bp);

	LFS_CLEANERINFO(cip, fs, bp);
	++cip->clean;
	--cip->dirty;
	(void) VOP_BWRITE(bp);
	wakeup(&fs->lfs_avail);
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

	if (error = suser(p->p_ucred, &p->p_acflag)) {
		return (error);
}
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

/*
 * VFS_VGET call specialized for the cleaner.  The cleaner already knows the
 * daddr from the ifile, so don't look it up again.  If the cleaner is
 * processing IINFO structures, it may have the ondisk inode already, so
 * don't go retrieving it again.
 */
int
lfs_fastvget(mp, ino, daddr, vpp, dinp)
	struct mount *mp;
	ino_t ino;
	daddr_t daddr;
	struct vnode **vpp;
	struct dinode *dinp;
{
	register struct inode *ip;
	struct vnode *vp;
	struct ufsmount *ump;
	struct buf *bp;
	dev_t dev;
	int error;

	ump = VFSTOUFS(mp);
	dev = ump->um_dev;
	if ((*vpp = ufs_ihashget(dev, ino)) != NULL)
		return (0);

	/* Allocate new vnode/inode. */
	if (error = lfs_vcreate(mp, ino, &vp)) {
		*vpp = NULL;
		return (error);
	}

	/*
	 * Put it onto its hash chain and lock it so that other requests for
	 * this inode will block if they arrive while we are sleeping waiting
	 * for old data structures to be purged or for the contents of the
	 * disk portion of this inode to be read.
	 */
	ip = VTOI(vp);
	ufs_ihashins(ip);

	/*
	 * XXX
	 * This may not need to be here, logically it should go down with
	 * the i_devvp initialization.
	 * Ask Kirk.
	 */
	ip->i_lfs = ump->um_lfs;

	/* Read in the disk contents for the inode, copy into the inode. */
	if (dinp)
		if (error = copyin(dinp, &ip->i_din, sizeof(struct dinode)))
			return (error);
	else {
		if (error = bread(ump->um_devvp, daddr,
		    (int)ump->um_lfs->lfs_bsize, NOCRED, &bp)) {
			/*
			 * The inode does not contain anything useful, so it
			 * would be misleading to leave it on its hash chain.
			 * Iput() will return it to the free list.
			 */
			ufs_ihashrem(ip);

			/* Unlock and discard unneeded inode. */
			ufs_iput(ip);
			brelse(bp);
			*vpp = NULL;
			return (error);
		}
		ip->i_din = *lfs_ifind(ump->um_lfs, ino, bp->b_un.b_dino);
		brelse(bp);
	}

	/*
	 * Initialize the vnode from the inode, check for aliases.  In all
	 * cases re-init ip, the underlying vnode/inode may have changed.
	 */
	if (error = ufs_vinit(mp, lfs_specop_p, LFS_FIFOOPS, &vp)) {
		ufs_iput(ip);
		*vpp = NULL;
		return (error);
	}
	/*
	 * Finish inode initialization now that aliasing has been resolved.
	 */
	ip->i_devvp = ump->um_devvp;
	ip->i_flag |= IMOD;
	++ump->um_lfs->lfs_uinodes;
	VREF(ip->i_devvp);
	*vpp = vp;
	return (0);
}
struct buf *
lfs_fakebuf(vp, lbn, size, uaddr)
	struct vnode *vp;
	int lbn;
	size_t size;
	caddr_t uaddr;
{
	struct buf *bp;

	bp = lfs_newbuf(vp, lbn, 0);
	bp->b_saveaddr = uaddr;
	bp->b_bufsize = size;
	bp->b_bcount = size;
	bp->b_flags |= B_INVAL;
	return(bp);
}
