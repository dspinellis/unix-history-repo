/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_inode.c	7.42 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "mount.h"
#include "proc.h"
#include "file.h"
#include "buf.h"
#include "vnode.h"
#include "kernel.h"
#include "malloc.h"

#include "../ufs/quota.h"
#include "../ufs/inode.h"
#include "../ufs/ufsmount.h"
#include "lfs.h"
#include "lfs_extern.h"

#define	INOHSZ	512
#if	((INOHSZ&(INOHSZ-1)) == 0)
#define	INOHASH(dev,ino)	(((dev)+(ino))&(INOHSZ-1))
#else
#define	INOHASH(dev,ino)	(((unsigned)((dev)+(ino)))%INOHSZ)
#endif

union lfsihead {						/* LFS */
	union  lfsihead *ih_head[2];
	struct inode *ih_chain[2];
} lfsihead[INOHSZ];

								/* LFS */
extern int prtactive;	/* 1 => print out reclaim of active vnodes */

/*
 * Initialize hash links for inodes.
 */
lfs_init()
{
	register int i;
	register union lfsihead *ih = lfsihead;

printf("lfs_init\n");
#ifndef lint
	if (VN_MAXPRIVATE < sizeof(struct inode))
		panic("ihinit: too small");
#endif /* not lint */
	for (i = INOHSZ; --i >= 0; ih++) {
		ih->ih_head[0] = ih;
		ih->ih_head[1] = ih;
	}
#ifdef NOTLFS							/* LFS */
#ifdef QUOTA
	dqinit();
#endif /* QUOTA */
#endif
}

lfs_hqueue(ip)
	struct inode *ip;
{
	union lfsihead *ih;

printf("lfs_hqueue ino %d\n", ip->i_number);
	ih = &lfsihead[INOHASH(ip->i_dev, ip->i_number)];
	insque(ip, ih);
	ILOCK(ip);
}
	
	
/*
 * Look up a UFS dinode number to find its incore vnode.
 * If it is not in core, read it in from the specified device.
 * If it is in core, wait for the lock bit to clear, then
 * return the inode locked. Detection and handling of mount
 * points must be done by the calling routine.
 */
lfs_iget(xp, ino, ipp)
	struct inode *xp;
	ino_t ino;
	struct inode **ipp;
{
	dev_t dev = xp->i_dev;
	struct mount *mntp = ITOV(xp)->v_mount;
	register LFS *fs = VFSTOUFS(mntp)->um_lfs;		/* LFS */
	extern struct vnodeops ufs_vnodeops, spec_inodeops;
	register struct inode *ip, *iq;
	register struct vnode *vp;
	struct vnode *nvp;
	struct buf *bp;
	union lfsihead *ih;
	int i, error;

printf("lfs_iget ino %d\n", ino);
	ih = &lfsihead[INOHASH(dev, ino)];
loop:
	for (ip = ih->ih_chain[0]; ip != (struct inode *)ih; ip = ip->i_forw) {
		if (ino != ip->i_number || dev != ip->i_dev)
			continue;
		if ((ip->i_flag&ILOCKED) != 0) {
			ip->i_flag |= IWANT;
			sleep((caddr_t)ip, PINOD);
			goto loop;
		}
		if (vget(ITOV(ip)))
			goto loop;
		*ipp = ip;
		return(0);
	}

	/* Allocate new vnode/inode. */
	error = lfs_vcreate(mntp, ino, &nvp);
	if (error) {
		*ipp = 0;
		return (error);
	}
	ip = VTOI(nvp);
		
	/*
	 * Put it onto its hash chain and lock it so that other requests for
	 * this inode will block if they arrive while we are sleeping waiting
	 * for old data structures to be purged or for the contents of the
	 * disk portion of this inode to be read.
	 */
	insque(ip, ih);
	ILOCK(ip);

	/* Read in the disk contents for the inode, copy into the vnode. */
	if (error = bread(VFSTOUFS(mntp)->um_devvp, itod(fs, ino),
	    (int)fs->lfs_bsize, NOCRED, &bp)) {			/* LFS */
		/*
		 * The inode does not contain anything useful, so it would
		 * be misleading to leave it on its hash chain.
		 * Iput() will take care of putting it back on the free list.
		 */
		remque(ip);
		ip->i_forw = ip;
		ip->i_back = ip;
		/*
		 * Unlock and discard unneeded inode.
		 */
		iput(ip);
		brelse(bp);
		*ipp = 0;
		return (error);
	}
	ip->i_din = *lfs_ifind(fs, ino, bp->b_un.b_dino);
	brelse(bp);

	/*
	 * Initialize the associated vnode
	 */
	vp = ITOV(ip);
	vp->v_type = IFTOVT(ip->i_mode);
	if (vp->v_type == VFIFO) {
#ifdef FIFO
		extern struct vnodeops fifo_inodeops;
		vp->v_op = &fifo_inodeops;
#else
		iput(ip);
		*ipp = 0;
		return (EOPNOTSUPP);
#endif /* FIFO */
	}
	if (vp->v_type == VCHR || vp->v_type == VBLK) {
		vp->v_op = &spec_inodeops;
		if (nvp = checkalias(vp, ip->i_rdev, mntp)) {
			/*
			 * Reinitialize aliased inode.
			 */
			vp = nvp;
			iq = VTOI(vp);
			iq->i_vnode = vp;
			iq->i_flag = 0;
			ILOCK(iq);
			iq->i_din = ip->i_din;
			iq->i_dev = dev;
			iq->i_number = ino;
			insque(iq, ih);
			/*
			 * Discard unneeded vnode
			 */
			ip->i_mode = 0;
			iput(ip);
			ip = iq;
		}
	}
	if (ino == ROOTINO)
		vp->v_flag |= VROOT;

	VREF(ip->i_devvp);

	*ipp = ip;
	return (0);
}

/*
 * Last reference to an inode, write the inode out and if necessary,
 * truncate and deallocate the file.
 */
lfs_inactive(vp, p)
	struct vnode *vp;
	struct proc *p;
{
	register struct inode *ip = VTOI(vp);
	int mode, error = 0;

printf("lfs_inactive: ino %d mode %d nlink %d\n",
ip->i_number, ip->i_mode, ip->i_nlink);

	if (prtactive && vp->v_usecount != 0)
		vprint("ufs_inactive: pushing active", vp);
	/*
	 * Get rid of inodes related to stale file handles.
	 */
	if (ip->i_mode == 0) {
		if ((vp->v_flag & VXLOCK) == 0)
			vgone(vp);
		return (0);
	}
	ILOCK(ip);
	if (ip->i_nlink <= 0 && (vp->v_mount->mnt_flag & MNT_RDONLY) == 0) {
#ifdef QUOTA
		if (!getinoquota(ip))
			(void) chkiq(ip, -1, NOCRED, 0);
#endif
		error = lfs_itrunc(ip, (u_long)0, 0);		/* LFS */
		mode = ip->i_mode;
		ip->i_mode = 0;
		ip->i_rdev = 0;
		ip->i_flag |= IUPD|ICHG;
#ifdef NOTLFS							/* LFS */
		ifree(ip, ip->i_number, mode);
#else
		lfs_ifree(ip);
#endif
	}
	ITIMES(ip, &time, &time);
	IUNLOCK(ip);
	ip->i_flag = 0;
	/*
	 * If we are done with the inode, reclaim it
	 * so that it can be reused immediately.
	 */
	if (vp->v_usecount == 0 && ip->i_mode == 0)
		vgone(vp);
	return (error);
}

#define	SINGLE	0	/* index of single indirect block */
#define	DOUBLE	1	/* index of double indirect block */
#define	TRIPLE	2	/* index of triple indirect block */
/*
 * Truncate the inode ip to at most length size.  Free affected disk
 * blocks -- the blocks of the file are removed in reverse order.
 *
 * NB: triple indirect blocks are untested.
 */
lfs_itrunc(oip, length, flags)
	register struct inode *oip;
	u_long length;
	int flags;
{
	register daddr_t lastblock;
	daddr_t bn, lbn, lastiblock[NIADDR];
	register LFS *fs;					/* LFS */
	register struct inode *ip;
	struct buf *bp;
	int offset, osize, size, level;
	long count, nblocks, blocksreleased = 0;
	register int i;
	int aflags, error, allerror;
	struct inode tip;

	vnode_pager_setsize(ITOV(oip), length);
	if (oip->i_size <= length) {
		oip->i_flag |= ICHG|IUPD;
		ITIMES(oip, &time, &time);
		return (0);
	}
	/*
	 * Calculate index into inode's block list of
	 * last direct and indirect blocks (if any)
	 * which we want to keep.  Lastblock is -1 when
	 * the file is truncated to 0.
	 */
	fs = oip->i_lfs;					/* LFS */
	lastblock = lblkno(fs, length + fs->lfs_bsize - 1) - 1;	/* LFS */
	lastiblock[SINGLE] = lastblock - NDADDR;
	lastiblock[DOUBLE] = lastiblock[SINGLE] - NINDIR(fs);
	lastiblock[TRIPLE] = lastiblock[DOUBLE] - NINDIR(fs) * NINDIR(fs);
	nblocks = btodb(fs->lfs_bsize);				/* LFS */
	/*
	 * Update the size of the file. If the file is not being
	 * truncated to a block boundry, the contents of the
	 * partial block following the end of the file must be
	 * zero'ed in case it ever become accessable again because
	 * of subsequent file growth.
	 */
	osize = oip->i_size;
	offset = blkoff(fs, length);
	if (offset == 0) {
		oip->i_size = length;
	} else {
		lbn = lblkno(fs, length);
		aflags = B_CLRBUF;
		if (flags & IO_SYNC)
			aflags |= B_SYNC;
#ifdef QUOTA
		if (error = getinoquota(oip))
			return (error);
#endif	
		if (error = bread(ITOV(oip), lbn, fs->lfs_bsize, NOCRED, &bp))
			return (error);
		oip->i_size = length;
		size = blksize(fs);				/* LFS */
		(void) vnode_pager_uncache(ITOV(oip));
		bzero(bp->b_un.b_addr + offset, (unsigned)(size - offset));
		allocbuf(bp, size);
#ifdef NOTLFS
		if (flags & IO_SYNC)				/* LFS */
			bwrite(bp);
		else
			bdwrite(bp);
#else
		lfs_bwrite(bp);
#endif
	}
	/*
	 * Update file and block pointers
	 * on disk before we start freeing blocks.
	 * If we crash before free'ing blocks below,
	 * the blocks will be returned to the free list.
	 * lastiblock values are also normalized to -1
	 * for calls to indirtrunc below.
	 */
	/* Will need to modify the segment usage information */	/* LFS */
	tip = *oip;
	tip.i_size = osize;
	for (level = TRIPLE; level >= SINGLE; level--)
		if (lastiblock[level] < 0) {
			oip->i_ib[level] = 0;
			lastiblock[level] = -1;
		}
	for (i = NDADDR - 1; i > lastblock; i--)
		oip->i_db[i] = 0;
	oip->i_flag |= ICHG|IUPD;
#ifdef NOTLFS
	vinvalbuf(ITOV(oip), (length > 0));
	allerror = ITIMES(oip, &time, &time);
#else
	/* Need lfs_vinvalbuf to get rid of invalid buffers in the cache */
	ITIMES(oip, &time, &time);
	allerror = 0;
#endif

#ifdef NOTLFS
	/*
	 * Indirect blocks first.
	 */
	ip = &tip;
	for (level = TRIPLE; level >= SINGLE; level--) {
		bn = ip->i_ib[level];
		if (bn != 0) {
			error = indirtrunc(ip, bn, lastiblock[level], level,
				&count);
			if (error)
				allerror = error;
			blocksreleased += count;
			if (lastiblock[level] < 0) {
				ip->i_ib[level] = 0;
				blkfree(ip, bn, (off_t)fs->fs_bsize);
				blocksreleased += nblocks;
			}
		}
		if (lastiblock[level] >= 0)
			goto done;
	}
#else
	/* LFS -- not yet implemented.  Need to rewrite indirect blocks */
	panic("lfs_itrunc: not yet implemented");
#endif

	/*
	 * All whole direct blocks or frags.
	 */
	for (i = NDADDR - 1; i > lastblock; i--) {
		register off_t bsize;

		bn = ip->i_db[i];
		if (bn == 0)
			continue;
		ip->i_db[i] = 0;
		bsize = (off_t)blksize(fs);			/* LFS */
#ifdef NOTLFS
		blkfree(ip, bn, bsize);
#else
		/* LFS Update segment usage information */
#endif
		blocksreleased += btodb(bsize);
	}
	if (lastblock < 0)
		goto done;

	/*
	 * Finally, look for a change in size of the
	 * last direct block; release any frags.
	 */
	bn = ip->i_db[lastblock];
	if (bn != 0) {
		off_t oldspace, newspace;

		/*
		 * Calculate amount of space we're giving
		 * back as old block size minus new block size.
		 */
		oldspace = blksize(fs);				/* LFS */
		ip->i_size = length;
		newspace = blksize(fs);				/* LFS */
		if (newspace == 0)
			panic("lfs_itrunc: newspace");
		if (oldspace - newspace > 0) {
			/*
			 * Block number of space to be free'd is
			 * the old block # plus the number of frags
			 * required for the storage we're keeping.
			 */
			bn += numfrags(fs, newspace);
			blkfree(ip, bn, oldspace - newspace);
			blocksreleased += btodb(oldspace - newspace);
		}
	}
done:
/* BEGIN PARANOIA */
	for (level = SINGLE; level <= TRIPLE; level++)
		if (ip->i_ib[level] != oip->i_ib[level])
			panic("lfs_itrunc1");
	for (i = 0; i < NDADDR; i++)
		if (ip->i_db[i] != oip->i_db[i])
			panic("lfs_itrunc2");
/* END PARANOIA */
	oip->i_blocks -= blocksreleased;
	if (oip->i_blocks < 0)			/* sanity */
		oip->i_blocks = 0;
	oip->i_flag |= ICHG;
#ifdef QUOTA
	if (!getinoquota(oip))
		(void) chkdq(oip, -blocksreleased, NOCRED, 0);
#endif
	return (allerror);
}

/*
 * Release blocks associated with the inode ip and
 * stored in the indirect block bn.  Blocks are free'd
 * in LIFO order up to (but not including) lastbn.  If
 * level is greater than SINGLE, the block is an indirect
 * block and recursive calls to indirtrunc must be used to
 * cleanse other indirect blocks.
 *
 * NB: triple indirect blocks are untested.
 */
lfs_indirtrunc(ip, bn, lastbn, level, countp)
	register struct inode *ip;
	daddr_t bn, lastbn;
	int level;
	long *countp;
{
#ifdef NOTLFS
	register int i;
	struct buf *bp;
	register struct fs *fs = ip->i_fs;
	register daddr_t *bap;
	daddr_t *copy, nb, last;
	long blkcount, factor;
	int nblocks, blocksreleased = 0;
	int error, allerror = 0;

	/*
	 * Calculate index in current block of last
	 * block to be kept.  -1 indicates the entire
	 * block so we need not calculate the index.
	 */
	factor = 1;
	for (i = SINGLE; i < level; i++)
		factor *= NINDIR(fs);
	last = lastbn;
	if (lastbn > 0)
		last /= factor;
	nblocks = btodb(fs->fs_bsize);
	/*
	 * Get buffer of block pointers, zero those 
	 * entries corresponding to blocks to be free'd,
	 * and update on disk copy first.
	 */
#ifdef SECSIZE
	bp = bread(ip->i_dev, fsbtodb(fs, bn), (int)fs->fs_bsize,
	    fs->fs_dbsize);
#else SECSIZE
	error = bread(ip->i_devvp, fsbtodb(fs, bn), (int)fs->fs_bsize,
		NOCRED, &bp);
	if (error) {
		brelse(bp);
		*countp = 0;
		return (error);
	}
	bap = bp->b_un.b_daddr;
	MALLOC(copy, daddr_t *, fs->fs_bsize, M_TEMP, M_WAITOK);
	bcopy((caddr_t)bap, (caddr_t)copy, (u_int)fs->fs_bsize);
	bzero((caddr_t)&bap[last + 1],
	  (u_int)(NINDIR(fs) - (last + 1)) * sizeof (daddr_t));
	if (last == -1)
		bp->b_flags |= B_INVAL;
	error = bwrite(bp);
	if (error)
		allerror = error;
	bap = copy;

	/*
	 * Recursively free totally unused blocks.
	 */
	for (i = NINDIR(fs) - 1; i > last; i--) {
		nb = bap[i];
		if (nb == 0)
			continue;
		if (level > SINGLE) {
			error = indirtrunc(ip, nb, (daddr_t)-1, level - 1,
				&blkcount);
			if (error)
				allerror = error;
			blocksreleased += blkcount;
		}
		blkfree(ip, nb, (off_t)fs->fs_bsize);
		blocksreleased += nblocks;
	}

	/*
	 * Recursively free last partial block.
	 */
	if (level > SINGLE && lastbn >= 0) {
		last = lastbn % factor;
		nb = bap[i];
		if (nb != 0) {
			error = indirtrunc(ip, nb, last, level - 1, &blkcount);
			if (error)
				allerror = error;
			blocksreleased += blkcount;
		}
	}
	FREE(copy, M_TEMP);
	*countp = blocksreleased;
	return (allerror);
#else
	/* LFS IMPLEMENT -- lfs_indirtrunc */
	panic("lfs_indirtrunc not implemented");
#endif
}
