/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ffs_balloc.c	7.24 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/file.h>
#include <sys/vnode.h>

#include <vm/vm.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufs_extern.h>

#include <ufs/ffs/fs.h>
#include <ufs/ffs/ffs_extern.h>

/*
 * Balloc defines the structure of file system storage
 * by allocating the physical blocks on a device given
 * the inode and the logical block number in a file.
 */
ffs_balloc(ip, bn, size, cred, bpp, flags)
	register struct inode *ip;
	register daddr_t bn;
	int size;
	struct ucred *cred;
	struct buf **bpp;
	int flags;
{
	register struct fs *fs;
	register daddr_t nb;
	struct buf *bp, *nbp;
	struct vnode *vp = ITOV(ip);
	struct indir indirs[NIADDR + 2];
	int osize, nsize, num, j, error;
	daddr_t newb, lbn, *bap, pref;

	*bpp = (struct buf *)0;
	if (bn < 0)
		return (EFBIG);
	fs = ip->i_fs;
	lbn = bn;

	/*
	 * If the next write will extend the file into a new block,
	 * and the file is currently composed of a fragment
	 * this fragment has to be extended to be a full block.
	 */
	nb = lblkno(fs, ip->i_size);
	if (nb < NDADDR && nb < bn) {
		osize = blksize(fs, ip, nb);
		if (osize < fs->fs_bsize && osize > 0) {
			error = ffs_realloccg(ip, nb,
				ffs_blkpref(ip, nb, (int)nb, &ip->i_db[0]),
				osize, (int)fs->fs_bsize, cred, &bp);
			if (error)
				return (error);
			ip->i_size = (nb + 1) * fs->fs_bsize;
			vnode_pager_setsize(vp, (u_long)ip->i_size);
			ip->i_db[nb] = dbtofsb(fs, bp->b_blkno);
			ip->i_flag |= IUPD|ICHG;
			if (flags & B_SYNC)
				bwrite(bp);
			else
				bawrite(bp);
		}
	}
	/*
	 * The first NDADDR blocks are direct blocks
	 */
	if (bn < NDADDR) {
		nb = ip->i_db[bn];
		if (nb != 0 && ip->i_size >= (bn + 1) * fs->fs_bsize) {
			error = bread(vp, bn, fs->fs_bsize, NOCRED, &bp);
			if (error) {
				brelse(bp);
				return (error);
			}
			*bpp = bp;
			return (0);
		}
		if (nb != 0) {
			/*
			 * Consider need to reallocate a fragment.
			 */
			osize = fragroundup(fs, blkoff(fs, ip->i_size));
			nsize = fragroundup(fs, size);
			if (nsize <= osize) {
				error = bread(vp, bn, osize, NOCRED, &bp);
				if (error) {
					brelse(bp);
					return (error);
				}
			} else {
				error = ffs_realloccg(ip, bn,
				    ffs_blkpref(ip, bn, (int)bn, &ip->i_db[0]),
				    osize, nsize, cred, &bp);
				if (error)
					return (error);
			}
		} else {
			if (ip->i_size < (bn + 1) * fs->fs_bsize)
				nsize = fragroundup(fs, size);
			else
				nsize = fs->fs_bsize;
			error = ffs_alloc(ip, bn,
			    ffs_blkpref(ip, bn, (int)bn, &ip->i_db[0]),
			    nsize, cred, &newb);
			if (error)
				return (error);
			bp = getblk(vp, bn, nsize);
			bp->b_blkno = fsbtodb(fs, newb);
			if (flags & B_CLRBUF)
				clrbuf(bp);
		}
		ip->i_db[bn] = dbtofsb(fs, bp->b_blkno);
		ip->i_flag |= IUPD|ICHG;
		*bpp = bp;
		return (0);
	}
	/*
	 * Determine the number of levels of indirection.
	 */
	pref = 0;
	if (error = ufs_getlbns(vp, bn, indirs, &num))
		return(error);
#ifdef DIAGNOSTIC
	if (num < 1)
		panic ("ffs_balloc: ufs_bmaparray returned indirect block\n");
#endif
	/*
	 * Fetch the first indirect block allocating if necessary.
	 */
	--num;
	nb = ip->i_ib[indirs[0].in_off];
	if (nb == 0) {
		pref = ffs_blkpref(ip, lbn, 0, (daddr_t *)0);
	        if (error = ffs_alloc(ip, lbn, pref, (int)fs->fs_bsize,
		    cred, &newb))
			return (error);
		nb = newb;
		bp = getblk(vp, indirs[1].in_lbn, fs->fs_bsize);
		bp->b_blkno = fsbtodb(fs, newb);
		clrbuf(bp);
		/*
		 * Write synchronously so that indirect blocks
		 * never point at garbage.
		 */
		if (error = bwrite(bp)) {
			ffs_blkfree(ip, nb, fs->fs_bsize);
			return (error);
		}
		ip->i_ib[indirs[0].in_off] = newb;
		ip->i_flag |= IUPD|ICHG;
	}
	/*
	 * Fetch through the indirect blocks, allocating as necessary.
	 */
	for (j = 1; ; ) {
		error = bread(vp, indirs[j].in_lbn, (int)fs->fs_bsize, NOCRED,
		    &bp);
		if (error) {
			brelse(bp);
			return (error);
		}
		bap = bp->b_un.b_daddr;
		nb = bap[indirs[j].in_off];
		if (j == num)
			break;
		j += 1;
		if (nb != 0) {
			brelse(bp);
			continue;
		}
		if (pref == 0)
			pref = ffs_blkpref(ip, lbn, 0, (daddr_t *)0);
		if (error =
		    ffs_alloc(ip, lbn, pref, (int)fs->fs_bsize, cred, &newb)) {
			brelse(bp);
			return (error);
		}
		nb = newb;
		nbp = getblk(vp, indirs[j].in_lbn, fs->fs_bsize);
		nbp->b_blkno = fsbtodb(fs, nb);
		clrbuf(nbp);
		/*
		 * Write synchronously so that indirect blocks
		 * never point at garbage.
		 */
		if (error = bwrite(nbp)) {
			ffs_blkfree(ip, nb, fs->fs_bsize);
			brelse(bp);
			return (error);
		}
		bap[indirs[j - 1].in_off] = nb;
		/*
		 * If required, write synchronously, otherwise use
		 * delayed write.
		 */
		if (flags & B_SYNC) {
			bwrite(bp);
		} else {
			bdwrite(bp);
		}
	}
	/*
	 * Get the data block, allocating if necessary.
	 */
	if (nb == 0) {
		pref = ffs_blkpref(ip, lbn, indirs[j].in_off, &bap[0]);
		if (error =
		    ffs_alloc(ip, lbn, pref, (int)fs->fs_bsize, cred, &newb)) {
			brelse(bp);
			return (error);
		}
		nb = newb;
		nbp = getblk(vp, lbn, fs->fs_bsize);
		nbp->b_blkno = fsbtodb(fs, nb);
		if (flags & B_CLRBUF)
			clrbuf(nbp);
		bap[indirs[j].in_off] = nb;
		/*
		 * If required, write synchronously, otherwise use
		 * delayed write.
		 */
		if (flags & B_SYNC) {
			bwrite(bp);
		} else {
			bdwrite(bp);
		}
		*bpp = nbp;
		return (0);
	}
	brelse(bp);
	if (flags & B_CLRBUF) {
		error = bread(vp, lbn, (int)fs->fs_bsize, NOCRED, &nbp);
		if (error) {
			brelse(nbp);
			return (error);
		}
	} else {
		nbp = getblk(vp, lbn, fs->fs_bsize);
		nbp->b_blkno = fsbtodb(fs, nb);
	}
	*bpp = nbp;
	return (0);
}
