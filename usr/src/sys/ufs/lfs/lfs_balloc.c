/*
 * Copyright (c) 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_balloc.c	7.21 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/resourcevar.h>
#include <sys/specdev.h>
#include <sys/trace.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufsmount.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

/*
 * Bmap converts a the logical block number of a file to its physical block
 * number on the disk. The conversion is done by using the logical block
 * number to index into the array of block pointers described by the dinode.
 *
 * LFS has a different version of bmap from FFS because of a naming conflict.
 * In FFS, meta blocks are given real disk addresses at allocation time, and
 * are linked into the device vnode, using a logical block number which is
 * the same as the physical block number.  This can't be done by LFS because
 * blocks aren't given disk addresses until they're written, so there's no
 * way to distinguish the meta-data blocks for one file from any other file.
 * This means that meta-data blocks have to be on the vnode for the file so
 * they can be found, and have to have "names" different from the standard
 * data blocks.  To do this, we divide the name space into positive and
 * negative block numbers, and give the meta-data blocks negative logical
 * numbers.
 *
 * The mapping for meta-data blocks is as follows (assuming a 4K block size):
 *
 * -1 -- single indirect
 * -2 -- double indirect:
 *		single indirect blocks -4, -1027
 * -3 -- triple indirect:
 *		double indirect blocks -1028, -2051
 *		single indirect blocks -2052, -(1M + 2052 - 1)
 */
int
lfs_bmap(vp, bn, vpp, bnp)
	struct vnode *vp;
	register daddr_t bn;
	struct vnode **vpp;
	daddr_t *bnp;
{
	register struct inode *ip;
	register struct lfs *fs;
	register daddr_t nb;
	struct buf *bp;
	struct vnode *devvp;
	daddr_t *bap, daddr, lbn_ind, doing_a_triple;
	int error, j, off, sh, sh_ind;

	/*
	 * Check for underlying vnode requests and ensure that logical
	 * to physical mapping is requested.
	 */
	ip = VTOI(vp);
	if (vpp != NULL)
		*vpp = ip->i_devvp;
	if (bnp == NULL)
		return (0);

#ifdef VERBOSE
printf("lfs_bmap: block number %d, inode %d\n", bn, ip->i_number);
#endif
	/* The first NDADDR blocks are direct blocks. */
	if (bn < NDADDR) {
		nb = ip->i_db[bn];
		if (nb == LFS_UNUSED_DADDR)
			*bnp = UNASSIGNED;
		else
			*bnp = nb;
		return (0);
	}

	/* 
	 * The first NIADDR negative blocks are the indirect block pointers.
	 * Determine the number of levels of indirection.  After this loop
	 * is done, sh indicates the number of data blocks possible at the
	 * given level of indirection, lbn_ind is the logical block number
	 * of the next indirect block to retrieve, and NIADDR - j is the
	 * number of levels of indirection needed to locate the requested
	 * block.
	 */
	fs = ip->i_lfs;
	sh = 1;
	bn -= NDADDR;
	lbn_ind = 0;
	for (j = NIADDR; j > 0; j--) {
		--lbn_ind;
		sh *= NINDIR(fs);
		if (bn < sh)
			break;
		bn -= sh;
	}
	if (j == 0)
		return (EFBIG);

	/* 
	 * Fetch through the indirect blocks.  At each iteration, off is the
	 * offset into the bap array which is an array of disk addresses at
	 * the current level of indirection.
	 */
	bap = ip->i_ib;
	bp = NULL;
	devvp = VFSTOUFS(vp->v_mount)->um_devvp;
	off = NIADDR - j;
	doing_a_triple = 0;
	for (; j <= NIADDR; j++) {
		/*
		 * In LFS, it's possible to have a block appended to a file
		 * for which the meta-blocks have not yet been allocated.
		 * This is a win if the file never gets written or if the
		 * file's growing.
		 */
		if ((daddr = bap[off]) == 0) {
			daddr = UNASSIGNED;
			break;
		}
		/*
		 * Read in the appropriate indirect block.  LFS can't do a
		 * bread because bread knows that FFS will hand it the device
		 * vnode, not the file vnode, so the b_dev and b_blkno would
		 * be wrong.
		 *
		 * XXX
		 * This REALLY needs to be fixed, at the very least it needs
		 * to be rethought when the buffer cache goes away.
		 */
		if (bp)
			brelse(bp);
		bp = getblk(vp, lbn_ind, fs->lfs_bsize);
		if (bp->b_flags & (B_DONE | B_DELWRI)) {
			trace(TR_BREADHIT, pack(vp, size), lbn_ind);
		} else {
			bp->b_flags |= B_READ;
			bp->b_blkno = daddr;
			bp->b_dev = devvp->v_rdev;
			(devvp->v_op->vop_strategy)(bp);
			trace(TR_BREADMISS, pack(vp, size), lbn_ind);
			curproc->p_stats->p_ru.ru_inblock++;	/* XXX */
			if (error = biowait(bp)) {
				brelse(bp);
				return (error);
			}
		}
		bap = bp->b_un.b_daddr;
		sh /= NINDIR(fs);
		off = (bn / sh) % NINDIR(fs);

		/*
		 * Ahem.  Now the disgusting part.  We have to figure out
		 * the logical block number for the next meta-data block.
		 * There are really three equations...  Note the clever
		 * use of the doing_a_triple variable to hold the last
		 * offset into the block of pointers.
		 */
		switch(j) {
		case 1:
			/* The triple indirect block found in the inode. */
			doing_a_triple = off;
			lbn_ind = -(NIADDR + 1 + off + NINDIR(fs));
			break;
		case 2:
			/*
			 * The double indirect block found after indirecting
			 * through a triple indirect block.
			 */
			if (doing_a_triple)
				lbn_ind = -((doing_a_triple + 2) * NINDIR(fs) +
				    NIADDR + 1);

			/* The double indirect block found in the inode. */
			else
				lbn_ind = -(NIADDR + 1 + off);
			break;
		case 3:
			/*
			 * A single indirect block; lbn_ind isn't used again,
			 * so don't do anything.
			 */
			break;
		}
	}
	if (bp)
		brelse(bp);

	*bnp = daddr;
	return (0);
}
