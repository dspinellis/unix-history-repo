/*
 * Copyright (c) 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_balloc.c	7.22 (Berkeley) %G%
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
 * numbers.  Indirect blocks are addressed by the negative address of the
 * first data block to which they point.  Double indirect blocks are addressed
 * by one less than the address of the first indirect block to which they
 * point.  Triple indirect blocks are addressed by one less than the address
 * of the first double indirect block to which they point.
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
	daddr_t *bap, daddr, metalbn;
	long realbn;
	int error, j, off, sh;

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
	realbn = bn;
	if ((long)bn < 0)
		bn = -(long)bn;

	/* The first NDADDR blocks are direct blocks. */
	if (bn < NDADDR) {
		nb = ip->i_db[bn];
		if (nb == 0) {
			*bnp = UNASSIGNED;
			return (0);
		}
		*bnp = nb;
		return (0);
	}

	/* 
	 * Determine the number of levels of indirection.  After this loop
	 * is done, sh indicates the number of data blocks possible at the
	 * given level of indirection, and NIADDR - j is the number of levels
	 * of indirection needed to locate the requested block.
	 */
	bn -= NDADDR;
	fs = ip->i_lfs;
	sh = 1;
	for (j = NIADDR; j > 0; j--) {
		sh *= NINDIR(fs);
		if (bn < sh)
			break;
		bn -= sh;
	}
	if (j == 0)
		return (EFBIG);

	/* Calculate the address of the first meta-block. */
	if (realbn >= 0)
		metalbn = -(realbn - bn + NIADDR - j);
	else
		metalbn = -(-realbn - bn + NIADDR - j);

	/* 
	 * Fetch through the indirect blocks.  At each iteration, off is the
	 * offset into the bap array which is an array of disk addresses at
	 * the current level of indirection.
	 */
	bp = NULL;
	devvp = VFSTOUFS(vp->v_mount)->um_devvp;
	for (off = NIADDR - j, bap = ip->i_ib; j <= NIADDR; j++) {
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

		/* If searching for a meta-data block, quit when found. */
		if (metalbn == realbn)
			break;

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
		bp = getblk(vp, metalbn, fs->lfs_bsize);
		if (bp->b_flags & (B_DONE | B_DELWRI)) {
			trace(TR_BREADHIT, pack(vp, size), metalbn);
		} else {
			trace(TR_BREADMISS, pack(vp, size), metalbn);
			bp->b_blkno = daddr;
			bp->b_flags |= B_READ;
			bp->b_dev = devvp->v_rdev;
			(devvp->v_op->vop_strategy)(bp);
			curproc->p_stats->p_ru.ru_inblock++;	/* XXX */
			if (error = biowait(bp)) {
				brelse(bp);
				return (error);
			}
		}

		bap = bp->b_un.b_daddr;
		sh /= NINDIR(fs);
		off = (bn / sh) % NINDIR(fs);
		metalbn -= -1 + off * sh;
	}
	if (bp)
		brelse(bp);

	*bnp = daddr;
	return (0);
}
