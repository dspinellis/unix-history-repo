/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_balloc.c	7.17 (Berkeley) %G%
 */

#ifdef LOGFS
#include "param.h"
#include "systm.h"
#include "buf.h"
#include "time.h"
#include "resource.h"
#include "resourcevar.h"
#include "proc.h"
#include "file.h"
#include "vnode.h"
#include "mount.h"
#include "specdev.h"

#include "../ufs/quota.h"
#include "../ufs/inode.h"
#include "../ufs/ufsmount.h"
#include "trace.h"
#include "lfs.h"
#include "lfs_extern.h"

/*
 * Bmap converts a the logical block number of a file
 * to its physical block number on the disk. The conversion
 * is done by using the logical block number to index into
 * the array of block pointers described by the dinode.
 */
int
lfs_bmap(ip, bn, bnp)
	register struct inode *ip;
	register daddr_t bn;
	daddr_t	*bnp;
{
	register LFS *fs;					/* LFS */
	register daddr_t nb;
	struct vnode *devvp, *vp;
	struct buf *bp;
	daddr_t *bap, daddr;
	daddr_t lbn_ind;
	int j, off, sh;
	int error;

printf("lfs_bmap: block number %d, inode %d\n", bn, ip->i_number);
	fs = ip->i_lfs;						/* LFS */

	/*
	 * We access all blocks in the cache, even indirect blocks by means of
	 * a logical address. Indirect blocks (single, double, triple) all have
	 * negative block numbers. The first NDADDR blocks are direct blocks,
	 * the first NIADDR negative blocks are the indirect block pointers.
	 * The single, double and triple indirect blocks in the inode
	 * are addressed: -1, -2 and -3 respectively.  
	 * XXX we don't handle triple indirect at all.
	 */
	if (bn < 0) {
		/* Shouldn't be here -- we don't think */
		printf("lfs_bmap: NEGATIVE indirect block number %d\n", bn);
		panic("negative indirect block number");
	}

	/*
	 * The first NDADDR blocks are direct blocks
	 */
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
	 * Determine the number of levels of indirection.
	 */
	sh = 1;
	bn -= NDADDR;
	lbn_ind = 0;
	for (j = NIADDR; j > 0; j--) {
		lbn_ind--;
		sh *= NINDIR(fs);
		if (bn < sh)
			break;
		bn -= sh;
	}
	if (j == 0)
		return (EFBIG);
	/*
	 * Fetch through the indirect blocks.
	 */

	vp = ITOV(ip);
	devvp = VFSTOUFS(vp->v_mount)->um_devvp;
	for (off = NIADDR - j, bap = ip->i_ib; j <= NIADDR; j++) {
		if((daddr = bap[off]) == 0) {
			daddr = UNASSIGNED;
			break;
		}
		if (bp)
			brelse(bp);
		bp = getblk(vp, lbn_ind, fs->lfs_bsize);
		if (bp->b_flags & (B_DONE | B_DELWRI)) {
			trace(TR_BREADHIT, pack(vp, size), lbn_ind);
		} else {
			trace(TR_BREADMISS, pack(vp, size), lbn_ind);
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
		lbn_ind  = -(NIADDR + 1 + off);
	}
	if (bp)
		brelse(bp);

	*bnp = daddr;
	return (0);
}
#endif /* LOGFS */
