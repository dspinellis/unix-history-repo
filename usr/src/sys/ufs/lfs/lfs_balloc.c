/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_balloc.c	7.14 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "proc.h"
#include "file.h"
#include "vnode.h"

#include "../ufs/quota.h"
#include "../ufs/inode.h"
#include "lfs.h"
#include "lfs_extern.h"

/*
 * Bmap converts a the logical block number of a file
 * to its physical block number on the disk. The conversion
 * is done by using the logical block number to index into
 * the array of block pointers described by the dinode.
 */
lfs_bmap(ip, bn, bnp)
	register struct inode *ip;
	register daddr_t bn;
	daddr_t	*bnp;
{
	register LFS *fs;					/* LFS */
	register daddr_t nb;
	struct buf *bp;
	daddr_t *bap;
	int i, j, sh;
	int error;

printf("lfs_bmap: block number %d, inode %d\n", bn, ip->i_number);
	if (bn < 0)
		return (EFBIG);
	fs = ip->i_lfs;						/* LFS */

	/*
	 * The first NDADDR blocks are direct blocks
	 */
	if (bn < NDADDR) {
		nb = ip->i_db[bn];
		if (nb == 0) {
			*bnp = (daddr_t)-1;
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
	for (j = NIADDR; j > 0; j--) {
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
	nb = ip->i_ib[NIADDR - j];
	if (nb == 0) {
		*bnp = (daddr_t)-1;
		return (0);
	}
	for (; j <= NIADDR; j++) {
		if (error = bread(ip->i_devvp, nb, (int)fs->lfs_bsize,
		    NOCRED, &bp)) {		/* LFS */
			brelse(bp);
			return (error);
		}
		bap = bp->b_un.b_daddr;
		sh /= NINDIR(fs);
		i = (bn / sh) % NINDIR(fs);
		nb = bap[i];
		if (nb == 0) {
			*bnp = (daddr_t)-1;
			brelse(bp);
			return (0);
		}
		brelse(bp);
	}
	*bnp = nb;
	return (0);
}
