/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_subr.c	7.9 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/namei.h>
#include <sys/vnode.h>
#include <sys/buf.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

/*
 * Return buffer with the contents of block "offset" from the beginning of
 * directory "ip".  If "res" is non-zero, fill it in with a pointer to the
 * remaining space in the directory.
 */
int
lfs_blkatoff(vp, offset, res, bpp)
	struct vnode *vp;
	off_t offset;
	char **res;
	struct buf **bpp;
{
	register struct lfs *fs;
	struct inode *ip;
	struct buf *bp;
	daddr_t lbn;
	int bsize, error;

	ip = VTOI(vp);
	fs = ip->i_lfs;
	lbn = lblkno(fs, offset);
	bsize = blksize(fs);

	*bpp = NULL;
	if (error = bread(vp, lbn, bsize, NOCRED, &bp)) {
		brelse(bp);
		return (error);
	}
	if (res)
		*res = bp->b_un.b_addr + blkoff(fs, offset);
	*bpp = bp;
	return (0);
}
