/*
 * Copyright (c) 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_balloc.c	7.40 (Berkeley) %G%
 */
#include <sys/param.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/resourcevar.h>
#include <sys/trace.h>

#include <miscfs/specfs/specdev.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufsmount.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

int
lfs_balloc(vp, iosize, lbn, bpp)
	struct vnode *vp;
	u_long iosize;
	daddr_t lbn;
	struct buf **bpp;
{
	struct buf *ibp, *bp;
	struct inode *ip;
	struct lfs *fs;
	struct indir indirs[NIADDR+2];
	daddr_t daddr;
	int bb, error, i, num;

	ip = VTOI(vp);
	fs = ip->i_lfs;

	/* 
	 * Three cases: it's a block beyond the end of file, it's a block in
	 * the file that may or may not have been assigned a disk address or
	 * we're writing an entire block.  Note, if the daddr is unassigned,
	 * the block might still have existed in the cache (if it was read
	 * or written earlier).  If it did, make sure we don't count it as a
	 * new block or zero out its contents.  If it did not, make sure
	 * we allocate any necessary indirect blocks.
	 */

	*bpp = NULL;
	if (error = ufs_bmaparray(vp, lbn, &daddr, &indirs[0], &num, NULL ))
		return (error);

	*bpp = bp = getblk(vp, lbn, fs->lfs_bsize);
	bb = VFSTOUFS(vp->v_mount)->um_seqinc;
	if (daddr == UNASSIGNED)
		/* May need to allocate indirect blocks */
		for (i = 1; i < num; ++i)
			if (!indirs[i].in_exists) {
				ibp =
				    getblk(vp, indirs[i].in_lbn, fs->lfs_bsize);
				if (!(ibp->b_flags & (B_DONE | B_DELWRI))) {
					if (!ISSPACE(fs, bb, curproc->p_ucred)){
						ibp->b_flags |= B_INVAL;
						brelse(ibp);
						error = ENOSPC;
					} else {
						ip->i_blocks += bb;
						ip->i_lfs->lfs_bfree -= bb;
						clrbuf(ibp);
						error = VOP_BWRITE(ibp);
					}
				} else
					panic ("Indirect block should not exist");
			}
	if (error) {
		if (bp)
			brelse(bp);
		return(error);
	}


	/* Now, we may need to allocate the data block */
	if (!(bp->b_flags & (B_CACHE | B_DONE | B_DELWRI))) {
		if (daddr == UNASSIGNED) 
			if (!ISSPACE(fs, bb, curproc->p_ucred)) {
				bp->b_flags |= B_INVAL;
				brelse(bp);
				return(ENOSPC);
			} else {
				ip->i_blocks += bb;
				ip->i_lfs->lfs_bfree -= bb;
				if (iosize != fs->lfs_bsize)
					clrbuf(bp);
			}
		else if (iosize == fs->lfs_bsize)
			bp->b_blkno = daddr;		/* Skip the I/O */
		else  {
			bp->b_blkno = daddr;
			bp->b_flags != B_READ;
			VOP_STRATEGY(bp);
			return(biowait(bp));
		}
	}
	return (error);
}
