/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_bio.c	7.2 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/resourcevar.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

int
lfs_bwrite(bp)
	register BUF *bp;
{
#ifdef VERBOSE
printf("lfs_bwrite\n");
#endif
	/*
	 *
	 * LFS version of bawrite, bdwrite, bwrite.  Set the delayed write
	 * flag and use reassignbuf to move the buffer from the clean list
	 * to the dirty one, then unlock the buffer.  Note, we set the
	 * B_LOCKED flag, which causes brelse to move the buffer onto the
	 * LOCKED free list.  This is necessary, otherwise getnewbuf() would
	 * try to reclaim them using bawrite, which isn't going to work.
	 *
	 * XXX
	 * No accounting for the cost of the write is currently done.
	 * This is almost certainly wrong for synchronous operations, i.e. NFS.
	 */
	bp->b_flags &= ~(B_READ | B_DONE | B_ERROR);
	bp->b_flags |= B_DELWRI | B_LOCKED;
	reassignbuf(bp, bp->b_vp);
	brelse(bp);
	return (0);
}
