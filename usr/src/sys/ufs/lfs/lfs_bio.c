/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_bio.c	7.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/resourcevar.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

/*
 * LFS version of bawrite, bdwrite, bwrite.  Set the delayed write flag and
 * use reassignbuf to move the buffer from the clean list to the dirty one,
 * then unlock the buffer.
 *
 * XXX No accounting for the cost of the write is currently done.
 * XXX This is almost certainly wrong for synchronous operations, i.e. NFS.
 */
int
lfs_bwrite(bp)
	register BUF *bp;
{
	bp->b_flags &= ~(B_READ | B_DONE | B_ERROR);
	bp->b_flags |= B_WRITE | B_DELWRI;
	reassignbuf(bp, bp->b_vp);		/* XXX: do this inline? */
	brelse(bp);
	return (0);
}
