/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_bio.c	5.4 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/resourcevar.h>

#include <lfs/lfs.h>
#include <lfs/lfs_extern.h>

/*
 * LFS version of bawrite, bdwrite, bwrite.  Set the delayed write flag and
 * use reassignbuf to move the buffer from the clean list to the dirty one,
 * then unlock the buffer.
 */
int
lfs_bwrite(bp)
	register BUF *bp;
{
#ifdef DO_ACCOUNTING
	Not included as this gets called from lots of places where the
	current proc structure is probably wrong.  Ignore for now.
	curproc->p_stats->p_ru.ru_oublock++;	/* XXX: no one paid yet */
#endif
	bp->b_flags &= ~(B_READ | B_DONE | B_ERROR);
	bp->b_flags |= B_WRITE | B_DELWRI;
	reassignbuf(bp, bp->b_vp);		/* XXX: do this inline? */
	brelse(bp);
	return (0);
}
