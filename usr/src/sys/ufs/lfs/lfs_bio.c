/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_bio.c	5.1 (Berkeley) %G%
 */

#include "param.h"
#include "proc.h"
#include "buf.h"
#include "vnode.h"
#include "specdev.h"
#include "mount.h"
#include "trace.h"
#include "resourcevar.h"
#include "lfs.h"

/*
 * lfs_bwrite --
 *	LFS version of bawrite, bdwrite, bwrite.  Set the delayed write flag
 *	and use reassignbuf to move the buffer from the clean list to the
 *	dirty one.  Then unlock the buffer.
 */
lfs_bwrite(bp)
	register BUF *bp;
{
	curproc->p_stats->p_ru.ru_oublock++;	/* XXX: no one paid yet */
	bp->b_flags &= ~(B_READ | B_DONE | B_ERROR | B_DELWRI);
	bp->b_flags |= B_DELWRI;
	reassignbuf(bp, bp->b_vp);		/* XXX: do this inline */
	brelse(bp);
}
