/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ufs_machdep.c	7.1 (Berkeley) 5/8/90
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "buf.h"
#include "conf.h"
#include "proc.h"
#include "seg.h"
#include "vm.h"

#include "pte.h"

/*
 * Machine dependent handling of the buffer cache.
 */

/*
 * Expand or contract the actual memory allocated to a buffer.
 * If no memory is available, release buffer and take error exit
 */
allocbuf(tp, size)
	register struct buf *tp;
	int size;
{
	register struct buf *bp, *ep;
	int sizealloc, take, s;

	sizealloc = roundup(size, CLBYTES);
	/*
	 * Buffer size does not change
	 */
	if (sizealloc == tp->b_bufsize)
		goto out;
	/*
	 * Buffer size is shrinking.
	 * Place excess space in a buffer header taken from the
	 * BQ_EMPTY buffer list and placed on the "most free" list.
	 * If no extra buffer headers are available, leave the
	 * extra space in the present buffer.
	 */
	if (sizealloc < tp->b_bufsize) {
		ep = bfreelist[BQ_EMPTY].av_forw;
		if (ep == &bfreelist[BQ_EMPTY])
			goto out;
		s = splbio();
		bremfree(ep);
		ep->b_flags |= B_BUSY;
		splx(s);
		pagemove(tp->b_un.b_addr + sizealloc, ep->b_un.b_addr,
		    (int)tp->b_bufsize - sizealloc);
		ep->b_bufsize = tp->b_bufsize - sizealloc;
		tp->b_bufsize = sizealloc;
		ep->b_flags |= B_INVAL;
		ep->b_bcount = 0;
		brelse(ep);
		goto out;
	}
	/*
	 * More buffer space is needed. Get it out of buffers on
	 * the "most free" list, placing the empty headers on the
	 * BQ_EMPTY buffer header list.
	 */
	while (tp->b_bufsize < sizealloc) {
		take = sizealloc - tp->b_bufsize;
		bp = getnewbuf();
		if (take >= bp->b_bufsize)
			take = bp->b_bufsize;
		pagemove(&bp->b_un.b_addr[bp->b_bufsize - take],
		    &tp->b_un.b_addr[tp->b_bufsize], take);
		tp->b_bufsize += take;
		bp->b_bufsize = bp->b_bufsize - take;
		if (bp->b_bcount > bp->b_bufsize)
			bp->b_bcount = bp->b_bufsize;
		if (bp->b_bufsize <= 0) {
			bremhash(bp);
			binshash(bp, &bfreelist[BQ_EMPTY]);
			bp->b_dev = (dev_t)NODEV;
			bp->b_error = 0;
			bp->b_flags |= B_INVAL;
		}
		brelse(bp);
	}
out:
	tp->b_bcount = size;
	return (1);
}

/*
 * Release space associated with a buffer.
 */
bfree(bp)
	struct buf *bp;
{

	bp->b_bcount = 0;
}
