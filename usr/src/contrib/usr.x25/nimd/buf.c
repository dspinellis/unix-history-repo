/*
 * Miscellanious buffer management routines
 *
 * Frank Pronk
 * The University of British Columbia
 * Laboratory for Computational Vision
 * Copyright (c)
 */

#include "buf.h"

char	*malloc();

/*
 * return a buffer that contains room
 * for 'size' bytes of data.
 */

struct buf *
getbuf (size)
{
	register struct buf *bp;

	if ((bp = (struct buf *)malloc (size + sizeof(char *)*4)) == 0)
		abort ();
	bp->b_next = bp->b_prev = bp;
	bp->b_top = bp->b_bot = bp->b_data;
	return (bp);
}

/*
 * Initialize a buffer queue by
 * freeing all buffers on the queue
 * and resetting the queue to its
 * initial state.
 */

InitQueue (qp)
register struct bufhd *qp;
{
	register struct buf *bp;

	if (qp->b_next)
		while (!QEMPTY (qp)) {
			dequeue (bp = qp->b_next, qp);
			free ((char *)bp);
		}
	qp->b_prev = qp->b_next = (struct buf *)qp;
	qp->b_count = 0;
}

/*
 * Allocate a buffer and try to read 'size' bytes from 'fd'.
 * Return zero on error; otherwise return a pointer to the buffer.
 */

struct buf *
FillBuf(fd, size, who)
char *who;
{
	register int n;
	register struct buf *bp;

	bp = getbuf (size);
	if ((n = read (fd, bp->b_bot, size)) <= 0) {
		free ((char *)bp);
		return (0);
	}
	bp->b_top += n;
	NimTrace (who, bp->b_bot, n);
	return (bp);
}

/*
 * Try to flush the queue of buffers headed by 'qp'
 * by writing to 'fd'.  Returns non-zero on error.
 */

FlushQueue (fd, qp, who)
register struct bufhd *qp;
char *who;
{
	register int cc;
	register struct buf *bp;

	while (!QEMPTY(qp)) {
		bp = qp->b_next;
		if ((cc = write (fd, bp->b_bot, SIZE (bp))) < 0)
			return (-1);
		NimTrace (who, bp->b_bot, cc);
		bp->b_bot += cc;
		qp->b_count -= cc;
		if (ISEMPTY(bp)) {
			dequeue (bp, qp);
			free ((char *)bp);
		}
	}
	return (0);
}

/*
 * Copy string 's' to buffer 'bp'.
 */

StrToBuf(s, bp)
register char *s;
register struct buf *bp;
{
	while(*s)
		PUTCHAR (*s++, bp);
}

/*
 * Copy the contents of buffer 'from' to 'to'.
 */

BufCopy(from, to)
struct buf *from, *to;
{
	register int l = SIZE (from);

	bcopy (from->b_bot, to->b_top, l);
	to->b_top += l;
}

/*
 * place buffer 'bp' on the end of
 * the queue headed by 'qp'.
 */

enqueue (bp, qp)
register struct buf *bp;
register struct bufhd *qp;
{
	qp->b_prev->b_next = bp;
	bp->b_prev = qp->b_prev;
	qp->b_prev = bp;
	bp->b_next = (struct buf *)qp;
	qp->b_count += SIZE (bp);
}

/*
 * remove buffer 'bp' from the
 * queue headed by 'qp'.
 */

dequeue (bp, qp)
register struct buf *bp;
register struct bufhd *qp;
{
	bp->b_prev->b_next = bp->b_next;
	bp->b_next->b_prev = bp->b_prev;
	qp->b_count -= SIZE (bp);
}
