/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mark Nudleman.
 * 
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)ch.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * Low level character input from the input file.
 * We use these special purpose routines which optimize moving
 * both forward and backward from the current read pointer.
 */

#include "less.h"

public int file = -1;		/* File descriptor of the input file */

/*
 * Pool of buffers holding the most recently used blocks of the input file.
 */
#define BUFSIZ	1024
struct buf {
	struct buf *next, *prev;
	long block;
	int datasize;
	char data[BUFSIZ];
};
public int nbufs;

/*
 * The buffer pool is kept as a doubly-linked circular list,
 * in order from most- to least-recently used.
 * The circular list is anchored by buf_anchor.
 */
#define	END_OF_CHAIN	((struct buf *)&buf_anchor)
#define	buf_head	buf_anchor.next
#define	buf_tail	buf_anchor.prev

static struct {
	struct buf *next, *prev;
} buf_anchor = { END_OF_CHAIN, END_OF_CHAIN };

extern int clean_data;
extern int ispipe;
extern int autobuf;
extern int cbufs;
extern int sigs;

/*
 * Current position in file.
 * Stored as a block number and an offset into the block.
 */
static long ch_block;
static int ch_offset;

/* 
 * Length of file, needed if input is a pipe.
 */
static POSITION ch_fsize;

/*
 * Number of bytes read, if input is standard input (a pipe).
 */
static POSITION last_piped_pos;

/*
 * Get the character pointed to by the read pointer.
 * ch_get() is a macro which is more efficient to call
 * than fch_get (the function), in the usual case 
 * that the block desired is at the head of the chain.
 */
#define	ch_get()   ((buf_head->block == ch_block && \
		     ch_offset < buf_head->datasize) ? \
			buf_head->data[ch_offset] : fch_get())
	static int
fch_get()
{
	register struct buf *bp;
	register int n;
	register char *p;
	POSITION pos;

	/*
	 * Look for a buffer holding the desired block.
	 */
	for (bp = buf_head;  bp != END_OF_CHAIN;  bp = bp->next)
		if (bp->block == ch_block)
		{
			if (ch_offset >= bp->datasize)
				/*
				 * Need more data in this buffer.
				 */
				goto read_more;
			/*
			 * On a pipe, we don't sort the buffers LRU
			 * because this can cause gaps in the buffers.
			 * For example, suppose we've got 12 1K buffers,
			 * and a 15K input stream.  If we read the first 12K
			 * sequentially, then jump to line 1, then jump to
			 * the end, the buffers have blocks 0,4,5,6,..,14.
			 * If we then jump to line 1 again and try to
			 * read sequentially, we're out of luck when we
			 * get to block 1 (we'd get the "pipe error" below).
			 * To avoid this, we only sort buffers on a pipe
			 * when we actually READ the data, not when we
			 * find it already buffered.
			 */
			if (ispipe)
				return (bp->data[ch_offset]);
			goto found;
		}
	/*
	 * Block is not in a buffer.  
	 * Take the least recently used buffer 
	 * and read the desired block into it.
	 * If the LRU buffer has data in it, 
	 * and autobuf is true, and input is a pipe, 
	 * then try to allocate a new buffer first.
	 */
	if (autobuf && ispipe && buf_tail->block != (long)(-1))
		(void) ch_addbuf(1);
	bp = buf_tail;
	bp->block = ch_block;
	bp->datasize = 0;

    read_more:
	pos = (ch_block * BUFSIZ) + bp->datasize;
	if (ispipe)
	{
		/*
		 * The data requested should be immediately after
		 * the last data read from the pipe.
		 */
		if (pos != last_piped_pos)
		{
			error("pipe error");
			quit();
		}
	} else
		lseek(file, pos, 0);

	/*
	 * Read the block.
	 * If we read less than a full block, we just return the
	 * partial block and pick up the rest next time.
	 */
	n = iread(file, &bp->data[bp->datasize], BUFSIZ - bp->datasize);
	if (n == READ_INTR)
		return (EOI);
	if (n < 0)
	{
		error("read error");
		quit();
	}
	if (ispipe)
		last_piped_pos += n;

	bp->datasize += n;

	/*
	 * Set an EOI marker in the buffered data itself.
	 * Then ensure the data is "clean": there are no 
	 * extra EOI chars in the data and that the "meta"
	 * bit (the 0200 bit) is reset in each char.
	 */
	if (n == 0)
	{
		ch_fsize = pos;
		bp->data[bp->datasize++] = EOI;
	}

	if (!clean_data)
	{
		p = &bp->data[bp->datasize];
		while (--n >= 0)
		{
			*--p &= 0177;
			if (*p == EOI)
				*p = '@';
		}
	}

    found:
	if (buf_head != bp)
	{
		/*
		 * Move the buffer to the head of the buffer chain.
		 * This orders the buffer chain, most- to least-recently used.
		 */
		bp->next->prev = bp->prev;
		bp->prev->next = bp->next;

		bp->next = buf_head;
		bp->prev = END_OF_CHAIN;
		buf_head->prev = bp;
		buf_head = bp;
	}

	if (ch_offset >= bp->datasize)
		/*
		 * After all that, we still don't have enough data.
		 * Go back and try again.
		 */
		goto read_more;

	return (bp->data[ch_offset]);
}

/*
 * Determine if a specific block is currently in one of the buffers.
 */
	static int
buffered(block)
	long block;
{
	register struct buf *bp;

	for (bp = buf_head;  bp != END_OF_CHAIN;  bp = bp->next)
		if (bp->block == block)
			return (1);
	return (0);
}

/*
 * Seek to a specified position in the file.
 * Return 0 if successful, non-zero if can't seek there.
 */
	public int
ch_seek(pos)
	register POSITION pos;
{
	long new_block;

	new_block = pos / BUFSIZ;
	if (!ispipe || pos == last_piped_pos || buffered(new_block))
	{
		/*
		 * Set read pointer.
		 */
		ch_block = new_block;
		ch_offset = pos % BUFSIZ;
		return (0);
	}
	return (1);
}

/*
 * Seek to the end of the file.
 */
	public int
ch_end_seek()
{
	if (!ispipe)
		return (ch_seek(ch_length()));

	/*
	 * Do it the slow way: read till end of data.
	 */
	while (ch_forw_get() != EOI)
		if (sigs)
			return (1);
	return (0);
}

/*
 * Seek to the beginning of the file, or as close to it as we can get.
 * We may not be able to seek there if input is a pipe and the
 * beginning of the pipe is no longer buffered.
 */
	public int
ch_beg_seek()
{
	register struct buf *bp, *firstbp;

	/*
	 * Try a plain ch_seek first.
	 */
	if (ch_seek((POSITION)0) == 0)
		return (0);

	/*
	 * Can't get to position 0.
	 * Look thru the buffers for the one closest to position 0.
	 */
	firstbp = bp = buf_head;
	if (bp == END_OF_CHAIN)
		return (1);
	while ((bp = bp->next) != END_OF_CHAIN)
		if (bp->block < firstbp->block)
			firstbp = bp;
	ch_block = firstbp->block;
	ch_offset = 0;
	return (0);
}

/*
 * Return the length of the file, if known.
 */
	public POSITION
ch_length()
{
	if (ispipe)
		return (ch_fsize);
	return ((POSITION)(lseek(file, (off_t)0, L_XTND)));
}

/*
 * Return the current position in the file.
 */
	public POSITION
ch_tell()
{
	return (ch_block * BUFSIZ + ch_offset);
}

/*
 * Get the current char and post-increment the read pointer.
 */
	public int
ch_forw_get()
{
	register int c;

	c = ch_get();
	if (c != EOI && ++ch_offset >= BUFSIZ)
	{
		ch_offset = 0;
		ch_block ++;
	}
	return (c);
}

/*
 * Pre-decrement the read pointer and get the new current char.
 */
	public int
ch_back_get()
{
	if (--ch_offset < 0)
	{
		if (ch_block <= 0 || (ispipe && !buffered(ch_block-1)))
		{
			ch_offset = 0;
			return (EOI);
		}
		ch_offset = BUFSIZ - 1;
		ch_block--;
	}
	return (ch_get());
}

/*
 * Allocate buffers.
 * Caller wants us to have a total of at least want_nbufs buffers.
 * keep==1 means keep the data in the current buffers;
 * otherwise discard the old data.
 */
	public void
ch_init(want_nbufs, keep)
	int want_nbufs;
	int keep;
{
	register struct buf *bp;
	char message[80];

	cbufs = nbufs;
	if (nbufs < want_nbufs && ch_addbuf(want_nbufs - nbufs))
	{
		/*
		 * Cannot allocate enough buffers.
		 * If we don't have ANY, then quit.
		 * Otherwise, just report the error and return.
		 */
		sprintf(message, "cannot allocate %d buffers",
			want_nbufs - nbufs);
		error(message);
		if (nbufs == 0)
			quit();
		return;
	}

	if (keep)
		return;

	/*
	 * We don't want to keep the old data,
	 * so initialize all the buffers now.
	 */
	for (bp = buf_head;  bp != END_OF_CHAIN;  bp = bp->next)
		bp->block = (long)(-1);
	last_piped_pos = (POSITION)0;
	ch_fsize = NULL_POSITION;
	(void) ch_seek((POSITION)0);
}

/*
 * Allocate some new buffers.
 * The buffers are added to the tail of the buffer chain.
 */
	static int
ch_addbuf(nnew)
	int nnew;
{
	register struct buf *bp;
	register struct buf *newbufs;

	/*
	 * We don't have enough buffers.  
	 * Allocate some new ones.
	 */
	newbufs = (struct buf *) calloc(nnew, sizeof(struct buf));
	if (newbufs == NULL)
		return (1);

	/*
	 * Initialize the new buffers and link them together.
	 * Link them all onto the tail of the buffer list.
	 */
	nbufs += nnew;
	cbufs = nbufs;
	for (bp = &newbufs[0];  bp < &newbufs[nnew];  bp++)
	{
		bp->next = bp + 1;
		bp->prev = bp - 1;
		bp->block = (long)(-1);
	}
	newbufs[nnew-1].next = END_OF_CHAIN;
	newbufs[0].prev = buf_tail;
	buf_tail->next = &newbufs[0];
	buf_tail = &newbufs[nnew-1];
	return (0);
}
