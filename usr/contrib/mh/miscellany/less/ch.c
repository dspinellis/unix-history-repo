/*
 * Low level character input from the input file.
 * We use these special purpose routines which optimize moving
 * both forward and backward from the current read pointer.
 */

#include "less.h"

public int file = -1;	/* File descriptor of the input file */

/*
 * Pool of buffers holding the most recently used blocks of the input file.
 */
#define BUFSIZ	1024
static struct buf {
	struct buf *next, *prev;
	long block;
	char data[BUFSIZ];
};
static struct buf *bufs = NULL;
public int nbufs;

/*
 * The buffer pool is kept as a doubly-linked circular list,
 * in order from most- to least-recently used.
 * The circular list is anchored by buf_anchor.
 */
static struct {
	struct buf *next, *prev;
} buf_anchor;
#define	END_OF_CHAIN	((struct buf *)&buf_anchor)
#define	buf_head	buf_anchor.next
#define	buf_tail	buf_anchor.prev

/*
 * If we fail to allocate enough memory for buffers, we try to limp
 * along with a minimum number of buffers.  
 */
#define	DEF_NBUFS	2	/* Minimum number of buffers */

extern int clean_data;
extern int ispipe;

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
 * Largest block number read if input is standard input (a pipe).
 */
static long last_piped_block;

/*
 * Get the character pointed to by the read pointer.
 * ch_get() is a macro which is more efficient to call
 * than fch_get (the function), in the usual case 
 * that the block desired is at the head of the chain.
 */
#define	ch_get()   ((buf_head->block == ch_block) ? \
			buf_head->data[ch_offset] : fch_get())
	static int
fch_get()
{
	register struct buf *bp;
	register int n;
	register int end;
	POSITION pos;

	/*
	 * Look for a buffer holding the desired block.
	 */
	for (bp = buf_head;  bp != END_OF_CHAIN;  bp = bp->next)
		if (bp->block == ch_block)
			goto found;
	/*
	 * Block is not in a buffer.  
	 * Take the least recently used buffer 
	 * and read the desired block into it.
	 */
	bp = buf_tail;
	bp->block = ch_block;
	pos = ch_block * BUFSIZ;
	if (ispipe)
	{
		/*
		 * The block requested should be one more than
		 * the last block read.
		 */
		if (ch_block != ++last_piped_block)
		{
			/* This "should not happen". */
			char message[80];
			sprintf(message, "Pipe error: last %ld, want %ld\n",
				last_piped_block-1, ch_block);
			error(message);
			quit();
		}
	} else
		lseek(file, pos, 0);

	/*
	 * Read the block.  This may take several reads if the input
	 * is coming from standard input, due to the nature of pipes.
	 */
	end = 0;
	while ((n = read(file, &bp->data[end], BUFSIZ-end)) > 0)
		if ((end += n) >= BUFSIZ)
			break;

	if (n < 0)
	{
		error("read error");
		quit();
	}

	/*
	 * Set an EOF marker in the buffered data itself.
	 * Then ensure the data is "clean": there are no 
	 * extra EOF chars in the data and that the "meta"
	 * bit (the 0200 bit) is reset in each char.
	 */
	if (end < BUFSIZ)
	{
		ch_fsize = pos + end;
		bp->data[end] = EOF;
	}

	if (!clean_data)
		while (--end >= 0)
		{
			bp->data[end] &= 0177;
			if (bp->data[end] == EOF)
				bp->data[end] = '@';
		}

    found:
	/* if (buf_head != bp) {this is guaranteed by the ch_get macro} */
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
	if (!ispipe || new_block == last_piped_block + 1 || buffered(new_block))
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
	if (ispipe)
	{
		/*
		 * Do it the slow way: read till end of data.
		 */
		while (ch_forw_get() != EOF)
			;
	} else
	{
		(void) ch_seek((POSITION)(lseek(file, (off_t)0, 2)));
	}
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
	return ((POSITION)(lseek(file, (off_t)0, 2)));
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
	if (c != EOF && ++ch_offset >= BUFSIZ)
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
	register int c;

	if (--ch_offset < 0)
	{
		if (ch_block <= 0 || (ispipe && !buffered(ch_block-1)))
		{
			ch_offset = 0;
			return (EOF);
		}
		ch_offset = BUFSIZ - 1;
		ch_block--;
	}
	c = ch_get();
	return (c);
}

/*
 * Initialize the buffer pool to all empty.
 * Caller suggests that we use want_nbufs buffers.
 */
	public void
ch_init(want_nbufs)
	int want_nbufs;
{
	register struct buf *bp;
	char *calloc();

	if (nbufs < want_nbufs)
	{
		/*
		 * We don't have enough buffers.  
		 * Free what we have (if any) and allocate some new ones.
		 */
		if (bufs != NULL)
			free((char *)bufs);
		bufs = (struct buf *) calloc(want_nbufs, sizeof(struct buf));
		nbufs = want_nbufs;
		if (bufs == NULL)
		{
			/*
			 * Couldn't get that many.
			 * Try for a small default number of buffers.
			 */
			char message[80];
			sprintf(message,
			  "Cannot allocate %d buffers.  Using %d buffers.", 
			  nbufs, DEF_NBUFS);
			error(message);
			bufs = (struct buf *) calloc(DEF_NBUFS, sizeof(struct buf));
			nbufs = DEF_NBUFS;
			if (bufs == NULL)
			{
				/*
				 * Couldn't even get the smaller number of bufs.
				 * Something is wrong here, don't continue.
				 */
				sprintf(message, 
				"Cannot even allocate %d buffers!  Quitting.\n",
				  DEF_NBUFS);
				error(message);
				quit();
				/*NOTREACHED*/
			}
		}
	}

	/*
	 * Initialize the buffers to empty.
	 * Set up the circular list.
	 */
	for (bp = &bufs[0];  bp < &bufs[nbufs];  bp++)
	{
		bp->next = bp + 1;
		bp->prev = bp - 1;
		bp->block = (long)(-1);
	}
	bufs[0].prev = bufs[nbufs-1].next = END_OF_CHAIN;
	buf_head = &bufs[0];
	buf_tail = &bufs[nbufs-1];
	last_piped_block = -1;
	ch_fsize = NULL_POSITION;
	(void) ch_seek((POSITION)0);
}
