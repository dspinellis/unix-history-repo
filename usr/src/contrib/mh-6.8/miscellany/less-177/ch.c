/*
 * Low level character input from the input file.
 * We use these special purpose routines which optimize moving
 * both forward and backward from the current read pointer.
 */

#include "less.h"

public int file = -1;		/* File descriptor of the input file */
public int ignore_eoi;

/*
 * Pool of buffers holding the most recently used blocks of the input file.
 */
#define BUFSIZ	1024
struct buf {
	struct buf *next, *prev;  /* Must be first to match struct filestate */
	long block;
	unsigned int datasize;
	unsigned char data[BUFSIZ];
};

/*
 * The buffer pool is kept as a doubly-linked circular list,
 * in order from most- to least-recently used.
 * The circular list is anchored by the file state "thisfile".
 *
 * The file state is maintained in a filestate structure.
 * There are two such structures, one used when input is a pipe
 * and the other when input is an ordinary file.
 * This is so that we can leave a pipe, look and other files,
 * and return to the pipe without losing buffered data.
 * Buffered data can be reconstructed for a non-pipe file by
 * simply re-reading the file, but a pipe cannot be re-read.
 */

struct filestate {
	struct buf *next, *prev;   /* Must be first to match struct buf */
	POSITION fpos;
	int nbufs;
	long block;
	int offset;
	POSITION fsize;
};

#define	END_OF_CHAIN	((struct buf *)thisfile)
#define	buf_head	thisfile->next
#define	buf_tail	thisfile->prev
#define	ch_nbufs	thisfile->nbufs
#define	ch_block	thisfile->block
#define	ch_offset	thisfile->offset
#define	ch_fpos		thisfile->fpos
#define	ch_fsize	thisfile->fsize

static struct filestate pipefile =
	{ (struct buf *)&pipefile, (struct buf *)&pipefile };

static struct filestate nonpipefile = 
	{ (struct buf *)&nonpipefile, (struct buf *)&nonpipefile };

static struct filestate *thisfile;

extern int ispipe;
extern int autobuf;
extern int sigs;
#if LOGFILE
extern int logfile;
extern char *namelogfile;
#endif

static int ch_addbuf();


/*
 * Get the character pointed to by the read pointer.
 * ch_get() is a macro which is more efficient to call
 * than fch_get (the function), in the usual case 
 * that the block desired is at the head of the chain.
 */
#define	ch_get()   ((ch_block == buf_head->block && \
		     ch_offset < buf_head->datasize) ? \
			buf_head->data[ch_offset] : fch_get())
	static int
fch_get()
{
	register struct buf *bp;
	register int n;
	register int slept;
	POSITION pos;
	POSITION len;

	slept = 0;

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
		if (ch_addbuf(1))
			/*
			 * Allocation failed: turn off autobuf.
			 */
			autobuf = 0;
	bp = buf_tail;
	bp->block = ch_block;
	bp->datasize = 0;

    read_more:
	pos = (ch_block * BUFSIZ) + bp->datasize;
	if ((len = ch_length()) != NULL_POSITION && pos >= len)
		/*
		 * At end of file.
		 */
		return (EOI);

	if (pos != ch_fpos)
	{
		/*
		 * Not at the correct position: must seek.
		 * If input is a pipe, we're in trouble (can't seek on a pipe).
		 * Some data has been lost: just return "?".
		 */
		if (ispipe)
			return ('?');
		if (lseek(file, (offset_t)pos, 0) == BAD_LSEEK)
		{
 			error("seek error", NULL_PARG);
 			quit(1);
 		}
 		ch_fpos = pos;
 	}

	/*
	 * Read the block.
	 * If we read less than a full block, that's ok.
	 * We use partial block and pick up the rest next time.
	 */
	n = iread(file, &bp->data[bp->datasize], 
		(unsigned int)(BUFSIZ - bp->datasize));
	if (n == READ_INTR)
		return (EOI);
	if (n < 0)
	{
		error("read error", NULL_PARG);
		quit(1);
	}
	ch_fpos += n;

#if LOGFILE
	/*
	 * If we have a log file, write the new data to it.
	 */
	if (logfile >= 0 && n > 0)
		write(logfile, (char *) &bp->data[bp->datasize], n);
#endif

	bp->datasize += n;

	/*
	 * If we have read to end of file, set ch_fsize to indicate
	 * the position of the end of file.
	 */
	if (n == 0)
	{
		ch_fsize = pos;
		if (ignore_eoi)
		{
			/*
			 * We are ignoring EOF.
			 * Wait a while, then try again.
			 */
			if (!slept)
				ierror("Waiting for data", NULL_PARG);
			sleep(1);
			slept = 1;
		}
		if (sigs)
			return (EOI);
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

#if LOGFILE
/*
 * Close the logfile.
 * If we haven't read all of standard input into it, do that now.
 */
	public void
end_logfile()
{
	static int tried = 0;

	if (logfile < 0)
		return;
	if (!tried && ch_fsize == NULL_POSITION)
	{
		tried = 1;
		ierror("Finishing logfile", NULL_PARG);
		while (ch_forw_get() != EOI)
			if (sigs)
				break;
	}
	close(logfile);
	logfile = -1;
	namelogfile = NULL;
}

/*
 * Start a log file AFTER less has already been running.
 * Invoked from the - command; see toggle_option().
 * Write all the existing buffered data to the log file.
 */
	public void
sync_logfile()
{
	register struct buf *bp;
	long block;
	long last_block;

	last_block = (ch_fpos + BUFSIZ - 1) / BUFSIZ;
	for (block = 0;  block <= last_block;  block++)
		for (bp = buf_head;  bp != END_OF_CHAIN;  bp = bp->next)
			if (bp->block == block)
			{
				write(logfile, (char *) bp->data, bp->datasize);
				break;
			}
}

#endif

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
	POSITION len;

	len = ch_length();
	if (pos < ch_zero() || (len != NULL_POSITION && pos > len))
		return (1);

	new_block = pos / BUFSIZ;
	if (ispipe && pos != ch_fpos && !buffered(new_block))
		return (1);
	/*
	 * Set read pointer.
	 */
	ch_block = new_block;
	ch_offset = pos % BUFSIZ;
	return (0);
}

/*
 * Seek to the end of the file.
 */
	public int
ch_end_seek()
{
	POSITION len;

	if (!ispipe)
		ch_fsize = filesize(file);

	len = ch_length();
	if (len != NULL_POSITION)
		return (ch_seek(len));

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
	if (ch_seek(ch_zero()) == 0)
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
	if (ignore_eoi)
		return (NULL_POSITION);
	return (ch_fsize);
}

/*
 * Return the current position in the file.
 */
#define	tellpos(blk,off)   ((POSITION)((((long)(blk)) * BUFSIZ) + (off)))

	public POSITION
ch_tell()
{
	return (tellpos(ch_block, ch_offset));
}

/*
 * Get the current char and post-increment the read pointer.
 */
	public int
ch_forw_get()
{
	register int c;

	c = ch_get();
	if (c == EOI)
		return (EOI);
	if (ch_offset < BUFSIZ-1)
		ch_offset++;
	else
	{
#if __ZOFFSET /* NOT WORKING */
		if (ch_fsize != NULL_POSITION && 
		    tellpos(ch_block+1, 0) >= ch_fsize)
			return (EOI);
#endif
		ch_block ++;
		ch_offset = 0;
	}
	return (c);
}

/*
 * Pre-decrement the read pointer and get the new current char.
 */
	public int
ch_back_get()
{
	if (ch_offset > 0)
		ch_offset --;
	else
	{
#if __ZOFFSET /* NOT WORKING */
		if (tellpos(ch_block-1, BUFSIZ-1) < ch_zero())
			return (EOI);
#else
		if (ch_block <= 0)
			return (EOI);
#endif
		if (ispipe && !buffered(ch_block-1))
			return (EOI);
		ch_block--;
		ch_offset = BUFSIZ-1;
	}
	return (ch_get());
}

/*
 * Allocate buffers.
 * Caller wants us to have a total of at least want_nbufs buffers.
 */
	public int
ch_nbuf(want_nbufs)
	int want_nbufs;
{
	PARG parg;

	if (ch_nbufs < want_nbufs && ch_addbuf(want_nbufs - ch_nbufs))
	{
		/*
		 * Cannot allocate enough buffers.
		 * If we don't have ANY, then quit.
		 * Otherwise, just report the error and return.
		 */
		parg.p_int = want_nbufs - ch_nbufs;
		error("Cannot allocate %d buffers", &parg);
		if (ch_nbufs == 0)
			quit(1);
	}
	return (ch_nbufs);
}

/*
 * Flush any saved file state, including buffer contents.
 */
	public void
ch_flush()
{
	register struct buf *bp;

	if (ispipe)
	{
		/*
		 * If input is a pipe, we don't flush buffer contents,
		 * since the contents can't be recovered.
		 */
		ch_fsize = NULL_POSITION;
		return;
	}

	/*
	 * Initialize all the buffers.
	 */
	for (bp = buf_head;  bp != END_OF_CHAIN;  bp = bp->next)
		bp->block = (long)(-1);

	/*
	 * Figure out the size of the file, if we can.
	 */
	ch_fsize = filesize(file);

	/*
	 * Seek to a known position: the beginning of the file.
	 */
	ch_fpos = 0;
	ch_block = ch_fpos / BUFSIZ;
	ch_offset = ch_fpos % BUFSIZ;

	if (lseek(file, (offset_t)0, 0) == BAD_LSEEK)
	{
		/*
		 * Warning only; even if the seek fails for some reason,
		 * there's a good chance we're at the beginning anyway.
		 * {{ I think this is bogus reasoning. }}
		 */
		error("seek error to 0", NULL_PARG);
	}
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
	ch_nbufs += nnew;
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

/*
 * Use the pipe file state.
 */
	public void
ch_pipe()
{
	thisfile = &pipefile;
}

/*
 * Use the non-pipe file state.
 */
	public void
ch_nonpipe()
{
	thisfile = &nonpipefile;
}
