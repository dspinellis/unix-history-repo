/* recycle.c */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


/* This file contains the functions perform garbage collection and allocate
 * reusable blocks.
 */

#include "config.h"
#include "vi.h"

#ifndef NO_RECYCLE
/* this whole file would have be skipped if NO_RECYCLE is defined */

extern long	lseek();

#define BTST(bitno, byte)	((byte) & (1 << (bitno)))
#define BSET(bitno, byte)	((byte) |= (1 << (bitno)))
#define BCLR(bitno, byte)	((byte) &= ~(1 << (bitno)))

#define TST(blkno)		((blkno) < MAXBIT ? BTST((blkno) & 7, bitmap[(blkno) >> 3]) : 1)
#define SET(blkno)		if ((blkno) < MAXBIT) BSET((blkno) & 7, bitmap[(blkno) >> 3])
#define CLR(blkno)		if ((blkno) < MAXBIT) BCLR((blkno) & 7, bitmap[(blkno) >> 3])

/* bitmap of free blocks in first 4096k of tmp file */
static unsigned char bitmap[512];
#define MAXBIT	(sizeof bitmap << 3)

/* this function locates all free blocks in the current tmp file */
void garbage()
{
	int	i;
	BLK	oldhdr;

	/* start by assuming every block is free */
	for (i = 0; i < sizeof bitmap; i++)
	{
		bitmap[i] = 255;
	}

	/* header block isn't free */
#ifndef lint
	CLR(0);
#endif

	/* blocks needed for current hdr aren't free */
	for (i = 1; i < MAXBLKS; i++)
	{
		CLR(hdr.n[i]);
	}

	/* blocks needed for undo version aren't free */
	lseek(tmpfd, 0L, 0);
	if (read(tmpfd, &oldhdr, (unsigned)sizeof oldhdr) != sizeof oldhdr)
	{
		msg("garbage() failed to read oldhdr??");
		for (i = 0; i < sizeof bitmap; i++)
		{
			bitmap[i] = 0;
		}
		return;
	}
	for (i = 1; i < MAXBLKS; i++)
	{
		CLR(oldhdr.n[i]);
	}

	/* blocks needed for cut buffers aren't free */
	for (i = cutneeds(&oldhdr) - 1; i >= 0; i--)
	{
		CLR(oldhdr.n[i]);
	}
}

/* This function allocates the first available block in the tmp file */
long allocate()
{
	int	i;
	long	offset;

	/* search for the first byte with a free bit set */
	for (i = 0; i < sizeof bitmap && bitmap[i] == 0; i++)
	{
	}

	/* if we hit the end of the bitmap, return the end of the file */
	if (i == sizeof bitmap)
	{
		offset = lseek(tmpfd, 0L, 2);
	}
	else /* compute the offset for the free block */
	{
		for (i <<= 3; TST(i) == 0; i++)
		{
		}
		offset = (long)i * (long)BLKSIZE;

		/* mark the block as "allocated" */
		CLR(i);
	}

	return offset;
}

#endif
