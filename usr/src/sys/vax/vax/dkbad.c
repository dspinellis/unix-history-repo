/*	dkbad.c	4.1	81/05/08	*/

#ifdef DKBAD
#include "../h/param.h"
#include "../h/buf.h"
#include "../h/dkbad.h"

/*
 * Search the bad block table looking for
 * the specified block.  Return index if found.
 * Return -1 if not found.
 */

isbad(bt, cyl, trk, sec)
	register struct dkbad *bt;
{
	register int i;
	register long blk, bblk;

	if (bt->bt_magic != BADMAGIC)
		return(-1);
	blk = ((long)cyl << 16) + (trk << 8) + sec;
	for (i = 0; i < 126; i++) {
		bblk = ((long)bt->bt_bad[i].bt_cyl << 16) + bt->bt_bad[i].bt_trksec;
		if (blk == bblk)
			return(i);
		if (blk < bblk || bblk < 0)
			break;
	}
	return(-1);
}
#endif
