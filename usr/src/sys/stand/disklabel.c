/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)disklabel.c	7.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/disklabel.h>

char *
getdisklabel(buf, lp)
	const char *buf;
	struct disklabel *lp;
{
	register struct buf *bp;
	struct disklabel *dlp, *elp;
	char *msg = (char *)0;

	elp = (struct disklabel *)(buf + DEV_BSIZE - sizeof(*dlp));
	for (dlp = (struct disklabel *)buf; dlp <= elp;
	    dlp = (struct disklabel *)((char *)dlp + sizeof(long))) {
		if (dlp->d_magic != DISKMAGIC || dlp->d_magic2 != DISKMAGIC) {
			if (msg == (char *)0)
				msg = "no disk label";
		} else if (dlp->d_npartitions > MAXPARTITIONS ||
			   dkcksum(dlp) != 0)
			msg = "disk label corrupted";
		else {
			*lp = *dlp;
			msg = (char *)0;
			break;
		}
	}
	return (msg);
}

/*
 * Compute checksum for disk label.
 */
dkcksum(lp)
	register struct disklabel *lp;
{
	register u_short *start, *end;
	register u_short sum = 0;

	start = (u_short *)lp;
	end = (u_short *)&lp->d_partitions[lp->d_npartitions];
	while (start < end)
		sum ^= *start++;
	return (sum);
}
