/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbrmflag() removes flag from buffer.
 */
#include "pdbuf.h"

extern char *CURPBUF;			/* current database buffer */

void
pbrmflag(flag)
	register char *flag;		/* flag string */
{
	register char *bp;		/* buffer pointer */
	char *pbskipfield();		/* skip to next non-key field */
	void pbshrink();		/* shrink buffer */

	bp = CURPBUF;
	while (*(bp = pbskipfield(bp)) != '\0')
		if (bp[0] == flag[0] && bp[1] == flag[1] &&
			(bp[2] == ':' || bp[2] == '\0'))
			{
			if (bp[2] == _PBFS)
				pbshrink(bp, 3);
			else
				pbshrink(bp, 2);
			break;
			}
}
