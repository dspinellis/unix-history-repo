/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbrmstring() removes string identified by id from buffer.
 */
#include "pdbuf.h"

extern char *CURPBUF;			/* current database buffer */

void
pbrmstring(id)
	register char *id;		/* string identifier */
{
	register char *bp;		/* buffer pointer */
	char *pbskipfield();		/* skip to next non-key field */
	int flen;			/* field length */
	int pblenfield();		/* length of nonm-key field */
	void pbshrink();		/* shrink buffer */
	
	bp = CURPBUF;
	while (*(bp = pbskipfield(bp)) != '\0')
		if (bp[0] == id[0] && bp[1] == id[1] && bp[2] == '=')
			{
			flen = pblenfield(bp);
			if (bp[flen] == _PBFS)
				flen++;
			pbshrink(bp, flen);
			break;
			}
}
