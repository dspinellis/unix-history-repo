/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbrmkey() removes key from buffer.
 */
#include "pdbuf.h"

extern char *CURPBUF;			/* current database buffer */

void
pbrmkey(key)
	char *key;			/* key string */
{
	register char *bp;		/* buffer pointer */
	char *pbskipkey();		/* skip key field */
	int klen;			/* key string length */
	int pbcmpkey();			/* compare keys */
	int strlen();			/* string length */
	void pbshrink();		/* shrink buffer */
	
	bp = CURPBUF;
	klen = strlen(key);
	while (*bp != _PBFS && *bp != '\0')
		{
		if (pbcmpkey(key, bp) == 0)
			{
			if (bp[klen] == _PBKS)
				klen++;
			pbshrink(bp, klen);
			break;
			}
		bp = pbskipkey(bp);
		}
}
