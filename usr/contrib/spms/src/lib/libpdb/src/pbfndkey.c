/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbfndkey() searchs for key in buffer and returns a pointer to its
 * location, otherwise null.
 */
#include "null.h"
#include "pdbuf.h"

extern char *CURPBUF;			/* current database buffer */

char *
pbfndkey(key)
	char *key;			/* key string */
{
	register char *bp;		/* buffer pointer */
	char *pbskipkey();		/* skip to next key */
	int pbcmpkey();			/* compare keys */
	
	bp = CURPBUF;
	while (*bp != _PBFS && *bp != '\0')
		{
		if (pbcmpkey(key, bp) == 0)
			return(bp);
		bp = pbskipkey(bp);
		}
	return(NULL);
}
