/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbcmpkey() compares keys and returns an integer greater than, equal to,
 * or less than 0, depending on whether key is lexicographically greater
 * than, equal to, or less than the key field pointed to by bp.
 */
#include "pdbuf.h"

pbcmpkey(key, bp)
	register char *key;		/* key string */
	register char *bp;		/* buffer pointer */
{
	for (; *key == *bp && *key != '\0'; key++, bp++)
		continue;
	if (*key == '\0' && (*bp == _PBKS || *bp == _PBFS || *bp == '\0'))
		return(0);
	return(*key - *bp);
}
