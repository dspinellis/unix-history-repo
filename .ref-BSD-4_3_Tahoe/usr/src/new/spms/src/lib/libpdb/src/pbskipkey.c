/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbskipkey() advances the buffer pointer bp to the next key field.
 */
#include "pdbuf.h"

char *
pbskipkey(bp)
	register char *bp;		/* buffer pointer */
{
	while (*bp != _PBKS && *bp != _PBFS && *bp != '\0')
		bp++;
	if (*bp == _PBKS)
		bp++;
	return(bp);
}
