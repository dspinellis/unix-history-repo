/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbskipfield() advances the buffer pointer bp to the next field.
 */
#include "pdbuf.h"

char *
pbskipfield(bp)
	register char *bp;		/* buffer pointer */
{
	while (*bp != _PBFS && *bp != '\0')
		bp++;
	if (*bp == _PBFS)
		bp++;
	return(bp);
}
