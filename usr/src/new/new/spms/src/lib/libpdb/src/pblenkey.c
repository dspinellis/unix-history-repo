/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pblenkey() returns length of key field pointed to by buffer pointer bp.
 */
#include "pdbuf.h"

pblenkey(bp)
	register char *bp;		/* buffer pointer */
{
	register int n;			/* length counter */

	for (n = 0; *bp != _PBKS && *bp != _PBFS && *bp != '\0'; bp++, n++)
		continue;
	return(n);
}
