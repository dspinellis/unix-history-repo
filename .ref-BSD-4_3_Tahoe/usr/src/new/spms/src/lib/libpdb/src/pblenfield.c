/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pblenfield() returns length of field pointed to by buffer pointer bp.
 */
#include "pdbuf.h"

pblenfield(bp)
	register char *bp;		/* buffer pointer */
{
	register int n;			/* length counter */

	for (n = 0; *bp != _PBFS && *bp != '\0'; bp++, n++)
		continue;
	return(n);
}
