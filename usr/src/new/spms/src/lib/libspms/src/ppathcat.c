/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * ppathcat() concatenates project pathname components pp1 and pp2 into
 * character buffer pp1_pp2. Returns pp1_pp2.
 */
#include "path.h"

char *
ppathcat(pp1_pp2, pp1, pp2)
	register char *pp1;
	register char *pp2;
	register char *pp1_pp2;
{
	register int pplen;		/* maximum pathname length */
	char *spp1_pp2;			/* start of pp1_pp2 */

	spp1_pp2 = pp1_pp2;
	for (pplen = PPATHSIZE; pplen > 0; pplen--, pp1_pp2++, pp1++)
		if ((*pp1_pp2 = *pp1) == '\0')
			break;
	if (*pp2 != '\0' && pplen > 0)
		{
		if (pp1_pp2 != spp1_pp2 && pp1_pp2[-1] != _PPSC)
			{
			*pp1_pp2++ = _PPSC;
			pplen--;
			}
		for (; pplen > 0; pplen--, pp1_pp2++, pp2++)
			if ((*pp1_pp2 = *pp2) == '\0')
				break;
		}
	if (pplen == 0)
		{
		*--pp1_pp2 = '\0';
		warn("project pathname too long");
		}
	return(spp1_pp2);
}
