/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pathcat() concatenates path components p1 and p2 into character buffer
 * p1_p2. Returns p1_p2.
 */
#include <stdio.h>
#include "path.h"

char *
pathcat(p1_p2, p1, p2)
	register char *p1;
	register char *p2;
	register char *p1_p2;
{
	register int plen;		/* maximum pathname length */
	char *sp1_p2;			/* start of p1_p2 */

	sp1_p2 = p1_p2;
	for (plen = PATHSIZE; plen > 0; plen--, p1_p2++, p1++)
		if ((*p1_p2 = *p1) == '\0')
			break;
	if (*p2 != '\0' && plen > 0)
		{
		if (p1_p2 != sp1_p2 && p1_p2[-1] != _PSC)
			{
			*p1_p2++ = _PSC;
			plen--;
			}
		for (; plen > 0; plen--, p1_p2++, p2++)
			if ((*p1_p2 = *p2) == '\0')
				break;
		}
	if (plen == 0)
		{
		*--p1_p2 = '\0';
		warn("pathname too long");
		}
	return(sp1_p2);
}
