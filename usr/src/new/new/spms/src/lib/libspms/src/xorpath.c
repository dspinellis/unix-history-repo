/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * xorpath() strips pathname p1 from pathname p2 if p1 is a leading
 * component of pathname p2. Returns p2.
 */
#include "path.h"

char *
xorpath(p1, p2)
	register char *p1;		/* subpathname */
	register char *p2;		/* pathname */
{
	char *sp2;			/* start of pathname */

	sp2 = p2;
	for (; *p1 == *p2; p1++, p2++)
		if (*p1 == '\0')
			break;
	if (*p1 == '\0')
		return((*p2 == _PSC) ? ++p2 : p2);
	else
		return(sp2);
}
