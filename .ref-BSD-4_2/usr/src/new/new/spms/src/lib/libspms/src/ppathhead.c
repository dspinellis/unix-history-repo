/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * ppathhead() removes tail of project pathname and returns pathname.
 * The tail is defined as that part of the pathname after the last separator.
 */
#include "path.h"

char *
ppathhead(ppathname)
	register char *ppathname;
{
	register char *ls;		/* last separator character */
	register char *p;		/* project pathname pointer */

	for (p = ppathname; *p != '\0'; p++)
		if (*p == _PPSC)
			ls = p;
	*ls = '\0';
	return(ppathname);
}
