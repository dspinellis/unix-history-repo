/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbcmpfield() compares non-key fields and returns an integer greater than,
 * equal to, or less than 0, depending on whether field is lexicographically
 * greater than, equal to, or less than the non-key field pointed to by bp.
 */
#include "pdbuf.h"

pbcmpfield(field, bp)
	register char *field;		/* field string */
	register char *bp;		/* buffer pointer */
{
	for (; *field == *bp && *field != '\0'; field++, bp++)
		continue;
	if (*field == '\0' && (*bp == _PBFS || *bp == '\0'))
		return(0);
	return(*field - *bp);
}
