/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbfndflag() searchs for flag in buffer and returns integer YES if found,
 * otherwise NO.
 */
#include "pdbuf.h"
#include "yesno.h"

extern char *CURPBUF;			/* current database buffer */

pbfndflag(flag)
	register char *flag;		/* flag string */
{
	register char *bp;		/* buffer pointer */
	char *pbskipfield();		/* skip to next non-key field */

	bp = CURPBUF;
	while (*(bp = pbskipfield(bp)) != '\0')
		if (bp[0] == flag[0] && bp[1] == flag[1] &&
			(bp[2] == _PBFS || bp[2] == '\0'))
			return(YES);
	return(NO);
}
