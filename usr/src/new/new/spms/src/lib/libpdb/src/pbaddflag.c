/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbaddflag() adds a flag to the tail of the buffer. The buffer is not
 * changed if flag already exists. Returns integer NO if buffer space
 * exceeded, otherwise YES.
 */
#include "pdbuf.h"
#include "yesno.h"

extern char *CURPBUF;			/* current database buffer */

pbaddflag(flag)
	register char *flag;		/* flag string */
{
	register char *bp;		/* buffer pointer */
	char *pbskipfield();		/* skip to next non-key field */
	char *strcpy();			/* string copy */
	int pbstretch();		/* stretch buffer */

	bp = CURPBUF;
	while (*(bp = pbskipfield(bp)) != '\0')
		if (bp[0] == flag[0] && bp[1] == flag[1] &&
			(bp[2] == _PBFS || bp[2] == '\0'))
			return(YES);	/* flag already exists */
	if (bp == CURPBUF)
		{
		*bp++ = _PBFS;		/* prepare virgin buffer */
		*bp = '\0';
		}
	if (pbstretch(bp, 3) == NO)
		return(NO);
	bp[0] = flag[0];		/* add flag */
	bp[1] = flag[1];
	bp[2] = _PBFS;			/* add field separator */
	return(YES);
}
