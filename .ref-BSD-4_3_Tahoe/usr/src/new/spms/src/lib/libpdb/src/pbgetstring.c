/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbgetstring() loads the string identified by id into string. Returns
 * string or null character if id not found.
 */
#include "pdbuf.h"

extern char *CURPBUF;			/* current database buffer */

char *
pbgetstring(id, string)
	register char *id;		/* string identifier */
	char *string;			/* string argument */
{
	register char *bp;		/* buffer pointer */
	char *pbcpyfield();		/* copy non-key field */

#ifdef OPTIMIZE
	for (bp = CURPBUF; *bp != '\0'; bp++)
		if (*bp == _PBFS)
			if (bp[1] == id[0] && bp[2] == id[1] && bp[3] == '=')
				{
				bp += 4;
				break;
				}
#else
	char *pbskipfield();		/* skip to next non-key field */

	bp = CURPBUF;
	while (*(bp = pbskipfield(bp)) != '\0')
		if (bp[0] == id[0] && bp[1] == id[1] && bp[2] == '=')
			{
			bp += 3;
			break;
			}
#endif
	return(pbcpyfield(string, bp));
}
