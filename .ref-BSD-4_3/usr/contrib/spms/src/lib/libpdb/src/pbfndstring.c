/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbfndstring() searchs for string identified by id in buffer and returns
 * a pointer to its location, otherwise null.
 */
#include "null.h"
#include "pdbuf.h"

extern char *CURPBUF;			/* current database buffer */

char *
pbfndstring(id)
	register char *id;		/* string identifier */
{
	register char *bp;		/* buffer pointer */
	char *pbskipfield();		/* skip to next non-key field */
	
	bp = CURPBUF;
	while (*(bp = pbskipfield(bp)) != '\0')
		if (bp[0] == id[0] && bp[1] == id[1] && bp[2] == '=')
			return(bp+3);
	return(NULL);
}
