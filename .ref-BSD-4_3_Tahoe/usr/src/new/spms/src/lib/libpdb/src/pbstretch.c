/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbstretch() stretches the buffer by n characters just before the point
 * marked by buffer pointer bp. Negative n shrinks the buffer by n
 * characters. Returns integer NO if buffer space exceeded, otherwise YES.
 */
#include <stdio.h>
#include "path.h"
#include "pdb.h"
#include "yesno.h"

extern char *CURPBUF;			/* current database buffer */
extern PDB *CURPDB;			/* current database stream */

pbstretch(bp, n)
	register char *bp;		/* buffer pointer */
	int n;				/* stretch amount */
{
	register char *lowerbp;		/* lower roving buffer pointer */
	register char *upperbp;		/* upper roving buffer pointer */
	char *sprintf();		/* print output to string */
	
	if (n > 0)
		{
		for (lowerbp = bp; *lowerbp != '\0'; lowerbp++)
			continue;
		upperbp = lowerbp + n;
		if (upperbp >= CURPBUF + PBUFSIZE)
			{
			sprintf(CURPDB->perr, "%s database buffer exceeded",
				CURPDB->path);
			CURPDB->flag |= _PERR;
			return(NO);
			}
		while (lowerbp >= bp)
			*upperbp-- = *lowerbp--;
		}
	else if (n < 0)
		{
		for (upperbp = bp; *upperbp != '\0'; upperbp++)
			continue;
		lowerbp = bp - n;
		if (lowerbp >= upperbp)
			*bp = '\0';
		while (lowerbp <= upperbp)
			*bp++ = *lowerbp++;
		}
	return(YES);
}
