/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbaddstring() adds an id=string to the tail of the buffer. If id already
 * exists the corresponding string is replaced. Returns integer NO if
 * buffer space exceeded, otherwise YES.
 */
#include "pdbuf.h"
#include "yesno.h"

extern char *CURPBUF;			/* current database buffer */

pbaddstring(id, string)
	register char *id;		/* string identifier */
	char *string;			/* string argument */
{
	register char *bp;		/* buffer pointer */
	char *pbskipfield();		/* skip to next non-key field */
	char *strcpy();			/* string copy */
	char *strncpy();		/* copy n characters */
	int chgflen;			/* incremental field length */
	int flen;			/* field length */
	int pbstretch();		/* stretch buffer */
	int slen;			/* string length */
	int strlen();			/* string length */
	
	bp = CURPBUF;
	slen = strlen(string);
	chgflen = slen + 4;		/* add id= and field separator */
	while (*(bp = pbskipfield(bp)) != '\0')
		if (bp[0] == id[0] && bp[1] == id[1] && bp[2] == '=')
			{
			flen = pblenfield(bp);
			chgflen -= (bp[flen]==_PBFS) ? flen+1 : flen;
			break;
			}
	if (bp == CURPBUF)
		{
		*bp++ = _PBFS;		/* prepare virgin buffer */
		*bp = '\0';
		}
	if (pbstretch(bp, chgflen) == NO)
		return(NO);
	bp[0] = id[0];			/* add id= */
	bp[1] = id[1];
	bp[2] = '=';
	strncpy(bp+3, string, slen);	/* insert string */
	bp[slen+3] = _PBFS;		/* add field separator */
	return(YES);
}
