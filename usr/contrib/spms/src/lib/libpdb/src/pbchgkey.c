/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbchgkey() substitutes newkey for oldkey. Returns integer NO if buffer
 * space exceeded, otherwise YES.
 */
#include "null.h"
#include "pdbuf.h"
#include "yesno.h"

pbchgkey(oldkey, newkey)
	char *oldkey;			/* old key string */
	char *newkey;			/* new key string */
{
	register char *bp;		/* buffer pointer */
	char *pbfndkey();		/* find key */
	char *strncpy();		/* copy n characters */
	int chgklen;			/* incremental key string length */
	int newklen;			/* new key string length */
	int pbstretch();		/* stretch buffer */
	int strlen();			/* string length */
	
	if ((bp = pbfndkey(newkey)) != NULL)
		pbrmkey(oldkey);
	else if ((bp = pbfndkey(oldkey)) != NULL)
		{
		newklen = strlen(newkey);
		chgklen = newklen - strlen(oldkey);
		if (pbstretch(bp, chgklen) == NO)
			return(NO);
		strncpy(bp, newkey, newklen);
		}
	return(YES);
}
