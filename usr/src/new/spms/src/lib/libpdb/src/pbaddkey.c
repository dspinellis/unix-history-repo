/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbaddkey() inserts a key at the head of the buffer. The buffer is not
 * changed if key already exists. Returns integer NO if buffer exceeded,
 * otherwise YES.
 */
#include "pdbuf.h"
#include "yesno.h"

extern char *CURPBUF;			/* current database buffer */

pbaddkey(key)
	char *key;			/* key string */
{
	register char *bp;		/* buffer pointer */
	char *pbskipkey();		/* skip to next key */
	char *strncpy();		/* copy n characters */
	int klen;			/* key string length */
	int pbcmpkey();			/* compare keys */
	int pbstretch();		/* stretch buffer */
	int strlen();			/* string length */
	
	bp = CURPBUF;
	while (*bp != _PBFS && *bp != '\0')
		{
		if (pbcmpkey(key, bp) == 0)
			return(YES);	/* key already exists */
		bp = pbskipkey(bp);
		}
	if (bp == CURPBUF && *bp != _PBFS)
		{
		*bp++ = _PBFS;		/* prepare virgin buffer */
		*bp = '\0';
		}
	klen = strlen(key);
	if (pbstretch(CURPBUF, klen+1) == NO)
		return(NO);
	strncpy(CURPBUF, key, klen);	/* insert key */
	CURPBUF[klen] = _PBKS;		/* add key field separator */
	return(YES);
}
