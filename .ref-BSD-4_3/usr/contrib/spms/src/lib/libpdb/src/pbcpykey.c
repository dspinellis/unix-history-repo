/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbcpykey() copies the key field pointed to by bp to key. Returns
 * null-terminated key.
 */
#include "pdbuf.h"

char *
pbcpykey(key, bp)
	register char *key;		/* key string */
	register char *bp;		/* buffer pointer */
{
	char *keysave;

	keysave = key;
	while (*bp != _PBKS && *bp != _PBFS && *bp != '\0')
		*key++ = *bp++;
	*key = '\0';
	return(keysave);
}
