/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * hthash() returns a hash value for string, s.
 */
#include "hash.h"

hthash(s, hash)
	register char *s;		/* string */
	HASH *hash;			/* hash table */
{
	register int hashval;		/* hash value for string */

	for (hashval = 0; *s != '\0'; s++)
		hashval += *s;
	return(hashval % hash->hashsiz);
}
