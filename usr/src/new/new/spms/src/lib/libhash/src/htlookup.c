/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * htlookup() returns a pointer to a hash table entry if found, otherwise null.
 */
#include "null.h"
#include "hash.h"
#include "macro.h"

HASHBLK *
htlookup(key, hash)
	char *key;			/* key for hash table entry */
	HASH *hash;			/* hash table */
{
	HASHBLK *htb;			/* hash table entry block */
	int hthash();			/* calculate hash value */

	for (htb = (hash->hashtab)[hthash(key, hash)]; htb != NULL; htb = htb->h_next)
		if (EQUAL(htb->h_key, key))
			return(htb);	/* found */
	return(NULL);			/* not found */
}
