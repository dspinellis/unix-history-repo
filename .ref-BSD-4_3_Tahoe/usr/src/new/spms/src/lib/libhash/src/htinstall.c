/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * htinstall() installs a new entry in a hash table if it doesn't already
 * exist. If it does, the old definition and value is superseded. Returns
 * a pointer to the entry, or null if out of memory.
 */
#include "null.h"
#include "hash.h"

HASHBLK *
htinstall(key, def, val, hash)
	char *key;			/* key for hash table entry */
	char *def;			/* definition string */
	int val;			/* integer value */
	HASH *hash;			/* hash table */
{
	char *malloc();			/* memory allocator */
	char *strsav();			/* save string somewhere */
	HASHBLK *htb;			/* hash table entry block */
	HASHBLK *htlookup();		/* find hash table entry */
	int hashval;			/* hash value for key */
	int hthash();			/* calculate hash value */

	if ((htb = htlookup(key, hash)) == NULL)
		{			/* not found */
		if ((htb = (HASHBLK *) malloc(sizeof(HASHBLK))) == NULL)
			return(NULL);
		if ((htb->h_key = strsav(key)) == NULL)
			return(NULL);
		hashval = hthash(key, hash);
		htb->h_next = (hash->hashtab)[hashval];
		(hash->hashtab)[hashval] = htb;
		}
	else	{			/* found */
		free(htb->h_def);	/* free previous definition */
		}
	if ((htb->h_def = strsav(def)) == NULL)
		return(NULL);
	htb->h_val = val;
	return(htb);
}
