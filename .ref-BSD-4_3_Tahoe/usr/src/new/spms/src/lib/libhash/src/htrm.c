/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

#include "null.h"
#include "hash.h"
#include "macro.h"

/*
 * htrm() removes a hash table entry. If key is null, the entire hash
 * table is removed.
 */
void
htrm(key, hash)
	char *key;			/* key for hash table entry */
	HASH *hash;			/* hash table */
{
	HASHBLK *htbrm();		/* remove hash table block */
	HASHBLK *htc;			/* first hash table block in chain */
	int hashval;			/* hash value for key */
	int hthash();			/* compute hash value */
	int i;				/* hash table index */

	if (key == NULL)
		{
		for (i = 0; i < hash->hashsiz; i++)
			if ((htc = (hash->hashtab)[i]) != NULL)
				htc = htbrm(key, htc);
		free((char *) hash);
		}
	else	{
		hashval = hthash(key, hash);
		if ((htc = (hash->hashtab)[hashval]) != NULL)
			(hash->hashtab)[hashval] = htbrm(key, htc);
		}
}



/*
 * htbrm() removes a hash table block identified by key. If key is null, the
 * entire chain is removed. Returns a pointer to the first block in the chain.
 */
HASHBLK *
htbrm(key, htc)
	char *key;			/* key string */
	HASHBLK *htc;			/* hash table block chain */
{
	HASHBLK *curblk;		/* current list block */
	HASHBLK *nxtblk;		/* next list block */
	HASHBLK *prvblk;		/* previous list block */

	if (key == NULL)
		while (htc != NULL)
			{
			nxtblk = htc->h_next;
			free(htc->h_key);
			free(htc->h_def);
			free((char *) htc);
			htc = nxtblk;
			}
	else	{
		/* first block is a special case */
		if (EQUAL(htc->h_key, key))
			{
			nxtblk = htc->h_next;
			free(htc->h_key);
			free(htc->h_def);
			free((char *) htc);
			htc = nxtblk;
			}
		else	{
			/* remainder of list */
			prvblk = htc;
			curblk = htc->h_next;
			while (curblk != NULL)
				if (EQUAL(curblk->h_key, key))
					{
					prvblk->h_next = curblk->h_next;
					free(curblk->h_key);
					free(curblk->h_def);
					free((char *) curblk);
					curblk = prvblk->h_next;
					break;
					}
				else	{
					prvblk = curblk;
					curblk = curblk->h_next;
					}
			}
		}
	return(htc);
}
