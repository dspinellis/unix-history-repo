/* $Header: hash.c,v 1.1 85/03/14 15:38:16 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */
#include "null.h"
#include "hash.h"
#include "macro.h"

/*
 * hthash() returns a hash value for string, s.
 */
hthash(s, hash)
	register char *s;		/* string */
	HASH *hash;			/* hash table */
{
	register int hashval;		/* hash value for string */

	for (hashval = 0; *s != '\0'; s++)
		hashval += *s;
	return(hashval % hash->hashsiz);
}



/*
 * htinit() returns a pointer to a new hash table, or a null pointer if
 * out of memory.
 */
HASH *
htinit(hashsiz)
	unsigned int hashsiz;		/* hash table size */
{
	char *malloc();			/* allocate memory */
	char *calloc();			/* allocate and zero memory */
	HASH *ht;			/* pointer to hash table struct */
	HASHBLK **pt;			/* pointer to hash pointer table */

	if ((ht = (HASH *) malloc(sizeof(HASH))) == NULL ||
	    (pt = (HASHBLK **) calloc(hashsiz, sizeof(HASHBLK *))) == NULL)
		{
		warn("out of memory");
		return(NULL);
		}
	ht->hashtab = pt;
	ht->hashsiz = hashsiz;
	return(ht);
}



/*
 * htinstall() installs a new entry in a hash table if it doesn't already
 * exist. If it does, the old definition and value is superseded. Returns
 * a pointer to the entry, or null if out of memory.
 */
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
		htb->h_sub = NULL;
		htb->h_tag = NULL;
		}
	else	{			/* found */
		free(htb->h_def);	/* free previous definition */
		}
	if ((htb->h_def = strsav(def)) == NULL)
		return(NULL);
	htb->h_val = val;
	return(htb);
}



/*
 * htlookup() returns a pointer to a hash table entry if found, otherwise null.
 */
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
