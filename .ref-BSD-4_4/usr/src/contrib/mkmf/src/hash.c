/*
 * Copyright (c) 1983, 1985, 1991 Peter J. Nicklin.
 * Copyright (c) 1991 Version Technology.
 * All Rights Reserved.
 *
 * $License: VT.1.1 $
 * Redistribution and use in source and binary forms,  with or without
 * modification,  are permitted provided that the following conditions
 * are met:  (1) Redistributions of source code must retain the  above
 * copyright  notice,  this  list  of  conditions  and  the  following
 * disclaimer.  (2) Redistributions in binary form must reproduce  the
 * above  copyright notice,  this list of conditions and the following
 * disclaimer in the  documentation  and/or other  materials  provided
 * with  the  distribution.  (3) All advertising materials  mentioning
 * features or  use  of  this  software  must  display  the  following
 * acknowledgement:  ``This  product  includes  software  developed by
 * Version Technology.''  Neither the name of Version  Technology  nor
 * the  name  of  Peter J. Nicklin  may  be used to endorse or promote
 * products derived from this software without specific prior  written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY VERSION TECHNOLOGY ``AS IS''  AND  ANY
 * EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT LIMITED TO, THE
 * IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL  VERSION  TECHNOLOGY  BE
 * LIABLE  FOR ANY DIRECT,  INDIRECT,  INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR  CONSEQUENTIAL DAMAGES   (INCLUDING,   BUT   NOT   LIMITED   TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF  LIABILITY,  WHETHER  IN  CONTRACT,  STRICT LIABILITY,  OR  TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING  IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE,  EVEN  IF  ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 * Report problems and direct questions to nicklin@netcom.com
 *
 * $Header: hash.c,v 4.3 91/11/25 19:44:59 nicklin Exp $
 *
 * Author: Peter J. Nicklin
 */
#include "null.h"
#include "hash.h"
#include "macro.h"
#include "true.h"

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
		nocore();
		return(NULL);
		}
	ht->hashtab = pt;
	ht->headblk = -1;
	ht->thisblk = NULL;
	ht->hashsiz = hashsiz;
	ht->nk = 0;
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
		hash->thisblk = (hash->hashtab)[hashval] = htb;
		htb->h_sub = NULL;
		htb->h_tag = NULL;
		}
	else	{			/* found: free previous definition */
		if (htb->h_def != NULL)
			free(htb->h_def);
		}
	if (def == NULL)
		htb->h_def = NULL;
	else if ((htb->h_def = strsav(def)) == NULL)
		return(NULL);
	htb->h_val = val;
	hash->nk++;
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
			return(hash->thisblk = htb);	/* found */
	return(hash->thisblk = NULL);			/* not found */
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
				(void) htbrm(key, htc);
		free((char *) hash);
		}
	else	{
		hashval = hthash(key, hash);
		if ((htc = (hash->hashtab)[hashval]) != NULL)
			(hash->hashtab)[hashval] = htbrm(key, htc);
		hash->nk--;
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
			if (htc->h_def != NULL)
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
			if (htc->h_def != NULL)
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
					if (htc->h_def != NULL)
						free(curblk->h_def);
					free((char *) curblk);
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



/*
 * htnext() positions the current hash table pointer at the next hash
 * table block. Returns FALSE if no more blocks, otherwise TRUE.
 */
int
htnext(hash)
	HASH *hash;			/* hash table */
{
	register int i;			/* hash table index */

	if (hash->thisblk == NULL ||
	   ((hash->thisblk = hash->thisblk->h_next) == NULL))
		{
		for (i = hash->headblk+1; i < hash->hashsiz; i++)
			{
			if ((hash->hashtab)[i] != NULL)
				{
				  hash->thisblk = (hash->hashtab)[i];
				  break;
				}
			}
		hash->headblk = i;
		}
	return((hash->thisblk != NULL) ? TRUE : FALSE);
}



/*
 * htrewind() resets the current hash table block pointer to the beginning
 * of the hash table (actually one element before the beginning, so that
 * htnext() can increment the current hash table block pointer to the
 * first element.
 */
void
htrewind(hash)
	HASH *hash;			/* hash table */
{
	hash->headblk = -1;
	hash->thisblk = NULL;
}
