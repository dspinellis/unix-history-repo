/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * htinit() returns a pointer to a new hash table, or a null pointer if
 * out of memory.
 */
#include "null.h"
#include "hash.h"

HASH *
htinit(hashsiz)
	unsigned int hashsiz;		/* hash table size */
{
	char *calloc();			/* memory allocator + zero init */
	char *malloc();			/* memory allocator */
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
