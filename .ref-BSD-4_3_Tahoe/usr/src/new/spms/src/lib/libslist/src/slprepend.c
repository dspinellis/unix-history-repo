/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slprepend() saves a null-terminated key string somewhere and inserts a
 * pointer to the key at the head of list slist. Returns a pointer to
 * the somewhere, or a null pointer if out of memory.
 */
#include "macro.h"
#include "null.h"
#include "slist.h"

char *
slprepend(key, slist)
	char *key;			/* key string */
	SLIST *slist;			/* pointer to list head block */
{
	char *malloc();			/* memory allocator */
	char *strcpy();			/* string copy */
	int strlen();			/* string length */
	SLBLK *slbptr;			/* pointer to list block */
	unsigned int klen;		/* key length */

	if (slist == NULL)
		return(NULL);
	klen = strlen(key);
	slist->maxkey = MAX(slist->maxkey, klen);
	if ((slbptr = (SLBLK *) malloc(sizeof(SLBLK))) == NULL ||
	    (slbptr->key = malloc(klen+1)) == NULL)
		{
		warn("out of memory");
		return(NULL);
		}
	strcpy(slbptr->key, key);
	if (slist->head == NULL)
		{
		slbptr->next = NULL;
		slist->head = slist->tail = slbptr;
		}
	else	{
		slbptr->next = slist->head;
		slist->head = slbptr;
		}
	slist->nk++;
	return(slbptr->key);
}
