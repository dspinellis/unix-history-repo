/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slsprepend() saves a null-terminated key + string somewhere and inserts a
 * pointer to the key at the head of list slslist. Returns a pointer to
 * the somewhere, or a null pointer if out of memory.
 */
#include "macro.h"
#include "null.h"
#include "slslist.h"

char *
slsprepend(key, string, slslist)
	char *key;			/* key string */
	char *string;			/* non-key string */
	SLSLIST *slslist;		/* pointer to list head block */
{
	char *malloc();			/* memory allocator */
	char *strcpy();			/* string copy */
	int strlen();			/* string length */
	SLSBLK *slsbptr;		/* pointer to list block */
	unsigned int klen;		/* key length */
	unsigned int slen;		/* string length */

	if (slslist == NULL)
		return(NULL);
	klen = strlen(key);
	slen = strlen(string);
	slslist->maxkey = MAX(slslist->maxkey, klen);
	slslist->maxstr = MAX(slslist->maxstr, slen);
	if ((slsbptr = (SLSBLK *) malloc(sizeof(SLSBLK))) == NULL ||
	    (slsbptr->key = malloc(klen+1)) == NULL ||
	    (slsbptr->string = malloc(slen+1)) == NULL)
		{
		warn("out of memory");
		return(NULL);
		}
	strcpy(slsbptr->key, key);
	strcpy(slsbptr->string, string);
	if (slslist->head == NULL)
		{
		slsbptr->next = NULL;
		slslist->head = slslist->tail = slsbptr;
		}
	else	{
		slsbptr->next = slslist->head;
		slslist->head = slsbptr;
		}
	slslist->nk++;
	return(slsbptr->key);
}
