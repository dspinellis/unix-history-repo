/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slinit() returns a pointer to the head block of a new list, or null
 * pointer if out of memory.
 */
#include "null.h"
#include "slist.h"

SLIST *
slinit()
{
	char *malloc();			/* memory allocator */
	SLIST *slist;			/* pointer to list head block */

	if ((slist = (SLIST *) malloc(sizeof(SLIST))) == NULL)
		{
		warn("out of memory");
		return(NULL);
		}
	slist->nk = 0;
	slist->maxkey = 0;
	slist->head = slist->curblk = slist->tail = NULL;
	return(slist);
}
