/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slsinit() returns a pointer to the head block of a new list, or null
 * pointer if out of memory.
 */
#include "null.h"
#include "slslist.h"

SLSLIST *
slsinit()
{
	char *malloc();			/* memory allocator */
	SLSLIST *slslist;		/* pointer to list head block */

	if ((slslist = (SLSLIST *) malloc(sizeof(SLSLIST))) == NULL)
		{
		warn("out of memory");
		return(NULL);
		}
	slslist->nk = 0;
	slslist->maxkey = slslist->maxstr = 0;
	slslist->head = slslist->curblk = slslist->tail = NULL;
	return(slslist);
}
