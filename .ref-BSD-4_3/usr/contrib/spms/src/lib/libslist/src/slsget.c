/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slsget() returns a pointer to the next block in list slslist, or null
 * upon reaching end of list.
 */
#include "null.h"
#include "slslist.h"

SLSBLK *
slsget(slslist)
	SLSLIST *slslist;		/* pointer to list head block */
{
	SLSBLK *slsbptr;		/* block pointer */

	if (slslist->curblk != NULL)
		{
		slsbptr = slslist->curblk;	
		slslist->curblk = slslist->curblk->next;
		return(slsbptr);
		}
	else
		return(NULL);
}
