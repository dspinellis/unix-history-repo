/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slrewind() resets the current key pointer to the first key in list
 * slist.
 */
#include "slist.h"

void
slrewind(slist)
	SLIST *slist;			/* pointer to list head block */
{
	slist->curblk = slist->head;
}
