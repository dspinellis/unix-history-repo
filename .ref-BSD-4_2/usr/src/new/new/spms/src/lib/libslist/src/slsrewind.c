/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slsrewind() resets the current key pointer to the first key in list
 * slslist.
 */
#include "slslist.h"

void
slsrewind(slslist)
	SLSLIST *slslist;		/* pointer to list head block */
{
	slslist->curblk = slslist->head;
}
