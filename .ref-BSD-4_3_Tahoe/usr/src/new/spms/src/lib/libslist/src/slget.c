/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slget() returns a pointer to the next key in list slist, or null upon
 * reaching end of list.
 */
#include "null.h"
#include "slist.h"

char *
slget(slist)
	SLIST *slist;			/* pointer to list head block */
{
	char *key;			/* key pointer */

	if (slist->curblk != NULL)
		{
		key = slist->curblk->key;	
		slist->curblk = slist->curblk->next;
		return(key);
		}
	else
		return(NULL);
}
