/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slpop() removes the (nkey+1)th item from list slist, if item matches key.
 * If key is null, the item is removed regardless. Keys are numbered
 * from 0 starting at the head of the list. Integer YES is returned if a
 * key was popped, otherwise NO.
 */
#include "macro.h"
#include "null.h"
#include "slist.h"
#include "yesno.h"

slpop(key, nkey, slist)
	char *key;			/* key string */
	int nkey;			/* number of key to be removed */
	SLIST *slist;			/* pointer to list head block */
{
	SLBLK *curblk;			/* current list block */
	SLBLK *nxtblk;			/* next list block */
	SLBLK *prvblk;			/* previous list block */

	if (nkey < 1)
		{
		/* first block is a special case */
		if (slist->head == NULL)
			goto nopop;
		else if (key != NULL && !EQUAL(slist->head->key, key))
			goto nopop;
		else	{
			nxtblk = slist->head->next;
			free(slist->head->key);
			free((char *) slist->head);
			slist->head = nxtblk;
			slist->nk--;
			}
		if (slist->head == NULL)
			slist->tail = NULL;
		}
	else	{
		/* remainder of list */
		if (slist->head == NULL)
			goto nopop;
		else	{
			prvblk = slist->head;
			curblk = slist->head->next;
			while (curblk != NULL && --nkey > 0)
				{
				prvblk = curblk;
				curblk = curblk->next;
				}
			if (curblk == NULL)
				goto nopop;
			else if (key != NULL && !EQUAL(curblk->key, key))
				goto nopop;
			else	{
				if (curblk == slist->tail)
					slist->tail = prvblk;
				prvblk->next = curblk->next;
				free(curblk->key);
				free((char *) curblk);
				slist->nk--;
				}
			}
		}
	return(YES);

nopop:	return(NO);
}
