/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slrm() removes all instances of key from list slist. If key is null,
 * the entire list is removed.
 */
#include "macro.h"
#include "null.h"
#include "slist.h"

void
slrm(key, slist)
	char *key;			/* key string */
	SLIST *slist;			/* pointer to list head block */
{
	SLBLK *curblk;			/* current list block */
	SLBLK *nxtblk;			/* next list block */
	SLBLK *prvblk;			/* previous list block */

	if (key == NULL)
		{
		while (slist->head != NULL)
			{
			nxtblk = slist->head->next;
			free(slist->head->key);
			free((char *) slist->head);
			slist->head = nxtblk;
			}
		free((char *) slist);
		}
	else	{
		/* first block is a special case */
		while (slist->head != NULL)
			{
			if (!EQUAL(slist->head->key, key))
				break;
			nxtblk = slist->head->next;
			free(slist->head->key);
			free((char *) slist->head);
			slist->head = nxtblk;
			slist->nk--;
			}
		if (slist->head == NULL)
			slist->tail = NULL;

		/* remainder of list */
		if (slist->head != NULL)
			{
			prvblk = slist->head;
			curblk = slist->head->next;
			while (curblk != NULL)
				if (EQUAL(curblk->key, key))
					{
					if (curblk == slist->tail)
						slist->tail = prvblk;
					prvblk->next = curblk->next;
					free(curblk->key);
					free((char *) curblk);
					curblk = prvblk->next;
					slist->nk--;
					}
				else	{
					prvblk = curblk;
					curblk = curblk->next;
					}
			}
		}
}
