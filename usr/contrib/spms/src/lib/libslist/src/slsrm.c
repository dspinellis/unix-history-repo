/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slsrm() removes all instances of key+string from list slslist.
 * If key is null the entire list is removed.
 */
#include "macro.h"
#include "null.h"
#include "slslist.h"

void
slsrm(key, slslist)
	char *key;			/* key string */
	SLSLIST *slslist;		/* pointer to list head block */
{
	SLSBLK *curblk;			/* current list block */
	SLSBLK *nxtblk;			/* next list block */
	SLSBLK *prvblk;			/* previous list block */

	if (key == NULL)
		{
		while (slslist->head != NULL)
			{
			nxtblk = slslist->head->next;
			free(slslist->head->key);
			free(slslist->head->string);
			free((char *) slslist->head);
			slslist->head = nxtblk;
			}
		free((char *) slslist);
		}
	else	{
		/* first block is a special case */
		while (slslist->head != NULL)
			{
			if (!EQUAL(slslist->head->key, key))
				break;
			nxtblk = slslist->head->next;
			free(slslist->head->key);
			free(slslist->head->string);
			free((char *) slslist->head);
			slslist->head = nxtblk;
			slslist->nk--;
			}
		if (slslist->head == NULL)
			slslist->tail = NULL;

		/* remainder of list */
		if (slslist->head != NULL)
			{
			prvblk = slslist->head;
			curblk = slslist->head->next;
			while (curblk != NULL)
				if (EQUAL(curblk->key, key))
					{
					if (curblk == slslist->tail)
						slslist->tail = prvblk;
					prvblk->next = curblk->next;
					free(curblk->key);
					free(curblk->string);
					free((char *) curblk);
					curblk = prvblk->next;
					slslist->nk--;
					}
				else	{
					prvblk = curblk;
					curblk = curblk->next;
					}
			}
		}
}
