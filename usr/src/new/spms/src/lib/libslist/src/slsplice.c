/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include "slist.h"

/*
 * slsplice() splices slist2 to the tail of slist1.
 */
void
slsplice(slist1, slist2)
	SLIST *slist1;			/* receiving list */
	SLIST *slist2;			/* list to be spliced */
{
	if (SLNUM(slist2) > 0)
		if (SLNUM(slist1) == 0)
			{
			SLNUM(slist1) = SLNUM(slist2);
			slist1->head = slist2->head;
			slist1->tail = slist2->tail;
			}
		else	{
			SLNUM(slist1) += SLNUM(slist2);
			slist1->tail->next = slist2->head;
			slist1->tail = slist2->tail;
			}
}
