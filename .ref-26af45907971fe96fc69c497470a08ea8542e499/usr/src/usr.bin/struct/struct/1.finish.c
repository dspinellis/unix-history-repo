/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)1.finish.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include "def.h"
#include "1.incl.h"

fingraph()
	{
	/* if any entry statements, add a DUMVX with arcs to all entry statements */
	if (ENTLST)
		{
		ARC(START,0) = addum(ARC(START,0),ENTLST);
		freelst(ENTLST);
		}
	/* if any FMTVX, add a DUMVX with arcs to all FMTVX's */
	if (FMTLST)
		{
		ARC(START,0) = addum(ARC(START,0),FMTLST);
		freelst(FMTLST);
		}
	}

addum(v,lst)
VERT v;
struct list *lst;
	{
	VERT new;
	int count,i;
	struct list *ls;
	count = lslen(lst);		/* length of lst */
	new = create(DUMVX,1+count);
	ARC(new,0) = v;
	for (i = count, ls = lst; i >= 1; --i, ls = ls->nxtlist)
		{
		ASSERT(ls,addum);
		ARC(new,i) = ls->elt;
		}
	ASSERT(!ls, addum);
	return(new);
	}
